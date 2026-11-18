package io.forge.jam.node

import java.util.Base64

import io.circe.Json
import io.circe.parser.parse
import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.trie.{TrieNode, TrieNodeType}
import io.forge.jam.network.{PeerStreamHandler, StreamKind}
import io.forge.jam.protocol.state.{StateKey, StateKeys}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class MissingStateReadersSpec extends AnyFunSuite with Matchers with LoopbackFixture:

  private def b64(bytes: Array[Byte]): String = Base64.getEncoder.encodeToString(bytes)

  private def withBrokenBest(id: String)(body: (JamNode, ChainHead) => Unit): Unit =
    withNodes(1, id) { nodes =>
      val node = nodes.head
      author(nodes, 1 to 2)
      val best = node.chain.best
      val trie = node.chain.stateAt(best.hash).get
      val key = StateKeys.simpleKey(StateKeys.ACTIVITY_STATISTICS)
      trie.read(key) shouldBe defined
      val root = node.chain.stateBackendForTesting.readNode(best.stateRoot).get
      root.nodeType shouldBe TrieNodeType.Branch
      val bit = (key.toArray(0) & 0x80) != 0
      val child = Hash.wrapUnchecked((if bit then root.right else root.left).toArrayUnsafe)
      node.chain.stateBackendForTesting.loseNodesForTesting(child)
      body(node, best)
    }

  private def call(api: RpcApi, method: String, params: Json*): Json =
    val request = Json.obj(
      "jsonrpc" -> Json.fromString("2.0"),
      "id" -> Json.fromInt(1),
      "method" -> Json.fromString(method),
      "params" -> Json.arr(params*)
    )
    api.handle(request.noSpaces, new RpcSession(_ => ())).getOrElse(fail("no response"))

  test("RPC: a state read at a block whose state is no longer whole answers 'block not available'") {
    withBrokenBest("missing-state-rpc") { (node, best) =>
      val api = new RpcApi(node.chain, node.spec.genesisHeaderHash, () => 0, () => false, node.preimages.submit)
      val h = Json.fromString(b64(best.hash.bytes.toArray))
      val key = Json.fromString(b64(StateKeys.simpleKey(StateKeys.ACTIVITY_STATISTICS).toArray))
      for (method, params) <- List(
          "statistics" -> List(h),
          "stateValue" -> List(h, key),
          "beefyRoot" -> List(h)
        )
      do
        val error = call(api, method, params*).hcursor.downField("error")
        withClue(s"$method: ") {
          error.get[Int]("code") shouldBe Right(1)
          error.get[String]("message").toOption.get should include("is not available")
        }
    }
  }

  test("refine lookup: an anchor state found missing part way reads as lost, not as a failure inside the PVM") {
    withBrokenBest("missing-state-lookup") { (node, best) =>
      val lookup = new StateHistoricalLookup(node.chain).forAnchor(best.stateRoot)
      lookup.anchorAvailable shouldBe true // its root is still there
      (0L until 64L).foreach(id => lookup.serviceExists(id))
      (0L until 64L).foreach(id => lookup.historicalLookup(id, best.slot, Hash.zero))
      lookup.anchorLost shouldBe true
      lookup.unlessLost(Right(1)) shouldBe Left("lookup anchor state not held")
    }
  }

  test("refine lookup: a preimage found once is not read from the anchor's state again for the same package") {
    withNodes(1, "lookup-cache") { nodes =>
      val node = nodes.head
      author(nodes, 1 to 2)
      val best = node.chain.best
      val code = node.chain.stateViewOf(best).accumulation.serviceAccounts.find(_.id == 0).get.data.service.codeHash
      val lookup = new StateHistoricalLookup(node.chain).forAnchor(best.stateRoot)
      val first = lookup.historicalLookup(0L, best.slot, code)
      first shouldBe defined
      val root = node.chain.stateBackendForTesting.readNode(best.stateRoot).get
      node.chain.stateBackendForTesting.loseNodesForTesting(Hash.wrapUnchecked(root.left.toArrayUnsafe))
      node.chain.stateBackendForTesting.loseNodesForTesting(Hash.wrapUnchecked(root.right.toArrayUnsafe))
      lookup.historicalLookup(0L, best.slot, code).map(_.toSeq) shouldBe first.map(_.toSeq)
      lookup.anchorLost shouldBe false
    }
  }

  test("BEEFY: a commitment on a block whose state is no longer whole is not acted on, and nothing escapes") {
    withBrokenBest("missing-state-beefy") { (node, best) =>
      val beefy = new BeefyService(node.chain, node.distribution, None)
      noException should be thrownBy beefy.onCommitment(
        BeefyMessages.SignedCommitment(best.hash, best.slot, 0, Hash.zero, new Array[Byte](144), new Array[Byte](48))
      )
      beefy.proof(best.hash) shouldBe None
    }
  }

  test("CE 129: a state request for a block whose state is no longer whole is reset, like one for an unknown block") {
    withNodes(2, "missing-state-ce129") { nodes =>
      val Seq(a, b) = nodes.toSeq
      author(Seq(a), 1 to 2)
      val best = a.chain.best
      val root = a.chain.stateBackendForTesting.readNode(best.stateRoot).get
      a.chain.stateBackendForTesting.loseNodesForTesting(Hash.wrapUnchecked(root.left.toArrayUnsafe))
      val serving: JamNode => Byte => Option[PeerStreamHandler] =
        node => kind => Option.when(kind == StreamKind.StateRequest)(node.sync.stateRequestHandler)
      val (_, bToA) = join(a, b, serving)
      StateDownload.request(best.hash, timeoutMs = 5000)(bToA, JamBytes(new Array[Byte](31))) shouldBe None
      bToA.isOpen shouldBe true
    }
  }
