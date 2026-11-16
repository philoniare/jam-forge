package io.forge.jam.node

import io.forge.jam.core.JamBytes
import io.forge.jam.core.trie.{InMemoryTrieBackend, StateTrie}
import io.forge.jam.network.{PeerStreamHandler, StreamKind}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class StateDownloadSpec extends AnyFunSuite with Matchers with LoopbackFixture:

  private val first = JamBytes(new Array[Byte](31))
  private val last = JamBytes(Array.fill[Byte](31)(0xff.toByte))

  private def serving(node: JamNode): Byte => Option[PeerStreamHandler] =
    kind => Option.when(kind == StreamKind.StateRequest)(node.sync.stateRequestHandler)

  test("a node's state is downloaded over CE 129 in many small responses and equals the original") {
    withNodes(2, "state-download") { nodes =>
      val Seq(a, b) = nodes.toSeq
      author(Seq(a), 1 to 6)
      val head = a.chain.best
      val (_, bToA) = join(a, b, serving)

      val store = new InMemoryTrieBackend()
      var requests = 0
      val ask = (conn: io.forge.jam.network.PeerConnection, start: JamBytes) =>
        requests += 1
        StateDownload.request(head.hash, maxBytes = 2048)(conn, start)
      val keys = new StateDownload(head.hash, head.stateRoot, store, ask).run(List(bToA))

      val original = a.chain.stateAt(head.hash).get.range(first, last, Int.MaxValue)._2
      keys shouldBe Right(original.size.toLong)
      requests should be > 5
      val copy = StateTrie.at(store, head.stateRoot)
      copy.range(first, last, Int.MaxValue)._2 shouldBe original
      info(s"${original.size} keys in $requests requests")
    }
  }

  test("a peer that changes a value, drops a key or stops early is refused, and the next peer is used") {
    withNodes(2, "state-download-liar") { nodes =>
      val Seq(a, b) = nodes.toSeq
      author(Seq(a), 1 to 4)
      val head = a.chain.best
      val (_, honest) = join(a, b, serving)
      val (_, liar) = join(a, b, serving)
      val real = StateDownload.request(head.hash, maxBytes = 4096)

      def run(lie: (List[Array[Byte]], IndexedSeq[(JamBytes, JamBytes)]) => (List[Array[Byte]], IndexedSeq[(JamBytes, JamBytes)]))
          : (Either[String, Long], Int) =
        var liarAsked = 0
        val ask = (conn: io.forge.jam.network.PeerConnection, start: JamBytes) =>
          if conn eq liar then
            liarAsked += 1
            real(conn, start).map((bn, ps) => if liarAsked == 1 then (bn, ps) else lie(bn, ps))
          else real(conn, start)
        val store = new InMemoryTrieBackend()
        val result = new StateDownload(head.hash, head.stateRoot, store, ask).run(List(liar, honest))
        if result.isRight then StateTrie.at(store, head.stateRoot).rootHash shouldBe head.stateRoot
        (result, liarAsked)

      val expected = a.chain.stateAt(head.hash).get.range(first, last, Int.MaxValue)._2.size.toLong
      val lies = List[(String, (List[Array[Byte]], IndexedSeq[(JamBytes, JamBytes)]) => (List[Array[Byte]], IndexedSeq[(JamBytes, JamBytes)]))](
        "a value changed" -> ((bn, ps) => (bn, ps.updated(0, ps(0)._1 -> JamBytes(ps(0)._2.toArray :+ 7.toByte)))),
        "the first key dropped" -> ((bn, ps) => (bn, ps.drop(1))),
        "the state said to end" -> ((bn, _) => (bn, IndexedSeq.empty))
      )
      lies.foreach { (what, lie) =>
        withClue(s"$what: ") {
          val (result, liarAsked) = run(lie)
          result shouldBe Right(expected)
          liarAsked shouldBe 2 // not asked again after the lie
        }
      }

      val alone = new StateDownload(
        head.hash,
        head.stateRoot,
        new InMemoryTrieBackend(),
        (conn, start) => real(conn, start).map((bn, _) => (bn, IndexedSeq.empty))
      ).run(List(liar))
      alone.isLeft shouldBe true
    }
  }

  test("a state root other than the block's is never satisfied") {
    withNodes(2, "state-download-root") { nodes =>
      val Seq(a, b) = nodes.toSeq
      author(Seq(a), 1 to 2)
      val head = a.chain.best
      val (_, bToA) = join(a, b, serving)
      val wrongRoot = io.forge.jam.core.Hashing.blake2b256(head.stateRoot.bytes)
      new StateDownload(head.hash, wrongRoot, new InMemoryTrieBackend(), StateDownload.request(head.hash))
        .run(List(bToA))
        .isLeft shouldBe true
    }
  }
