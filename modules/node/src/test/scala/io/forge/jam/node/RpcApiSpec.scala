package io.forge.jam.node

import java.util.Base64
import java.util.concurrent.ConcurrentLinkedQueue

import scala.jdk.CollectionConverters.*

import io.circe.Json
import io.circe.parser.parse
import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.protocol.state.{StateKey, StateKeys}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RpcApiSpec extends AnyFunSuite with Matchers with LoopbackFixture:

  private def b64(bytes: Array[Byte]): String = Base64.getEncoder.encodeToString(bytes)
  private def b64(h: Hash): String = b64(h.bytes.toArray)

  private final class Client(api: RpcApi):
    val notifications = new ConcurrentLinkedQueue[Json]()
    val session = new RpcSession(n => notifications.add(n): Unit)
    private var ids = 0

    def call(method: String, params: Json*): Either[Json, Json] =
      ids += 1
      val request = Json.obj(
        "jsonrpc" -> Json.fromString("2.0"),
        "id" -> Json.fromInt(ids),
        "method" -> Json.fromString(method),
        "params" -> Json.arr(params*)
      )
      val response = api.handle(request.noSpaces, session).getOrElse(fail("no response"))
      response.hcursor.get[Int]("id") shouldBe Right(ids)
      response.hcursor.get[String]("jsonrpc") shouldBe Right("2.0")
      response.hcursor.downField("error").focus.toLeft(response.hcursor.downField("result").focus.get)

    def result(method: String, params: Json*): Json = call(method, params*).fold(e => fail(s"$method: $e"), identity)
    def errorCode(method: String, params: Json*): Int =
      call(method, params*).fold(_.hcursor.get[Int]("code").toOption.get, r => fail(s"$method answered $r"))

    def notified(subscription: Json): List[Json] =
      notifications.asScala.toList.filter(_.hcursor.downField("params").get[Json]("subscription").contains(subscription))
        .map(_.hcursor.downField("params").get[Json]("result").toOption.get)

  private def str(s: String): Json = Json.fromString(s)
  private def num(n: Long): Json = Json.fromLong(n)

  private def withApi(blocks: Int)(body: (JamNode, RpcApi, Client) => Unit): Unit =
    withNodes(1, "rpc-api") { nodes =>
      val node = nodes.head
      if blocks > 0 then author(nodes, 1 to blocks)
      var syncing = false
      val api = new RpcApi(node.chain, node.spec.genesisHeaderHash, () => 3, () => syncing, node.preimages.submit)
      body(node, api, new Client(api))
    }

  test("blocks: best, finalized, parent and state root, with hashes in padded Base64") {
    withApi(blocks = 3) { (node, _, rpc) =>
      val best = node.chain.best
      rpc.result("bestBlock") shouldBe Json.obj("header_hash" -> str(b64(best.hash)), "slot" -> num(3))
      rpc.result("finalizedBlock") shouldBe
        Json.obj("header_hash" -> str(b64(node.spec.genesisHeaderHash)), "slot" -> num(0))

      val second = node.chain.parentHash(best.hash).get
      rpc.result("parent", str(b64(best.hash))) shouldBe Json.obj("header_hash" -> str(b64(second)), "slot" -> num(2))
      val first = node.chain.parentHash(second).get
      rpc.result("parent", str(b64(first))) shouldBe
        Json.obj("header_hash" -> str(b64(node.spec.genesisHeaderHash)), "slot" -> num(0))
      rpc.result("stateRoot", str(b64(best.hash))) shouldBe str(b64(best.stateRoot))
      rpc.result("syncState") shouldBe Json.obj("num_peers" -> num(3), "status" -> str("Completed"))
    }
  }

  test("state: statistics, a raw state value, and the services of a block") {
    withApi(blocks = 2) { (node, _, rpc) =>
      val best = str(b64(node.chain.best.hash))
      val statsKey = StateKeys.simpleKey(StateKeys.ACTIVITY_STATISTICS)
      val stats = node.chain.readRawState(statsKey).get.toArray
      rpc.result("statistics", best) shouldBe str(b64(stats))
      rpc.result("stateValue", best, str(b64(statsKey.toArray))) shouldBe str(b64(stats))
      rpc.result("stateValue", best, str(b64(Array.fill[Byte](31)(0x77)))) shouldBe Json.Null

      rpc.result("listServices", best) shouldBe Json.arr(num(0))
      val account = node.chain.readRawState(StateKey.computeServiceAccountKey(0)).get.toArray
      rpc.result("serviceData", best, num(0)) shouldBe str(b64(account))
      rpc.result("serviceData", best, num(12345)) shouldBe Json.Null
      rpc.result("serviceValue", best, num(0), str(b64(Array[Byte](0)))) shouldBe Json.Null

      val codeHash = Hash(java.util.Arrays.copyOfRange(account, 1, 33))
      val code = node.chain
        .readRawState(StateKey.computeServiceDataStateKey(0, 0xfffffffeL, JamBytes(codeHash.bytes.toArray)))
        .getOrElse(fail("service 0's code is not in the genesis state"))
        .toArray
      rpc.result("servicePreimage", best, num(0), str(b64(codeHash))) shouldBe str(b64(code))
      rpc.result("servicePreimageLen", best, num(0), str(b64(codeHash))) shouldBe num(code.length.toLong)
      rpc.result("serviceRequest", best, num(0), str(b64(codeHash)), num(code.length.toLong)).asArray.map(_.size) shouldBe Some(1)
      rpc.result("serviceRequest", best, num(0), str(b64(codeHash)), num(code.length + 1L)) shouldBe Json.Null
      rpc.result("servicePreimage", best, num(0), str(b64(Hash.zero))) shouldBe Json.Null
    }
  }

  test("parameters are the protocol constants under JIP-2's names") {
    withApi(blocks = 0) { (node, _, rpc) =>
      val v1 = rpc.result("parameters").hcursor.downField("V1")
      val config = node.chain.config
      v1.get[Int]("core_count") shouldBe Right(config.coresCount)
      v1.get[Int]("epoch_period") shouldBe Right(config.epochLength)
      v1.get[Int]("slot_period_sec") shouldBe Right(config.slotDuration)
      v1.get[Int]("rotation_period") shouldBe Right(config.rotationPeriod)
      v1.get[Int]("epoch_tail_start") shouldBe Right(config.ticketCutoff)
      v1.get[Int]("max_input") shouldBe Right(13_791_360)
      v1.keys.map(_.size) shouldBe Some(29)
    }
  }

  test("errors: an unknown block is code 1 with its hash, bad parameters -32602, an unknown method -32601") {
    withApi(blocks = 1) { (_, api, rpc) =>
      val unknown = Hash(Array.fill[Byte](32)(9))
      val error = rpc.call("stateRoot", str(b64(unknown))).left.getOrElse(fail("answered"))
      error.hcursor.get[Int]("code") shouldBe Right(1)
      error.hcursor.get[String]("data") shouldBe Right(b64(unknown))
      rpc.errorCode("parent", str(b64(unknown))) shouldBe 1
      rpc.errorCode("statistics", str(b64(unknown))) shouldBe 1

      rpc.errorCode("stateRoot") shouldBe -32602
      rpc.errorCode("stateRoot", str("zz")) shouldBe -32602
      rpc.errorCode("stateRoot", str(b64(Array[Byte](1, 2, 3)))) shouldBe -32602
      rpc.errorCode("serviceData", str(b64(unknown)), str("0")) shouldBe -32602
      rpc.errorCode("noSuchMethod") shouldBe -32601

      def code(text: String) = api.handle(text, rpc.session).get.hcursor.downField("error").get[Int]("code")
      code("not json") shouldBe Right(-32700)
      code("[1,2]") shouldBe Right(-32600)
      code("""{"jsonrpc":"2.0","id":1}""") shouldBe Right(-32600)
      code("""{"jsonrpc":"2.0","id":1,"method":"bestBlock","params":{"a":1}}""") shouldBe Right(-32602)
      api.handle("""{"jsonrpc":"2.0","method":"bestBlock"}""", rpc.session) shouldBe None
    }
  }

  test("a block subscription is told the head at once and each new head after, until it is cancelled") {
    withApi(blocks = 1) { (node, api, rpc) =>
      val best = rpc.result("subscribeBestBlock")
      val finalized = rpc.result("subscribeFinalizedBlock")
      api.refresh(List(rpc.session))
      rpc.notified(best).map(_.hcursor.get[Long]("slot").toOption.get) shouldBe List(1L)
      rpc.notified(finalized).map(_.hcursor.get[Long]("slot").toOption.get) shouldBe List(0L)
      rpc.notifications.asScala.map(_.hcursor.get[String]("method").toOption.get).toSet shouldBe
        Set("subscribeBestBlock", "subscribeFinalizedBlock")
      rpc.notifications.asScala.foreach(_.hcursor.downField("id").focus shouldBe None)

      api.refresh(List(rpc.session)) // nothing moved: nothing is sent
      rpc.notifications.size shouldBe 2

      author(List(node), 2 to 3)
      api.refresh(List(rpc.session))
      rpc.notified(best).map(_.hcursor.get[Long]("slot").toOption.get) shouldBe List(1L, 3L)
      node.chain.finalize(node.chain.parentHash(node.chain.best.hash).get).isRight shouldBe true
      api.refresh(List(rpc.session))
      rpc.notified(finalized).map(_.hcursor.get[Long]("slot").toOption.get) shouldBe List(0L, 2L)

      rpc.result("unsubscribeFinalizedBlock", best) shouldBe Json.False // another method's subscription
      rpc.result("unsubscribeBestBlock", best) shouldBe Json.True
      rpc.result("unsubscribeBestBlock", best) shouldBe Json.False
      author(List(node), 4 to 4)
      api.refresh(List(rpc.session))
      rpc.notified(best).size shouldBe 2
    }
  }

  test("a value subscription carries the block and the value, and is sent only when the value changes") {
    withApi(blocks = 1) { (node, api, rpc) =>
      val stats = rpc.result("subscribeStatistics", Json.False)
      val absent = rpc.result("subscribeServiceData", num(777), Json.False)
      api.refresh(List(rpc.session))
      author(List(node), 2 to 2)
      api.refresh(List(rpc.session))

      val sent = rpc.notified(stats)
      sent.map(_.hcursor.get[Long]("slot").toOption.get) shouldBe List(1L, 2L) // every block changes the statistics
      sent.last.hcursor.get[String]("header_hash") shouldBe Right(b64(node.chain.best.hash))
      sent.last.hcursor.get[String]("value") shouldBe
        Right(b64(node.chain.readRawState(StateKeys.simpleKey(StateKeys.ACTIVITY_STATISTICS)).get.toArray))
      rpc.notified(absent).map(_.hcursor.downField("value").focus) shouldBe List(Some(Json.Null))
    }
  }

  test("a connection holds a bounded number of subscriptions") {
    withApi(blocks = 0) { (_, _, rpc) =>
      (1 to RpcApi.MaxSubscriptions).foreach(_ => rpc.result("subscribeBestBlock"))
      rpc.errorCode("subscribeBestBlock") shouldBe 0
    }
  }

  test("a work-package not yet reported is reportable while its anchor is in the recent history of the block asked about") {
    withApi(blocks = 12) { (node, _, rpc) =>
      val chainUp = Iterator.iterate(node.chain.best.hash)(h => node.chain.parentHash(h).get).take(12).toVector
      val wp = str(b64(Array.fill[Byte](32)(7)))
      def status(at: Int, anchor: Int) = rpc.result("workPackageStatus", str(b64(chainUp(at))), wp, str(b64(chainUp(anchor))))
      def reportable(n: Int) = Json.obj("Reportable" -> Json.obj("remaining_blocks" -> num(n.toLong)))

      status(at = 0, anchor = 0) shouldBe reportable(8)
      status(at = 0, anchor = 1) shouldBe reportable(7)
      status(at = 0, anchor = 7) shouldBe reportable(1)
      status(at = 0, anchor = 8) shouldBe Json.obj("Failed" -> str("Anchor is too old"))
      status(at = 3, anchor = 0) shouldBe Json.obj("Failed" -> str("Anchor is on a different fork"))

      val unknown = str(b64(Array.fill[Byte](32)(9)))
      rpc.errorCode("workPackageStatus", str(b64(chainUp(0))), wp, unknown) shouldBe 1
      rpc.errorCode("workPackageStatus", unknown, wp, str(b64(chainUp(0)))) shouldBe 1

      val following = rpc.result("subscribeWorkPackageStatus", wp, str(b64(chainUp(0))), Json.False)
      rpc.session.subscriptions.size shouldBe 1
      rpc.errorCode("subscribeWorkPackageStatus", wp, unknown, Json.False) shouldBe 1
      following.isNumber shouldBe true
    }
  }

  test("segments that cannot be had are code 3, and a node with no guarantor in reach takes no work-package") {
    withApi(blocks = 1) { (_, _, rpc) =>
      val root = str(b64(Array.fill[Byte](32)(7)))
      rpc.errorCode("fetchSegments", root, Json.arr(num(0))) shouldBe 3
      rpc.errorCode("fetchWorkPackageSegments", root, Json.arr(num(0))) shouldBe 3
      rpc.errorCode("fetchSegments", root, Json.arr(num(-1))) shouldBe -32602
      rpc.errorCode("fetchSegments", root, num(0)) shouldBe -32602
      rpc.errorCode("submitWorkPackage", num(0), str("AAAA"), Json.arr()) shouldBe 0
      rpc.errorCode("submitWorkPackage", num(0), str("AAAA"), Json.arr(str("zz"))) shouldBe -32602
      rpc.errorCode("submitWorkPackageBundle", num(0), str("AAAA")) shouldBe 0
      rpc.errorCode("submitWorkPackageBundle", num(0)) shouldBe -32602
    }
  }

