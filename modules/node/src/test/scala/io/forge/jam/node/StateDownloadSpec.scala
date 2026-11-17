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

  test("a peer that drips tiny responses is left after three for the next peer, and the progress is kept") {
    withNodes(2, "state-download-drip") { nodes =>
      val Seq(a, b) = nodes.toSeq
      author(Seq(a), 1 to 4)
      val head = a.chain.best
      val (_, dripping) = join(a, b, serving)
      val (_, fast) = join(a, b, serving)
      val drip = StateDownload.request(head.hash, maxBytes = 200)
      val full = StateDownload.request(head.hash)
      var dripAsked = 0
      val fastStarts = scala.collection.mutable.ListBuffer.empty[JamBytes]
      val ask = (conn: io.forge.jam.network.PeerConnection, start: JamBytes) =>
        if conn eq dripping then
          dripAsked += 1
          drip(conn, start)
        else
          fastStarts += start
          full(conn, start)
      val store = new InMemoryTrieBackend()
      val result = new StateDownload(head.hash, head.stateRoot, store, ask).run(List(dripping, fast))
      val expected = a.chain.stateAt(head.hash).get.range(first, last, Int.MaxValue)._2.size.toLong
      result shouldBe Right(expected)
      dripAsked shouldBe 3
      fastStarts.size should (be >= 1 and be <= 2) // the rest, and the empty answer that says it was the rest
      fastStarts.head should not be first // the fast peer went on from where the dripping one left off
      StateTrie.at(store, head.stateRoot).rootHash shouldBe head.stateRoot
    }
  }

  test("three peers that are all slow are each left in turn and asked again, and the download finishes") {
    withNodes(2, "state-download-all-slow") { nodes =>
      val Seq(a, b) = nodes.toSeq
      author(Seq(a), 1 to 4)
      val head = a.chain.best
      val peers = (1 to 3).map(_ => join(a, b, serving)._2).toList
      val drip = StateDownload.request(head.hash, maxBytes = 600)
      val asked = scala.collection.mutable.ListBuffer.empty[Int]
      val ask = (conn: io.forge.jam.network.PeerConnection, start: JamBytes) =>
        asked += peers.indexWhere(_ eq conn)
        drip(conn, start)
      val store = new InMemoryTrieBackend()
      val result = new StateDownload(head.hash, head.stateRoot, store, ask).run(peers)
      result shouldBe Right(a.chain.stateAt(head.hash).get.range(first, last, Int.MaxValue)._2.size.toLong)
      asked.size should be > 9
      asked.take(10).toList shouldBe List(0, 0, 0, 1, 1, 1, 2, 2, 2, 0)
    }
  }

  test("a response that takes more than 5 s counts as slow, and an attempt that writes nothing for long ends") {
    withNodes(2, "state-download-stall") { nodes =>
      val Seq(a, b) = nodes.toSeq
      author(Seq(a), 1 to 2)
      val head = a.chain.best
      val (_, slow) = join(a, b, serving)
      val (_, other) = join(a, b, serving)
      var now = 0L
      val real = StateDownload.request(head.hash, maxBytes = 2048)
      var slowAsked = 0
      val ask = (conn: io.forge.jam.network.PeerConnection, start: JamBytes) =>
        if conn eq slow then
          slowAsked += 1
          now += 6000
        real(conn, start)
      new StateDownload(head.hash, head.stateRoot, new InMemoryTrieBackend(), ask, requestedBytes = 2048, clock = () => now)
        .run(List(slow, other)).isRight shouldBe true
      slowAsked shouldBe 3

      now = 0L
      var failedAsks = 0
      val failing = (_: io.forge.jam.network.PeerConnection, _: JamBytes) =>
        failedAsks += 1
        now += 30_000
        None
      val stalled = new StateDownload(head.hash, head.stateRoot, new InMemoryTrieBackend(), failing, clock = () => now)
        .run(List.fill(20)(other))
      stalled.isLeft shouldBe true
      stalled.left.toOption.get should include("nothing written")
      failedAsks shouldBe 5
    }
  }

  test("a CE 129 response over the size bound is refused and its sender told as malformed") {
    withNodes(2, "state-download-bound") { nodes =>
      val Seq(a, b) = nodes.toSeq
      author(Seq(a), 1 to 2)
      val head = a.chain.best
      val (_, bToA) = join(a, b, serving)
      var malformed = 0
      StateDownload.request(head.hash, maxBytes = 2048, malformed = _ => malformed += 1, bound = _ => 1000)(bToA, first) shouldBe None
      malformed shouldBe 1
      StateDownload.request(head.hash, maxBytes = 2048, malformed = _ => malformed += 1)(bToA, first).isDefined shouldBe true
      malformed shouldBe 1
      StateDownload.responseBound(2048) shouldBe 2048 + 31872 + 40 + 32L * 1024 * 1024
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
