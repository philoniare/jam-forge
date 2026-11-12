package io.forge.jam.node

import io.forge.jam.core.Hashing
import io.forge.jam.core.primitives.{Ed25519PublicKey, Hash}
import io.forge.jam.node.GrandpaMessages.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** The bounded structures behind the GRANDPA request tracking, the commits waiting for a block and the CE 133/134
  * receive buffers, flooded directly.
  */
class BoundedStructuresSpec extends AnyFunSuite with Matchers:

  private def h(i: Int): Hash = Hashing.blake2b256(s"h$i".getBytes("UTF-8"))
  private def peer(i: Int): Ed25519PublicKey = Ed25519PublicKey(h(i).bytes)

  test("request tracker: one unanswered request per peer, whatever keys it names") {
    var now = 0L
    val tracker = new RequestTracker[(Long, Long)](retryMillis = 30000, clock = () => now)
    // One peer naming thousands of rounds, and a handful of peers doing the same.
    tracker.tryStart((0L, 2L), peer(0)) shouldBe true
    (3L to 5000L).foreach(r => tracker.tryStart((0L, r), peer(0)) shouldBe false)
    (0 until 5000).foreach(i => tracker.tryStart((0L, 10000L + i), peer(i % 8)))
    tracker.size shouldBe 8
    // The same key is asked once, by whoever names it first.
    tracker.tryStart((0L, 2L), peer(99)) shouldBe false
  }

  test("request tracker: forgotten on a valid answer's state, on failure and on timeout") {
    var now = 0L
    val tracker = new RequestTracker[Long](retryMillis = 30000, clock = () => now)
    tracker.tryStart(1L, peer(0)) shouldBe true
    // Unanswered (failed, or still running): neither the key nor the peer is asked again before the retry time.
    now = 29999
    tracker.tryStart(1L, peer(1)) shouldBe false
    tracker.tryStart(2L, peer(0)) shouldBe false
    // Timed out: both may be asked again, and the old entry is gone.
    now = 30000
    tracker.tryStart(1L, peer(1)) shouldBe true
    tracker.size shouldBe 1
    // A valid answer frees the peer at once; the key stays until the state it brought makes it moot.
    tracker.succeeded(1L)
    tracker.tryStart(2L, peer(1)) shouldBe true
    tracker.tryStart(1L, peer(2)) shouldBe false
    tracker.retain(_ > 1L)
    tracker.size shouldBe 1
    tracker.tryStart(1L, peer(2)) shouldBe true
    // Everything expires in the end.
    now = 100000
    tracker.tryStart(3L, peer(3)) shouldBe true
    tracker.size shouldBe 1
  }

  private def commit(slot: Long): Commit = Commit(Vote(h(slot.toInt), Hash.zero, slot), Nil)

  test("pending commits: capped at the highest targets, pruned by finality and set, and still retried") {
    val pending = new PendingCommits(max = 64)
    (1L to 5000L).foreach(slot => pending.put(h(slot.toInt), 1, 0, commit(slot), Nil))
    (1L to 5000L).foreach(slot => pending.put(h(slot.toInt), 1, 0, commit(slot), Nil)) // replays
    pending.size shouldBe 64
    // The commit for the highest block is the one that finalizes everything: it is still there to retry on import.
    pending.take(h(5000)).map(_._3.target.slot) shouldBe Some(5000L)
    pending.take(h(1)) shouldBe None
    // Finality passing a target, or a set change, drops what can no longer finalize anything.
    pending.prune(finalizedSlot = 4990, setId = 0)
    pending.size shouldBe 9
    pending.prune(finalizedSlot = 4990, setId = 1)
    pending.size shouldBe 0
  }

  test("stream intake: keeps the protocol's messages and nothing a peer sends beyond them") {
    val intake = new StreamIntake(maxMessages = 2, maxBytes = 1024)
    intake.offer(new Array[Byte](100)) shouldBe true
    intake.offer(new Array[Byte](200)) shouldBe true
    (0 until 5000).foreach(_ => intake.offer(new Array[Byte](1)) shouldBe false)
    intake.size shouldBe 2
    intake.bytes shouldBe 300L
    intake.messages.map(_.length) shouldBe List(100, 200)

    // The byte total holds across messages, and a stream over it keeps nothing more, however small.
    val sized = new StreamIntake(maxMessages = 2, maxBytes = 1024)
    sized.offer(new Array[Byte](1000)) shouldBe true
    sized.offer(new Array[Byte](25)) shouldBe false
    sized.offer(new Array[Byte](1)) shouldBe false
    sized.bytes shouldBe 1000L
  }
