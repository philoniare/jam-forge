package io.forge.jam.node

import io.forge.jam.core.{ChainConfig, Hashing}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.node.GrandpaMessages.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

/** GRANDPA wire format and voting between in-memory voters. */
class GrandpaSpec extends AnyFunSuite with Matchers:

  private def h(name: String): Hash = Hashing.blake2b256(name.getBytes("UTF-8"))

  private final case class Block(parent: Hash, slot: Long, root: Hash, marker: Option[List[Array[Byte]]])

  /** Blocks shared by every voter; each voter keeps its own finalized block. */
  private final class Blocks:
    val genesis: Hash = h("genesis")
    val byHash = mutable.HashMap(genesis -> Block(Hash.zero, 0L, h("root-genesis"), None))
    var tip: Hash = genesis
    def add(name: String, slot: Long, marker: Option[List[Array[Byte]]] = None): Hash =
      val hash = h(name)
      byHash(hash) = Block(tip, slot, h(s"root-$name"), marker)
      tip = hash
      hash
    def vote(hash: Hash): Vote = Vote(hash, byHash(hash).root, byHash(hash).slot)

  private final class FakeChain(blocks: Blocks) extends GrandpaChain:
    var finalizedHash: Hash = blocks.genesis
    def finalized: Vote = blocks.vote(finalizedHash)
    def best: Vote = blocks.vote(blocks.tip)
    def slotOf(x: Hash): Option[Long] = blocks.byHash.get(x).map(_.slot)
    def parentOf(x: Hash): Option[Hash] = blocks.byHash.get(x).map(_.parent).filter(blocks.byHash.contains)
    def stateRootOf(x: Hash): Option[Hash] = blocks.byHash.get(x).map(_.root)
    def headerBytes(x: Hash): Option[Array[Byte]] = None
    def epochMarkerKeys(x: Hash): Option[List[Array[Byte]]] = blocks.byHash.get(x).flatMap(_.marker)
    def finalizeBlock(x: Hash): Either[String, Unit] =
      if !isAncestor(finalizedHash, x) then Left("not a descendant of the finalized block")
      else
        finalizedHash = x
        Right(())

  /** Voters exchanging messages through a queue; time moves only when the test advances it. */
  private final class Net(blocks: Blocks, keySets: IndexedSeq[ValidatorKeySet]):
    var now = 0L
    val queue = mutable.Queue.empty[(Int, Any)]
    val chains = keySets.map(_ => new FakeChain(blocks))
    val commits = mutable.ListBuffer.empty[(Long, Long, Commit)]
    val set = VoterSet(0, blocks.vote(blocks.genesis), keySets.map(_.ed25519Public))
    val voters: IndexedSeq[GrandpaVoter] = keySets.indices.map { i =>
      val out = new GrandpaOutbound:
        def broadcastVote(m: VoteMessage): Unit = queue.enqueue(i -> m)
        def broadcastCommit(round: Long, setId: Long, c: Commit): Unit =
          commits += ((round, setId, c)); queue.enqueue(i -> (round, setId, c))
        def broadcastState(round: Long, setId: Long, slot: Long): Unit = ()
      new GrandpaVoter(chains(i), out, set, Some(keySets(i)), clock = () => now)
    }

    def run(millis: Long, deliver: (Int, Int) => Boolean = (_, _) => true): Unit =
      val end = now + millis
      while now < end do
        now += 250
        voters.foreach(_.step())
        while queue.nonEmpty do
          val (from, msg) = queue.dequeue()
          voters.indices.filter(j => j != from && deliver(from, j)).foreach { j =>
            msg match
              case m: VoteMessage                          => voters(j).onVote(m)
              case (round: Long, setId: Long, c: Commit) => voters(j).onCommit(round, setId, c)
              case _                                       => ()
          }

  private val keys = (0 until 4).map(ValidatorKeySet.dev)

  test("a CE 149 vote is 177 bytes and round-trips with a valid signature") {
    val vote = Vote(h("b"), h("r"), 7L)
    val signed = sign(Prevote, vote, 3L, 1L, keys(0))
    val bytes = encodeVoteMessage(VoteMessage(3L, 1L, SignedMessage(Prevote, signed)))
    bytes.length shouldBe 177
    val decoded = decodeVoteMessage(bytes).toOption.get
    decoded.round shouldBe 3L
    decoded.setId shouldBe 1L
    decoded.message.signed.vote shouldBe vote
    verify(Prevote, decoded.message.signed, 3L, 1L) shouldBe true
    // The message type, round and set id are all signed.
    verify(Precommit, decoded.message.signed, 3L, 1L) shouldBe false
    verify(Prevote, decoded.message.signed, 4L, 1L) shouldBe false
    verify(Prevote, decoded.message.signed, 3L, 2L) shouldBe false
  }

  test("commit, justification, state and catch-up messages round-trip") {
    val vote = Vote(h("b"), h("r"), 9L)
    val pcs = keys.take(3).map(k => sign(Precommit, vote, 2L, 0L, k)).toList
    val commit = Commit(vote, pcs)
    val (round, setId, c) = decodeCommitMessage(encodeCommitMessage(2L, 0L, commit)).toOption.get
    (round, setId, c.target, c.precommits.map(_.vote)) shouldBe ((2L, 0L, vote, pcs.map(_.vote)))
    val headers = io.forge.jam.core.types.header.Header.headerCodec(ChainConfig.TINY.validatorCount, ChainConfig.TINY.epochLength)
    val j = decodeJustification(encodeJustification(Justification(2L, 0L, commit, Nil)), headers).toOption.get
    j.commit.precommits.size shouldBe 3
    decodeState(encodeState(5L, 1L, 42L)) shouldBe Right((5L, 1L, 42L))
    decodeCatchUpRequest(encodeCatchUpRequest(5L, 1L)) shouldBe Right((5L, 1L))
    val cu = decodeCatchUp(encodeCatchUp(CatchUp(4L, pcs, pcs, vote))).toOption.get
    (cu.round, cu.prevotes.size, cu.precommits.size, cu.base) shouldBe ((4L, 3, 3, vote))
  }

  test("supermajority is n - floor((n-1)/3): 5 of 6, 3 of 4") {
    VoterSet(0, Vote(Hash.zero, Hash.zero, 0), (0 until 6).map(i => Array.fill(32)(i.toByte))).threshold shouldBe 5
    VoterSet(0, Vote(Hash.zero, Hash.zero, 0), keys.map(_.ed25519Public)).threshold shouldBe 3
  }

  test("four voters finalize the best block") {
    val blocks = new Blocks
    (1 to 5).foreach(i => blocks.add(s"b$i", i.toLong))
    val net = new Net(blocks, keys)
    net.run(10_000)
    net.chains.map(_.finalizedHash).distinct shouldBe Seq(h("b5"))
    net.commits.map(_._3.target.hash) should contain(h("b5"))
  }

  test("finality follows a growing chain over many rounds") {
    val blocks = new Blocks
    val net = new Net(blocks, keys)
    (1 to 6).foreach { i =>
      blocks.add(s"b$i", i.toLong)
      net.run(6_000)
    }
    net.chains.map(_.finalizedHash).distinct shouldBe Seq(h("b6"))
    net.voters.map(_.round).min should be > 3L
  }

  test("two of four voters cannot finalize") {
    val blocks = new Blocks
    (1 to 3).foreach(i => blocks.add(s"b$i", i.toLong))
    val net = new Net(blocks, keys)
    // Only voters 0 and 1 hear each other: 2 votes of the 3 needed.
    net.run(10_000, deliver = (from, to) => from < 2 && to < 2)
    net.chains.map(_.finalizedHash).distinct shouldBe Seq(blocks.genesis)
  }

  test("votes stop at an epoch-marker block, which moves to the next voter set") {
    val blocks = new Blocks
    blocks.add("b1", 1)
    blocks.add("b2", 2)
    blocks.add("b3", 3, marker = Some(keys.map(_.ed25519Public).toList))
    blocks.add("b4", 4)
    val net = new Net(blocks, keys)
    net.run(10_000)
    net.voters.map(_.voterSet.setId).distinct shouldBe Seq(1L)
    net.commits.map(_._3.target.hash).distinct.head shouldBe h("b3")
    net.commits.filter(_._3.target.hash == h("b3")).map(_._2).distinct shouldBe Seq(0L)
    // Set 1 carries on to the best block.
    net.run(10_000)
    net.chains.map(_.finalizedHash).distinct shouldBe Seq(h("b4"))
    net.commits.filter(_._3.target.hash == h("b4")).map(_._2).distinct shouldBe Seq(1L)
  }

  test("a lagging node finalizes from a commit, and rejects one without a supermajority") {
    val blocks = new Blocks
    (1 to 3).foreach(i => blocks.add(s"b$i", i.toLong))
    val net = new Net(blocks, keys)
    net.run(10_000)
    val (round, setId, commit) = net.commits.last
    val follower = new FakeChain(blocks)
    val observer = new GrandpaVoter(follower, new GrandpaOutbound:
      def broadcastVote(m: VoteMessage): Unit = ()
      def broadcastCommit(round: Long, setId: Long, c: Commit): Unit = ()
      def broadcastState(round: Long, setId: Long, slot: Long): Unit = ()
    , net.set, None)
    observer.onCommit(round, setId, commit.copy(precommits = commit.precommits.take(2))).isLeft shouldBe true
    follower.finalizedHash shouldBe blocks.genesis
    observer.onCommit(round, setId, commit) shouldBe Right(None)
    follower.finalizedHash shouldBe commit.target.hash
  }
