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
    var unaudited: Set[Hash] = Set.empty
    override def isAudited(x: Hash): Boolean = !unaudited.contains(x)
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
    /** Each voter's last persisted (set, round, our votes). */
    val progress = mutable.Map.empty[Int, (Long, Long, List[VoteMessage])]
    /** Voter `i` as it starts: set 0, round 1, or the persisted round when `resume`. */
    def newVoter(i: Int, resume: Boolean = false): GrandpaVoter =
      val out = new GrandpaOutbound:
        def broadcastVote(m: VoteMessage): Unit = queue.enqueue(i -> m)
        def broadcastCommit(round: Long, setId: Long, c: Commit): Unit =
          commits += ((round, setId, c)); queue.enqueue(i -> (round, setId, c))
        def broadcastState(round: Long, setId: Long, slot: Long): Unit = ()
      new GrandpaVoter(
        chains(i),
        out,
        set,
        Some(keySets(i)),
        clock = () => now,
        onProgress = (s, r, votes) => progress(i) = (s, r, votes),
        resume = Option.when(resume)(progress.get(i).map((_, r, v) => (r, v))).flatten
      )
    val voters: mutable.ArrayBuffer[GrandpaVoter] = mutable.ArrayBuffer.from(keySets.indices.map(i => newVoter(i)))

    def run(
        millis: Long,
        deliver: (Int, Int) => Boolean = (_, _) => true,
        silent: (Int, Any) => Boolean = (_, _) => false
    ): Unit =
      val end = now + millis
      while now < end do
        now += 250
        voters.foreach(_.step())
        while queue.nonEmpty do
          val (from, msg) = queue.dequeue()
          voters.indices.filter(j => j != from && deliver(from, j) && !silent(from, msg)).foreach { j =>
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

  test("voters vote only for audited blocks, so finality waits below an unaudited one") {
    val blocks = new Blocks
    (1 to 5).foreach(i => blocks.add(s"b$i", i.toLong))
    val net = new Net(blocks, keys)
    net.chains.foreach(_.unaudited = Set(h("b3")))
    net.run(10_000)
    net.chains.map(_.finalizedHash).distinct shouldBe Seq(h("b2"))
    // Once b3 is audited the votes move on to the best block.
    net.chains.foreach(_.unaudited = Set.empty)
    net.run(10_000)
    net.chains.map(_.finalizedHash).distinct shouldBe Seq(h("b5"))
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

  test("a voter cut off across set changes adopts them from the finalized chain and votes again") {
    val markerKeys = Some(keys.map(_.ed25519Public).toList)
    val blocks = new Blocks
    blocks.add("b1", 1)
    blocks.add("b2", 2, marker = markerKeys)
    blocks.add("b3", 3)
    blocks.add("b4", 4, marker = markerKeys)
    val net = new Net(blocks, keys)
    val cutOff = 3
    net.run(30_000, (from, to) => from != cutOff && to != cutOff)
    net.voters.take(3).map(_.voterSet.setId).distinct shouldBe Seq(2L)
    net.voters(cutOff).voterSet.setId shouldBe 0L
    // Back online, it follows its peers' finality (as SyncService does), but their commits name set 2.
    net.chains(cutOff).finalizedHash = net.chains(0).finalizedHash
    net.voters(cutOff).step()
    net.voters(cutOff).voterSet.setId shouldBe 2L
    // Voter 0 goes away: the other two need the returning voter for 3 of 4.
    blocks.add("b5", 5)
    net.run(20_000, (from, to) => from != 0 && to != 0)
    net.chains(1).finalizedHash shouldBe h("b5")
    net.chains(cutOff).finalizedHash shouldBe h("b5")
  }

  test("a restarted voter joins the round that f + 1 voters are in") {
    val blocks = new Blocks
    (1 to 3).foreach(i => blocks.add(s"b$i", i.toLong))
    val net = new Net(blocks, keys)
    net.run(20_000)
    net.voters(1).round should be > 3L
    net.voters(3) = net.newVoter(3) // restarted: round 1 of the same set
    blocks.add("b4", 4)
    // Voter 0 goes away: voters 1 and 2 need the restarted one.
    net.run(20_000, (from, to) => from != 0 && to != 0)
    net.voters(3).round shouldBe net.voters(1).round
    net.chains(1).finalizedHash shouldBe h("b4")
  }

  test("a commit for a set we have not reached is replayed once the finalized chain moves us there") {
    val blocks = new Blocks
    blocks.add("b1", 1, marker = Some(keys.map(_.ed25519Public).toList))
    blocks.add("b2", 2)
    val net = new Net(blocks, keys)
    val cutOff = 3
    net.run(20_000, (from, to) => from != cutOff && to != cutOff)
    val (round, setId, commit) = net.commits.filter(_._2 == 1L).last
    net.chains(0).finalizedHash shouldBe h("b2")
    // The set 1 commit arrives just before we follow the peers' finality to b2 (soak3, node 5).
    net.voters(cutOff).onCommit(round, setId, commit).isLeft shouldBe true
    net.chains(cutOff).finalizedHash = h("b2")
    net.voters(cutOff).step()
    net.voters(cutOff).voterSet.setId shouldBe 1L
    net.voters(cutOff).round shouldBe round + 1
  }

  test("a voter left a round behind joins the next one from relayed votes and finishes it") {
    val blocks = new Blocks
    (1 to 3).foreach(i => blocks.add(s"b$i", i.toLong))
    val net = new Net(blocks, keys)
    net.run(10_000)
    blocks.add("b4", 4)
    // Voter 3 is cut off; once b4 is final, voter 0 stops voting, as a silent peer would.
    var lastRound0 = Long.MaxValue
    val silent0: (Int, Any) => Boolean = (from, msg) =>
      from == 0 && (msg match { case m: VoteMessage => m.round > lastRound0; case _ => lastRound0 < Long.MaxValue })
    var waited = 0
    while net.chains(1).finalizedHash != h("b4") && waited < 20_000 do
      net.run(250, (from, to) => from != 3 && to != 3, silent0); waited += 250
    lastRound0 = net.voters(1).round - 1
    blocks.add("b5", 5)
    net.run(10_000, (from, to) => from != 3 && to != 3, silent0)
    net.voters(3).round should be < net.voters(1).round
    // Back online, nobody resends the votes of the others' round: still stuck.
    net.run(10_000, silent = silent0)
    net.chains(1).finalizedHash shouldBe h("b4")
    // Voter 1 relays the round's votes it holds (GrandpaService does this while a round is stalled).
    net.voters(1).roundVotes.foreach(net.voters(3).onVote)
    net.voters(3).round shouldBe net.voters(1).round
    net.run(10_000, silent = silent0)
    net.chains(1).finalizedHash shouldBe h("b5")
    net.chains(3).finalizedHash shouldBe h("b5")
  }

  test("a voter one round behind joins the next round when f + 1 voters are in it") {
    val blocks = new Blocks
    blocks.add("b1", 1)
    val net = new Net(blocks, keys)
    var waited = 0
    while (net.voters(1).round < 2 || net.voters(1).roundVotes.map(_.message.signed.signerKey).distinct.size < 2) &&
      waited < 20_000
    do
      net.run(250); waited += 250
    net.voters(1).round shouldBe 2L
    val behind = net.newVoter(3) // round 1, missed the votes that completed it
    net.voters(1).roundVotes.foreach(behind.onVote)
    behind.round shouldBe 2L
  }

  test("a restarted voter resumes its persisted round with the votes it cast, so it never votes twice in a round") {
    val blocks = new Blocks
    (1 to 3).foreach(i => blocks.add(s"b$i", i.toLong))
    val net = new Net(blocks, keys)
    var waited = 0
    while net.voters(3).ourVotes.size < 2 && waited < 20_000 do
      net.run(250); waited += 250
    val before = net.voters(3)
    val restarted = net.newVoter(3, resume = true)
    restarted.round shouldBe before.round
    restarted.ourVotes.map(m => (m.message.kind, m.message.signed.vote)) shouldBe
      before.ourVotes.map(m => (m.message.kind, m.message.signed.vote))
    net.voters(3) = restarted
    blocks.add("b4", 4)
    net.run(10_000)
    net.chains.map(_.finalizedHash).distinct shouldBe Seq(h("b4"))
  }
