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
    var genesisNext: Option[List[Array[Byte]]] = None
    override def genesisNextKeys: Option[List[Array[Byte]]] = genesisNext
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
    val log = mutable.Map.empty[Int, mutable.ListBuffer[Any]]
    var onPrevote: (Int, VoteMessage) => Unit = (_, _) => ()
    /** Voter `i` as it starts: set 0, round 1, or the persisted round when `resume`. */
    def newVoter(i: Int, resume: Boolean = false): GrandpaVoter =
      val out = new GrandpaOutbound:
        def broadcastVote(m: VoteMessage): Unit =
          log.getOrElseUpdate(i, mutable.ListBuffer.empty) += m
          if m.message.kind == Prevote then onPrevote(i, m)
          queue.enqueue(i -> m)
        def broadcastCommit(round: Long, setId: Long, c: Commit): Unit =
          commits += ((round, setId, c)); queue.enqueue(i -> (round, setId, c))
        def broadcastState(round: Long, setId: Long, slot: Long): Unit = ()
      new GrandpaVoter(
        chains(i),
        out,
        set,
        Some(keySets(i)),
        clock = () => now,
        onProgress = (s, r, votes, durable) =>
          progress(i) = (s, r, votes)
          log.getOrElseUpdate(i, mutable.ListBuffer.empty) += ((r, votes, durable)),
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
    val headers = io.forge.jam.core.types.header.Header.headerCodec(ChainConfig.TINY.epochLength)
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

  test("each prevote and precommit is in a durably written round record before it is sent; a new round's record is not synced") {
    val blocks = new Blocks
    val net = new Net(blocks, keys)
    (1 to 4).foreach { i =>
      blocks.add(s"b$i", i.toLong)
      net.run(6_000)
    }
    net.chains.map(_.finalizedHash).distinct shouldBe Seq(h("b4"))
    keys.indices.foreach { i =>
      val events = net.log(i).toList
      val sent = events.collect { case m: VoteMessage if m.message.kind != PrimaryPropose => m }
      sent.size should be >= 4
      events.zipWithIndex.foreach {
        case (m: VoteMessage, at) if m.message.kind != PrimaryPropose =>
          withClue(s"voter $i, ${if m.message.kind == Prevote then "prevote" else "precommit"} of round ${m.round}: ") {
            events.take(at).reverse.collectFirst { case p: (Long, List[VoteMessage], Boolean) @unchecked => p } match
              case Some((round, votes, durable)) =>
                durable shouldBe true
                round shouldBe m.round
                votes should contain(m)
              case None => fail("sent before any record was written")
          }
        case _ => ()
      }
      val synced = events.collect { case (_, _, true) => () }.size
      synced shouldBe sent.size
      events.collect { case (_, _, false) => () }.size should be > 0
    }
  }

  test("a vote whose record cannot be written durably is not sent, now or by a resend, and no other is cast instead") {
    val blocks = new Blocks
    blocks.add("b1", 1)
    val chain = new FakeChain(blocks)
    var now = 0L
    val sent = mutable.ListBuffer.empty[VoteMessage]
    val set = VoterSet(0, blocks.vote(blocks.genesis), keys.map(_.ed25519Public))
    val voter = new GrandpaVoter(chain, new GrandpaOutbound:
      def broadcastVote(m: VoteMessage): Unit = sent += m
      def broadcastCommit(round: Long, setId: Long, c: Commit): Unit = ()
      def broadcastState(round: Long, setId: Long, slot: Long): Unit = ()
    , set, Some(keys(0)), clock = () => now,
      onProgress = (_, _, _, durable) => if durable then throw new java.io.IOException("injected: disk full"))
    (1 to 20).foreach { _ =>
      now += 250
      try voter.step() catch case _: java.io.IOException => ()
    }
    sent.filter(_.message.kind != PrimaryPropose) shouldBe empty
    voter.ourVotes shouldBe Nil
    voter.roundVotes.filter(_.message.signed.signerKey == keys(0).ed25519Key) shouldBe Nil
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

  // polkajam v0.8.0, six validators on a genesis whose staging set swaps three of them: the set that follows the
  // first epoch marker still finalizes with all six, and the one after the second marker cannot.
  test("the set after an epoch marker is that epoch's validators; the marker's keys take over one marker later") {
    val incoming = (4 until 8).map(ValidatorKeySet.dev)
    val nextEpoch = Some(incoming.map(_.ed25519Public).toList)
    val blocks = new Blocks
    blocks.add("b1", 1, marker = nextEpoch)
    blocks.add("b2", 2)
    blocks.add("b3", 3, marker = nextEpoch)
    blocks.add("b4", 4)
    val net = new Net(blocks, keys)
    net.run(40_000)
    // Set 1 began at b1 with the genesis validators, who finalized up to the next marker.
    net.chains.map(_.finalizedHash).distinct shouldBe Seq(h("b3"))
    net.voters.map(_.voterSet.setId).distinct shouldBe Seq(2L)
    net.voters(0).voterSet.members shouldBe incoming.map(_.ed25519Key).toSet
  }

  test("the first epoch marker brings in the genesis state's next-epoch validators") {
    val incoming = (4 until 8).map(ValidatorKeySet.dev)
    val blocks = new Blocks
    blocks.add("b1", 1, marker = Some(keys.map(_.ed25519Public).toList))
    blocks.add("b2", 2)
    val net = new Net(blocks, keys)
    net.chains.foreach(_.genesisNext = Some(incoming.map(_.ed25519Public).toList))
    net.run(20_000)
    net.chains.map(_.finalizedHash).distinct shouldBe Seq(h("b1"))
    net.voters(0).voterSet.members shouldBe incoming.map(_.ed25519Key).toSet
  }

  test("a voter following finality across markers takes each set's keys from the marker before it") {
    val incoming = (4 until 8).map(ValidatorKeySet.dev)
    val blocks = new Blocks
    blocks.add("b1", 1, marker = Some(incoming.map(_.ed25519Public).toList))
    blocks.add("b2", 2)
    val net = new Net(blocks, keys)
    val cutOff = 3
    net.run(20_000, (from, to) => from != cutOff && to != cutOff)
    net.chains(0).finalizedHash shouldBe h("b2")
    net.chains(cutOff).finalizedHash = h("b2")
    net.voters(cutOff).step()
    net.voters(cutOff).voterSet.setId shouldBe 1L
    net.voters(cutOff).voterSet.members shouldBe keys.map(_.ed25519Key).toSet
  }

  private def observerOf(blocks: Blocks): (GrandpaVoter, FakeChain) =
    val chain = new FakeChain(blocks)
    val quiet = new GrandpaOutbound:
      def broadcastVote(m: VoteMessage): Unit = ()
      def broadcastCommit(round: Long, setId: Long, c: Commit): Unit = ()
      def broadcastState(round: Long, setId: Long, slot: Long): Unit = ()
    (new GrandpaVoter(chain, quiet, VoterSet(0, blocks.vote(blocks.genesis), keys.map(_.ed25519Public)), None, clock = () => 0L), chain)

  private def vote(kind: Int, block: Vote, by: Int, round: Long = 1L): VoteMessage =
    VoteMessage(round, 0L, SignedMessage(kind, sign(kind, block, round, 0L, keys(by))))

  test("an equivocating voter counts towards every block, and both its votes are kept") {
    val blocks = new Blocks
    (1 to 2).foreach(i => blocks.add(s"b$i", i.toLong))
    val (voter, _) = observerOf(blocks)
    val (b1, b2) = (blocks.vote(h("b1")), blocks.vote(h("b2")))
    voter.onVote(vote(Prevote, b2, by = 1))
    voter.onVote(vote(Prevote, b2, by = 2))
    voter.onVote(vote(Prevote, b1, by = 3))
    voter.prevoteGhost(voter.roundState).map(_.hash) shouldBe Some(h("b1"))
    // Voter 3 prevotes again, differently: it can no longer be held to b1, so b2 has 3 of 4.
    voter.onVote(vote(Prevote, b2, by = 3))
    voter.prevoteGhost(voter.roundState).map(_.hash) shouldBe Some(h("b2"))
    voter.roundState.prevoteEquivocations.get(keys(3).ed25519Key).map((a, b) => (a.vote, b.vote)) shouldBe Some((b1, b2))
    // A third vote changes nothing, and the pair is relayed as evidence.
    voter.onVote(vote(Prevote, blocks.vote(blocks.genesis), by = 3))
    voter.roundState.prevoteEquivocations.size shouldBe 1
    voter.roundVotes.count(_.message.signed.signerKey == keys(3).ed25519Key) shouldBe 2
  }

  test("an equivocator's precommits finalize with two honest ones, and the commit carrying both verifies") {
    val blocks = new Blocks
    (1 to 2).foreach(i => blocks.add(s"b$i", i.toLong))
    val (voter, chain) = observerOf(blocks)
    val (b1, b2) = (blocks.vote(h("b1")), blocks.vote(h("b2")))
    val pair = List(sign(Precommit, b1, 1L, 0L, keys(3)), sign(Precommit, blocks.vote(blocks.genesis), 1L, 0L, keys(3)))
    val honest = List(sign(Precommit, b2, 1L, 0L, keys(1)), sign(Precommit, b2, 1L, 0L, keys(2)))
    voter.verifyCommit(1L, 0L, Commit(b2, honest ++ pair)) shouldBe Right(())
    // The same vote twice is not an equivocation, three votes are not a commit, and one honest vote is too few.
    voter.verifyCommit(1L, 0L, Commit(b2, honest ++ List(pair.head, pair.head))).isLeft shouldBe true
    voter.verifyCommit(1L, 0L, Commit(b2, honest ++ pair :+ sign(Precommit, b2, 1L, 0L, keys(3)))).isLeft shouldBe true
    voter.verifyCommit(1L, 0L, Commit(b2, honest.take(1) ++ pair)).isLeft shouldBe true
    (honest ++ pair).foreach(p => voter.onVote(VoteMessage(1L, 0L, SignedMessage(Precommit, p))))
    chain.finalizedHash shouldBe h("b2")
  }

  test("a commit is refused when its target or too many precommits name a state root other than ours") {
    val blocks = new Blocks
    (1 to 2).foreach(i => blocks.add(s"b$i", i.toLong))
    val (voter, _) = observerOf(blocks)
    val b2 = blocks.vote(h("b2"))
    val forged = b2.copy(stateRoot = h("another root"))
    val good = (0 until 3).map(i => sign(Precommit, b2, 1L, 0L, keys(i))).toList
    voter.verifyCommit(1L, 0L, Commit(b2, good)) shouldBe Right(())
    voter.verifyCommit(1L, 0L, Commit(forged, good)).isLeft shouldBe true
    // A precommit for another root is a vote for another block: 2 of 4 are left for b2.
    voter.verifyCommit(1L, 0L, Commit(b2, good.take(2) :+ sign(Precommit, forged, 1L, 0L, keys(2)))).isLeft shouldBe true
    voter.verifyCommit(1L, 0L, Commit(b2, good :+ sign(Precommit, forged, 1L, 0L, keys(3)))) shouldBe Right(())
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

  test("a voter knows which voters it has heard from since it started; after a restart that is nobody") {
    val blocks = new Blocks
    (1 to 3).foreach(i => blocks.add(s"b$i", i.toLong))
    val net = new Net(blocks, keys)
    val everyone = keys.map(_.ed25519Key).toSet
    var waited = 0
    def others(i: Int) = everyone - keys(i).ed25519Key
    while net.voters.indices.exists(i => net.voters(i).votersHeard != others(i)) && waited < 30_000 do
      net.run(250); waited += 250
    // Each has heard every other voter: where they are in the rounds is known from their votes.
    net.voters.indices.foreach(i => net.voters(i).votersHeard shouldBe others(i))

    // A restarted voter is back in its round with the votes it had cast, and has heard nobody: whether the
    // others are still in that round it cannot tell, so they are the ones a stalled round has it ask.
    val restarted = net.newVoter(3, resume = true)
    restarted.votersHeard shouldBe Set.empty
    net.voters(1).ourVotes should not be empty
    net.voters(1).ourVotes.foreach(restarted.onVote)
    restarted.votersHeard shouldBe Set(keys(1).ed25519Key)
  }

  test("after a checkpoint start the set id is learned only from a commit our voters alone could have signed") {
    val markerKeys = Some(keys.map(_.ed25519Public).toList)
    val blocks = new Blocks
    blocks.add("b1", 1)
    val b2 = blocks.add("b2", 2, marker = markerKeys)
    val b3 = blocks.add("b3", 3)
    val b4 = blocks.add("b4", 4)
    val b5 = blocks.add("b5", 5, marker = markerKeys)
    val b6 = blocks.add("b6", 6)
    val chain = new FakeChain(blocks)
    chain.finalizedHash = b3 // the block the chain was started at
    var adopted = List.empty[Long]
    val observer = new GrandpaVoter(chain, new GrandpaOutbound:
      def broadcastVote(m: VoteMessage): Unit = fail("nothing is voted while the set is not known")
      def broadcastCommit(round: Long, setId: Long, c: Commit): Unit = ()
      def broadcastState(round: Long, setId: Long, slot: Long): Unit = ()
    , VoterSet(0, blocks.vote(blocks.genesis), keys.map(_.ed25519Public)), None, onSetAdopted = s => adopted ::= s.setId)
    observer.restartAt(VoterSet(GrandpaVoter.UnknownSet, blocks.vote(b2), keys.map(_.ed25519Public)))
    observer.step()

    def commit(target: Hash, signedSet: Long, signers: Int = 3) =
      Commit(blocks.vote(target), keys.take(signers).map(k => sign(Precommit, blocks.vote(target), 4L, signedSet, k)).toList)

    observer.onCommit(4L, 3L, commit(blocks.byHash(b2).parent, 3L)).isLeft shouldBe true // an earlier set's
    observer.onCommit(4L, 9L, commit(b6, 9L)).isLeft shouldBe true // a later set's, past the marker
    observer.onCommit(4L, 7L, commit(b4, 7L, signers = 2)).isLeft shouldBe true // no supermajority
    observer.onCommit(4L, 7L, commit(b4, 6L)).isLeft shouldBe true // signed over another id than claimed
    observer.voterSet.setId shouldBe GrandpaVoter.UnknownSet
    chain.finalizedHash shouldBe b3
    val unknownBlock = Vote(h("elsewhere"), h("r"), 4L)
    observer.onCommit(4L, 7L, Commit(unknownBlock, keys.take(3).map(k => sign(Precommit, unknownBlock, 4L, 7L, k)).toList)) shouldBe
      Right(Some(unknownBlock.hash))
    observer.voterSet.setId shouldBe GrandpaVoter.UnknownSet

    observer.onCommit(4L, 7L, commit(b4, 7L)) shouldBe Right(None)
    observer.voterSet.setId shouldBe 7L
    chain.finalizedHash shouldBe b4
    observer.onCommit(5L, 7L, commit(b5, 7L).copy(precommits = keys.take(3).map(k => sign(Precommit, blocks.vote(b5), 5L, 7L, k)).toList)) shouldBe Right(None)
    chain.finalizedHash shouldBe b5
    observer.voterSet.setId shouldBe 8L
    adopted.reverse shouldBe List(GrandpaVoter.UnknownSet, 7L, 8L)
  }

  test("after a checkpoint start the next set's precommits, repackaged under a target of ours, teach no set id") {
    val markerKeys = Some(keys.map(_.ed25519Public).toList)
    val blocks = new Blocks
    blocks.add("b1", 1)
    val b2 = blocks.add("b2", 2, marker = markerKeys)
    val b3 = blocks.add("b3", 3)
    val b4 = blocks.add("b4", 4)
    val b5 = blocks.add("b5", 5, marker = markerKeys) // ends set 7; set 8 has the same keys
    val b6 = blocks.add("b6", 6)
    val chain = new FakeChain(blocks)
    chain.finalizedHash = b3
    val observer = new GrandpaVoter(chain, new GrandpaOutbound:
      def broadcastVote(m: VoteMessage): Unit = ()
      def broadcastCommit(round: Long, setId: Long, c: Commit): Unit = ()
      def broadcastState(round: Long, setId: Long, slot: Long): Unit = ()
    , VoterSet(0, blocks.vote(blocks.genesis), keys.map(_.ed25519Public)), None)
    observer.restartAt(VoterSet(GrandpaVoter.UnknownSet, blocks.vote(b2), keys.map(_.ed25519Public)))
    def precommits(v: Vote, round: Long, setId: Long) = keys.take(3).map(k => sign(Precommit, v, round, setId, k)).toList

    observer.onCommit(2L, 8L, Commit(blocks.vote(b4), precommits(blocks.vote(b6), 2L, 8L))).isLeft shouldBe true
    observer.voterSet.setId shouldBe GrandpaVoter.UnknownSet
    chain.finalizedHash shouldBe b3
    List(b4, b5).foreach { target =>
      val refused = observer.onCommit(3L, 8L, Commit(blocks.vote(target), precommits(blocks.vote(b5), 3L, 8L)))
      refused.isLeft shouldBe true
      refused.left.toOption.get should not be GrandpaVoter.BadPrecommitSignature // not held against the peer
    }
    observer.voterSet.setId shouldBe GrandpaVoter.UnknownSet
    chain.finalizedHash shouldBe b3
    val unheld = Vote(h("unheld"), h("r"), 5L)
    observer.onCommit(4L, 7L, Commit(blocks.vote(b4), precommits(unheld, 4L, 7L))) shouldBe Right(Some(unheld.hash))
    observer.voterSet.setId shouldBe GrandpaVoter.UnknownSet
    val pairOutOfRange = List(sign(Precommit, blocks.vote(b4), 4L, 7L, keys(2)), sign(Precommit, blocks.vote(b6), 4L, 7L, keys(2)))
    observer.onCommit(4L, 7L, Commit(blocks.vote(b4), precommits(blocks.vote(b4), 4L, 7L).take(2) ++ pairOutOfRange))
      .isLeft shouldBe true
    observer.voterSet.setId shouldBe GrandpaVoter.UnknownSet

    observer.onCommit(4L, 7L, Commit(blocks.vote(b4), precommits(blocks.vote(b4), 4L, 7L))) shouldBe Right(None)
    observer.voterSet.setId shouldBe 7L
    chain.finalizedHash shouldBe b4
    val _ = b5
  }

  test("after a checkpoint start with the set id given, the voter votes in that set and learns no other") {
    val blocks = new Blocks
    val b1 = blocks.add("b1", 1)
    val b2 = blocks.add("b2", 2)
    val b3 = blocks.add("b3", 3)
    val chain = new FakeChain(blocks)
    chain.finalizedHash = b1
    var now = 0L
    val cast = mutable.ListBuffer.empty[VoteMessage]
    val voter = new GrandpaVoter(chain, new GrandpaOutbound:
      def broadcastVote(m: VoteMessage): Unit = cast += m
      def broadcastCommit(round: Long, setId: Long, c: Commit): Unit = ()
      def broadcastState(round: Long, setId: Long, slot: Long): Unit = ()
    , VoterSet(0, blocks.vote(blocks.genesis), keys.map(_.ed25519Public)), Some(keys(0)), clock = () => now)
    voter.restartAt(VoterSet(7L, blocks.vote(b1), keys.map(_.ed25519Public)))
    def commit(target: Hash, setId: Long) =
      Commit(blocks.vote(target), keys.take(3).map(k => sign(Precommit, blocks.vote(target), 1L, setId, k)).toList)
    voter.onCommit(1L, 8L, commit(b2, 8L)).isLeft shouldBe true
    voter.onCommit(1L, 6L, commit(b2, 6L)).isLeft shouldBe true
    voter.voterSet.setId shouldBe 7L
    chain.finalizedHash shouldBe b1
    now += 2500
    voter.step()
    cast.map(_.setId).distinct.toList shouldBe List(7L)
    voter.onCommit(1L, 7L, commit(b3, 7L)) shouldBe Right(None)
    chain.finalizedHash shouldBe b3
  }

  test("a paused voter signs no vote, whatever it hears, and votes once unpaused") {
    val blocks = new Blocks
    blocks.add("b1", 1)
    blocks.add("b2", 2)
    val chain = new FakeChain(blocks)
    var now = 0L
    var paused = true
    val cast = mutable.ListBuffer.empty[VoteMessage]
    val set = VoterSet(0, blocks.vote(blocks.genesis), keys.map(_.ed25519Public))
    val primary = keys.indexWhere(k => set.primary(1L) == k.ed25519Key)
    val voter = new GrandpaVoter(chain, new GrandpaOutbound:
      def broadcastVote(m: VoteMessage): Unit = cast += m
      def broadcastCommit(round: Long, setId: Long, c: Commit): Unit = ()
      def broadcastState(round: Long, setId: Long, slot: Long): Unit = ()
    , set, Some(keys(primary)), clock = () => now, paused = () => paused)
    (1 to 40).foreach { _ =>
      now += 250
      voter.step()
    }
    keys.indices.filter(_ != primary).foreach { i =>
      voter.onVote(vote(Prevote, blocks.vote(h("b2")), i))
      voter.onVote(vote(Precommit, blocks.vote(h("b2")), i))
    }
    cast shouldBe empty
    paused = false
    voter.step()
    cast.map(_.message.kind).toSet should contain(Prevote)
  }

  test("a primary's proposal of a block we have not audited does not make us prevote it") {
    val blocks = new Blocks
    val b1 = blocks.add("b1", 1)
    val b2 = blocks.add("b2", 2)
    val b3 = blocks.add("b3", 3)
    val set = VoterSet(0, blocks.vote(blocks.genesis), keys.map(_.ed25519Public))
    val round = 1L
    val primary = keys.find(k => set.primary(round) == k.ed25519Key).get
    val us = keys.find(_ ne primary).get
    val chain = new FakeChain(blocks)
    chain.unaudited = Set(b3) // b3 makes a report available that we have not finished auditing
    var now = 0L
    val cast = mutable.ListBuffer.empty[VoteMessage]
    val voter = new GrandpaVoter(chain, new GrandpaOutbound:
      def broadcastVote(m: VoteMessage): Unit = cast += m
      def broadcastCommit(round: Long, setId: Long, c: Commit): Unit = ()
      def broadcastState(round: Long, setId: Long, slot: Long): Unit = ()
    , set, Some(us), clock = () => now, gossipMillis = 1000L)

    voter.onVote(VoteMessage(round, 0L, SignedMessage(PrimaryPropose, sign(PrimaryPropose, blocks.vote(b3), round, 0L, primary))))
    now += 2500
    voter.step()
    val prevotes = cast.filter(_.message.kind == Prevote).map(_.message.signed.vote.hash).toList
    prevotes should have size 1
    prevotes.head should not be b3
    prevotes.head shouldBe b2 // the best audited block, as without the proposal
    val _ = b1
  }

  private final class RoundTwo:
    val blocks = new Blocks
    val a1 = blocks.add("a1", 1)
    val a2 = blocks.add("a2", 2)
    val a3 = blocks.add("a3", 3)
    val a4 = blocks.add("a4", 4)
    blocks.tip = blocks.genesis
    val x1 = blocks.add("x1", 1)
    blocks.tip = a1
    val c2 = blocks.add("c2", 2)
    val c3 = blocks.add("c3", 3)
    val set = VoterSet(0, blocks.vote(blocks.genesis), keys.map(_.ed25519Public))
    val primary = keys.find(k => set.primary(2) == k.ed25519Key).get
    val us = keys.find(_ ne primary).get
    val others = keys.filter(_ ne us)
    val chain = new FakeChain(blocks)
    var now = 0L
    val cast = mutable.ListBuffer.empty[VoteMessage]
    val voter = new GrandpaVoter(chain, new GrandpaOutbound:
      def broadcastVote(m: VoteMessage): Unit = cast += m
      def broadcastCommit(round: Long, setId: Long, c: Commit): Unit = ()
      def broadcastState(round: Long, setId: Long, slot: Long): Unit = ()
    , set, Some(us), clock = () => now, gossipMillis = 1000L)

    def send(kind: Int, v: Vote, round: Long, by: ValidatorKeySet): Boolean =
      voter.onVote(VoteMessage(round, 0L, SignedMessage(kind, sign(kind, v, round, 0L, by))))
    def prevotes(round: Long): List[Hash] =
      cast.filter(m => m.round == round && m.message.kind == Prevote).map(_.message.signed.vote.hash).toList

    others.foreach(k => send(Prevote, blocks.vote(a3), 1L, k))
    now = 2000
    voter.step()
    prevotes(1) shouldBe List(c3)
    others.take(2).foreach(k => send(Precommit, blocks.vote(a1), 1L, k))

    voter.round shouldBe 2L
    chain.finalizedHash shouldBe a1
    private val round1 = voter.roundState(1L).get
    voter.estimate(round1).map(_.hash) shouldBe Some(a1)
    voter.prevoteGhost(round1).map(_.hash) shouldBe Some(a3)
    voter.roundState.primaryProposal shouldBe None
    voter.prevoteFloor(voter.roundState).hash shouldBe a1
    prevotes(2) shouldBe Nil

    def propose(proposal: Vote, by: ValidatorKeySet = primary): Unit = send(PrimaryPropose, proposal, 2L, by): Unit
    def prevote(): Hash =
      now += 2000
      voter.step()
      prevotes(2) should have size 1
      prevotes(2).head

  test("a primary's proposal within the last prevote GHOST and audited moves our prevote onto its fork") {
    val t = new RoundTwo
    t.propose(t.blocks.vote(t.a2))
    t.voter.prevoteFloor(t.voter.roundState).hash shouldBe t.a2
    t.prevote() shouldBe t.a2 // our best, c3, is not on a2's chain
    val u = new RoundTwo
    u.propose(u.blocks.vote(u.a3))
    u.prevote() shouldBe u.a3
  }

  test("a primary's proposal above the last prevote GHOST is not taken") {
    val t = new RoundTwo
    t.propose(t.blocks.vote(t.a4))
    t.voter.roundState.primaryProposal.map(_.hash) shouldBe Some(t.a4)
    t.voter.prevoteFloor(t.voter.roundState).hash shouldBe t.a1
    t.prevote() shouldBe t.c3
  }

  test("a primary's proposal is not taken when it, or a block below it above the estimate, is not audited") {
    val t = new RoundTwo
    t.chain.unaudited = Set(t.a2)
    t.propose(t.blocks.vote(t.a3))
    t.voter.prevoteFloor(t.voter.roundState).hash shouldBe t.a1
    t.prevote() shouldBe t.c3
    val u = new RoundTwo
    u.chain.unaudited = Set(u.a2)
    u.propose(u.blocks.vote(u.a2))
    u.voter.prevoteFloor(u.voter.roundState).hash shouldBe u.a1
    u.prevote() shouldBe u.c3
  }

  test("a primary's proposal of the estimate, of a block not above it, or of a block we do not hold has no effect") {
    def check(proposal: RoundTwo => Vote): Unit =
      val t = new RoundTwo
      t.propose(proposal(t))
      t.voter.roundState.primaryProposal shouldBe Some(proposal(t))
      t.voter.prevoteFloor(t.voter.roundState).hash shouldBe t.a1
      t.prevote() shouldBe t.c3
    check(t => t.blocks.vote(t.a1)) // the estimate itself
    check(t => t.blocks.vote(t.blocks.genesis)) // below it
    check(t => t.blocks.vote(t.x1)) // on another fork
    check(t => t.blocks.vote(t.a2).copy(stateRoot = h("another root"))) // a2 with a root other than ours
    check(_ => Vote(h("elsewhere"), h("r"), 2L)) // a block we do not hold
  }

  test("a proposal from a voter that is not the round's primary is ignored") {
    val t = new RoundTwo
    val notPrimary = t.others.find(_ ne t.primary).get
    t.propose(t.blocks.vote(t.a2), by = notPrimary)
    t.voter.roundState.primaryProposal shouldBe None
    t.prevote() shouldBe t.c3
  }

  test("finality keeps up while a Byzantine primary proposes the newest block, which nobody has audited yet") {
    val blocks = new Blocks
    val net = new Net(blocks, keys)
    val byzantine = keys(0) // votes honestly, but proposes the chain tip in the rounds it leads
    var refused = 0
    var unauditedPrevotes = 0
    net.onPrevote = (i, m) =>
      if i != 0 then
        val v = net.voters(i)
        val rs = v.roundState(m.round).get
        if !net.chains(i).isAudited(m.message.signed.vote.hash) then unauditedPrevotes += 1
        val floor = v.prevoteFloor(rs)
        rs.primaryProposal.foreach { b =>
          if b != floor && net.chains(i).isAncestor(floor.hash, b.hash) then refused += 1
        }
    val ownProposals: (Int, Any) => Boolean = (from, msg) =>
      from == 0 && (msg match { case m: VoteMessage => m.message.kind == PrimaryPropose; case _ => false })
    def injectProposals(): Unit =
      (1 until 4).foreach { j =>
        val v = net.voters(j)
        if net.set.primary(v.round) == byzantine.ed25519Key && v.roundState.primaryProposal.isEmpty then
          val tip = blocks.vote(blocks.tip)
          v.onVote(VoteMessage(v.round, 0L, SignedMessage(PrimaryPropose, sign(PrimaryPropose, tip, v.round, 0L, byzantine))))
      }
    (1 to 8).foreach { i =>
      val b = blocks.add(s"b$i", i.toLong)
      net.chains.foreach(_.unaudited = Set(b)) // each block takes 10 s to audit
      (1 to 48).foreach { tick =>
        if tick == 41 then net.chains.foreach(_.unaudited = Set.empty)
        net.run(250, silent = ownProposals)
        injectProposals()
      }
    }
    var waited = 0
    while net.chains.exists(_.finalizedHash != h("b8")) && waited < 10_000 do
      net.run(250, silent = ownProposals); injectProposals(); waited += 250
    net.chains.map(_.finalizedHash).distinct shouldBe Seq(h("b8"))
    refused should be > 0
    unauditedPrevotes shouldBe 0
  }

