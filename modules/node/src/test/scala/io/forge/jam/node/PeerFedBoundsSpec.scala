package io.forge.jam.node

import java.nio.file.{Files, Path, Paths}

import io.circe.Decoder
import io.circe.parser.decode
import io.forge.jam.core.{ChainConfig, Hashing, JamBytes, constants}
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.tickets.{RingVrfSignatureSize, TicketEnvelope}
import io.forge.jam.crypto.Ed25519ZebraWrapper
import io.forge.jam.node.GrandpaMessages.*
import io.forge.jam.protocol.traces.Genesis
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import spire.math.UByte

import scala.collection.mutable

/** What a peer can make the protocol services hold: each test floods one peer-fed structure (replays, far-future
  * rounds and sets, unknown validators, forged signatures) and checks both that it stays within its bound and that
  * the legitimate message is still processed.
  */
class PeerFedBoundsSpec extends AnyFunSuite with Matchers:

  private def h(name: String): Hash = Hashing.blake2b256(name.getBytes("UTF-8"))

  // ---------------------------------------------------------------------------
  // GRANDPA: commits of sets we have not reached
  // ---------------------------------------------------------------------------

  private final case class Blk(parent: Hash, slot: Long, marker: Option[List[Array[Byte]]])

  private final class Chain extends GrandpaChain:
    val genesis: Hash = h("genesis")
    val blocks = mutable.HashMap(genesis -> Blk(Hash.zero, 0L, None))
    var tip: Hash = genesis
    var finalizedHash: Hash = genesis
    def add(name: String, slot: Long, marker: Option[List[Array[Byte]]] = None): Hash =
      val hash = h(name)
      blocks(hash) = Blk(tip, slot, marker)
      tip = hash
      hash
    def vote(x: Hash): Vote = Vote(x, h(s"root-${x.toHex}"), blocks(x).slot)
    def finalized: Vote = vote(finalizedHash)
    def best: Vote = vote(tip)
    def slotOf(x: Hash): Option[Long] = blocks.get(x).map(_.slot)
    def parentOf(x: Hash): Option[Hash] = blocks.get(x).map(_.parent).filter(blocks.contains)
    def stateRootOf(x: Hash): Option[Hash] = blocks.get(x).map(_ => h(s"root-${x.toHex}"))
    def headerBytes(x: Hash): Option[Array[Byte]] = None
    def epochMarkerKeys(x: Hash): Option[List[Array[Byte]]] = blocks.get(x).flatMap(_.marker)
    def finalizeBlock(x: Hash): Either[String, Unit] =
      if !isAncestor(finalizedHash, x) then Left("not a descendant of the finalized block")
      else
        finalizedHash = x
        Right(())

  private val quiet = new GrandpaOutbound:
    def broadcastVote(m: VoteMessage): Unit = ()
    def broadcastCommit(round: Long, setId: Long, c: Commit): Unit = ()
    def broadcastState(round: Long, setId: Long, finalizedSlot: Long): Unit = ()

  private val voterKeys = (0 until 4).map(ValidatorKeySet.dev)

  private def commitFor(chain: Chain, target: Hash, round: Long, setId: Long): Commit =
    val v = chain.vote(target)
    Commit(v, voterKeys.take(3).map(k => sign(Precommit, v, round, setId, k)).toList)

  test("GRANDPA: commits for far-future sets are bounded; the commit of the next set still finalizes") {
    val chain = new Chain
    val publicKeys = voterKeys.map(_.ed25519Public)
    val b1 = chain.add("b1", 1, marker = Some(publicKeys.toList)) // ends set 0
    val b2 = chain.add("b2", 2)
    val voter = new GrandpaVoter(chain, quiet, VoterSet(0, chain.vote(chain.genesis), publicKeys), None)

    // Unverifiable commits (the sets' keys are unknown) for thousands of set ids, highest first, with replays.
    val junk = Commit(chain.vote(b2), Nil)
    (5000L to 2L by -1).foreach(set => voter.onCommit(1, set, junk).isLeft shouldBe true)
    (2L to 5000L).foreach(set => voter.onCommit(7, set, junk))
    voter.futureCommitSets.size should be <= GrandpaVoter.MaxFutureCommitSets

    // The honest commit of the next set, arriving ahead of the set change, is kept in spite of the flood...
    voter.onCommit(1, 1, commitFor(chain, b2, 1, 1)).isLeft shouldBe true
    (6000L to 9000L).foreach(set => voter.onCommit(1, set, junk))
    voter.futureCommitSets should contain(1L)
    voter.futureCommitSets.size should be <= GrandpaVoter.MaxFutureCommitSets

    // ...and is replayed when set 0's commit finalizes the marker block.
    voter.onCommit(1, 0, commitFor(chain, b1, 1, 0)) shouldBe Right(None)
    voter.voterSet.setId shouldBe 1L
    chain.finalizedHash shouldBe b2
    voter.futureCommitSets should not contain 1L
  }

  // ---------------------------------------------------------------------------
  // Safrole tickets: CE 131 forwards and next-epoch tickets
  // ---------------------------------------------------------------------------

  private val baseDir = sys.props.get("jam.base.dir").map(Paths.get(_)).getOrElse(Paths.get("."))
  private val genesisPath = baseDir.resolve("jamtestvectors/traces/fuzzy/genesis.json")

  private def loadGenesis(): Option[Genesis] =
    if !Files.exists(genesisPath) then None
    else
      given Decoder[Genesis] = Genesis.decoder
      decode[Genesis](Files.readString(genesisPath)).toOption

  private def cleanup(dir: Path): Unit =
    Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p))

  private val devKeys = (0 until 6).map(ValidatorKeySet.dev)

  test("tickets: a replayed CE 131 ticket is queued for forwarding once; next-epoch tickets are capped") {
    val genesis = loadGenesis().getOrElse(cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available"))
    val spec = ChainSpec(
      id = "ticket-bounds",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )
    val dirA = Files.createTempDirectory("jam-tix-bounds-a")
    val dirB = Files.createTempDirectory("jam-tix-bounds-b")
    var nodeA: JamNode = null
    var nodeB: JamNode = null
    try
      // A makes valid tickets; B holds every validator key, so it is the proxy of each and takes them on CE 131.
      nodeA = new JamNode(spec, NodeConfig(dataDir = dirA, slotTicking = false)).start()
      nodeB = new JamNode(spec, NodeConfig(dataDir = dirB, slotTicking = false)).start()
      nodeA.enableAuthoring(devKeys)
      nodeB.tickets.setValidatorKeys(devKeys)
      nodeA.tickets.maybeGenerate(nodeA.chain.best.slot)
      val view = nodeA.chain.stateView()
      val epoch = view.timeslot / spec.config.epochLength
      val valid = nodeA.pools
        .takeTickets(epoch, TicketService.contestEntropy(view), Set.empty, Int.MaxValue, Int.MaxValue)
        .map(_._2)
      valid.size should be >= 2
      val maxTickets = spec.config.validatorCount * spec.config.ticketsPerValidator
      val tickets = nodeB.tickets

      // One valid ticket replayed: every copy is accepted, one forward is queued.
      val replayed = TicketCodec.encode(epoch, valid.head)
      (1 to 300).foreach(_ => tickets.receive(replayed, asProxy = true, "flood") shouldBe true)
      tickets.pendingForwardCount shouldBe 1
      nodeB.pools.ticketCount shouldBe 1

      // Forged proofs for this epoch and tickets for epochs we cannot check are refused and leave nothing behind.
      def forged(i: Int): TicketEnvelope =
        val proof = new Array[Byte](RingVrfSignatureSize)
        java.nio.ByteBuffer.wrap(proof).putInt(i)
        TicketEnvelope(UByte(i % 3), JamBytes(proof))
      (1 to 200).foreach(i => tickets.receive(TicketCodec.encode(epoch, forged(i)), asProxy = true, "flood") shouldBe false)
      (1 to 2000).foreach { i =>
        tickets.receive(TicketCodec.encode(epoch + 2 + i, forged(i)), asProxy = true, "flood") shouldBe false
      }
      tickets.pendingForwardCount shouldBe 1
      tickets.deferredCount shouldBe 0

      // Tickets for the next epoch cannot be verified yet: thousands of distinct ones, and replays of them, are
      // held up to the number of valid tickets a contest can have.
      (1 to 5000).foreach(i => tickets.receive(TicketCodec.encode(epoch + 1, forged(i)), asProxy = i % 2 == 0, "flood"))
      (1 to 5000).foreach(i => tickets.receive(TicketCodec.encode(epoch + 1, forged(i)), asProxy = true, "flood"))
      tickets.deferredCount should be <= maxTickets
      tickets.deferredCount should be > 0

      // The rest of the legitimate tickets are still pooled and queued, each once.
      valid.foreach(env => tickets.receive(TicketCodec.encode(epoch, env), asProxy = true, "honest") shouldBe true)
      valid.foreach(env => tickets.receive(TicketCodec.encode(epoch, env), asProxy = true, "honest") shouldBe true)
      nodeB.pools.ticketCount shouldBe valid.size
      tickets.pendingForwardCount shouldBe valid.size
      tickets.pendingForwardCount should be <= maxTickets

      // Before forwarding is due nothing is sent; once the lottery is over (slot index `ticketCutoff - 1`) what is
      // still queued is dropped, not sent to authors that can no longer include it.
      val e = spec.config.epochLength
      tickets.releaseForwards(epoch * e, appliedSlot = epoch * e + 5)
      tickets.pendingForwardCount shouldBe valid.size
      tickets.releaseForwards(epoch * e + spec.config.ticketCutoff - 1, appliedSlot = epoch * e)
      tickets.pendingForwardCount shouldBe 0
    finally
      if nodeA != null then nodeA.shutdown()
      if nodeB != null then nodeB.shutdown()
      cleanup(dirA)
      cleanup(dirB)
  }

/** The CE 144/145 flood used by [[DevnetAuditorSpec]] before its honest run: nothing the auditor is not auditing may
  * accumulate, and the run that follows shows announcements and judgments are still processed.
  */
object AuditFlood extends Matchers:

  private val rng = new scala.util.Random(144145)

  private def randomHash(): Hash =
    val b = new Array[Byte](32)
    rng.nextBytes(b)
    Hash(b)

  /** A CE 145 message: `Epoch ++ Validator Index ++ Validity ++ Report Hash ++ Signature`. */
  def judgment(index: Int, key: ValidatorKeySet, report: Hash, valid: Boolean, forge: Boolean = false): Array[Byte] =
    val context = if valid then constants.JAM_VALID_BYTES else constants.JAM_INVALID_BYTES
    val signature =
      if forge then Array.fill[Byte](64)(7) else Ed25519ZebraWrapper.sign(key.ed25519Secret, context ++ report.bytes)
    Array[Byte](0, 0, 0, 0, (index & 0xff).toByte, ((index >> 8) & 0xff).toByte, (if valid then 1 else 0).toByte) ++
      report.bytes ++ signature

  /** `keys(i)` must be validator `i` of the head's current set, and the auditor must track no block yet. */
  def run(auditor: AuditorService, pools: ExtrinsicPools, keys: IndexedSeq[ValidatorKeySet]): Unit =
    // Genuine judgments, valid and invalid, on reports no block holds: any validator can sign these, and anyone
    // can replay old ones.
    val genuine = (0 until 3000).map { i =>
      val v = i % keys.size
      judgment(v, keys(v), randomHash(), valid = i % 2 == 0)
    }
    genuine.foreach(auditor.receiveJudgment)
    genuine.take(500).foreach(auditor.receiveJudgment) // replays
    // Forged signatures, and indices outside the validator set.
    (0 until 1000).foreach(i => auditor.receiveJudgment(judgment(i % keys.size, keys(0), randomHash(), true, forge = true)))
    (0 until 1000).foreach(i => auditor.receiveJudgment(judgment(keys.size + i, keys(0), randomHash(), false)))

    // Announcements for blocks we do not have: from validators, from peers that are not validators, replayed,
    // and with oversized evidence.
    def head(n: Int): Array[Byte] =
      AuditSelection.encodeHead(randomHash(), n, AuditSelection.Announcement(Seq(0 -> randomHash()), new Array[Byte](64)))
    val evidence = new Array[Byte](96)
    val announcers = keys.take(3).map(_.ed25519Public)
    val announced = (0 until 3000).map(i => (announcers(i % announcers.size), head(i % 4)))
    announced.foreach((peer, hd) => auditor.receiveAnnouncement(peer, hd, evidence))
    announced.take(500).foreach((peer, hd) => auditor.receiveAnnouncement(peer, hd, evidence))
    (0 until 1000).foreach(_ => auditor.receiveAnnouncement(randomHash().bytes, head(0), evidence))
    (0 until 20).foreach(_ => auditor.receiveAnnouncement(announcers(0), head(1), new Array[Byte](1 << 20)))

    val (judgedReports, recorded, waitingJudgments, waitingAnnouncements) = auditor.bufferSizes
    // No block is tracked, so no judgment is recorded and none reaches the disputes pool.
    judgedReports shouldBe 0
    recorded shouldBe 0
    pools.judgmentCount shouldBe 0
    waitingJudgments should be <= keys.size * AuditorService.PendingJudgmentsPerValidator
    waitingJudgments should be > 0 // the genuine ones were taken, within the bound
    waitingAnnouncements should be <= announcers.size * AuditorService.PendingAnnouncementsPerPeer
    waitingAnnouncements should be > 0
