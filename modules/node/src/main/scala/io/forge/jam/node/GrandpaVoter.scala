package io.forge.jam.node

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.primitives.Hash
import io.forge.jam.node.GrandpaMessages.*

import scala.collection.mutable

/** What the GRANDPA voter needs from the chain. Slots grow along every chain, so they order ancestors. */
trait GrandpaChain:
  def finalized: Vote
  def best: Vote
  def slotOf(h: Hash): Option[Long]
  def parentOf(h: Hash): Option[Hash]
  def stateRootOf(h: Hash): Option[Hash]
  def headerBytes(h: Hash): Option[Array[Byte]]

  /** Ed25519 keys of the epoch marker in block `h`'s header, when it has one. */
  def epochMarkerKeys(h: Hash): Option[List[Array[Byte]]]
  def finalizeBlock(h: Hash): Either[String, Unit]

  /** `a` is `b` or one of its ancestors. */
  def isAncestor(a: Hash, b: Hash): Boolean =
    if a == b then true
    else
      slotOf(a) match
        case None => false
        case Some(aSlot) =>
          var cursor = parentOf(b)
          var found = false
          while !found && cursor.exists(c => slotOf(c).exists(_ >= aSlot)) do
            if cursor.contains(a) then found = true else cursor = cursor.flatMap(parentOf)
          found

/** What the voter sends. */
trait GrandpaOutbound:
  def broadcastVote(m: VoteMessage): Unit
  def broadcastCommit(round: Long, setId: Long, c: Commit): Unit
  def broadcastState(round: Long, setId: Long, finalizedSlot: Long): Unit

/** A GRANDPA voter set: its id, the block it starts from and its Ed25519 keys. */
final case class VoterSet(setId: Long, base: Vote, keys: IndexedSeq[Array[Byte]]):
  val members: Set[Seq[Byte]] = keys.map(_.toSeq).toSet
  def size: Int = keys.size

  /** Votes needed for a supermajority: n − ⌊(n−1)/3⌋. */
  def threshold: Int = size - (size - 1) / 3

  /** f + 1 votes include at least one honest voter. */
  def honestThreshold: Int = (size - 1) / 3 + 1

  /** JAMNP-S: `primary = V[round mod validators]`, V sorted by key. */
  private val sortedKeys = keys.sortWith((a, b) => java.util.Arrays.compareUnsigned(a, b) < 0)
  def primary(round: Long): Seq[Byte] = sortedKeys((round % size).toInt).toSeq

/** One GRANDPA round's votes and our progress in it. */
final class RoundState(val round: Long, val startedAt: Long):
  val prevotes = mutable.LinkedHashMap.empty[Seq[Byte], SignedVote]
  val precommits = mutable.LinkedHashMap.empty[Seq[Byte], SignedVote]
  var primaryProposal: Option[Vote] = None
  var proposed = false
  var prevoted = false
  var precommitted = false

/** GRANDPA voting (GP 0.8.0 §grandpa, after Stewart 2020) over the JAMNP-S wire format. Not thread safe: the
  * caller runs every method on one thread. Votes for a set stop at the first unfinalized block carrying an epoch
  * marker; finalizing that block moves to the next set, whose keys come from the marker.
  *
  * @param gossipMillis
  *   T: prevote at round start + 2T, precommit at + 4T at the latest
  */
final class GrandpaVoter(
    chain: GrandpaChain,
    out: GrandpaOutbound,
    initialSet: VoterSet,
    keys: Option[ValidatorKeySet],
    clock: () => Long = () => System.currentTimeMillis(),
    gossipMillis: Long = 1000L,
    /** Called with each block GRANDPA finalized, its justification and whether it ended a voter set. */
    onFinalized: (Vote, Justification, Boolean) => Unit = (_, _, _) => ()
) extends LazyLogging:

  private var set: VoterSet = initialSet
  private var current: RoundState = new RoundState(1, clock())
  private val rounds = mutable.TreeMap(1L -> current)

  def voterSet: VoterSet = set
  def round: Long = current.round
  def roundState(r: Long): Option[RoundState] = rounds.get(r)

  private def ourKey: Option[Seq[Byte]] = keys.map(_.ed25519Public.toSeq).filter(set.members.contains)

  // ---------------------------------------------------------------------------
  // vote graph
  // ---------------------------------------------------------------------------

  /** Blocks from `h` down to (excluding) the slot of `base`, newest first; empty when `h` is not above `base`. */
  private def pathToBase(h: Hash, base: Vote): List[(Hash, Long)] =
    val out = mutable.ListBuffer.empty[(Hash, Long)]
    var cursor: Option[Hash] = Some(h)
    var reachedBase = false
    while !reachedBase && cursor.isDefined do
      val c = cursor.get
      chain.slotOf(c) match
        case Some(slot) if slot > base.slot =>
          out += c -> slot
          cursor = chain.parentOf(c)
        case Some(_) => reachedBase = c == base.hash; cursor = None
        case None    => cursor = None
    if reachedBase then out.toList else Nil

  /** Votes supporting each block above the base: a vote supports its target and every ancestor. */
  private def support(votes: Iterable[SignedVote], base: Vote): Map[Hash, (Long, Int)] =
    val counts = mutable.HashMap.empty[Hash, (Long, Int)]
    votes.foreach { v =>
      if consistentRoot(v.vote) then
        pathToBase(v.vote.hash, base).foreach { case (h, slot) =>
          counts.updateWith(h)(c => Some(slot -> (c.map(_._2).getOrElse(0) + 1)))
        }
    }
    counts.toMap

  /** A vote naming a state root other than ours for a block we validated is a vote for a different block. */
  private def consistentRoot(v: Vote): Boolean = chain.stateRootOf(v.hash).forall(_ == v.stateRoot)

  private def voteFor(h: Hash, slot: Long): Option[Vote] =
    if h == set.base.hash then Some(set.base)
    else chain.stateRootOf(h).map(Vote(h, _, slot))

  /** The highest block with a supermajority of `votes` (the GHOST); the set base when nothing above has one. */
  private def ghost(votes: Iterable[SignedVote], threshold: Int = set.threshold): Option[Vote] =
    if votes.size < threshold then None
    else
      val counts = support(votes, set.base)
      val best = counts.filter(_._2._2 >= threshold).maxByOption(_._2._1)
      best match
        case Some((h, (slot, _))) => voteFor(h, slot)
        case None                 => Some(set.base)

  def prevoteGhost(r: RoundState): Option[Vote] = ghost(r.prevotes.values)
  def precommitGhost(r: RoundState): Option[Vote] = ghost(r.precommits.values)

  /** The highest ancestor of the prevote GHOST that can still reach a precommit supermajority. */
  def estimate(r: RoundState): Option[Vote] =
    prevoteGhost(r).orElse(precommitGhost(r)).map { g =>
      val counts = support(r.precommits.values, set.base)
      val missing = set.size - r.precommits.size
      val path = pathToBase(g.hash, set.base)
      path
        .find { case (h, _) => counts.get(h).map(_._2).getOrElse(0) + missing >= set.threshold }
        .flatMap((h, slot) => voteFor(h, slot))
        .getOrElse(set.base)
    }

  /** No later precommits can change the estimate. */
  def completable(r: RoundState): Boolean =
    (prevoteGhost(r).orElse(precommitGhost(r)), estimate(r)) match
      case (Some(g), Some(e)) =>
        if e.hash != g.hash then true
        else
          val counts = support(r.precommits.values, set.base)
          val missing = set.size - r.precommits.size
          r.precommits.size >= set.threshold && !counts.exists { case (h, (slot, n)) =>
            slot > g.slot && n + missing >= set.threshold && chain.isAncestor(g.hash, h)
          }
      case _ => false

  /** The estimate a round must build on: the previous round's, else the set base. */
  private def previousEstimate(r: Long): Vote =
    rounds.get(r - 1).flatMap(estimate).getOrElse(lastFinalizedInSet)

  private def lastFinalizedInSet: Vote =
    val f = chain.finalized
    if f.slot >= set.base.slot then f else set.base

  // ---------------------------------------------------------------------------
  // voting
  // ---------------------------------------------------------------------------

  /** Our best block to vote for: the best head, not past the first unfinalized epoch-marker block (the set change),
    * and never off the chain of `floor`.
    */
  private def voteTarget(floor: Vote): Vote =
    val best = chain.best
    if !chain.isAncestor(floor.hash, best.hash) then floor
    else
      val path = pathToBase(best.hash, floor)
      path.reverse.find((h, _) => chain.epochMarkerKeys(h).isDefined) match
        case Some((h, slot)) => voteFor(h, slot).getOrElse(floor)
        case None            => best

  private def cast(kind: Int, vote: Vote): Unit =
    keys.filter(k => set.members.contains(k.ed25519Public.toSeq)).foreach { k =>
      val signed = sign(kind, vote, current.round, set.setId, k)
      record(current, kind, signed)
      out.broadcastVote(VoteMessage(current.round, set.setId, SignedMessage(kind, signed)))
      logger.debug(s"GRANDPA round ${current.round} set ${set.setId}: ${kindName(kind)} ${vote.short}")
    }

  private def kindName(kind: Int): String = kind match
    case Prevote   => "prevote"
    case Precommit => "precommit"
    case _         => "primary propose"

  private def record(r: RoundState, kind: Int, sv: SignedVote): Boolean =
    val votes = if kind == Prevote then r.prevotes else r.precommits
    votes.get(sv.signerKey) match
      case Some(existing) =>
        if existing.vote != sv.vote then
          logger.warn(s"GRANDPA equivocation in round ${r.round} by ${hexKey(sv.signer)}; keeping the first vote")
        false
      case None =>
        votes.put(sv.signerKey, sv)
        true

  private def hexKey(k: Array[Byte]): String = k.take(6).map(b => f"${b & 0xff}%02x").mkString

  /** Our own votes in the current round, to resend to voters that connected after we cast them. */
  def ourVotes: List[VoteMessage] =
    ourKey.toList.flatMap { k =>
      val r = current
      List(Prevote -> r.prevotes.get(k), Precommit -> r.precommits.get(k)).collect { case (kind, Some(v)) =>
        VoteMessage(r.round, set.setId, SignedMessage(kind, v))
      }
    }

  /** Advance the current round as far as the votes and timers allow. */
  def step(): Unit =
    val r = current
    val now = clock()
    val prevEstimate = previousEstimate(r.round)
    if ourKey.isDefined then
      if !r.proposed && ourKey.contains(set.primary(r.round)) then
        r.proposed = true
        if prevEstimate.slot > chain.finalized.slot then cast(PrimaryPropose, prevEstimate)
      if !r.prevoted && (now >= r.startedAt + 2 * gossipMillis || completable(r)) then
        val floor = r.primaryProposal.filter(p => chain.isAncestor(prevEstimate.hash, p.hash)).getOrElse(prevEstimate)
        r.prevoted = true
        cast(Prevote, voteTarget(floor))
      if r.prevoted && !r.precommitted then
        val timedOut = now >= r.startedAt + 4 * gossipMillis
        // Prevotes sent before we connected are not resent; f + 1 precommits prove a prevote supermajority exists.
        val target = prevoteGhost(r).orElse(Option.when(timedOut)(ghost(r.precommits.values, set.honestThreshold)).flatten)
        target.filter(g => chain.isAncestor(prevEstimate.hash, g.hash)).foreach { g =>
          if timedOut || completable(r) then
            r.precommitted = true
            cast(Precommit, g)
        }
    if !checkFinality() && (r.precommitted || ourKey.isEmpty) && completable(r) then startRound(r.round + 1)

  private def startRound(n: Long): Unit =
    current = rounds.getOrElseUpdate(n, new RoundState(n, clock()))
    rounds.keys.filter(_ < n - 2).toList.foreach(rounds.remove)
    out.broadcastState(n, set.setId, chain.finalized.slot)

  /** Finalize the highest precommit GHOST above our finalized block, in any round we track; true when the voter set
    * changed.
    */
  private def checkFinality(): Boolean =
    val setId = set.setId
    val justified = rounds.values.toList.reverse.flatMap { r =>
      precommitGhost(r).filter(_.slot > chain.finalized.slot).map { g =>
        val commit = Commit(g, r.precommits.values.filter(p => chain.isAncestor(g.hash, p.vote.hash)).toList)
        Justification(r.round, setId, commit, ancestriesFor(commit))
      }
    }
    justified.maxByOption(_.commit.target.slot) match
      case None => false
      case Some(j) =>
        finalizeWith(j) match
          case None => false
          case Some(changed) =>
            out.broadcastCommit(j.round, setId, j.commit)
            changed

  /** Headers routing each precommit target down to the commit target (exclusive). */
  private def ancestriesFor(c: Commit): List[Array[Byte]] =
    c.precommits
      .flatMap(p => pathToBase(p.vote.hash, c.target))
      .map(_._1)
      .distinct
      .flatMap(chain.headerBytes)

  /** Finalize a justified block: None when it failed, else whether the voter set changed (the block carries an epoch
    * marker).
    */
  private def finalizeWith(j: Justification): Option[Boolean] =
    val target = j.commit.target
    chain.finalizeBlock(target.hash) match
      case Left(err) =>
        logger.warn(s"GRANDPA could not finalize ${target.short}: $err")
        None
      case Right(()) =>
        logger.info(s"GRANDPA finalized ${target.short} in round ${j.round} of set ${j.setId}")
        val change = chain.epochMarkerKeys(target.hash)
        onFinalized(target, j, change.isDefined)
        change match
          case Some(markerKeys) =>
            set = VoterSet(set.setId + 1, target, markerKeys.toIndexedSeq)
            rounds.clear()
            current = new RoundState(1, clock())
            rounds.put(1, current)
            logger.info(s"GRANDPA set change to set ${set.setId} at ${target.short} (${set.size} voters)")
            out.broadcastState(1, set.setId, target.slot)
            Some(true)
          case None => Some(false)

  // ---------------------------------------------------------------------------
  // inbound
  // ---------------------------------------------------------------------------

  /** CE 149. Returns false when the vote was rejected. */
  def onVote(m: VoteMessage): Boolean =
    val sv = m.message.signed
    logger.debug(
      s"GRANDPA ${kindName(m.message.kind)} from ${hexKey(sv.signer)}: round ${m.round} set ${m.setId} ${sv.vote.short}" +
        s" (we are at round ${current.round} set ${set.setId})"
    )
    if m.setId != set.setId then false
    else if !set.members.contains(sv.signerKey) then
      logger.debug(s"GRANDPA vote from a non-voter ${hexKey(sv.signer)}")
      false
    else if m.round + 2 < current.round || m.round > current.round + 1 then false
    else if !verify(m.message.kind, sv, m.round, m.setId) then
      logger.warn(s"GRANDPA vote with a bad signature from ${hexKey(sv.signer)}")
      false
    else
      val r = rounds.getOrElseUpdate(m.round, new RoundState(m.round, clock()))
      m.message.kind match
        case PrimaryPropose =>
          if sv.signerKey == set.primary(m.round) && r.primaryProposal.isEmpty then r.primaryProposal = Some(sv.vote)
        case kind => record(r, kind, sv)
      step()
      true

  /** Checks a commit against the current set: valid distinct precommits, a supermajority, all at or above the
    * target. Ancestry headers route precommits for blocks we do not store.
    */
  def verifyCommit(round: Long, setId: Long, c: Commit, ancestries: List[Array[Byte]] = Nil): Either[String, Unit] =
    val routed = ancestries.flatMap(h => GrandpaVoter.headerLink(h)).map(l => l.hash -> l).toMap
    def descends(target: Vote, h: Hash, depth: Int = 0): Boolean =
      if h == target.hash then true
      else if chain.slotOf(h).isDefined then chain.isAncestor(target.hash, h)
      else depth < routed.size && routed.get(h).exists(l => descends(target, l.parent, depth + 1))
    if setId != set.setId then Left(s"set $setId, expected ${set.setId}")
    else
      val signers = c.precommits.map(_.signerKey)
      if signers.distinct.size != signers.size then Left("duplicate precommit signer")
      else if !signers.forall(set.members.contains) then Left("precommit by a non-voter")
      else if c.precommits.size < set.threshold then Left(s"${c.precommits.size} precommits, need ${set.threshold}")
      else if !c.precommits.forall(p => verify(Precommit, p, round, setId)) then Left("bad precommit signature")
      else if !c.precommits.forall(p => descends(c.target, p.vote.hash)) then Left("precommit not on the target's chain")
      else Right(())

  /** CE 150 or a fetched justification. Returns the block to fetch when the target is not stored yet. */
  def onCommit(round: Long, setId: Long, c: Commit, ancestries: List[Array[Byte]] = Nil): Either[String, Option[Hash]] =
    if c.target.slot <= chain.finalized.slot then Right(None)
    else if chain.slotOf(c.target.hash).isEmpty then
      verifyCommitSignatures(round, setId, c).map(_ => Some(c.target.hash))
    else
      verifyCommit(round, setId, c, ancestries).map { _ =>
        if finalizeWith(Justification(round, setId, c, ancestries)).contains(false) && round >= current.round then
          startRound(round + 1)
        None
      }

  private def verifyCommitSignatures(round: Long, setId: Long, c: Commit): Either[String, Unit] =
    if setId != set.setId then Left(s"set $setId, expected ${set.setId}")
    else if c.precommits.map(_.signerKey).distinct.count(set.members.contains) < set.threshold then
      Left("not enough voters")
    else if !c.precommits.forall(p => verify(Precommit, p, round, setId)) then Left("bad precommit signature")
    else Right(())

  /** CE 152 response: adopt a later round's votes and move past it. */
  def onCatchUp(setId: Long, c: CatchUp): Either[String, Unit] =
    if setId != set.setId then Left("other set")
    else if c.round < current.round then Left("not ahead of us")
    else
      val prevotes = c.prevotes.filter(p => set.members.contains(p.signerKey) && verify(Prevote, p, c.round, setId))
      val precommits =
        c.precommits.filter(p => set.members.contains(p.signerKey) && verify(Precommit, p, c.round, setId))
      if prevotes.map(_.signerKey).distinct.size < set.threshold ||
        precommits.map(_.signerKey).distinct.size < set.threshold
      then Left("catch-up without supermajorities")
      else
        val r = rounds.getOrElseUpdate(c.round, new RoundState(c.round, clock()))
        prevotes.foreach(record(r, Prevote, _))
        precommits.foreach(record(r, Precommit, _))
        r.prevoted = true
        r.precommitted = true
        logger.info(s"GRANDPA caught up to round ${c.round} of set $setId")
        if !checkFinality() then startRound(c.round + 1)
        Right(())

  /** CE 152 request: votes of our last completed round when it is at least `round`. */
  def catchUpFor(round: Long, setId: Long): Option[CatchUp] =
    if setId != set.setId then None
    else
      rounds.get(current.round - 1).filter(r => r.round >= round && completable(r)).map { r =>
        CatchUp(r.round, r.prevotes.values.toList, r.precommits.values.toList, lastFinalizedInSet)
      }

  /** CE 151: a voter more than a round ahead in our set warrants a catch-up request. */
  def wantsCatchUp(peerRound: Long, peerSetId: Long): Boolean =
    peerSetId == set.setId && peerRound > current.round + 1

object GrandpaVoter:

  /** Hash, parent and slot of an encoded header. */
  final case class HeaderLink(hash: Hash, parent: Hash, slot: Long)

  def headerLink(header: Array[Byte]): Option[HeaderLink] =
    Option.when(header.length >= 100)(
      HeaderLink(
        io.forge.jam.core.Hashing.blake2b256(header),
        Hash(header.take(32)),
        io.forge.jam.core.LittleEndian.get(header, 96, 4)
      )
    )
