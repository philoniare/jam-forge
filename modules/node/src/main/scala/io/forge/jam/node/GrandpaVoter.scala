package io.forge.jam.node

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.primitives.{Ed25519PublicKey, Hash}
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

  /** Ed25519 keys of the epoch marker in block `h`'s header, when it has one: the validators of the epoch after the
    * one `h` begins (γ_P'), not of `h`'s own.
    */
  def epochMarkerKeys(h: Hash): Option[List[Array[Byte]]]

  /** Ed25519 keys of the genesis state's next-epoch validators (γ_P), active from the first epoch change. None: the
    * genesis validators stay.
    */
  def genesisNextKeys: Option[List[Array[Byte]]] = None
  def finalizeBlock(h: Hash): Either[String, Unit]

  /** isAudited: a block may be voted for only once audited. */
  def isAudited(h: Hash): Boolean = true

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

final case class VoterSet(
    setId: Long,
    base: Vote,
    keys: IndexedSeq[Array[Byte]],
    next: Option[IndexedSeq[Array[Byte]]] = None
):
  val members: Set[Ed25519PublicKey] = keys.map(Ed25519PublicKey(_)).toSet
  def size: Int = keys.size

  /** Votes needed for a supermajority: n − ⌊(n−1)/3⌋. */
  def threshold: Int = size - (size - 1) / 3

  /** f + 1 votes include at least one honest voter. */
  def honestThreshold: Int = (size - 1) / 3 + 1

  /** JAMNP-S: `primary = V[round mod validators]`, V sorted by key. */
  private val sortedKeys = keys.sortWith((a, b) => java.util.Arrays.compareUnsigned(a, b) < 0)
  def primary(round: Long): Ed25519PublicKey = Ed25519PublicKey(sortedKeys((round % size).toInt))

/** One GRANDPA round's votes and our progress in it. */
final class RoundState(val round: Long, val startedAt: Long):
  val prevotes = mutable.LinkedHashMap.empty[Ed25519PublicKey, SignedVote]
  val precommits = mutable.LinkedHashMap.empty[Ed25519PublicKey, SignedVote]

  /** Voters that cast two different prevotes (precommits) in this round, with both votes: the evidence. Such a voter
    * is not in `prevotes` (`precommits`).
    */
  val prevoteEquivocations = mutable.LinkedHashMap.empty[Ed25519PublicKey, (SignedVote, SignedVote)]
  val precommitEquivocations = mutable.LinkedHashMap.empty[Ed25519PublicKey, (SignedVote, SignedVote)]
  var primaryProposal: Option[Vote] = None
  var proposed = false
  var prevoted = false
  var precommitted = false

/** GRANDPA voting (GP 0.8.0 §grandpa, after Stewart 2020) over the JAMNP-S wire format. Not thread safe: the
  * caller runs every method on one thread. Votes for a set stop at the first unfinalized block carrying an epoch
  * marker; finalizing that block moves to the next set: the validators of the epoch the block begins (its κ'), which
  * are the keys of the marker before it. The block's own marker names the validators one epoch later. A round's
  * primary proposes the previous round's estimate; we prevote from that proposal only when it lies within the
  * previous round's prevote GHOST and we have audited it ([[prevoteFloor]]), else from our own estimate.
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
    onFinalized: (Vote, Justification, Boolean) => Unit = (_, _, _) => (),
    /** Called with each voter set we move to, by our own justification or from the finalized chain. */
    onSetAdopted: VoterSet => Unit = _ => (),
    onProgress: (Long, Long, List[VoteMessage], Boolean) => Unit = (_, _, _, _) => (),
    /** Round and our votes in it from before a restart, in `initialSet`. */
    resume: Option[(Long, List[VoteMessage])] = None,
    paused: () => Boolean = () => false
) extends LazyLogging:

  private var set: VoterSet = initialSet

  /** The set that follows `of` once the epoch-marker block `base` is finalized. Its voters are the validators of the
    * epoch `base` begins: those the marker before it named, which is `of`'s own base (at genesis, the genesis
    * state's next-epoch validators; for a set re-based at a checkpoint without that marker, the keys it carries).
    */
  private def successor(of: VoterSet, base: Vote): VoterSet =
    val named =
      chain.epochMarkerKeys(of.base.hash).map(_.toIndexedSeq)
        .orElse(if of.setId == 0 then chain.genesisNextKeys.map(_.toIndexedSeq) else None)
        .orElse(of.next)
    VoterSet(of.setId + 1, base, named.getOrElse(of.keys))
  private var lastSyncedFinal: Hash = Hash.zero
  private var current: RoundState = new RoundState(resume.fold(1L)(_._1), clock())
  private val rounds = mutable.TreeMap(current.round -> current)

  // A restarted voter goes back to its round with the votes it cast there: peers that never resend their votes
  // are waiting in that round, and voting again differently would be an equivocation.
  resume.foreach { (_, votes) =>
    votes.filter(m => m.setId == set.setId && m.round == current.round).foreach { m =>
      val sv = m.message.signed
      m.message.kind match
        case PrimaryPropose => current.proposed = true; current.primaryProposal = Some(sv.vote)
        case Prevote        => current.prevoted = true; record(current, Prevote, sv)
        case _              => current.precommitted = true; record(current, Precommit, sv)
    }
  }

  /** Votes and the latest commit for sets we have not reached yet, replayed once we change to that set: a peer's
    * commit can arrive just before the finalized chain that moves us to its set.
    */
  private val futureVotes = mutable.ListBuffer.empty[VoteMessage]
  private val futureCommits = mutable.TreeMap.empty[Long, (Long, Commit, List[Array[Byte]])]

  /** Keep a commit of a later set, whose keys we do not know yet, so it cannot be checked: the latest round per set,
    * for the [[GrandpaVoter.MaxFutureCommitSets]] sets nearest ours. False when it was not kept.
    */
  private def keepFutureCommit(round: Long, setId: Long, c: Commit, ancestries: List[Array[Byte]]): Boolean =
    if futureCommits.get(setId).exists(_._1 >= round) then false
    else if futureCommits.contains(setId) || futureCommits.size < GrandpaVoter.MaxFutureCommitSets then
      futureCommits.put(setId, (round, c, ancestries))
      true
    else if setId < futureCommits.lastKey then
      futureCommits.remove(futureCommits.lastKey)
      futureCommits.put(setId, (round, c, ancestries))
      true
    else false

  /** Sets we hold an unchecked commit for. */
  private[node] def futureCommitSets: List[Long] = futureCommits.keys.toList

  def voterSet: VoterSet = set

  def restartAt(newSet: VoterSet): Unit =
    set = newSet
    lastSyncedFinal = Hash.zero
    futureVotes.clear()
    futureCommits.clear()
    rounds.clear()
    current = new RoundState(1, clock())
    rounds.put(1, current)
    onSetAdopted(set)
    onProgress(set.setId, 1, Nil, false)
    logger.info(
      s"GRANDPA restarted at ${set.base.short} with ${set.size} voters, set " +
        (if set.setId == GrandpaVoter.UnknownSet then "not yet known" else set.setId.toString)
    )

  private def inCurrentRange(target: Vote): Boolean =
    val f = chain.finalized
    var cursor = chain.parentOf(target.hash)
    var ok = target.slot > f.slot
    while ok && !cursor.contains(f.hash) do
      cursor match
        case Some(h) if chain.slotOf(h).exists(_ > f.slot) && chain.epochMarkerKeys(h).isEmpty => cursor = chain.parentOf(h)
        case _                                                                              => ok = false
    ok

  private var markerWaitLogged = false

  private def learnSetId(round: Long, setId: Long, c: Commit, ancestries: List[Array[Byte]]): Either[String, Option[Hash]] =
    val unknown = set
    def held(v: Vote) = chain.slotOf(v.hash).isDefined
    if setId < 0 then Left("no set id")
    else
      set = unknown.copy(setId = setId)
      val bySigner = c.precommits.groupBy(_.signerKey).values.toList
      val counted = bySigner.filter(_.forall(p =>
        held(p.vote) && inCurrentRange(p.vote) && chain.epochMarkerKeys(p.vote.hash).isEmpty
      ))
      val unheld = c.precommits.map(_.vote).find(v => !held(v))
      if counted.size < set.threshold && unheld.isEmpty && !markerWaitLogged &&
        c.precommits.exists(p => chain.epochMarkerKeys(p.vote.hash).isDefined)
      then
        markerWaitLogged = true
        logger.info(
          "GRANDPA set id not learned from a commit whose precommits sit on the epoch's first block; " +
            "--sync-from <hash>:<set id> avoids the wait"
        )
      val proven =
        if !held(c.target) then verifyCommitSignatures(round, setId, c).map(_ => Some(c.target.hash))
        else if !inCurrentRange(c.target) then Left("not for a block the set at our finalized block votes on")
        else if counted.size < set.threshold && unheld.isDefined then
          verifyCommitSignatures(round, setId, c).map(_ => unheld.map(_.hash))
        else verifyCommit(round, setId, c.copy(precommits = counted.flatten), ancestries).map(_ => None)
      proven match
        case Right(None) =>
          logger.info(s"GRANDPA set $setId learned from the commit for ${c.target.short}")
          onSetAdopted(set)
          onProgress(set.setId, current.round, Nil, false)
          onCommit(round, setId, c, ancestries)
        case Right(wanted) =>
          set = unknown
          Right(wanted)
        case Left(why) =>
          set = unknown
          Left(s"set not yet known; $why")
  def round: Long = current.round
  private[node] def roundState: RoundState = current
  def roundState(r: Long): Option[RoundState] = rounds.get(r)

  private def ourKey: Option[Ed25519PublicKey] = keys.map(_.ed25519Key).filter(set.members.contains)

  // ---------------------------------------------------------------------------
  // vote graph
  // ---------------------------------------------------------------------------

  /** Paths to the set base already walked, by block: `pathCache(b)` is `b`'s path, so a block's path is its own
    * entry in front of its parent's and the lists share their tails (one cell per block). Headers and parent links
    * never change, so an entry stays right for as long as the base does; it is dropped when the base moves.
    */
  private val pathCache = mutable.HashMap.empty[Hash, List[(Hash, Long)]]
  private var pathCacheBase: Hash = null

  /** Blocks from `h` down to (excluding) the slot of `base`, newest first; empty when `h` is not above `base`.
    * Every round tallies every vote's path on every step, so the walk is remembered: only blocks not seen before
    * are read from the chain.
    */
  private def pathToBase(h: Hash, base: Vote): List[(Hash, Long)] =
    if pathCacheBase != base.hash || pathCache.size > GrandpaVoter.MaxCachedPaths then
      pathCache.clear()
      pathCacheBase = base.hash
    val pending = mutable.ListBuffer.empty[(Hash, Long)]
    var cursor: Option[Hash] = Some(h)
    var tail: List[(Hash, Long)] = null
    var isTarget = true
    while tail == null && cursor.isDefined do
      val c = cursor.get
      pathCache.get(c) match
        // A target the chain no longer holds has no path, cached or not (as when it is walked afresh).
        case Some(t) if !isTarget || chain.slotOf(c).isDefined => tail = t
        case Some(_)                                           => cursor = None
        case None =>
          chain.slotOf(c) match
            case Some(slot) if slot > base.slot =>
              pending += c -> slot
              cursor = chain.parentOf(c)
            case Some(_) =>
              if c == base.hash then tail = Nil
              cursor = None
            case None => cursor = None
      isTarget = false
    if tail == null then Nil
    else
      var path = tail
      pending.reverseIterator.foreach { e =>
        path = e :: path
        pathCache.put(e._1, path)
      }
      path

  /** Votes supporting each block above the base: a vote supports its target and every ancestor. Votes are counted
    * per distinct target first (a round has a handful), so a path is added once with its number of votes rather
    * than once per vote.
    */
  private def support(votes: Iterable[SignedVote], base: Vote): Map[Hash, (Long, Int)] =
    val perTarget = mutable.LinkedHashMap.empty[Vote, Int]
    votes.foreach(v => perTarget.updateWith(v.vote)(n => Some(n.getOrElse(0) + 1)))
    val counts = mutable.HashMap.empty[Hash, (Long, Int)]
    perTarget.foreach { case (vote, n) =>
      if consistentRoot(vote) then
        pathToBase(vote.hash, base).foreach { case (h, slot) =>
          counts.updateWith(h)(c => Some(slot -> (c.map(_._2).getOrElse(0) + n)))
        }
    }
    counts.toMap

  /** A vote naming a state root other than ours for a block we validated is a vote for a different block. */
  private def consistentRoot(v: Vote): Boolean = chain.stateRootOf(v.hash).forall(_ == v.stateRoot)

  private def voteFor(h: Hash, slot: Long): Option[Vote] =
    if h == set.base.hash then Some(set.base)
    else chain.stateRootOf(h).map(Vote(h, _, slot))

  /** The highest block with a supermajority of `votes` (the GHOST); the set base when nothing above has one.
    * `equivocators` voters voted twice: they count towards every block (Stewart 2020, §2.3), since neither vote
    * binds them.
    */
  private def ghost(votes: Iterable[SignedVote], threshold: Int = set.threshold, equivocators: Int = 0): Option[Vote] =
    if votes.size + equivocators < threshold then None
    else
      val counts = support(votes, set.base)
      val best = counts.filter(_._2._2 + equivocators >= threshold).maxByOption(_._2._1)
      best match
        case Some((h, (slot, _))) => voteFor(h, slot)
        case None                 => Some(set.base)

  def prevoteGhost(r: RoundState): Option[Vote] = ghost(r.prevotes.values, equivocators = r.prevoteEquivocations.size)
  def precommitGhost(r: RoundState): Option[Vote] =
    ghost(r.precommits.values, equivocators = r.precommitEquivocations.size)

  /** The highest ancestor of the prevote GHOST that can still reach a precommit supermajority. */
  def estimate(r: RoundState): Option[Vote] =
    prevoteGhost(r).orElse(precommitGhost(r)).map { g =>
      val counts = support(r.precommits.values, set.base)
      // Voters yet to precommit and equivocators (in no vote map) may both still count towards any block.
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
          r.precommits.size + r.precommitEquivocations.size >= set.threshold && !counts.exists { case (h, (slot, n)) =>
            slot > g.slot && n + missing >= set.threshold && descends(h, g)
          }
      case _ => false

  /** `h`, a block counted by [[support]] (so its path reaches the set base), is `g` or has it as an ancestor. */
  private def descends(h: Hash, g: Vote): Boolean =
    if h == g.hash || g.hash == set.base.hash then true
    else if g.slot > set.base.slot then pathToBase(h, set.base).exists(_._1 == g.hash)
    else chain.isAncestor(g.hash, h)

  /** The estimate a round must build on: the previous round's, else the set base. */
  private def previousEstimate(r: Long): Vote =
    rounds.get(r - 1).flatMap(estimate).getOrElse(lastFinalizedInSet)

  private def lastFinalizedInSet: Vote =
    val f = chain.finalized
    if f.slot >= set.base.slot then f else set.base

  // ---------------------------------------------------------------------------
  // voting
  // ---------------------------------------------------------------------------

  private def prevoteFloor(r: RoundState, prevEstimate: Vote): Vote =
    r.primaryProposal
      .filter { b =>
        val above = pathToBase(b.hash, prevEstimate) // empty unless we hold B and it strictly descends from E_{r-1}
        above.headOption.flatMap((h, slot) => voteFor(h, slot)).contains(b) &&
        above.forall((h, _) => chain.isAudited(h)) &&
        rounds.get(r.round - 1).flatMap(prevoteGhost).exists(g => chain.isAncestor(b.hash, g.hash))
      }
      .getOrElse(prevEstimate)

  private[node] def prevoteFloor(r: RoundState): Vote = prevoteFloor(r, previousEstimate(r.round))

  /** Our best block to vote for: the best head, not past the first unfinalized epoch-marker block (the set change)
    * nor past the last block before an unaudited one, and never off the chain of `floor`. Only blocks above `floor`
    * are checked for audit: every exit returns `floor` or an audited block above it, so the caller must pass a floor
    * that is the estimate to build on or itself audited ([[prevoteFloor]]).
    */
  private def voteTarget(floor: Vote): Vote =
    val best = chain.best
    if !chain.isAncestor(floor.hash, best.hash) then floor
    else
      val ascending = pathToBase(best.hash, floor).reverse
      val audited = ascending.takeWhile((h, _) => chain.isAudited(h))
      if audited.size < ascending.size then
        logger.debug(s"GRANDPA vote held at ${audited.size} of ${ascending.size} blocks above the floor: not audited")
      val marker = audited.indexWhere((h, _) => chain.epochMarkerKeys(h).isDefined)
      val upTo = if marker >= 0 then audited.take(marker + 1) else audited
      upTo.lastOption match
        case Some((h, slot)) if upTo.size < ascending.size => voteFor(h, slot).getOrElse(floor)
        case Some(_)                                       => best
        case None                                          => floor

  private def cast(kind: Int, vote: Vote): Unit =
    keys.filter(k => set.members.contains(k.ed25519Key)).foreach { k =>
      val signed = sign(kind, vote, current.round, set.setId, k)
      if kind == PrimaryPropose then
        current.primaryProposal = Some(signed.vote)
        onProgress(set.setId, current.round, ourVotes, false)
      else
        record(current, kind, signed)
        try onProgress(set.setId, current.round, ourVotes, true)
        catch
          case e: Throwable =>
            (if kind == Prevote then current.prevotes else current.precommits).remove(k.ed25519Key)
            throw e
      out.broadcastVote(VoteMessage(current.round, set.setId, SignedMessage(kind, signed)))
      logger.debug(s"GRANDPA round ${current.round} set ${set.setId}: ${kindName(kind)} ${vote.short}")
    }

  private def kindName(kind: Int): String = kind match
    case Prevote   => "prevote"
    case Precommit => "precommit"
    case _         => "primary propose"

  private def record(r: RoundState, kind: Int, sv: SignedVote): Boolean =
    val (votes, equivocations) =
      if kind == Prevote then (r.prevotes, r.prevoteEquivocations) else (r.precommits, r.precommitEquivocations)
    if equivocations.contains(sv.signerKey) then false
    else
      votes.get(sv.signerKey) match
        case Some(existing) if existing.vote != sv.vote =>
          logger.warn(
            s"GRANDPA equivocation in round ${r.round} by ${hexKey(sv.signer)}: ${kindName(kind)}s for " +
              s"${existing.vote.short} and ${sv.vote.short}"
          )
          votes.remove(sv.signerKey)
          equivocations.put(sv.signerKey, (existing, sv))
          true
        case Some(_) => false
        case None =>
          votes.put(sv.signerKey, sv)
          true

  private def hexKey(k: Array[Byte]): String = k.take(6).map(b => f"${b & 0xff}%02x").mkString

  /** Every vote we hold for the current round, ours and relayed, to resend while the round is stalled: a voter that
    * was cut off gets the votes it needs to finish the round, since some peers never resend.
    */
  def roundVotes: List[VoteMessage] = votesOf(current)

  def previousRoundVotes: List[VoteMessage] = rounds.get(current.round - 1).toList.flatMap(votesOf)

  def heardThisRound: Set[Ed25519PublicKey] =
    val r = current
    (r.prevotes.keysIterator ++ r.precommits.keysIterator ++ r.prevoteEquivocations.keysIterator ++
      r.precommitEquivocations.keysIterator).toSet

  private def votesOf(r: RoundState): List[VoteMessage] =
    (allPrevotes(r).map(Prevote -> _) ++ allPrecommits(r).map(Precommit -> _)).map { (kind, v) =>
      VoteMessage(r.round, set.setId, SignedMessage(kind, v))
    }

  /** A round's votes with both votes of each equivocator, so that every voter we send them to counts as we do. */
  private def allPrevotes(r: RoundState): List[SignedVote] =
    r.prevotes.values.toList ++ r.prevoteEquivocations.values.flatMap((a, b) => List(a, b))
  private def allPrecommits(r: RoundState): List[SignedVote] =
    r.precommits.values.toList ++ r.precommitEquivocations.values.flatMap((a, b) => List(a, b))

  private val heard = mutable.Set.empty[Ed25519PublicKey]

  def votersHeard: Set[Ed25519PublicKey] = heard.toSet

  /** Our own votes in the current round, to resend to voters that connected after we cast them. */
  def ourVotes: List[VoteMessage] =
    ourKey.toList.flatMap { k =>
      val r = current
      List(Prevote -> r.prevotes.get(k), Precommit -> r.precommits.get(k)).collect { case (kind, Some(v)) =>
        VoteMessage(r.round, set.setId, SignedMessage(kind, v))
      }
    }

  /** Adopt the set changes of a chain finalized without us, e.g. by following peers after a partition: every finalized
    * epoch-marker block above our base ended a set, since votes never pass the first unfinalized marker. Peers that
    * only send their latest commit would otherwise leave us in an old set for good. True when the
    * set changed.
    */
  def adoptFinalizedSetChanges(): Boolean =
    val f = chain.finalized
    if set.setId == GrandpaVoter.UnknownSet || f.slot <= set.base.slot || f.hash == lastSyncedFinal then false
    else
      lastSyncedFinal = f.hash
      val markers = pathToBase(f.hash, set.base).reverse.filter((h, _) => chain.epochMarkerKeys(h).isDefined)
      markers.foreach { (h, slot) =>
        set = successor(set, Vote(h, chain.stateRootOf(h).getOrElse(Hash.zero), slot))
        logger.info(s"GRANDPA adopted set ${set.setId} at ${set.base.short} from the finalized chain")
        onSetAdopted(set)
      }
      if markers.nonEmpty then
        rounds.clear()
        current = new RoundState(1, clock())
        rounds.put(1, current)
        out.broadcastState(1, set.setId, f.slot)
        onProgress(set.setId, 1, Nil, false)
        replayFuture()
      markers.nonEmpty

  /** Feed the buffered votes and commit of the set we just reached. */
  private def replayFuture(): Unit =
    val (now, later) = futureVotes.toList.filter(_.setId >= set.setId).partition(_.setId == set.setId)
    futureVotes.clear()
    futureVotes ++= later
    now.foreach(acceptVote)
    futureCommits.keys.filter(_ < set.setId).toList.foreach(futureCommits.remove)
    futureCommits.remove(set.setId).foreach { (round, c, anc) =>
      logger.info(s"GRANDPA replaying the commit for round $round of set ${set.setId}")
      onCommit(round, set.setId, c, anc): Unit
    }
    rounds.keys.toList.reverse.takeWhile(_ > current.round).find(joinLaterRound): Unit

  /** Votes from at least f + 1 voters in a later round mean an honest voter saw our round completable (we restarted,
    * or missed the votes that completed it); join that round, keeping its votes.
    */
  private def joinLaterRound(n: Long): Boolean =
    rounds.get(n).exists { r =>
      val signers = (r.prevotes.keySet ++ r.precommits.keySet).size
      val join = n > current.round && signers >= set.honestThreshold
      if join then
        logger.info(s"GRANDPA joining round $n of set ${set.setId} from round ${current.round}: $signers voters are there")
        startRound(n)
      join
    }

  /** Advance the current round as far as the votes and timers allow. */
  def step(): Unit =
    if set.setId == GrandpaVoter.UnknownSet then return // nothing can be signed for a set we cannot name
    if paused() then return
    adoptFinalizedSetChanges()
    val r = current
    val now = clock()
    val prevEstimate = previousEstimate(r.round)
    if ourKey.isDefined then
      if !r.proposed && ourKey.contains(set.primary(r.round)) then
        r.proposed = true
        if prevEstimate.slot > chain.finalized.slot then cast(PrimaryPropose, prevEstimate)
      if !r.prevoted && (now >= r.startedAt + 2 * gossipMillis || completable(r)) then
        val floor = prevoteFloor(r, prevEstimate)
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
    onProgress(set.setId, n, ourVotes, false)

  /** Finalize the highest precommit GHOST above our finalized block, in any round we track; true when the voter set
    * changed.
    */
  private def checkFinality(): Boolean =
    val setId = set.setId
    val justified = rounds.values.toList.reverse.flatMap { r =>
      precommitGhost(r).filter(_.slot > chain.finalized.slot).map { g =>
        val direct = r.precommits.values.filter(p => chain.isAncestor(g.hash, p.vote.hash)).toList
        // Equivocators' precommits are added only when the others fall short: the pairs are what proves their weight.
        val pairs =
          if direct.size >= set.threshold then Nil else r.precommitEquivocations.values.flatMap((a, b) => List(a, b)).toList
        val commit = Commit(g, direct ++ pairs)
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
        val change = chain.epochMarkerKeys(target.hash).isDefined
        onFinalized(target, j, change)
        change match
          case true =>
            set = successor(set, target)
            onSetAdopted(set)
            rounds.clear()
            current = new RoundState(1, clock())
            rounds.put(1, current)
            logger.info(s"GRANDPA set change to set ${set.setId} at ${target.short} (${set.size} voters)")
            out.broadcastState(1, set.setId, target.slot)
            onProgress(set.setId, 1, Nil, false)
            replayFuture()
            Some(true)
          case false => Some(false)

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
    if m.setId > set.setId then
      if futureVotes.size < GrandpaVoter.MaxFutureVotes then futureVotes += m
      false
    else
      val ok = acceptVote(m)
      if ok then
        joinLaterRound(m.round): Unit
        step()
      ok

  /** Check and record a vote of our set without advancing. */
  private def acceptVote(m: VoteMessage): Boolean =
    val sv = m.message.signed
    if m.setId != set.setId then false
    else if !set.members.contains(sv.signerKey) then
      logger.debug(s"GRANDPA vote from a non-voter ${hexKey(sv.signer)}")
      false
    else if m.round + 2 < current.round || m.round > current.round + GrandpaVoter.MaxRoundsAhead then false
    else if !verify(m.message.kind, sv, m.round, m.setId) then
      logger.warn(s"GRANDPA vote with a bad signature from ${hexKey(sv.signer)}")
      false
    else
      heard += sv.signerKey
      val r = rounds.getOrElseUpdate(m.round, new RoundState(m.round, clock()))
      m.message.kind match
        case PrimaryPropose =>
          if sv.signerKey == set.primary(m.round) && r.primaryProposal.isEmpty then r.primaryProposal = Some(sv.vote)
        case kind => record(r, kind, sv)
      true

  /** Checks a commit against the current set: validly signed precommits, one per voter and at or above the target,
    * or two different ones from a voter (an equivocation, which counts whatever it names), and a supermajority of
    * voters among them. A target or precommit naming a state root other than ours for a block we hold is for another
    * block: the commit is refused, the precommit not counted. Ancestry headers route precommits for blocks we do not
    * store.
    */
  def verifyCommit(round: Long, setId: Long, c: Commit, ancestries: List[Array[Byte]] = Nil): Either[String, Unit] =
    val routed = ancestries.flatMap(h => GrandpaVoter.headerLink(h)).map(l => l.hash -> l).toMap
    def descends(target: Vote, h: Hash, depth: Int = 0): Boolean =
      if h == target.hash then true
      else if chain.slotOf(h).isDefined then chain.isAncestor(target.hash, h)
      else depth < routed.size && routed.get(h).exists(l => descends(target, l.parent, depth + 1))
    if setId != set.setId then Left(s"set $setId, expected ${set.setId}")
    else
      val bySigner = c.precommits.groupBy(_.signerKey)
      val (single, twice) = bySigner.values.toList.partition(_.size == 1)
      val singles = single.flatten
      if twice.exists(vs => vs.size > 2 || vs(0).vote == vs(1).vote) then Left("duplicate precommit signer")
      else if !bySigner.keys.forall(set.members.contains) then Left("precommit by a non-voter")
      else if bySigner.size < set.threshold then Left(s"${bySigner.size} precommits, need ${set.threshold}")
      else if !c.precommits.forall(p => verify(Precommit, p, round, setId)) then Left(GrandpaVoter.BadPrecommitSignature)
      else if !consistentRoot(c.target) then Left("target names a state root other than ours")
      else if !singles.forall(p => descends(c.target, p.vote.hash)) then Left("precommit not on the target's chain")
      else
        val counted = singles.count(p => consistentRoot(p.vote)) + twice.size
        if counted < set.threshold then Left(s"$counted precommits for the target, need ${set.threshold}")
        else Right(())

  /** CE 150 or a fetched justification. Returns the block to fetch when the target is not stored yet. */
  def onCommit(round: Long, setId: Long, c: Commit, ancestries: List[Array[Byte]] = Nil): Either[String, Option[Hash]] =
    if set.setId == GrandpaVoter.UnknownSet then learnSetId(round, setId, c, ancestries)
    else if setId > set.setId then
      val kept = keepFutureCommit(round, setId, c, ancestries)
      Left(s"set $setId, expected ${set.setId}; ${if kept then "kept for later" else "not kept"}")
    else if c.target.slot <= chain.finalized.slot then
      // Already finalized (e.g. by following peers), but the commit still says the round is over.
      if setId == set.setId && round >= current.round && chain.slotOf(c.target.hash).isDefined then
        verifyCommit(round, setId, c, ancestries).map(_ => { startRound(round + 1); None })
      else Right(None)
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
    else if !c.precommits.forall(p => verify(Precommit, p, round, setId)) then Left(GrandpaVoter.BadPrecommitSignature)
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
        CatchUp(r.round, allPrevotes(r), allPrecommits(r), lastFinalizedInSet)
      }

  /** CE 151: a voter more than a round ahead in our set warrants a catch-up request. */
  def wantsCatchUp(peerRound: Long, peerSetId: Long): Boolean =
    peerSetId == set.setId && peerRound > current.round + 1

object GrandpaVoter:

  val UnknownSet: Long = -1L

  val BadPrecommitSignature = "bad precommit signature"

  /** Votes this many rounds ahead of ours are kept, so that a restarted voter can join the others' round. */
  val MaxRoundsAhead: Long = 256

  /** Votes kept for sets we have not reached yet. */
  val MaxFutureVotes: Int = 1024

  /** Blocks whose path to the set base is remembered: an epoch's blocks with room for forks; cleared when passed. */
  val MaxCachedPaths: Int = 16384

  /** Sets ahead of ours whose latest commit is kept. A set's keys are in the epoch marker that ends the set before
    * it, so these commits are held unverified and any peer can send one for any set id. An honest commit runs ahead
    * of us by the set change we are about to finalize, or by the few we cross at once when adopting a finalized chain;
    * a voter further behind is brought forward by warp sync (CE 153), one verified justification per set, and gets
    * the next commit when it arrives. The nearest sets are kept, so far-future ids cannot displace a commit we
    * are about to need.
    */
  val MaxFutureCommitSets: Int = 4

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
