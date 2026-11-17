package io.forge.jam.node

import io.forge.jam.core.Shuffle
import io.forge.jam.core.codec.JamCodecs
import io.forge.jam.core.primitives.Hash
import io.forge.jam.crypto.Ed25519

/** The deterministic parts of auditing. */
object AuditSelection:

  /** Seconds per audit tranche (`C_tranche-seconds`). */
  val TrancheSeconds: Int = 8

  /** Expected auditors drawn per no-show in the tranche before (`C_audit-bias-factor`). */
  val BiasFactor: Int = 2

  /** Tranches after which a finalized block we never saw audited stops being audited (our own rule). */
  val GiveUpTranches: Int = 10

  /** Cores drawn by the tranche-0 VRF. */
  val InitialCores: Int = 10

  val AuditContext: Array[Byte] = "jam_audit".getBytes("UTF-8")
  val AnnounceContext: Array[Byte] = "jam_announce".getBytes("UTF-8")

  /** Tranche at `nowMillis` for a block whose slot started at `originMillis`: ⌊(T − P·H_t) / C_tranche-seconds⌋. */
  def trancheAt(nowMillis: Long, originMillis: Long): Int =
    if nowMillis < originMillis then 0
    else ((nowMillis - originMillis) / (TrancheSeconds * 1000L)).toInt

  /** VRF input of s_0, eq:initialaudit: `$jam_audit ++ Y(H_v)`. */
  def initialInput(headerVrfOutput: Array[Byte]): Array[Byte] = AuditContext ++ headerVrfOutput

  /** VRF input of s_n(w), eq:latertranches: `$jam_audit ++ Y(H_v) ++ H(w) ++ n`. */
  def laterInput(headerVrfOutput: Array[Byte], reportHash: Hash, tranche: Int): Array[Byte] =
    AuditContext ++ headerVrfOutput ++ reportHash.bytes.toArray :+ tranche.toByte

  /** a_0: the cores among the first ten of F(q, Y(s_0)) whose entry is not ∅. */
  def initialCores(q: IndexedSeq[Option[?]], seedOutput: Array[Byte]): Seq[Int] =
    Shuffle.jamComputeShuffle(q.size, Hash(seedOutput)).take(InitialCores).filter(c => q(c).isDefined)

  /** a_n(w) for n > 0: `V / (256 · C_bias) · Y(s_n(w))₀ < noShows`, in integers, on the first byte of the VRF
    * output. This is the rule we apply to our own keys.
    */
  def escalates(seedOutput: Array[Byte], validatorCount: Int, noShows: Int): Boolean =
    val d = seedOutput(0) & 0xff
    validatorCount.toLong * d < noShows.toLong * 256L * BiasFactor

  /** a_n(w) on the first two bytes of the VRF output:
    * `V / (65536 · C_bias) · E₂⁻¹(Y(s_n(w))[0..2]) < noShows`.
    */
  def escalatesWide(seedOutput: Array[Byte], validatorCount: Int, noShows: Int): Boolean =
    val d = (seedOutput(0) & 0xff) | ((seedOutput(1) & 0xff) << 8)
    validatorCount.toLong * d < noShows.toLong * 65536L * BiasFactor

  /** Whether a peer's later-tranche announcement is a valid selection: under either rule, so peers on the one-byte
    * and on the two-byte rule are both heard. Accepting an extra auditor only adds judgments; rejecting a genuine one
    * would make its report look like a no-show.
    */
  def peerEscalates(seedOutput: Array[Byte], validatorCount: Int, noShows: Int): Boolean =
    escalates(seedOutput, validatorCount, noShows) || escalatesWide(seedOutput, validatorCount, noShows)

  def noShowsNeeded(seedOutput: Array[Byte], validatorCount: Int): Int =
    val d = (seedOutput(0) & 0xff).toLong
    val wide = d | ((seedOutput(1) & 0xffL) << 8)
    val narrowNeeds = validatorCount * d / (256L * BiasFactor)
    val wideNeeds = validatorCount * wide / (65536L * BiasFactor)
    math.min(narrowNeeds, wideNeeds).toInt + 1

  /** x_n of eq:announcement: `E₂(core) ++ H(w)` per announced report, in the order the announcement lists them, with
    * no length prefix. That is what polkajam v0.8.0 signs: its two-report announcements verify only in wire order
    * (run AA, 2026-10-22).
    */
  def announcedItems(items: Seq[(Int, Hash)]): Array[Byte] =
    items.flatMap((core, h) => Array((core & 0xff).toByte, ((core >> 8) & 0xff).toByte) ++ h.bytes.toArray).toArray

  /** The Ed25519 message of an announcement S, eq:announcement: `$jam_announce ++ n ++ x_n ++ H(header)`. */
  def announcementMessage(tranche: Int, items: Seq[(Int, Hash)], headerHash: Hash): Array[Byte] =
    (AnnounceContext :+ tranche.toByte) ++ announcedItems(items) ++ headerHash.bytes.toArray

  /** U(w): no negative judgment and some tranche n with A_n(w) ⊆ J⊤(w), or more than two thirds of the validators
    * judged positively. An empty tranche (∅ ⊆ J⊤) counts once it has ended, whichever tranche it is: until then an
    * announcement may still arrive, and under the literal reading every report would be audited before any does
    * (inferred reading: n ranges over ended tranches when A_n is empty).
    *
    * What this node did not see must not read as an empty tranche: an empty tranche counts only from `watchedFrom`,
    * the first tranche it watched from its start, and a tranche in `lost` (one of its announcements was dropped
    * unread) never counts.
    */
  def audited(
      announcedBy: scala.collection.Map[Int, scala.collection.Set[Int]],
      positive: scala.collection.Set[Int],
      negative: scala.collection.Set[Int],
      validatorCount: Int,
      currentTranche: Int,
      watchedFrom: Int = 0,
      lost: scala.collection.Set[Int] = Set.empty
  ): Boolean =
    val supermajority = 3 * positive.size > 2 * validatorCount
    val someTrancheDone = (0 to currentTranche).exists { n =>
      val a = announcedBy.getOrElse(n, Set.empty[Int])
      !lost(n) && (if a.isEmpty then n >= watchedFrom && currentTranche > n else a.subsetOf(positive))
    }
    supermajority || (negative.isEmpty && someTrancheDone)

  /** A block whose accumulated report at least a third of validators judge invalid is disregarded. */
  def disregards(negative: scala.collection.Set[Int], validatorCount: Int): Boolean = 3 * negative.size >= validatorCount

  // ---------------------------------------------------------------------------
  // CE 144 wire format (jam-np simple.md "CE 144: Audit announcement"; upstream jam-np#23 will replace it)
  // ---------------------------------------------------------------------------

  /** `len++[Core Index ++ Work-Report Hash] ++ Ed25519 Signature`. */
  final case class Announcement(items: Seq[(Int, Hash)], signature: Array[Byte]):
    def encode: Array[Byte] =
      JamCodecs.encodeCompactInteger(items.size.toLong) ++ items.flatMap { (core, h) =>
        Array((core & 0xff).toByte, ((core >> 8) & 0xff).toByte) ++ h.bytes.toArray
      } ++ signature

  /** A no-show: `Validator Index ++ Announcement` from the previous tranche. */
  final case class NoShow(validator: Int, announcement: Announcement)

  /** The validators that `noShows` proves announced `report` in the tranche before `tranche`: each named by an index
    * into `validators`, with an announcement of that tranche and block that lists the report and carries that
    * validator's signature. Anything else is a claim and does not count towards a_n. That the validator has not
    * judged since cannot be proven, and is taken on the announcer's word (it is the announcer's view, eq:noshows).
    */
  def provenNoShows(
      noShows: Seq[NoShow],
      report: Hash,
      tranche: Int,
      headerHash: Hash,
      validators: IndexedSeq[Array[Byte]]
  ): Set[Int] =
    noShows.iterator.distinctBy(_.validator).collect {
      case NoShow(i, a)
          if validators.indices.contains(i) && a.items.exists(_._2 == report) &&
            Ed25519.verify(validators(i), announcementMessage(tranche - 1, a.items, headerHash), a.signature) =>
        i
    }.toSet

  def enoughNoShows(
      noShows: Seq[NoShow],
      needed: Int,
      report: Hash,
      tranche: Int,
      headerHash: Hash,
      validators: IndexedSeq[Array[Byte]],
      held: Int => Option[Announcement] = _ => None,
      verify: (Array[Byte], Array[Byte], Array[Byte]) => Boolean = Ed25519.verify
  ): Boolean =
    val named = scala.collection.mutable.Set.empty[Int]
    val it = noShows.iterator
    var proven = 0
    var ok = true
    while ok && proven < needed && it.hasNext do
      val NoShow(i, a) = it.next()
      ok = validators.indices.contains(i) && named.add(i) && a.items.exists(_._2 == report) &&
        (held(i).exists(h => h.items == a.items && java.util.Arrays.equals(h.signature, a.signature)) ||
          verify(validators(i), announcementMessage(tranche - 1, a.items, headerHash), a.signature))
      if ok then proven += 1
    ok && proven >= needed

  /** Message 1: `Header Hash ++ Tranche ++ Announcement`. */
  def encodeHead(headerHash: Hash, tranche: Int, a: Announcement): Array[Byte] =
    headerHash.bytes.toArray ++ Array(tranche.toByte) ++ a.encode

  /** Message 2 for tranche 0: the Bandersnatch signature s_0. */
  def encodeFirstEvidence(s0: Array[Byte]): Array[Byte] = s0

  /** Message 2 for tranche n > 0: per announced report, `s_n(w) ++ len++[No-Show]`. */
  def encodeLaterEvidence(perReport: Seq[(Array[Byte], Seq[NoShow])]): Array[Byte] =
    perReport.flatMap { (sig, noShows) =>
      sig ++ JamCodecs.encodeCompactInteger(noShows.size.toLong) ++ noShows.flatMap { ns =>
        Array((ns.validator & 0xff).toByte, ((ns.validator >> 8) & 0xff).toByte) ++ ns.announcement.encode
      }
    }.toArray

  private final class Reader(bytes: Array[Byte], var off: Int = 0):
    def take(n: Int): Array[Byte] =
      if off + n > bytes.length then throw new IllegalArgumentException("truncated")
      val out = java.util.Arrays.copyOfRange(bytes, off, off + n)
      off += n
      out
    def u16: Int =
      val b = take(2)
      (b(0) & 0xff) | ((b(1) & 0xff) << 8)
    def compact: Long =
      val (v, c) = JamCodecs.decodeCompactInteger(bytes, off)
      off += c
      v
    def announcement: Announcement =
      val n = compact.toInt
      if n < 0 || n > 1024 then throw new IllegalArgumentException(s"bad item count $n")
      val items = (0 until n).map(_ => (u16, Hash(take(32))))
      Announcement(items, take(64))
    def done: Boolean = off == bytes.length

  def decodeHead(bytes: Array[Byte]): Either[String, (Hash, Int, Announcement)] =
    try
      val r = new Reader(bytes)
      val header = Hash(r.take(32))
      val tranche = r.take(1)(0) & 0xff
      val a = r.announcement
      if r.done then Right((header, tranche, a)) else Left("trailing bytes")
    catch case e: Exception => Left(s"bad announcement: ${e.getMessage}")

  def decodeLaterEvidence(
      bytes: Array[Byte],
      reports: Int,
      maxNoShows: Int
  ): Either[String, Seq[(Array[Byte], Seq[NoShow])]] =
    try
      val r = new Reader(bytes)
      val out = (0 until reports).map { _ =>
        val sig = r.take(96)
        val count = r.compact
        if count < 0 || count > maxNoShows then throw new IllegalArgumentException(s"bad no-show count $count")
        val n = count.toInt
        sig -> (0 until n).map(_ => NoShow(r.u16, r.announcement))
      }
      if r.done then Right(out) else Left("trailing bytes")
    catch case e: Exception => Left(s"bad evidence: ${e.getMessage}")
