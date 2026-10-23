package io.forge.jam.node

import io.forge.jam.core.Shuffle
import io.forge.jam.core.codec.JamCodecs
import io.forge.jam.core.primitives.Hash

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

  /** a_n(w) for n > 0: `V / (65536 · C_bias) · E₂⁻¹(Y(s_n(w))[0..2]) < noShows`, in integers. */
  def escalates(seedOutput: Array[Byte], validatorCount: Int, noShows: Int): Boolean =
    val d = (seedOutput(0) & 0xff) | ((seedOutput(1) & 0xff) << 8)
    validatorCount.toLong * d < noShows.toLong * 65536L * BiasFactor

  /** x_n of eq:announcement: `E₂(core) ++ H(w)` per announced report, in the order the announcement lists them, with
    * no length prefix. That is what polkajam v0.8.0 signs: its two-report announcements verify only in wire order
    * (run AA, 2026-10-22).
    */
  def announcedItems(items: Seq[(Int, Hash)]): Array[Byte] =
    items.flatMap((core, h) => Array((core & 0xff).toByte, ((core >> 8) & 0xff).toByte) ++ h.bytes.toArray).toArray

  /** The Ed25519 message of an announcement S, eq:announcement: `$jam_announce ++ n ++ x_n ++ H(header)`. */
  def announcementMessage(tranche: Int, items: Seq[(Int, Hash)], headerHash: Hash): Array[Byte] =
    (AnnounceContext :+ tranche.toByte) ++ announcedItems(items) ++ headerHash.bytes.toArray

  /** U(w): no negative judgment and some started tranche whose announced auditors all judged positively, or more
    * than two thirds of the validators judged positively. A tranche 0 that nobody announced in counts only once it
    * has ended, and an empty later tranche never counts: under the literal ∅ ⊆ J⊤ every report would read as
    * audited before any announcement arrives (inferred reading).
    */
  def audited(
      announcedBy: Map[Int, Set[Int]],
      positive: Set[Int],
      negative: Set[Int],
      validatorCount: Int,
      currentTranche: Int
  ): Boolean =
    val supermajority = 3 * positive.size > 2 * validatorCount
    val someTrancheDone = (0 to currentTranche).exists { n =>
      val a = announcedBy.getOrElse(n, Set.empty)
      if a.isEmpty then n == 0 && currentTranche > 0 else a.subsetOf(positive)
    }
    supermajority || (negative.isEmpty && someTrancheDone)

  /** A block whose accumulated report at least a third of validators judge invalid is disregarded. */
  def disregards(negative: Set[Int], validatorCount: Int): Boolean = 3 * negative.size >= validatorCount

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

  def decodeLaterEvidence(bytes: Array[Byte], reports: Int): Either[String, Seq[(Array[Byte], Seq[NoShow])]] =
    try
      val r = new Reader(bytes)
      val out = (0 until reports).map { _ =>
        val sig = r.take(96)
        val n = r.compact.toInt
        sig -> (0 until n).map(_ => NoShow(r.u16, r.announcement))
      }
      if r.done then Right(out) else Left("trailing bytes")
    catch case e: Exception => Left(s"bad evidence: ${e.getMessage}")
