package io.forge.jam.node

import io.forge.jam.core.codec.{ByteReader, ByteWriter, Codec, CodecDecodingException}
import io.forge.jam.core.primitives.{Ed25519PublicKey, Hash}
import io.forge.jam.crypto.Ed25519

/** GRANDPA wire types of JAMNP-S. Integers are little endian, `len++` is a GP
  * compact length.
  */
object GrandpaMessages:

  val Prevote = 0
  val Precommit = 1
  val PrimaryPropose = 2

  val SigningContext: Array[Byte] = "jam_grandpa_vote".getBytes("US-ASCII")

  /** Warp sync responses carry at most this many fragments (CE 153). */
  val MaxWarpFragments = 32

  /** `Target ++ Slot`, where `Target = Header Hash ++ Posterior State Root`. */
  final case class Vote(hash: Hash, stateRoot: Hash, slot: Long):
    def short: String = s"${hash.toHex.take(18)}@$slot"

  /** A vote with its signature and the voter's Ed25519 key. */
  final case class SignedVote(vote: Vote, signature: Array[Byte], signer: Array[Byte]):
    lazy val signerKey: Ed25519PublicKey = Ed25519PublicKey(signer)

  /** `Message Type ++ Vote ++ Signature ++ Ed25519 Public`. */
  final case class SignedMessage(kind: Int, signed: SignedVote)

  /** CE 149 payload: `Round ++ Set Id ++ Signed Message`. */
  final case class VoteMessage(round: Long, setId: Long, message: SignedMessage)

  /** `Target ++ Slot ++ len++[Signed Precommit]`. */
  final case class Commit(target: Vote, precommits: List[SignedVote])

  /** `Round ++ Set Id ++ Commit ++ len++[Header]`; headers are kept encoded. */
  final case class Justification(round: Long, setId: Long, commit: Commit, ancestries: List[Array[Byte]])

  /** CE 152 response: `Round ++ len++[Signed Prevote] ++ len++[Signed Precommit] ++ Base Hash ++ Base Number`. */
  final case class CatchUp(round: Long, prevotes: List[SignedVote], precommits: List[SignedVote], base: Vote)

  /** CE 153 element: `Header ++ Grandpa Justification`. */
  final case class WarpFragment(header: Array[Byte], justification: Justification)

  /** Bytes signed by a voter: `"jam_grandpa_vote" ++ Message Type ++ Vote ++ Round ++ Set Id`. */
  def signingPayload(kind: Int, vote: Vote, round: Long, setId: Long): Array[Byte] =
    val w = new ByteWriter(128)
    w.bytes(SigningContext)
    w.u8(kind)
    writeVote(w, vote)
    w.u64(round)
    w.u32(setId)
    w.toArray

  def sign(kind: Int, vote: Vote, round: Long, setId: Long, keys: ValidatorKeySet): SignedVote =
    val sig = io.forge.jam.crypto.Ed25519ZebraWrapper.sign(keys.ed25519Secret, signingPayload(kind, vote, round, setId))
    SignedVote(vote, sig, keys.ed25519Public)

  def verify(kind: Int, sv: SignedVote, round: Long, setId: Long): Boolean =
    sv.signature.length == 64 && sv.signer.length == 32 &&
      Ed25519.verify(sv.signer, signingPayload(kind, sv.vote, round, setId), sv.signature)

  // ---------------------------------------------------------------------------
  // encoding
  // ---------------------------------------------------------------------------

  def writeVote(w: ByteWriter, v: Vote): Unit =
    w.bytes(v.hash.bytes)
    w.bytes(v.stateRoot.bytes)
    w.u32(v.slot)

  private def writeSigned(w: ByteWriter, s: SignedVote): Unit =
    writeVote(w, s.vote)
    w.bytes(s.signature)
    w.bytes(s.signer)

  private def writeCommit(w: ByteWriter, c: Commit): Unit =
    writeVote(w, c.target)
    w.compact(c.precommits.size.toLong)
    c.precommits.foreach(writeSigned(w, _))

  private def writeJustification(w: ByteWriter, j: Justification): Unit =
    w.u64(j.round)
    w.u32(j.setId)
    writeCommit(w, j.commit)
    w.compact(j.ancestries.size.toLong)
    j.ancestries.foreach(w.bytes(_))

  /** Round ++ set id ++ kind ++ target ++ signature ++ signer. */
  val VoteMessageSize: Int = 177

  def encodeVoteMessage(m: VoteMessage): Array[Byte] =
    val w = new ByteWriter(VoteMessageSize)
    w.u64(m.round)
    w.u32(m.setId)
    w.u8(m.message.kind)
    writeSigned(w, m.message.signed)
    w.toArray

  def encodeCommitMessage(round: Long, setId: Long, c: Commit): Array[Byte] =
    val w = new ByteWriter(256)
    w.u64(round)
    w.u32(setId)
    writeCommit(w, c)
    w.toArray

  def encodeState(round: Long, setId: Long, slot: Long): Array[Byte] =
    val w = new ByteWriter(16)
    w.u64(round)
    w.u32(setId)
    w.u32(slot)
    w.toArray

  def encodeCatchUpRequest(round: Long, setId: Long): Array[Byte] =
    val w = new ByteWriter(12)
    w.u64(round)
    w.u32(setId)
    w.toArray

  def encodeCatchUp(c: CatchUp): Array[Byte] =
    val w = new ByteWriter(512)
    w.u64(c.round)
    w.compact(c.prevotes.size.toLong)
    c.prevotes.foreach(writeSigned(w, _))
    w.compact(c.precommits.size.toLong)
    c.precommits.foreach(writeSigned(w, _))
    writeVote(w, c.base)
    w.toArray

  def encodeJustification(j: Justification): Array[Byte] =
    val w = new ByteWriter(512)
    writeJustification(w, j)
    w.toArray

  def encodeWarpFragments(fs: List[WarpFragment]): Array[Byte] =
    val w = new ByteWriter(1024)
    w.compact(fs.size.toLong)
    fs.foreach { f =>
      w.bytes(f.header)
      writeJustification(w, f.justification)
    }
    w.toArray

  // ---------------------------------------------------------------------------
  // decoding
  // ---------------------------------------------------------------------------

  private def readVote(r: ByteReader): Vote =
    Vote(Hash(r.bytes(32)), Hash(r.bytes(32)), r.u32())

  private def readSigned(r: ByteReader): SignedVote =
    SignedVote(readVote(r), r.bytes(64), r.bytes(32))

  private def readList[A](r: ByteReader, max: Int)(f: ByteReader => A): List[A] =
    val n = r.compactInt()
    if n > max then r.fail(s"list of $n exceeds $max")
    List.fill(n)(f(r))

  private def readCommit(r: ByteReader): Commit =
    Commit(readVote(r), readList(r, 4096)(readSigned))

  /** Reads one encoded header and returns its bytes. */
  private def readHeader(bytes: Array[Byte], r: ByteReader, headers: Codec[?]): Array[Byte] =
    val start = r.position
    headers.read(r)
    java.util.Arrays.copyOfRange(bytes, start, r.position)

  private def readJustification(bytes: Array[Byte], r: ByteReader, headers: Codec[?]): Justification =
    val round = r.u64()
    val setId = r.u32()
    val commit = readCommit(r)
    Justification(round, setId, commit, readList(r, 4096)(readHeader(bytes, _, headers)))

  private def exact[A](bytes: Array[Byte])(f: ByteReader => A): Either[String, A] =
    try
      val r = new ByteReader(bytes)
      val v = f(r)
      if r.isEmpty then Right(v) else Left(s"${r.remaining} trailing byte(s)")
    catch case e: CodecDecodingException => Left(e.getMessage)

  def decodeVoteMessage(bytes: Array[Byte]): Either[String, VoteMessage] =
    exact(bytes) { r =>
      val round = r.u64()
      val setId = r.u32()
      val kind = r.u8()
      if kind > PrimaryPropose then r.fail(s"unknown message type $kind")
      VoteMessage(round, setId, SignedMessage(kind, readSigned(r)))
    }

  def decodeCommitMessage(bytes: Array[Byte]): Either[String, (Long, Long, Commit)] =
    exact(bytes)(r => (r.u64(), r.u32(), readCommit(r)))

  def decodeState(bytes: Array[Byte]): Either[String, (Long, Long, Long)] =
    exact(bytes)(r => (r.u64(), r.u32(), r.u32()))

  def decodeCatchUpRequest(bytes: Array[Byte]): Either[String, (Long, Long)] =
    exact(bytes)(r => (r.u64(), r.u32()))

  def decodeCatchUp(bytes: Array[Byte]): Either[String, CatchUp] =
    exact(bytes) { r =>
      val round = r.u64()
      val prevotes = readList(r, 4096)(readSigned)
      val precommits = readList(r, 4096)(readSigned)
      CatchUp(round, prevotes, precommits, readVote(r))
    }

  def decodeJustification(bytes: Array[Byte], headers: Codec[?]): Either[String, Justification] =
    exact(bytes)(readJustification(bytes, _, headers))

  def decodeWarpFragments(bytes: Array[Byte], headers: Codec[?]): Either[String, List[WarpFragment]] =
    exact(bytes) { r =>
      readList(r, MaxWarpFragments) { r =>
        val header = readHeader(bytes, r, headers)
        WarpFragment(header, readJustification(bytes, r, headers))
      }
    }
