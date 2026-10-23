package io.forge.jam.node

import io.forge.jam.core.codec.{ByteReader, ByteWriter, CodecDecodingException}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.crypto.Bls

/** BEEFY commitments: validator v signs `$jam_beefy ++ last(β_H).b`, the accumulation
  * output log super-peak of the finalized block, with its BLS key κ'[v]_bls. JAMNP-S has no BEEFY stream yet, so the
  * wire format here is our own (prototype).
  */
object BeefyMessages:

  /** X_beefy. */
  val Context: Array[Byte] = "$jam_beefy".getBytes("US-ASCII")

  /** Prototype stream kind, outside the CE range JAMNP-S assigns (128-153). */
  val StreamKind: Byte = 254.toByte

  val SignedCommitmentSize: Int = 32 + 4 + 2 + 32 + Bls.PublicKeySize + Bls.SignatureSize

  def payload(superPeak: Hash): Array[Byte] = Context ++ superPeak.bytes

  /** Super-majority of `v` validators: ⌊2V/3⌋ + 1. */
  def threshold(v: Int): Int = 2 * v / 3 + 1

  /** One validator's commitment for a block. The signer's BLS key travels along because dev chain specs register
    * all-zero BLS keys; when κ'[v]_bls is set the receiver requires it to match.
    */
  final case class SignedCommitment(
      block: Hash,
      slot: Long,
      validatorIndex: Int,
      superPeak: Hash,
      blsKey: Array[Byte],
      signature: Array[Byte]
  )

  /** A finality proof for third parties: the signers' bitfield over κ' (bit v is bit v%8 of byte v/8) and the sum of
    * their signatures.
    */
  final case class BeefyProof(block: Hash, slot: Long, superPeak: Hash, signers: Array[Byte], aggregate: Array[Byte]):
    def signerIndices: List[Int] = (0 until signers.length * 8).filter(i => (signers(i / 8) >> (i % 8) & 1) == 1).toList

  def sign(block: Hash, slot: Long, index: Int, superPeak: Hash, keys: ValidatorKeySet): SignedCommitment =
    SignedCommitment(block, slot, index, superPeak, keys.blsPublic, Bls.sign(keys.blsSecret, payload(superPeak)))

  def verify(c: SignedCommitment): Boolean = Bls.verify(c.blsKey, payload(c.superPeak), c.signature)

  /** Aggregate commitments to one block (all on the same super-peak) from distinct validators of a `v`-sized set. */
  def aggregate(v: Int, cs: Seq[SignedCommitment]): Option[BeefyProof] =
    cs.headOption.flatMap { c =>
      val bits = new Array[Byte]((v + 7) / 8)
      cs.foreach(s => bits(s.validatorIndex / 8) = (bits(s.validatorIndex / 8) | 1 << (s.validatorIndex % 8)).toByte)
      Bls.aggregate(cs.map(_.signature)).map(BeefyProof(c.block, c.slot, c.superPeak, bits, _))
    }

  /** Checks a proof against the BLS keys of κ' at its block: a super-majority signed its super-peak. */
  def verifyProof(p: BeefyProof, keys: IndexedSeq[Array[Byte]]): Boolean =
    val signers = p.signerIndices
    signers.size >= threshold(keys.size) && signers.forall(_ < keys.size) &&
    Bls.verifyAggregate(signers.map(keys), payload(p.superPeak), p.aggregate)

  // ---------------------------------------------------------------------------
  // encoding: Header Hash ++ Slot (u32) ++ Validator Index (u16) ++ Super-peak ++ BLS Key (144) ++ Signature (112)
  // ---------------------------------------------------------------------------

  def encode(c: SignedCommitment): Array[Byte] =
    val w = new ByteWriter(SignedCommitmentSize)
    w.bytes(c.block.bytes)
    w.u32(c.slot)
    w.u16(c.validatorIndex)
    w.bytes(c.superPeak.bytes)
    w.bytes(c.blsKey)
    w.bytes(c.signature)
    w.toArray

  def decode(bytes: Array[Byte]): Either[String, SignedCommitment] =
    if bytes.length != SignedCommitmentSize then Left(s"BEEFY commitment of ${bytes.length} bytes")
    else
      try
        val r = new ByteReader(bytes)
        Right(
          SignedCommitment(
            Hash(r.bytes(32)),
            r.u32(),
            r.u16(),
            Hash(r.bytes(32)),
            r.bytes(Bls.PublicKeySize),
            r.bytes(Bls.SignatureSize)
          )
        )
      catch case e: CodecDecodingException => Left(e.getMessage)

  /** Header Hash ++ Slot ++ Super-peak ++ len++Signers ++ Aggregate (48). */
  def encodeProof(p: BeefyProof): Array[Byte] =
    val w = new ByteWriter(128)
    w.bytes(p.block.bytes)
    w.u32(p.slot)
    w.bytes(p.superPeak.bytes)
    w.compact(p.signers.length.toLong)
    w.bytes(p.signers)
    w.bytes(p.aggregate)
    w.toArray

  def decodeProof(bytes: Array[Byte]): Either[String, BeefyProof] =
    try
      val r = new ByteReader(bytes)
      val block = Hash(r.bytes(32))
      val slot = r.u32()
      val peak = Hash(r.bytes(32))
      val p = BeefyProof(block, slot, peak, r.bytes(r.compactInt()), r.bytes(Bls.AggregateSize))
      if r.isEmpty then Right(p) else Left(s"${r.remaining} trailing byte(s)")
    catch case e: CodecDecodingException => Left(e.getMessage)
