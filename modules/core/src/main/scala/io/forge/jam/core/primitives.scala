package io.forge.jam.core

import scala.annotation.targetName
import spire.math.{UShort, UInt, ULong}
import io.circe.Decoder

/**
 * Core primitive types for JAM using Spire unsigned types.
 *
 * These types provide the foundation for the entire JAM implementation,
 * offering type-safe wrappers around unsigned integers with zero-cost
 * abstractions via Scala 3 opaque types.
 *
 * Types that represent fixed-size byte sequences (Hash, PublicKey types)
 * wrap a private byte array and compare by content.
 */
object primitives:

  /** Constructors shared by the fixed-size byte wrappers. Only the companions use it; each class keeps its own equals/hashCode. */
  abstract class FixedBytes[T](val Size: Int, name: String):
    protected def make(bytes: Array[Byte]): T

    def apply(bytes: Array[Byte]): T =
      require(bytes.length == Size, s"$name must be $Size bytes, got ${bytes.length}")
      make(bytes.clone())

    def zero: T = make(new Array[Byte](Size))

    /** Takes ownership of `bytes` without copying; throws on wrong size. The caller must not mutate it afterwards. */
    def wrap(bytes: Array[Byte]): T =
      require(bytes.length == Size, s"$name must be $Size bytes, got ${bytes.length}")
      make(bytes)

    /** Copies `bytes` if it has the right size. */
    def fromArray(bytes: Array[Byte]): Option[T] =
      if bytes.length == Size then Some(make(bytes.clone())) else None

    def fromHex(hex: String): Either[String, T] =
      val cleanHex = if hex.startsWith("0x") then hex.drop(2) else hex
      if cleanHex.length != Size * 2 then
        Left(s"$name must be $Size bytes (${Size * 2} hex chars), got ${cleanHex.length / 2} bytes")
      else JamBytes.parseHex(cleanHex).map(make)

    given Decoder[T] = Decoder.decodeString.emap(hex => fromHex(hex).left.map(err => s"Invalid $name: $err"))

  // ============================================================================
  // Hash Types (32 bytes = 256 bits)
  // ============================================================================

  /**
   * A 256-bit hash value (32 bytes).
   * Used for Blake2b-256 hashes throughout the protocol.
   * Uses content-based equality.
   *
   */
  final case class Hash private (private val underlying: Array[Byte]):
    /** Returns a defensive copy of the underlying bytes (for Java interop) */
    def bytes: Array[Byte] = underlying.clone()
    /** The backing array without a copy; callers must not mutate it. */
    def toArrayUnsafe: Array[Byte] = underlying
    def toHex: String = JamBytes.toHex(underlying)
    def size: Int = underlying.length
    def toJamBytes: JamBytes = JamBytes.wrap(underlying)

    override def equals(obj: Any): Boolean = obj match
      case that: Hash => java.util.Arrays.equals(this.underlying, that.underlying)
      case _ => false

    // Cached: hashes key the trie's and the backends' hash maps, so one is hashed many times. Same value as before
    // (Arrays.hashCode), so hash-map iteration orders do not change. A racy single check: an int write is atomic,
    // and recomputing gives the same value. 0 means not computed yet.
    private var hashCodeCache: Int = 0

    override def hashCode(): Int =
      var h = hashCodeCache
      if h == 0 then
        h = java.util.Arrays.hashCode(underlying)
        hashCodeCache = h
      h

  object Hash extends FixedBytes[Hash](32, "Hash"):
    protected def make(bytes: Array[Byte]): Hash = new Hash(bytes)

    /** The all-zero hash. Shared: like every Hash, its array must never be mutated. */
    override val zero: Hash = new Hash(new Array[Byte](Size))

    /** No size check; for hot paths whose input is a 32-byte digest by construction. */
    def wrapUnchecked(bytes: Array[Byte]): Hash = new Hash(bytes)

    given Ordering[Hash] = (a, b) => java.util.Arrays.compareUnsigned(a.underlying, b.underlying)

  // ============================================================================
  // Bandersnatch Types
  // ============================================================================

  /**
   * Bandersnatch public key (32 bytes).
   */
  final case class BandersnatchPublicKey private (private val underlying: Array[Byte]):
    /** Returns a defensive copy of the underlying bytes (for Java interop) */
    def bytes: Array[Byte] = underlying.clone()
    /** The backing array without a copy; callers must not mutate it. */
    def toArrayUnsafe: Array[Byte] = underlying

    override def equals(obj: Any): Boolean = obj match
      case that: BandersnatchPublicKey => java.util.Arrays.equals(this.underlying, that.underlying)
      case _ => false

    override def hashCode(): Int = java.util.Arrays.hashCode(underlying)

  object BandersnatchPublicKey extends FixedBytes[BandersnatchPublicKey](32, "BandersnatchPublicKey"):
    protected def make(bytes: Array[Byte]): BandersnatchPublicKey = new BandersnatchPublicKey(bytes)

  /** Bandersnatch signature (96 bytes) */
  final case class BandersnatchSignature private (private val underlying: Array[Byte]):
    def bytes: Array[Byte] = underlying.clone()
    override def equals(obj: Any): Boolean = obj match
      case that: BandersnatchSignature => java.util.Arrays.equals(this.underlying, that.underlying)
      case _ => false
    override def hashCode(): Int = java.util.Arrays.hashCode(underlying)

  object BandersnatchSignature extends FixedBytes[BandersnatchSignature](96, "BandersnatchSignature"):
    protected def make(bytes: Array[Byte]): BandersnatchSignature = new BandersnatchSignature(bytes)

  // ============================================================================
  // Ed25519 Types
  // ============================================================================

  /**
   * Ed25519 public key (32 bytes).
   */
  final case class Ed25519PublicKey private (private val underlying: Array[Byte]):
    /** Returns a defensive copy of the underlying bytes (for Java interop) */
    def bytes: Array[Byte] = underlying.clone()
    /** The backing array without a copy; callers must not mutate it. */
    def toArrayUnsafe: Array[Byte] = underlying

    override def equals(obj: Any): Boolean = obj match
      case that: Ed25519PublicKey => java.util.Arrays.equals(this.underlying, that.underlying)
      case _ => false

    override def hashCode(): Int = java.util.Arrays.hashCode(underlying)

  object Ed25519PublicKey extends FixedBytes[Ed25519PublicKey](32, "Ed25519PublicKey"):
    protected def make(bytes: Array[Byte]): Ed25519PublicKey = new Ed25519PublicKey(bytes)

  /** Ed25519 signature (64 bytes) */
  final case class Ed25519Signature private (private val underlying: Array[Byte]):
    def bytes: Array[Byte] = underlying.clone()
    /** The backing array without a copy; callers must not mutate it. */
    def toArrayUnsafe: Array[Byte] = underlying
    override def equals(obj: Any): Boolean = obj match
      case that: Ed25519Signature => java.util.Arrays.equals(this.underlying, that.underlying)
      case _ => false
    override def hashCode(): Int = java.util.Arrays.hashCode(underlying)

  object Ed25519Signature extends FixedBytes[Ed25519Signature](64, "Ed25519Signature"):
    protected def make(bytes: Array[Byte]): Ed25519Signature = new Ed25519Signature(bytes)

  // ============================================================================
  // BLS Types
  // ============================================================================

  /**
   * BLS public key (144 bytes).
   */
  final case class BlsPublicKey private (private val underlying: Array[Byte]):
    /** Returns a defensive copy of the underlying bytes (for Java interop) */
    def bytes: Array[Byte] = underlying.clone()
    /** The backing array without a copy; callers must not mutate it. */
    def toArrayUnsafe: Array[Byte] = underlying

    override def equals(obj: Any): Boolean = obj match
      case that: BlsPublicKey => java.util.Arrays.equals(this.underlying, that.underlying)
      case _ => false

    override def hashCode(): Int = java.util.Arrays.hashCode(underlying)

  object BlsPublicKey extends FixedBytes[BlsPublicKey](144, "BlsPublicKey"):
    protected def make(bytes: Array[Byte]): BlsPublicKey = new BlsPublicKey(bytes)

  /** BLS signature (48 bytes) */
  final case class BlsSignature private (private val underlying: Array[Byte]):
    def bytes: Array[Byte] = underlying.clone()
    override def equals(obj: Any): Boolean = obj match
      case that: BlsSignature => java.util.Arrays.equals(this.underlying, that.underlying)
      case _ => false
    override def hashCode(): Int = java.util.Arrays.hashCode(underlying)

  object BlsSignature extends FixedBytes[BlsSignature](48, "BlsSignature"):
    protected def make(bytes: Array[Byte]): BlsSignature = new BlsSignature(bytes)

  // ============================================================================
  // Protocol Identifiers
  // ============================================================================

  /** Service identifier (32-bit unsigned, held in a Long) */
  opaque type ServiceId = Long

  object ServiceId:
    inline def apply(v: UInt): ServiceId = v.toLong
    @targetName("serviceIdFromInt")
    inline def apply(v: Int): ServiceId = v & 0xffffffffL
    inline def fromLong(v: Long): ServiceId = v & 0xffffffffL
    val Zero: ServiceId = 0L

  extension (sid: ServiceId)
    @targetName("serviceIdValue")
    inline def value: UInt = UInt(sid.toInt)
    @targetName("serviceIdToInt")
    inline def toInt: Int = sid.toInt
    @targetName("serviceIdToLong")
    inline def toLong: Long = sid

  /** Core index (16-bit unsigned, held in an Int) - identifies a validator core */
  opaque type CoreIndex = Int

  object CoreIndex:
    inline def apply(v: UShort): CoreIndex = v.toInt
    @targetName("coreIndexFromInt")
    inline def apply(v: Int): CoreIndex = v & 0xffff
    val Zero: CoreIndex = 0

  extension (ci: CoreIndex)
    @targetName("coreIndexValue")
    inline def value: UShort = UShort(ci)
    @targetName("coreIndexToInt")
    inline def toInt: Int = ci

  /** Validator index (16-bit unsigned, held in an Int) */
  opaque type ValidatorIndex = Int

  object ValidatorIndex:
    inline def apply(v: UShort): ValidatorIndex = v.toInt
    @targetName("validatorIndexFromInt")
    inline def apply(v: Int): ValidatorIndex = v & 0xffff

  extension (vi: ValidatorIndex)
    @targetName("validatorIndexValue")
    inline def value: UShort = UShort(vi)
    @targetName("validatorIndexToInt")
    inline def toInt: Int = vi

  // ============================================================================
  // Time and Epochs
  // ============================================================================

  /** Timeslot (32-bit unsigned, held in a Long) - time is measured in slots */
  opaque type Timeslot = Long

  object Timeslot:
    inline def apply(v: UInt): Timeslot = v.toLong
    @targetName("timeslotFromInt")
    inline def apply(v: Int): Timeslot = v & 0xffffffffL
    inline def fromLong(v: Long): Timeslot = v & 0xffffffffL
    val Zero: Timeslot = 0L

  extension (ts: Timeslot)
    @targetName("timeslotValue")
    inline def value: UInt = UInt(ts.toInt)
    @targetName("timeslotToInt")
    inline def toInt: Int = ts.toInt
    @targetName("timeslotToLong")
    inline def toLong: Long = ts
    @targetName("timeslotPlusInt")
    inline def +(n: Int): Timeslot = (ts + (n & 0xffffffffL)) & 0xffffffffL
    @targetName("timeslotMinusInt")
    inline def -(n: Int): Timeslot = (ts - (n & 0xffffffffL)) & 0xffffffffL

  /** Epoch index (32-bit unsigned, held in a Long) */
  opaque type EpochIndex = Long

  object EpochIndex:
    inline def apply(v: UInt): EpochIndex = v.toLong
    @targetName("epochIndexFromInt")
    inline def apply(v: Int): EpochIndex = v & 0xffffffffL
    val Zero: EpochIndex = 0L

  extension (e: EpochIndex)
    @targetName("epochIndexValue")
    inline def value: UInt = UInt(e.toInt)
    @targetName("epochIndexToInt")
    inline def toInt: Int = e.toInt
    @targetName("epochIndexToLong")
    inline def toLong: Long = e

  // ============================================================================
  // Gas and Balance
  // ============================================================================

  /** Gas amount (64-bit unsigned, held in a Long; compared unsigned) */
  opaque type Gas = Long

  object Gas:
    inline def apply(v: ULong): Gas = v.signed
    @targetName("gasFromLong")
    inline def apply(v: Long): Gas = v
    val Zero: Gas = 0L
    val Max: Gas = -1L

  extension (g: Gas)
    @targetName("gasValue")
    inline def value: ULong = ULong(g)
    @targetName("gasToLong")
    inline def toLong: Long = g
    @targetName("gasPlus")
    inline def +(other: Gas): Gas = g + other
    @targetName("gasMinus")
    inline def -(other: Gas): Gas = g - other
    @targetName("gasGe")
    inline def >=(other: Gas): Boolean = java.lang.Long.compareUnsigned(g, other) >= 0
    @targetName("gasLt")
    inline def <(other: Gas): Boolean = java.lang.Long.compareUnsigned(g, other) < 0

  /** Balance amount (64-bit unsigned, held in a Long) */
  opaque type Balance = Long

  object Balance:
    inline def apply(v: ULong): Balance = v.signed
    @targetName("balanceFromLong")
    inline def apply(v: Long): Balance = v
    val Zero: Balance = 0L

  extension (b: Balance)
    @targetName("balanceValue")
    inline def value: ULong = ULong(b)
    @targetName("balanceToLong")
    inline def toLong: Long = b
    @targetName("balancePlus")
    inline def +(other: Balance): Balance = b + other
    @targetName("balanceMinus")
    inline def -(other: Balance): Balance = b - other
