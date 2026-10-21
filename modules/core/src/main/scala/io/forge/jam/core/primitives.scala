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

  object Hash:
    val Size: Int = 32

    def apply(bytes: Array[Byte]): Hash =
      require(bytes.length == Size, s"Hash must be $Size bytes, got ${bytes.length}")
      new Hash(bytes.clone())

    /** The all-zero hash. Shared: like every Hash, its array must never be mutated. */
    val zero: Hash = new Hash(new Array[Byte](Size))

    /** Takes ownership of `bytes` without copying; throws on wrong size. The caller must not mutate it afterwards. */
    def wrap(bytes: Array[Byte]): Hash =
      require(bytes.length == Size, s"Hash must be $Size bytes, got ${bytes.length}")
      new Hash(bytes)

    /** Copies `bytes` if it has the right size. */
    def fromArray(bytes: Array[Byte]): Option[Hash] =
      if bytes.length == Size then Some(new Hash(bytes.clone())) else None

    def fromHex(hex: String): Either[String, Hash] =
      val cleanHex = if hex.startsWith("0x") then hex.drop(2) else hex
      if cleanHex.length != Size * 2 then
        Left(s"Hash must be $Size bytes (${Size * 2} hex chars), got ${cleanHex.length / 2} bytes")
      else JamBytes.parseHex(cleanHex).map(new Hash(_))

    given Decoder[Hash] = Decoder.decodeString.emap(hex => fromHex(hex).left.map(err => s"Invalid Hash: $err"))

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

  object BandersnatchPublicKey:
    val Size: Int = 32

    def apply(bytes: Array[Byte]): BandersnatchPublicKey =
      require(bytes.length == Size, s"BandersnatchPublicKey must be $Size bytes, got ${bytes.length}")
      new BandersnatchPublicKey(bytes.clone())

    def zero: BandersnatchPublicKey = new BandersnatchPublicKey(new Array[Byte](Size))

    /** Takes ownership of `bytes` without copying; throws on wrong size. The caller must not mutate it afterwards. */
    def wrap(bytes: Array[Byte]): BandersnatchPublicKey =
      require(bytes.length == Size, s"BandersnatchPublicKey must be $Size bytes, got ${bytes.length}")
      new BandersnatchPublicKey(bytes)

    /** Copies `bytes` if it has the right size. */
    def fromArray(bytes: Array[Byte]): Option[BandersnatchPublicKey] =
      if bytes.length == Size then Some(new BandersnatchPublicKey(bytes.clone())) else None

    def fromHex(hex: String): Either[String, BandersnatchPublicKey] =
      val cleanHex = if hex.startsWith("0x") then hex.drop(2) else hex
      if cleanHex.length != Size * 2 then
        Left(s"BandersnatchPublicKey must be $Size bytes (${Size * 2} hex chars), got ${cleanHex.length / 2} bytes")
      else JamBytes.parseHex(cleanHex).map(new BandersnatchPublicKey(_))

    given Decoder[BandersnatchPublicKey] = Decoder.decodeString.emap(hex => fromHex(hex).left.map(err => s"Invalid BandersnatchPublicKey: $err"))

  /** Bandersnatch signature (96 bytes) */
  final case class BandersnatchSignature private (private val underlying: Array[Byte]):
    def bytes: Array[Byte] = underlying.clone()
    override def equals(obj: Any): Boolean = obj match
      case that: BandersnatchSignature => java.util.Arrays.equals(this.underlying, that.underlying)
      case _ => false
    override def hashCode(): Int = java.util.Arrays.hashCode(underlying)

  object BandersnatchSignature:
    val Size: Int = 96
    def apply(bytes: Array[Byte]): BandersnatchSignature =
      require(bytes.length == Size)
      new BandersnatchSignature(bytes.clone())

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

  object Ed25519PublicKey:
    val Size: Int = 32

    def apply(bytes: Array[Byte]): Ed25519PublicKey =
      require(bytes.length == Size, s"Ed25519PublicKey must be $Size bytes, got ${bytes.length}")
      new Ed25519PublicKey(bytes.clone())

    def zero: Ed25519PublicKey = new Ed25519PublicKey(new Array[Byte](Size))

    /** Takes ownership of `bytes` without copying; throws on wrong size. The caller must not mutate it afterwards. */
    def wrap(bytes: Array[Byte]): Ed25519PublicKey =
      require(bytes.length == Size, s"Ed25519PublicKey must be $Size bytes, got ${bytes.length}")
      new Ed25519PublicKey(bytes)

    /** Copies `bytes` if it has the right size. */
    def fromArray(bytes: Array[Byte]): Option[Ed25519PublicKey] =
      if bytes.length == Size then Some(new Ed25519PublicKey(bytes.clone())) else None

    def fromHex(hex: String): Either[String, Ed25519PublicKey] =
      val cleanHex = if hex.startsWith("0x") then hex.drop(2) else hex
      if cleanHex.length != Size * 2 then
        Left(s"Ed25519PublicKey must be $Size bytes (${Size * 2} hex chars), got ${cleanHex.length / 2} bytes")
      else JamBytes.parseHex(cleanHex).map(new Ed25519PublicKey(_))

    given Decoder[Ed25519PublicKey] = Decoder.decodeString.emap(hex => fromHex(hex).left.map(err => s"Invalid Ed25519PublicKey: $err"))

  /** Ed25519 signature (64 bytes) */
  final case class Ed25519Signature private (private val underlying: Array[Byte]):
    def bytes: Array[Byte] = underlying.clone()
    /** The backing array without a copy; callers must not mutate it. */
    def toArrayUnsafe: Array[Byte] = underlying
    override def equals(obj: Any): Boolean = obj match
      case that: Ed25519Signature => java.util.Arrays.equals(this.underlying, that.underlying)
      case _ => false
    override def hashCode(): Int = java.util.Arrays.hashCode(underlying)

  object Ed25519Signature:
    val Size: Int = 64
    def apply(bytes: Array[Byte]): Ed25519Signature =
      require(bytes.length == Size)
      new Ed25519Signature(bytes.clone())

    given Decoder[Ed25519Signature] = Decoder.decodeString.emap { hex =>
      val cleanHex = if hex.startsWith("0x") then hex.drop(2) else hex
      if cleanHex.length != Size * 2 then
        Left(s"Ed25519Signature must be $Size bytes, got ${cleanHex.length / 2} bytes")
      else
        try
          val bytes = cleanHex.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
          Right(Ed25519Signature(bytes))
        catch
          case _: NumberFormatException => Left("Invalid hex character")
    }

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

  object BlsPublicKey:
    val Size: Int = 144

    def apply(bytes: Array[Byte]): BlsPublicKey =
      require(bytes.length == Size, s"BlsPublicKey must be $Size bytes, got ${bytes.length}")
      new BlsPublicKey(bytes.clone())

    def zero: BlsPublicKey = new BlsPublicKey(new Array[Byte](Size))

    /** Takes ownership of `bytes` without copying; throws on wrong size. The caller must not mutate it afterwards. */
    def wrap(bytes: Array[Byte]): BlsPublicKey =
      require(bytes.length == Size, s"BlsPublicKey must be $Size bytes, got ${bytes.length}")
      new BlsPublicKey(bytes)

    /** Copies `bytes` if it has the right size. */
    def fromArray(bytes: Array[Byte]): Option[BlsPublicKey] =
      if bytes.length == Size then Some(new BlsPublicKey(bytes.clone())) else None

    def fromHex(hex: String): Either[String, BlsPublicKey] =
      val cleanHex = if hex.startsWith("0x") then hex.drop(2) else hex
      if cleanHex.length != Size * 2 then
        Left(s"BlsPublicKey must be $Size bytes (${Size * 2} hex chars), got ${cleanHex.length / 2} bytes")
      else JamBytes.parseHex(cleanHex).map(new BlsPublicKey(_))

    given Decoder[BlsPublicKey] = Decoder.decodeString.emap(hex => fromHex(hex).left.map(err => s"Invalid BlsPublicKey: $err"))

  /** BLS signature (48 bytes) */
  final case class BlsSignature private (private val underlying: Array[Byte]):
    def bytes: Array[Byte] = underlying.clone()
    override def equals(obj: Any): Boolean = obj match
      case that: BlsSignature => java.util.Arrays.equals(this.underlying, that.underlying)
      case _ => false
    override def hashCode(): Int = java.util.Arrays.hashCode(underlying)

  object BlsSignature:
    val Size: Int = 48
    def apply(bytes: Array[Byte]): BlsSignature =
      require(bytes.length == Size)
      new BlsSignature(bytes.clone())

  // ============================================================================
  // Protocol Identifiers
  // ============================================================================

  /** Service identifier (32-bit) */
  opaque type ServiceId = UInt

  object ServiceId:
    def apply(v: UInt): ServiceId = v
    @targetName("serviceIdFromInt")
    def apply(v: Int): ServiceId = UInt(v)
    val Zero: ServiceId = UInt(0)

  extension (sid: ServiceId)
    @targetName("serviceIdValue")
    def value: UInt = sid
    @targetName("serviceIdToInt")
    def toInt: Int = sid.signed

  /** Core index (16-bit) - identifies a validator core */
  opaque type CoreIndex = UShort

  object CoreIndex:
    def apply(v: UShort): CoreIndex = v
    @targetName("coreIndexFromInt")
    def apply(v: Int): CoreIndex = UShort(v)
    val Zero: CoreIndex = UShort(0)

  extension (ci: CoreIndex)
    @targetName("coreIndexValue")
    def value: UShort = ci
    @targetName("coreIndexToInt")
    def toInt: Int = ci.toInt

  /** Validator index (16-bit) */
  opaque type ValidatorIndex = UShort

  object ValidatorIndex:
    def apply(v: UShort): ValidatorIndex = v
    @targetName("validatorIndexFromInt")
    def apply(v: Int): ValidatorIndex = UShort(v)

  extension (vi: ValidatorIndex)
    @targetName("validatorIndexValue")
    def value: UShort = vi
    @targetName("validatorIndexToInt")
    def toInt: Int = vi.toInt

  // ============================================================================
  // Time and Epochs
  // ============================================================================

  /** Timeslot (32-bit) - time is measured in slots */
  opaque type Timeslot = UInt

  object Timeslot:
    def apply(v: UInt): Timeslot = v
    @targetName("timeslotFromInt")
    def apply(v: Int): Timeslot = UInt(v)
    val Zero: Timeslot = UInt(0)

  extension (ts: Timeslot)
    @targetName("timeslotValue")
    def value: UInt = ts
    @targetName("timeslotToInt")
    def toInt: Int = ts.signed
    @targetName("timeslotPlusInt")
    def +(n: Int): Timeslot = ts + UInt(n)
    @targetName("timeslotMinusInt")
    def -(n: Int): Timeslot = ts - UInt(n)

  /** Epoch index (32-bit) */
  opaque type EpochIndex = UInt

  object EpochIndex:
    def apply(v: UInt): EpochIndex = v
    @targetName("epochIndexFromInt")
    def apply(v: Int): EpochIndex = UInt(v)
    val Zero: EpochIndex = UInt(0)

  extension (e: EpochIndex)
    @targetName("epochIndexValue")
    def value: UInt = e
    @targetName("epochIndexToInt")
    def toInt: Int = e.signed

  // ============================================================================
  // Gas and Balance
  // ============================================================================

  /** Gas amount (64-bit unsigned) */
  opaque type Gas = ULong

  object Gas:
    def apply(v: ULong): Gas = v
    @targetName("gasFromLong")
    def apply(v: Long): Gas = ULong(v)
    val Zero: Gas = ULong(0)
    val Max: Gas = ULong(-1L)

  extension (g: Gas)
    @targetName("gasValue")
    def value: ULong = g
    @targetName("gasToLong")
    def toLong: Long = g.signed
    @targetName("gasPlus")
    def +(other: Gas): Gas = g + other
    @targetName("gasMinus")
    def -(other: Gas): Gas = g - other
    @targetName("gasGe")
    def >=(other: Gas): Boolean = g >= other
    @targetName("gasLt")
    def <(other: Gas): Boolean = g < other

  /** Balance amount (64-bit unsigned) */
  opaque type Balance = ULong

  object Balance:
    def apply(v: ULong): Balance = v
    @targetName("balanceFromLong")
    def apply(v: Long): Balance = ULong(v)
    val Zero: Balance = ULong(0)

  extension (b: Balance)
    @targetName("balanceValue")
    def value: ULong = b
    @targetName("balanceToLong")
    def toLong: Long = b.signed
    @targetName("balancePlus")
    def +(other: Balance): Balance = b + other
    @targetName("balanceMinus")
    def -(other: Balance): Balance = b - other
