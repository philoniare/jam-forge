package io.forge.jam.core

import scala.annotation.targetName
import spire.math.{UByte, UShort, UInt, ULong}
import io.circe.Decoder
import io.forge.jam.core.codec.{JamEncoder, JamDecoder}
import io.forge.jam.core.json.JsonHelpers

/**
 * Core primitive types for JAM using Spire unsigned types.
 *
 * These types provide the foundation for the entire JAM implementation,
 * offering type-safe wrappers around unsigned integers with zero-cost
 * abstractions via Scala 3 opaque types.
 */
object primitives:

  // ══════════════════════════════════════════════════════════════════════════
  // Hash Types (32 bytes = 256 bits)
  // ══════════════════════════════════════════════════════════════════════════

  /**
   * A 256-bit hash value (32 bytes).
   * Used for Blake2b-256 hashes throughout the protocol.
   * Uses content-based equality.
   */
  final case class Hash private (private val underlying: Array[Byte]):
    def bytes: Array[Byte] = underlying.clone()
    def toHex: String = underlying.map(b => f"${b & 0xff}%02x").mkString
    def size: Int = underlying.length

    override def equals(obj: Any): Boolean = obj match
      case that: Hash => java.util.Arrays.equals(this.underlying, that.underlying)
      case _ => false

    override def hashCode(): Int = java.util.Arrays.hashCode(underlying)

  object Hash:
    val Size: Int = 32

    def apply(bytes: Array[Byte]): Hash =
      require(bytes.length == Size, s"Hash must be $Size bytes, got ${bytes.length}")
      new Hash(bytes.clone())

    def zero: Hash = new Hash(new Array[Byte](Size))

    def fromHex(hex: String): Either[String, Hash] =
      if hex.length != Size * 2 then
        Left(s"Hex string must be ${Size * 2} characters")
      else
        try
          val bytes = hex.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
          Right(new Hash(bytes))
        catch
          case _: NumberFormatException => Left("Invalid hex string")

    given JamEncoder[Hash] with
      def encode(a: Hash): JamBytes = JamBytes(a.bytes)

    given JamDecoder[Hash] with
      def decode(bytes: JamBytes, offset: Int): (Hash, Int) =
        (Hash(bytes.toArray.slice(offset, offset + Size)), Size)

    given Decoder[Hash] = Decoder.decodeString.emap { hex =>
      val cleanHex = if hex.startsWith("0x") then hex.drop(2) else hex
      if cleanHex.length != Size * 2 then
        Left(s"Hash must be $Size bytes (${Size * 2} hex chars), got ${cleanHex.length / 2} bytes")
      else
        fromHex(cleanHex).left.map(err => s"Invalid hash: $err")
    }

  // ══════════════════════════════════════════════════════════════════════════
  // Bandersnatch Types
  // ══════════════════════════════════════════════════════════════════════════

  /** Bandersnatch public key (32 bytes) */
  final case class BandersnatchPublicKey private (private val underlying: Array[Byte]):
    def bytes: Array[Byte] = underlying.clone()
    override def equals(obj: Any): Boolean = obj match
      case that: BandersnatchPublicKey => java.util.Arrays.equals(this.underlying, that.underlying)
      case _ => false
    override def hashCode(): Int = java.util.Arrays.hashCode(underlying)

  object BandersnatchPublicKey:
    val Size: Int = 32
    def apply(bytes: Array[Byte]): BandersnatchPublicKey =
      require(bytes.length == Size)
      new BandersnatchPublicKey(bytes.clone())
    def zero: BandersnatchPublicKey = new BandersnatchPublicKey(new Array[Byte](Size))

    given Decoder[BandersnatchPublicKey] = Decoder.decodeString.emap { hex =>
      val cleanHex = if hex.startsWith("0x") then hex.drop(2) else hex
      if cleanHex.length != Size * 2 then
        Left(s"BandersnatchPublicKey must be $Size bytes, got ${cleanHex.length / 2} bytes")
      else
        try
          val bytes = cleanHex.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
          Right(BandersnatchPublicKey(bytes))
        catch
          case _: NumberFormatException => Left("Invalid hex character")
    }

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

  // ══════════════════════════════════════════════════════════════════════════
  // Ed25519 Types
  // ══════════════════════════════════════════════════════════════════════════

  /** Ed25519 public key (32 bytes) */
  final case class Ed25519PublicKey private (private val underlying: Array[Byte]):
    def bytes: Array[Byte] = underlying.clone()
    override def equals(obj: Any): Boolean = obj match
      case that: Ed25519PublicKey => java.util.Arrays.equals(this.underlying, that.underlying)
      case _ => false
    override def hashCode(): Int = java.util.Arrays.hashCode(underlying)

  object Ed25519PublicKey:
    val Size: Int = 32
    def apply(bytes: Array[Byte]): Ed25519PublicKey =
      require(bytes.length == Size)
      new Ed25519PublicKey(bytes.clone())

    given JamEncoder[Ed25519PublicKey] with
      def encode(a: Ed25519PublicKey): JamBytes = JamBytes(a.bytes)

    given JamDecoder[Ed25519PublicKey] with
      def decode(bytes: JamBytes, offset: Int): (Ed25519PublicKey, Int) =
        (Ed25519PublicKey(bytes.toArray.slice(offset, offset + Size)), Size)

    given Decoder[Ed25519PublicKey] = Decoder.decodeString.emap { hex =>
      val cleanHex = if hex.startsWith("0x") then hex.drop(2) else hex
      if cleanHex.length != Size * 2 then
        Left(s"Ed25519PublicKey must be $Size bytes, got ${cleanHex.length / 2} bytes")
      else
        try
          val bytes = cleanHex.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
          Right(Ed25519PublicKey(bytes))
        catch
          case _: NumberFormatException => Left("Invalid hex character")
    }

  /** Ed25519 signature (64 bytes) */
  final case class Ed25519Signature private (private val underlying: Array[Byte]):
    def bytes: Array[Byte] = underlying.clone()
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

  // ══════════════════════════════════════════════════════════════════════════
  // BLS Types
  // ══════════════════════════════════════════════════════════════════════════

  /** BLS public key (144 bytes) */
  final case class BlsPublicKey private (private val underlying: Array[Byte]):
    def bytes: Array[Byte] = underlying.clone()
    override def equals(obj: Any): Boolean = obj match
      case that: BlsPublicKey => java.util.Arrays.equals(this.underlying, that.underlying)
      case _ => false
    override def hashCode(): Int = java.util.Arrays.hashCode(underlying)

  object BlsPublicKey:
    val Size: Int = 144
    def apply(bytes: Array[Byte]): BlsPublicKey =
      require(bytes.length == Size)
      new BlsPublicKey(bytes.clone())

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

  // ══════════════════════════════════════════════════════════════════════════
  // Protocol Identifiers
  // ══════════════════════════════════════════════════════════════════════════

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

  // ══════════════════════════════════════════════════════════════════════════
  // Time and Epochs
  // ══════════════════════════════════════════════════════════════════════════

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

  // ══════════════════════════════════════════════════════════════════════════
  // Gas and Balance
  // ══════════════════════════════════════════════════════════════════════════

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
