package io.forge.jam.core.types

import _root_.scodec.*
import _root_.scodec.bits.*
import _root_.scodec.codecs.*
import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.{Hash, ValidatorIndex, Ed25519Signature}
import io.forge.jam.core.scodec.JamCodecs.{compactInteger, hashCodec}
import io.forge.jam.core.json.JsonHelpers.parseHex
import io.circe.{Decoder, DecodingFailure}
import spire.math.{UShort, UInt}

/**
 * Work-related simple types
 */
object work:

  // Helper codec for Ed25519Signature (64 bytes)
  private val ed25519SigCodec: Codec[Ed25519Signature] =
    fixedSizeBytes(Ed25519Signature.Size.toLong, bytes).xmap(
      bv => Ed25519Signature(bv.toArray),
      sig => ByteVector(sig.bytes)
    )

  /**
   * Package specification containing hash, length, erasure root, exports root, and exports count.
   * Fixed size: 102 bytes (32 + 4 + 32 + 32 + 2)
   */
  final case class PackageSpec(
    hash: Hash,
    length: UInt,
    erasureRoot: Hash,
    exportsRoot: Hash,
    exportsCount: UShort
  )

  object PackageSpec:
    val Size: Int = Hash.Size + 4 + Hash.Size + Hash.Size + 2 // 102 bytes

    given Codec[PackageSpec] =
      (hashCodec :: uint32L :: hashCodec :: hashCodec :: uint16L).xmap(
        {
          case (hash, length, erasureRoot, exportsRoot, exportsCount) =>
            PackageSpec(hash, UInt(length.toInt), erasureRoot, exportsRoot, UShort(exportsCount))
        },
        ps => (ps.hash, ps.length.toLong & 0xffffffffL, ps.erasureRoot, ps.exportsRoot, ps.exportsCount.toInt)
      )

    given Decoder[PackageSpec] = Decoder.instance { cursor =>
      for
        hash <- cursor.get[Hash]("hash")
        length <- cursor.get[Long]("length")
        erasureRoot <- cursor.get[Hash]("erasure_root")
        exportsRoot <- cursor.get[Hash]("exports_root")
        exportsCount <- cursor.get[Int]("exports_count")
      yield PackageSpec(
        hash,
        UInt(length.toInt),
        erasureRoot,
        exportsRoot,
        UShort(exportsCount)
      )
    }

  /**
   * Execution result - either Ok with output data or an error.
   *
   * Encoding per Gray Paper:
   * - Ok: 0x00 + compact length prefix + data bytes
   * - OOG (Out of Gas): 0x01
   * - Panic: 0x02
   * - BadExports: 0x03
   * - Oversize: 0x04
   * - BadCode: 0x05
   * - CodeTooLarge: 0x06
   */
  enum ExecutionResult:
    case Ok(output: JamBytes)
    case OOG // Out of gas
    case Panic // Panic during execution
    case BadExports // Wrong number of exports
    case Oversize // Output too large
    case BadCode // Invalid code (BAD)
    case CodeTooLarge // Code too large (BIG)

  object ExecutionResult:
    private val OkTag: Byte = 0x00
    private val OOGTag: Byte = 0x01
    private val PanicTag: Byte = 0x02
    private val BadExportsTag: Byte = 0x03
    private val OversizeTag: Byte = 0x04
    private val BadCodeTag: Byte = 0x05
    private val CodeTooLargeTag: Byte = 0x06

    given Codec[ExecutionResult] = new Codec[ExecutionResult]:
      override def sizeBound: SizeBound = SizeBound.unknown

      override def encode(value: ExecutionResult): Attempt[BitVector] = value match
        case ExecutionResult.Ok(output) =>
          for
            prefix <- byte.encode(OkTag)
            lengthBits <- compactInteger.encode(output.length.toLong)
            dataBits <- bytes.encode(output.toByteVector)
          yield prefix ++ lengthBits ++ dataBits
        case ExecutionResult.OOG => byte.encode(OOGTag)
        case ExecutionResult.Panic => byte.encode(PanicTag)
        case ExecutionResult.BadExports => byte.encode(BadExportsTag)
        case ExecutionResult.Oversize => byte.encode(OversizeTag)
        case ExecutionResult.BadCode => byte.encode(BadCodeTag)
        case ExecutionResult.CodeTooLarge => byte.encode(CodeTooLargeTag)

      override def decode(bits: BitVector): Attempt[DecodeResult[ExecutionResult]] =
        byte.decode(bits).flatMap { result =>
          result.value match
            case OkTag =>
              compactInteger.decode(result.remainder).flatMap { lenResult =>
                fixedSizeBytes(lenResult.value, bytes).decode(lenResult.remainder).map { dataResult =>
                  DecodeResult(ExecutionResult.Ok(JamBytes.fromByteVector(dataResult.value)), dataResult.remainder)
                }
              }
            case OOGTag =>
              Attempt.successful(DecodeResult(ExecutionResult.OOG, result.remainder))
            case PanicTag =>
              Attempt.successful(DecodeResult(ExecutionResult.Panic, result.remainder))
            case BadExportsTag =>
              Attempt.successful(DecodeResult(ExecutionResult.BadExports, result.remainder))
            case OversizeTag =>
              Attempt.successful(DecodeResult(ExecutionResult.Oversize, result.remainder))
            case BadCodeTag =>
              Attempt.successful(DecodeResult(ExecutionResult.BadCode, result.remainder))
            case CodeTooLargeTag =>
              Attempt.successful(DecodeResult(ExecutionResult.CodeTooLarge, result.remainder))
            case other =>
              Attempt.failure(Err(s"Unknown ExecutionResult tag: $other"))
        }

    given Decoder[ExecutionResult] = Decoder.instance { cursor =>
      val ok = cursor.get[String]("ok").toOption
      val err = cursor.get[Int]("err").toOption
      val hasOutOfGas = cursor.downField("out_of_gas").succeeded
      val hasPanic = cursor.downField("panic").succeeded
      val hasBadExports = cursor.downField("bad_exports").succeeded
      val hasOutputOversize = cursor.downField("output_oversize").succeeded
      val hasBadCode = cursor.downField("bad_code").succeeded
      val hasCodeTooLarge = cursor.downField("code_too_large").succeeded

      ok match
        case Some(data) => Right(ExecutionResult.Ok(JamBytes(parseHex(data))))
        case None => err match
            case Some(1) => Right(ExecutionResult.OOG)
            case Some(2) => Right(ExecutionResult.Panic)
            case Some(3) => Right(ExecutionResult.BadExports)
            case Some(4) => Right(ExecutionResult.Oversize)
            case Some(5) => Right(ExecutionResult.BadCode)
            case Some(6) => Right(ExecutionResult.CodeTooLarge)
            case Some(_) => Right(ExecutionResult.Panic)
            case None =>
              if hasOutOfGas then Right(ExecutionResult.OOG)
              else if hasPanic then Right(ExecutionResult.Panic)
              else if hasBadExports then Right(ExecutionResult.BadExports)
              else if hasOutputOversize then Right(ExecutionResult.Oversize)
              else if hasBadCode then Right(ExecutionResult.BadCode)
              else if hasCodeTooLarge then Right(ExecutionResult.CodeTooLarge)
              else Left(DecodingFailure(
                "ExecutionResult: no recognized field (expected one of ok/err/out_of_gas/panic/bad_exports/output_oversize/bad_code/code_too_large)",
                cursor.history
              ))
    }

  /**
   * A vote containing validator index and Ed25519 signature.
   * Fixed size: 67 bytes (1 byte vote + 2 bytes index + 64 bytes signature)
   */
  final case class Vote(
    vote: Boolean,
    validatorIndex: ValidatorIndex,
    signature: Ed25519Signature
  )

  object Vote:
    val Size: Int = 1 + 2 + Ed25519Signature.Size // 67 bytes

    given Codec[Vote] =
      (byte :: uint16L :: ed25519SigCodec).xmap(
        {
          case (voteByte, idx, sig) =>
            Vote(voteByte != 0, ValidatorIndex(idx), sig)
        },
        v => ((if v.vote then 1 else 0).toByte, v.validatorIndex.value.toInt, v.signature)
      )

    given Decoder[Vote] = Decoder.instance { cursor =>
      for
        vote <- cursor.get[Boolean]("vote")
        validatorIndex <- cursor.get[Int]("index")
        signature <- cursor.get[Ed25519Signature]("signature")
      yield Vote(vote, ValidatorIndex(validatorIndex), signature)
    }
