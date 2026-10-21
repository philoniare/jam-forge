package io.forge.jam.core.types

import io.forge.jam.core.codec.*
import io.forge.jam.core.codec.codecs.*
import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.{Hash, ValidatorIndex, Ed25519Signature}
import io.forge.jam.core.codec.JamCodecs.hashCodec
import io.forge.jam.core.codec.PrimitiveCodecs
import io.forge.jam.core.json.JsonHelpers.parseHex
import io.circe.{Decoder, DecodingFailure}
import spire.math.{UShort, UInt}

/**
 * Work-related simple types
 */
object work:

  /**
   * Package specification (availability specifier).
   *
   * Fixed size: 104 bytes (32 + 4 + 32 + 2 + 32 + 2)
   */
  final case class PackageSpec(
    hash: Hash,
    length: UInt,
    erasureRoot: Hash,
    erasureShards: UShort,
    exportsRoot: Hash,
    exportsCount: UShort
  )

  object PackageSpec:
    val Size: Int = Hash.Size + 4 + Hash.Size + 2 + Hash.Size + 2 // 104 bytes

    given Codec[PackageSpec] =
      (hashCodec :: uint32L :: hashCodec :: uint16L :: hashCodec :: uint16L).xmap(
        {
          case (hash, length, erasureRoot, erasureShards, exportsRoot, exportsCount) =>
            PackageSpec(hash, UInt(length.toInt), erasureRoot, UShort(erasureShards), exportsRoot, UShort(exportsCount))
        },
        ps => (ps.hash, ps.length.toLong & 0xffffffffL, ps.erasureRoot, ps.erasureShards.toInt, ps.exportsRoot, ps.exportsCount.toInt)
      )

    given Decoder[PackageSpec] = Decoder.instance { cursor =>
      for
        hash <- cursor.get[Hash]("hash")
        length <- cursor.get[Long]("length")
        erasureRoot <- cursor.get[Hash]("erasure_root")
        erasureShards <- cursor.get[Int]("erasure_shards")
        exportsRoot <- cursor.get[Hash]("exports_root")
        exportsCount <- cursor.get[Int]("exports_count")
      yield PackageSpec(
        hash,
        UInt(length.toInt),
        erasureRoot,
        UShort(erasureShards),
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

    /** Work execution result: a tag byte, plus the length-prefixed output for `Ok` (GP 0.8.0 appendix C, O). */
    given Codec[ExecutionResult] = Codec.of(
      (w, value) =>
        value match
          case ExecutionResult.Ok(output) =>
            w.u8(OkTag)
            w.compact(output.length.toLong)
            w.bytes(output)
          case ExecutionResult.OOG          => w.u8(OOGTag)
          case ExecutionResult.Panic        => w.u8(PanicTag)
          case ExecutionResult.BadExports   => w.u8(BadExportsTag)
          case ExecutionResult.Oversize     => w.u8(OversizeTag)
          case ExecutionResult.BadCode      => w.u8(BadCodeTag)
          case ExecutionResult.CodeTooLarge => w.u8(CodeTooLargeTag),
      r =>
        r.u8().toByte match
          case OkTag           => ExecutionResult.Ok(JamBytes.wrap(r.bytes(r.compactInt())))
          case OOGTag          => ExecutionResult.OOG
          case PanicTag        => ExecutionResult.Panic
          case BadExportsTag   => ExecutionResult.BadExports
          case OversizeTag     => ExecutionResult.Oversize
          case BadCodeTag      => ExecutionResult.BadCode
          case CodeTooLargeTag => ExecutionResult.CodeTooLarge
          case other           => r.fail(s"Unknown ExecutionResult tag: $other")
    )

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
      (byte :: uint16L :: PrimitiveCodecs.ed25519Signature).xmap(
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
