package io.forge.jam.core

import codec.{JamEncoder, JamDecoder, encode, decodeAs}

/**
 * Generic result type for State Transition Functions (STFs).
 *
 * Encodes the ok/err discriminated union pattern used by all STF outputs.
 * Binary format:
 * - 1 byte discriminator (0 = success with Ok data, 1 = error with Err code)
 * - Followed by either Ok data (if discriminator = 0) or Err code (if discriminator = 1)
 *
 * @param ok Optional success data (Some when operation succeeded)
 * @param err Optional error code (Some when operation failed)
 */
final case class StfResult[+Ok, +Err](
  ok: Option[Ok] = None,
  err: Option[Err] = None
):
  /** Returns true if this result represents success */
  def isOk: Boolean = ok.isDefined

  /** Returns true if this result represents an error */
  def isErr: Boolean = err.isDefined

object StfResult:
  /** Create a successful result */
  def success[Ok, Err](data: Ok): StfResult[Ok, Err] =
    StfResult(ok = Some(data))

  /** Create an error result */
  def error[Ok, Err](code: Err): StfResult[Ok, Err] =
    StfResult(err = Some(code))

  /**
   * Unit codec for STF outputs that have no success data (e.g., PreimageOutput).
   * Encodes to zero bytes, decodes consuming zero bytes.
   */
  given JamEncoder[Unit] with
    def encode(a: Unit): JamBytes = JamBytes.empty

  given JamDecoder[Unit] with
    def decode(bytes: JamBytes, offset: Int): (Unit, Int) = ((), 0)

  /**
   * Generic codec for StfResult types.
   *
   * Requires encoders/decoders for both Ok and Err types.
   * Binary format matches the existing JAM protocol specification.
   */
  given stfResultEncoder[Ok: JamEncoder, Err: JamEncoder]: JamEncoder[StfResult[Ok, Err]] =
    new JamEncoder[StfResult[Ok, Err]]:
      def encode(a: StfResult[Ok, Err]): JamBytes =
        val builder = JamBytes.newBuilder
        a.ok match
          case Some(data) =>
            builder += 0.toByte // Success discriminator
            builder ++= data.encode
          case None =>
            builder += 1.toByte // Error discriminator
            builder ++= a.err.get.encode
        builder.result()

  /**
   * Generic decoder for StfResult types.
   *
   * Reads the discriminator byte and delegates to the appropriate decoder.
   */
  given stfResultDecoder[Ok: JamDecoder, Err: JamDecoder]: JamDecoder[StfResult[Ok, Err]] =
    new JamDecoder[StfResult[Ok, Err]]:
      def decode(bytes: JamBytes, offset: Int): (StfResult[Ok, Err], Int) =
        val discriminator = bytes.toArray(offset).toInt & 0xff
        if discriminator == 0 then
          // Success case
          val (ok, consumed) = bytes.decodeAs[Ok](offset + 1)
          (StfResult.success(ok), 1 + consumed)
        else
          // Error case
          val (err, consumed) = bytes.decodeAs[Err](offset + 1)
          (StfResult.error(err), 1 + consumed)
