package io.forge.jam.core

/**
 * Type alias for State Transition Function results using standard Either.
 *
 * StfResult[Ok, Err] = Either[Err, Ok]
 * - Right(data) represents success
 * - Left(error) represents failure
 *
 * Binary format (for codec):
 * - 1 byte discriminator (0 = success with Ok data, 1 = error with Err code)
 * - Followed by either Ok data (if discriminator = 0) or Err code (if discriminator = 1)
 */
type StfResult[+Ok, +Err] = Either[Err, Ok]

object StfResult:
  /** Create a successful result */
  def success[Ok, Err](data: Ok): StfResult[Ok, Err] = Right(data)

  /** Create an error result */
  def error[Ok, Err](code: Err): StfResult[Ok, Err] = Left(code)

  /** Check if result is successful */
  extension [Ok, Err](result: StfResult[Ok, Err])
    def isOk: Boolean = result.isRight
    def isErr: Boolean = result.isLeft

