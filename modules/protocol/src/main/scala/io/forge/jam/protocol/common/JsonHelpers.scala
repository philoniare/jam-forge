package io.forge.jam.protocol.common

import io.circe.DecodingFailure
import io.forge.jam.core.primitives.Hash

/**
 * Common JSON parsing helpers for protocol types.
 */
object JsonHelpers:

  /**
   * Parse a hex string to a 32-byte Hash.
   * Handles both "0x" prefixed and non-prefixed hex strings.
   */
  def parseHash(hex: String): Either[DecodingFailure, Hash] =
    val cleanHex = if hex.startsWith("0x") then hex.drop(2) else hex
    if cleanHex.length != 64 then
      Left(DecodingFailure(s"Hash must be 64 hex chars, got ${cleanHex.length}", Nil))
    else
      try
        val bytes = cleanHex.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
        Right(Hash(bytes))
      catch
        case _: NumberFormatException =>
          Left(DecodingFailure("Invalid hex character", Nil))

  /**
   * Parse a hex string to a byte array of any length.
   * Handles both "0x" prefixed and non-prefixed hex strings.
   */
  def parseHexBytes(hex: String): Either[DecodingFailure, Array[Byte]] =
    val cleanHex = if hex.startsWith("0x") then hex.drop(2) else hex
    if cleanHex.length % 2 != 0 then
      Left(DecodingFailure(s"Hex string must have even length, got ${cleanHex.length}", Nil))
    else
      try
        val bytes = cleanHex.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
        Right(bytes)
      catch
        case _: NumberFormatException =>
          Left(DecodingFailure("Invalid hex character", Nil))

  /**
   * Parse a hex string to a byte array of expected fixed length.
   */
  def parseHexBytesFixed(hex: String, expectedLength: Int): Either[DecodingFailure, Array[Byte]] =
    parseHexBytes(hex).flatMap { bytes =>
      if bytes.length != expectedLength then
        Left(DecodingFailure(s"Expected $expectedLength bytes, got ${bytes.length}", Nil))
      else
        Right(bytes)
    }
