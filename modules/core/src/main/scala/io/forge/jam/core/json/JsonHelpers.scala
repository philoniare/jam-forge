package io.forge.jam.core.json

import io.circe.DecodingFailure
import io.forge.jam.core.primitives.Hash

/**
 * Common JSON parsing helpers for JAM types.
 */
object JsonHelpers:

  /**
   * Parse a hex string to a byte array of any length.
   * Handles both "0x" prefixed and non-prefixed hex strings.
   */
  def parseHex(hex: String): Array[Byte] =
    val cleanHex = if hex.startsWith("0x") then hex.drop(2) else hex
    if cleanHex.isEmpty then Array.emptyByteArray
    else cleanHex.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray

  /**
   * Parse a hex string to a byte array, returning Either for error handling.
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

  /**
   * Parse a hex string to a 32-byte Hash.
   */
  def parseHash(hex: String): Either[DecodingFailure, Hash] =
    parseHexBytesFixed(hex, Hash.Size).map(Hash(_))

  /**
   * Parse a nested list of hex strings to a nested list of Hash.
   */
  def parseHashListList(lists: List[List[String]]): Either[DecodingFailure, List[List[Hash]]] =
    val results = lists.map { inner =>
      val innerResults = inner.map(parseHash)
      val (errors, successes) = innerResults.partitionMap(identity)
      if errors.nonEmpty then Left(errors.head) else Right(successes)
    }
    val (errors, successes) = results.partitionMap(identity)
    if errors.nonEmpty then Left(errors.head) else Right(successes)
