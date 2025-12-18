package io.forge.jam.core.json

import io.circe.Decoder
import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.*

/**
 * Hex string decoder utilities for parsing test vector JSON files.
 */
object decoders:

  /**
   * Decode a hex string to JamBytes.
   * Handles both "0x" prefix and plain hex strings.
   */
  given hexDecoder: Decoder[JamBytes] = Decoder.decodeString.emap { hex =>
    JamBytes.fromHex(hex).left.map(err => s"Invalid hex: $err")
  }

  /**
   * Decode a hex string to Hash (validates 32-byte length).
   */
  given hexToHash: Decoder[Hash] = Decoder.decodeString.emap { hex =>
    val cleanHex = if hex.startsWith("0x") then hex.drop(2) else hex
    if cleanHex.length != 64 then
      Left(s"Hash must be 32 bytes (64 hex chars), got ${cleanHex.length / 2} bytes")
    else
      Hash.fromHex(cleanHex).left.map(err => s"Invalid hash: $err")
  }

  /**
   * Decode a hex string to BandersnatchPublicKey (validates 32-byte length).
   */
  given hexToBandersnatchKey: Decoder[BandersnatchPublicKey] = Decoder.decodeString.emap { hex =>
    val cleanHex = if hex.startsWith("0x") then hex.drop(2) else hex
    if cleanHex.length != 64 then
      Left(s"BandersnatchPublicKey must be 32 bytes, got ${cleanHex.length / 2} bytes")
    else
      try
        val bytes = cleanHex.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
        Right(BandersnatchPublicKey(bytes))
      catch
        case _: NumberFormatException => Left("Invalid hex character")
  }

  /**
   * Decode a hex string to Ed25519PublicKey (validates 32-byte length).
   */
  given hexToEd25519Key: Decoder[Ed25519PublicKey] = Decoder.decodeString.emap { hex =>
    val cleanHex = if hex.startsWith("0x") then hex.drop(2) else hex
    if cleanHex.length != 64 then
      Left(s"Ed25519PublicKey must be 32 bytes, got ${cleanHex.length / 2} bytes")
    else
      try
        val bytes = cleanHex.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
        Right(Ed25519PublicKey(bytes))
      catch
        case _: NumberFormatException => Left("Invalid hex character")
  }

  /**
   * Decode a hex string to Ed25519Signature (validates 64-byte length).
   */
  given hexToEd25519Sig: Decoder[Ed25519Signature] = Decoder.decodeString.emap { hex =>
    val cleanHex = if hex.startsWith("0x") then hex.drop(2) else hex
    if cleanHex.length != 128 then
      Left(s"Ed25519Signature must be 64 bytes, got ${cleanHex.length / 2} bytes")
    else
      try
        val bytes = cleanHex.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
        Right(Ed25519Signature(bytes))
      catch
        case _: NumberFormatException => Left("Invalid hex character")
  }

  /**
   * Decode a hex string to BandersnatchSignature (validates 96-byte length).
   */
  given hexToBandersnatchSig: Decoder[BandersnatchSignature] = Decoder.decodeString.emap { hex =>
    val cleanHex = if hex.startsWith("0x") then hex.drop(2) else hex
    if cleanHex.length != 192 then
      Left(s"BandersnatchSignature must be 96 bytes, got ${cleanHex.length / 2} bytes")
    else
      try
        val bytes = cleanHex.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
        Right(BandersnatchSignature(bytes))
      catch
        case _: NumberFormatException => Left("Invalid hex character")
  }
