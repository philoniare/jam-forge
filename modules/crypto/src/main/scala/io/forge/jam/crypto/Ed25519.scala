package io.forge.jam.crypto

import io.forge.jam.core.primitives.{Ed25519PublicKey, Ed25519Signature}

/**
 * Ed25519 signature verification using ed25519-zebra with strict canonicity checks.
 *
 * This implementation uses the ed25519-zebra library from the Zcash Foundation
 * with additional canonicity validation required by Gray Paper.
 *
 * JAM requires stricter validation than ZIP-215:
 * - Public key y-coordinate must be < field prime p (canonical encoding)
 * - Public key must represent a valid point on the Ed25519 curve
 * - Signature R y-coordinate must be < field prime p (canonical encoding)
 * - Signature R must represent a valid point on the Ed25519 curve
 */
object Ed25519:

  // Ensure native library is loaded on first use
  private var libraryInitialized = false

  private def ensureLibraryLoaded(): Unit = synchronized {
    if !libraryInitialized then
      Ed25519ZebraWrapper.ensureLibraryLoaded()
      libraryInitialized = true
  }

  /** Size of an Ed25519 public key in bytes */
  val PublicKeySize: Int = 32

  /** Size of an Ed25519 signature in bytes */
  val SignatureSize: Int = 64

  /**
   * Verify an Ed25519 signature with strict canonicity checks.
   *
   * @param publicKey The 32-byte public key
   * @param message The message that was signed
   * @param signature The 64-byte signature (R || S)
   * @return true if the signature is valid, false otherwise
   */
  def verify(publicKey: Array[Byte], message: Array[Byte], signature: Array[Byte]): Boolean =
    if publicKey.length != PublicKeySize then return false
    if signature.length != SignatureSize then return false
    ensureLibraryLoaded()
    try
      val result = Ed25519ZebraWrapper.verify(publicKey, message, signature)
      result != null && result.length == 1 && result(0) == 1
    catch
      case _: RuntimeException => false

  /**
   * Verify an Ed25519 signature using typed primitives.
   *
   * @param publicKey The Ed25519 public key
   * @param message The message that was signed
   * @param signature The Ed25519 signature
   * @return true if the signature is valid, false otherwise
   */
  def verify(publicKey: Ed25519PublicKey, message: Array[Byte], signature: Ed25519Signature): Boolean =
    verify(publicKey.bytes, message, signature.bytes)
