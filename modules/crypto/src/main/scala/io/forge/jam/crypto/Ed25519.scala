package io.forge.jam.crypto

import io.forge.jam.core.primitives.{Ed25519PublicKey, Ed25519Signature}

/**
 * Ed25519 signature verification using ed25519-zebra (Zcash Foundation).
 */
object Ed25519:

  // Read on every verify from many threads; set once.
  @volatile private var libraryInitialized = false

  private def ensureLibraryLoaded(): Unit =
    if !libraryInitialized then
      synchronized {
        if !libraryInitialized then
          Ed25519ZebraWrapper.ensureLibraryLoaded()
          libraryInitialized = true
      }

  /** Size of an Ed25519 public key in bytes */
  val PublicKeySize: Int = 32

  /** Size of an Ed25519 signature in bytes */
  val SignatureSize: Int = 64

  /** Signatures per native batch: enough to amortise the batch's set-up, small enough to spread a block's over cores. */
  private val BatchChunk = 64

  /**
   * Verify an Ed25519 signature (ZIP-215 rules, see above).
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
    try Ed25519ZebraWrapper.verify(publicKey, message, signature)
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
    verify(publicKey.toArrayUnsafe, message, signature.toArrayUnsafe)

  /**
   * Whether every `(publicKey, message, signature)` verifies: the same answer as [[verify]] on each, from batch
   * verification. Batches of [[BatchChunk]] run in parallel.
   */
  def verifyAll(items: IndexedSeq[(Ed25519PublicKey, Array[Byte], Ed25519Signature)]): Boolean =
    firstInvalid(items) < 0

  /**
   * The index of the first signature in `items` that does not verify, or -1 if all do. Each chunk is checked as one
   * batch; only a failing chunk is re-checked one signature at a time to find which. The result is the one per-item
   * [[verify]] gives, since a batch rejects only when some item in it fails alone.
   */
  def firstInvalid(items: IndexedSeq[(Ed25519PublicKey, Array[Byte], Ed25519Signature)]): Int =
    val n = items.length
    if n == 0 then -1
    else
      ensureLibraryLoaded()
      val chunks = (n + BatchChunk - 1) / BatchChunk
      val failed = new java.util.concurrent.atomic.AtomicIntegerArray(chunks)
      java.util.stream.IntStream.range(0, chunks).parallel().forEach { c =>
        if !verifyBatch(items, c * BatchChunk, math.min(n, (c + 1) * BatchChunk)) then failed.set(c, 1)
      }
      var c = 0
      while c < chunks && failed.get(c) == 0 do c += 1
      if c == chunks then -1
      else
        // The first failing chunk holds the first invalid signature; a later chunk can only hold later ones.
        val end = math.min(n, (c + 1) * BatchChunk)
        var i = c * BatchChunk
        while i < end && verify(items(i)._1, items(i)._2, items(i)._3) do i += 1
        if i < end then i
        else
          // Every one verifies alone: not reachable under ZIP-215, but per-item results are the reference.
          var j = end
          while j < n && verify(items(j)._1, items(j)._2, items(j)._3) do j += 1
          if j < n then j else -1

  private def verifyBatch(
    items: IndexedSeq[(Ed25519PublicKey, Array[Byte], Ed25519Signature)],
    from: Int,
    until: Int
  ): Boolean =
    val count = until - from
    val keys = new Array[Byte](count * PublicKeySize)
    val sigs = new Array[Byte](count * SignatureSize)
    val ends = new Array[Int](count)
    var total = 0
    var i = 0
    while i < count do
      val (pk, msg, sig) = items(from + i)
      // A malformed item fails the batch; firstInvalid then finds it, and verify rejects it.
      if pk.toArrayUnsafe.length != PublicKeySize || sig.toArrayUnsafe.length != SignatureSize then return false
      total += msg.length
      ends(i) = total
      i += 1
    val messages = new Array[Byte](total)
    i = 0
    while i < count do
      val (pk, msg, sig) = items(from + i)
      System.arraycopy(pk.toArrayUnsafe, 0, keys, i * PublicKeySize, PublicKeySize)
      System.arraycopy(sig.toArrayUnsafe, 0, sigs, i * SignatureSize, SignatureSize)
      System.arraycopy(msg, 0, messages, ends(i) - msg.length, msg.length)
      i += 1
    try Ed25519ZebraWrapper.verifyBatch(keys, messages, ends, sigs)
    catch
      case _: RuntimeException => false
