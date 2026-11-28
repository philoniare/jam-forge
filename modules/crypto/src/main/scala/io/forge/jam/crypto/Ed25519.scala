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
    val key = if Verified.enabled then Verified.key(publicKey, message, signature) else null
    if key != null && Verified.contains(key) then true
    else
      val ok =
        try Ed25519ZebraWrapper.verify(publicKey, message, signature)
        catch case _: RuntimeException => false
      if ok && key != null then Verified.add(key)
      ok

  object Verified:
    val MaxEntries: Int = 1 << 16

    @volatile private var on = false
    def enabled: Boolean = on
    def enable(): Unit = on = true
    def disable(): Unit =
      on = false
      clear()

    private val seen =
      new java.util.LinkedHashMap[io.forge.jam.core.primitives.Hash, java.lang.Boolean](1024, 0.75f, false):
        override def removeEldestEntry(
          eldest: java.util.Map.Entry[io.forge.jam.core.primitives.Hash, java.lang.Boolean]
        ): Boolean = this.size() > MaxEntries

    private[crypto] def key(publicKey: Array[Byte], message: Array[Byte], signature: Array[Byte])
        : io.forge.jam.core.primitives.Hash =
      val in = new Array[Byte](publicKey.length + signature.length + message.length)
      System.arraycopy(publicKey, 0, in, 0, publicKey.length)
      System.arraycopy(signature, 0, in, publicKey.length, signature.length)
      System.arraycopy(message, 0, in, publicKey.length + signature.length, message.length)
      io.forge.jam.core.Hashing.blake2b256(in)

    private[crypto] def contains(key: io.forge.jam.core.primitives.Hash): Boolean =
      seen.synchronized(seen.containsKey(key))

    private[crypto] def add(key: io.forge.jam.core.primitives.Hash): Unit =
      seen.synchronized(seen.put(key, java.lang.Boolean.TRUE)): Unit

    def size: Int = seen.synchronized(seen.size())

    def clear(): Unit = seen.synchronized(seen.clear())

  private def unverified(items: IndexedSeq[(Ed25519PublicKey, Array[Byte], Ed25519Signature)])
      : (Array[io.forge.jam.core.primitives.Hash | Null], Array[Int]) | Null =
    if !Verified.enabled then null
    else
      val keys = new Array[io.forge.jam.core.primitives.Hash | Null](items.length)
      val rest = Array.newBuilder[Int]
      var i = 0
      while i < items.length do
        val (pk, msg, sig) = items(i)
        val wellFormed = pk.toArrayUnsafe.length == PublicKeySize && sig.toArrayUnsafe.length == SignatureSize
        val key = if wellFormed then Verified.key(pk.toArrayUnsafe, msg, sig.toArrayUnsafe) else null
        if key == null || !Verified.contains(key) then
          keys(i) = key
          rest += i
        i += 1
      (keys, rest.result())

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
    unverified(items) match
      case null => firstInvalidOf(items)
      case (keys, rest) =>
        if rest.isEmpty then -1
        else
          val failed = firstInvalidOf(if rest.length == items.length then items else rest.toIndexedSeq.map(items))
          if failed < 0 then
            rest.foreach(i => if keys(i) != null then Verified.add(keys(i).nn))
            -1
          else rest(failed)

  private def firstInvalidOf(items: IndexedSeq[(Ed25519PublicKey, Array[Byte], Ed25519Signature)]): Int =
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

  def validity(items: IndexedSeq[(Ed25519PublicKey, Array[Byte], Ed25519Signature)]): Array[Boolean] =
    unverified(items) match
      case null => validityOf(items)
      case (keys, rest) =>
        val valid = new Array[Boolean](items.length)
        java.util.Arrays.fill(valid, true)
        if rest.nonEmpty then
          val checked = validityOf(if rest.length == items.length then items else rest.toIndexedSeq.map(items))
          var j = 0
          while j < rest.length do
            val i = rest(j)
            valid(i) = checked(j)
            if checked(j) && keys(i) != null then Verified.add(keys(i).nn)
            j += 1
        valid

  private def validityOf(items: IndexedSeq[(Ed25519PublicKey, Array[Byte], Ed25519Signature)]): Array[Boolean] =
    val n = items.length
    val valid = new Array[Boolean](n)
    if n > 0 then
      ensureLibraryLoaded()
      val chunks = (n + BatchChunk - 1) / BatchChunk
      java.util.stream.IntStream.range(0, chunks).parallel().forEach { c =>
        val from = c * BatchChunk
        val until = math.min(n, from + BatchChunk)
        if verifyBatch(items, from, until) then java.util.Arrays.fill(valid, from, until, true)
        else
          var i = from
          while i < until do
            valid(i) = verify(items(i)._1, items(i)._2, items(i)._3)
            i += 1
      }
    valid

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
