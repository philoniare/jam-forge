package io.forge.jam.protocol

import io.forge.jam.core.Parallel
import io.forge.jam.core.primitives.{Ed25519PublicKey, Ed25519Signature}
import io.forge.jam.crypto.Ed25519

import java.util.concurrent.CompletableFuture
import scala.collection.mutable.ArrayBuffer

final class SignatureBatch:
  private val items = ArrayBuffer.empty[(Ed25519PublicKey, Array[Byte], Ed25519Signature)]

  def addAll(signatures: IterableOnce[(Ed25519PublicKey, Array[Byte], Ed25519Signature)]): Unit =
    items ++= signatures

  def verify(): Boolean = Ed25519.verifyAll(items.toIndexedSeq)

  /**
   * [[verify]] on the shared pool, over the signatures added so far: the keys, messages and signatures are copied out
   * on the calling thread, so the result does not depend on anything the caller does afterwards.
   */
  def verifyAsync(): CompletableFuture[Boolean] =
    if items.isEmpty then CompletableFuture.completedFuture(true)
    else
      val snapshot = items.toIndexedSeq
      Parallel.async(Ed25519.verifyAll(snapshot))
