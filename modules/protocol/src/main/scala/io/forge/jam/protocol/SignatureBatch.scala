package io.forge.jam.protocol

import io.forge.jam.core.primitives.{Ed25519PublicKey, Ed25519Signature}
import io.forge.jam.crypto.Ed25519

import scala.collection.mutable.ArrayBuffer

final class SignatureBatch:
  private val items = ArrayBuffer.empty[(Ed25519PublicKey, Array[Byte], Ed25519Signature)]

  def addAll(signatures: IterableOnce[(Ed25519PublicKey, Array[Byte], Ed25519Signature)]): Unit =
    items ++= signatures

  def verify(): Boolean = Ed25519.verifyAll(items.toIndexedSeq)
