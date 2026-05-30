package io.forge.jam.core

import org.bouncycastle.jcajce.provider.digest.Blake2b
import org.bouncycastle.crypto.digests.KeccakDigest
import primitives.Hash
import _root_.scodec.bits.ByteVector
import scala.annotation.targetName

/**
 * Hashing utilities for JAM protocol.
 *
 * Provides Blake2b-256 and Keccak-256 hash functions using Bouncy Castle.
 */
object Hashing:

  /**
   * Compute Blake2b-256 hash of the given data.
   *
   * @param data The input data as JamBytes
   * @return The 32-byte hash as Hash type
   */
  def blake2b256(data: JamBytes): Hash =
    blake2b256(data.toArray)

  /**
   * Compute Blake2b-256 hash of the given data.
   *
   * @param data The input data as byte array
   * @return The 32-byte hash as Hash type
   */
  def blake2b256(data: Array[Byte]): Hash =
    val digest = new Blake2b.Blake2b256()
    digest.update(data, 0, data.length)
    Hash(digest.digest())

  /**
   * Compute Blake2b-256 hash of the given data without copy.
   *
   * @param data The input data as ByteVector
   * @return The 32-byte hash as Hash type
   */
  @targetName("blake2b256ByteVector")
  def blake2b256(data: ByteVector): Hash =
    val digest = new Blake2b.Blake2b256()
    digest.update(data.toByteBuffer)
    Hash.fromByteVectorUnsafe(ByteVector(digest.digest()))

  /**
   * Compute Keccak-256 hash of the given data.
   *
   * @param data The input data as JamBytes
   * @return The 32-byte hash as Hash type
   */
  def keccak256(data: JamBytes): Hash =
    keccak256(data.toArray)

  /**
   * Compute Keccak-256 hash of the given data.
   *
   * @param data The input data as byte array
   * @return The 32-byte hash as Hash type
   */
  def keccak256(data: Array[Byte]): Hash =
    val digest = new KeccakDigest(256)
    val output = new Array[Byte](32)
    digest.update(data, 0, data.length)
    digest.doFinal(output, 0)
    Hash(output)
