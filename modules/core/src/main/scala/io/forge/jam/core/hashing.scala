package io.forge.jam.core

import org.bouncycastle.crypto.digests.Blake2bDigest
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

  private val blake2bLocal: ThreadLocal[Blake2bDigest] =
    ThreadLocal.withInitial(() => new Blake2bDigest(256))

  private val keccakLocal: ThreadLocal[KeccakDigest] =
    ThreadLocal.withInitial(() => new KeccakDigest(256))

  /**
   * Compute Blake2b-256 hash of the given data.
   *
   * @param data The input data as JamBytes
   * @return The 32-byte hash as Hash type
   */
  def blake2b256(data: JamBytes): Hash =
    val arr = data.toArrayUnsafe
    blake2b256(arr, 0, arr.length)

  /**
   * Compute Blake2b-256 hash of the given data.
   *
   * @param data The input data as byte array
   * @return The 32-byte hash as Hash type
   */
  def blake2b256(data: Array[Byte]): Hash =
    blake2b256(data, 0, data.length)

  /**
   * Compute Blake2b-256 hash of a slice of the given array without copying.
   *
   * @param data   The backing byte array
   * @param offset Start index of the slice to hash
   * @param length Number of bytes to hash starting at offset
   * @return The 32-byte hash as Hash type
   */
  def blake2b256(data: Array[Byte], offset: Int, length: Int): Hash =
    val digest = blake2bLocal.get()
    digest.reset()
    digest.update(data, offset, length)
    val output = new Array[Byte](digest.getDigestSize)
    digest.doFinal(output, 0)
    Hash(output)

  /**
   * Compute Blake2b-256 hash of the given data without copy.
   *
   * @param data The input data as ByteVector
   * @return The 32-byte hash as Hash type
   */
  @targetName("blake2b256ByteVector")
  def blake2b256(data: ByteVector): Hash =
    val arr = data.toArrayUnsafe // PERF-B2: digest only reads
    val digest = blake2bLocal.get()
    digest.reset()
    digest.update(arr, 0, arr.length)
    val output = new Array[Byte](digest.getDigestSize)
    digest.doFinal(output, 0)
    Hash.fromByteVectorUnsafe(ByteVector(output))

  /**
   * Compute Keccak-256 hash of the given data.
   *
   * @param data The input data as JamBytes
   * @return The 32-byte hash as Hash type
   */
  def keccak256(data: JamBytes): Hash =
    val arr = data.toArrayUnsafe // PERF-B2: digest only reads
    keccak256(arr)

  /**
   * Compute Keccak-256 hash of the given data.
   *
   * @param data The input data as byte array
   * @return The 32-byte hash as Hash type
   */
  def keccak256(data: Array[Byte]): Hash =
    keccak256(data, 0, data.length)

  /**
   * Compute Keccak-256 hash of a slice of the given array without copying.
   *
   * @param data   The backing byte array
   * @param offset Start index of the slice to hash
   * @param length Number of bytes to hash starting at offset
   * @return The 32-byte hash as Hash type
   */
  def keccak256(data: Array[Byte], offset: Int, length: Int): Hash =
    val digest = keccakLocal.get()
    digest.reset()
    val output = new Array[Byte](32)
    digest.update(data, offset, length)
    digest.doFinal(output, 0)
    Hash(output)
