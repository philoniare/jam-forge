package io.forge.jam.protocol

import io.forge.jam.core.primitives.Hash

/**
 * Test helper utilities for protocol tests.
 *
 * For hex string parsing, use JsonHelpers.parseHash instead.
 */
object TestHelpers:

  /**
   * Create a hash filled with a specific byte value.
   * Useful for creating predictable test hashes.
   */
  def hashFilled(value: Int): Hash =
    Hash(Array.fill(32)(value.toByte))
