package io.forge.jam.core

import spire.math.{UByte, UShort, UInt, ULong}

/**
 * Constants from the JAM Gray Paper.
 */
object constants:
  
  // ══════════════════════════════════════════════════════════════════════════
  // Validator and Core Counts
  // ══════════════════════════════════════════════════════════════════════════
  
  /** V = 1023: Total number of validators */
  val V: Int = 1023
  
  /** C = 341: Number of cores */
  val C: Int = 341
  
  /** Validators per core: V / C = 3 */
  val ValidatorsPerCore: Int = V / C
  
  // ══════════════════════════════════════════════════════════════════════════
  // Time Constants
  // ══════════════════════════════════════════════════════════════════════════
  
  /** P = 6: Slot duration in seconds */
  val P: Int = 6
  
  /** E = 600: Epoch length in slots (1 hour) */
  val E: Int = 600
  
  /** Y = 12: Rotation period for ticket accumulation */
  val Y: Int = 12

  /** H = 8: Recent history length in blocks */
  val H: Int = 8

  /** U = 10: Availability timeout in slots */
  val U: Int = 10

  // ══════════════════════════════════════════════════════════════════════════
  // Memory and Size Constants
  // ══════════════════════════════════════════════════════════════════════════
  
  /** ZP = 2^12 = 4096: Standard page size */
  val ZP: Int = 4096
  
  /** ZZ = 2^16 = 65536: Standard zone size */
  val ZZ: Int = 65536
  
  /** ZI = 2^24 = 16MB: Input data maximum size */
  val ZI: Int = 16777216
  
  /** ZA = 2: Dynamic page address alignment */
  val ZA: Int = 2
  
  // ══════════════════════════════════════════════════════════════════════════
  // Work Package Constants
  // ══════════════════════════════════════════════════════════════════════════
  
  /** WB = 12MB: Maximum work package blob size */
  val WB: Int = 12 * 1024 * 1024
  
  /** WR = 48KB: Maximum work report size */
  val WR: Int = 48 * 1024
  
  /** WG = 5 * 10^9: Minimum accumulate gas */
  val WG: Long = 5_000_000_000L
  
  /** WP = 10^10: Work package gas limit */
  val WP: Long = 10_000_000_000L
  
  // ══════════════════════════════════════════════════════════════════════════
  // Service Constants
  // ══════════════════════════════════════════════════════════════════════════
  
  /** Maximum number of services per block */
  val MaxServicesPerBlock: Int = 1024
  
  /** Maximum accumulation queue size */
  val MaxAccumulationQueueSize: Int = 256
  
  // ══════════════════════════════════════════════════════════════════════════
  // Crypto Constants
  // ══════════════════════════════════════════════════════════════════════════
  
  /** Hash size in bytes (Blake2b-256) */
  val HashSize: Int = 32
  
  /** Bandersnatch public key size */
  val BandersnatchKeySize: Int = 32
  
  /** Ed25519 public key size */
  val Ed25519KeySize: Int = 32
  
  /** BLS public key size */
  val BlsKeySize: Int = 144
