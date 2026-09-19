package io.forge.jam.core

/**
 * Constants from the JAM Gray Paper.
 */
object constants:
  
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
  // Authorization Constants
  // ══════════════════════════════════════════════════════════════════════════

  /** O = 8: Maximum number of items in the authorizations pool */
  val O: Int = 8

  /** Q = 80: Number of items in the authorizations queue */
  val Q: Int = 80

  // ══════════════════════════════════════════════════════════════════════════
  // Memory and Size Constants
  // ══════════════════════════════════════════════════════════════════════════
  
  /** ZP = 2^12 = 4096: Standard page size */
  val ZP: Int = 4096
  
  /** ZI = 2^24 = 16MB: Input data maximum size */
  val ZI: Int = 16777216
  
  /** ZA = 2: Dynamic page address alignment */
  val ZA: Int = 2
  
  // ══════════════════════════════════════════════════════════════════════════
  // Work Package Constants
  // ══════════════════════════════════════════════════════════════════════════
  
  /** Csegmentsize = 4104: erasure-coded segment size in bytes */
  val Csegmentsize: Long = 4104L

  /** Number of erasure pieces a sequence of `n` original segments expands to */
  def erasureExpandedPieces(n: Long): Long = (n * 65 + 63) / 64

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

  // ══════════════════════════════════════════════════════════════════════════
  // Signature Prefixes (for Ed25519 message signing)
  // ══════════════════════════════════════════════════════════════════════════

  /** Prefix for guarantee signatures (work report guarantees) */
  val JAM_GUARANTEE: String = "jam_guarantee"
  val JAM_GUARANTEE_BYTES: Array[Byte] = JAM_GUARANTEE.getBytes("UTF-8")

  /** Prefix for availability assurance signatures */
  val JAM_AVAILABLE: String = "jam_available"
  val JAM_AVAILABLE_BYTES: Array[Byte] = JAM_AVAILABLE.getBytes("UTF-8")

  /** Prefix for valid vote signatures (disputes) */
  val JAM_VALID: String = "jam_valid"
  val JAM_VALID_BYTES: Array[Byte] = JAM_VALID.getBytes("UTF-8")

  /** Prefix for invalid vote signatures (disputes) */
  val JAM_INVALID: String = "jam_invalid"
  val JAM_INVALID_BYTES: Array[Byte] = JAM_INVALID.getBytes("UTF-8")

  /** Cmaxservicecodesize: maximum size in bytes of a service's code blob. */
  val Cmaxservicecodesize: Int = 4_000_000

  /** Cmaxpackageimports: maximum imported segments per work package. */
  val Cmaxpackageimports: Int = 3072

  /** Cmaxpackageexports: maximum exported segments per work package. */
  val Cmaxpackageexports: Int = 3072

  /** Cpackageauthgas: gas allotted to a work package's is-authorized call. */
  val Cpackageauthgas: Long = 50_000_000L

  /**
   * Cmaxreportvarsize: bound on the variable-size portion of a work report —
   * the authorizer trace and the cumulative work-item output sizes.
   */
  val Cmaxreportvarsize: Int = 48 * 1024

  /** Cmaxauthcodesize: maximum size in bytes of an authorizer code blob. */
  val Cmaxauthcodesize: Int = 64_000

  /** Cmemosize: fixed size in bytes of a deferred-transfer memo. */
  val Cmemosize: Int = 128

  /**
   * Cminpublicindex: lowest service index available to publicly-created
   * services — indices below it are reserved.
   */
  val Cminpublicindex: Long = 65536L
