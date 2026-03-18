package io.forge.jam.crypto

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.{BandersnatchPublicKey, Hash}
import io.forge.jam.vrfs.BandersnatchWrapper as JniBandersnatchWrapper
import spire.math.UByte

import java.nio.file.{Files, Paths}

/**
 * Scala wrapper for Bandersnatch VRF operations.
 */
object BandersnatchVrf:

  /** Size of the ring commitment in bytes */
  val RingCommitmentSize: Int = 144

  /** Size of a Bandersnatch public key in bytes */
  val PublicKeySize: Int = 32

  /** Size of a ring VRF signature in bytes */
  val RingVrfSignatureSize: Int = 784

  /** Cached SRS data */
  private var srsData: Array[Byte] = _

  /** Flag to track if context has been initialized for a given ring size */
  private var initializedRingSizes: Set[Int] = Set.empty

  /**
   * Result of a successful ring VRF proof verification.
   * Contains the 32-byte ticket ID and the attempt index.
   */
  case class VerificationResult(ticketId: JamBytes, attempt: UByte)

  /**
   * Load the SRS (Structured Reference String) data from resources.
   */
  private def loadSrsData(): Array[Byte] = synchronized {
    if srsData != null then return srsData

    val resourcePath = "/zcash-srs-2-11-uncompressed.bin"

    // Try to load from classpath resources first
    val inputStream = Option(getClass.getResourceAsStream(resourcePath))

    val data = inputStream match
      case Some(stream) =>
        try
          stream.readAllBytes()
        finally
          stream.close()
      case None =>
        // Get base directory from system property or use current working directory
        val baseDir = System.getProperty("jam.base.dir", System.getProperty("user.dir"))

        // Try to load from file system paths
        val possiblePaths = List(
          s"$baseDir/modules/crypto/src/main/resources/zcash-srs-2-11-uncompressed.bin",
          s"$baseDir/modules/protocol/src/main/resources/zcash-srs-2-11-uncompressed.bin",
          s"$baseDir/jamtestvectors/stf/safrole/zcash-srs-2-11-uncompressed.bin"
        )

        val foundPath = possiblePaths.find(p => Files.exists(Paths.get(p)))
        foundPath match
          case Some(path) =>
            Files.readAllBytes(Paths.get(path))
          case None =>
            throw new RuntimeException(
              s"SRS file not found. Tried: $resourcePath (classpath) and ${possiblePaths.mkString(", ")}"
            )

    srsData = data
    data
  }

  /**
   * Ensure the native library is loaded and context is initialized for the given ring size.
   */
  private def ensureInitialized(ringSize: Int): Unit = synchronized {
    // Load the native library via the Java wrapper
    JniBandersnatchWrapper.ensureLibraryLoaded()

    if !initializedRingSizes.contains(ringSize) then
      val srs = loadSrsData()
      JniBandersnatchWrapper.initializeContext(srs, ringSize)
      initializedRingSizes = initializedRingSizes + ringSize
  }

  /**
   * Generate the ring root (commitment) from a list of Bandersnatch public keys.
   *
   * @param keys List of Bandersnatch public keys (32 bytes each)
   * @param ringSize The size of the ring (should match keys.size for full ring)
   * @return The 144-byte ring commitment, or None if generation fails
   */
  def generateRingRoot(
    keys: List[BandersnatchPublicKey],
    ringSize: Int
  ): Option[JamBytes] =
    try
      ensureInitialized(ringSize)

      // Concatenate all public keys into a single byte array
      val concatenatedKeys = keys.flatMap(_.bytes.toSeq).toArray

      val commitment = JniBandersnatchWrapper.getVerifierCommitment(ringSize, concatenatedKeys)

      if commitment == null || commitment.isEmpty then
        None
      else
        Some(JamBytes(commitment))
    catch
      case _: Exception =>
        None

  /**
   * Verify a ring VRF proof and extract the ticket ID if valid.
   *
   * @param signature The 784-byte ring VRF signature from the ticket envelope
   * @param ringCommitment The 144-byte ring commitment (gamma_z)
   * @param entropy The 32-byte entropy value (eta[2] - tickets entropy)
   * @param attempt The ticket attempt index (0-255)
   * @param ringSize The ring size for verification
   * @return Some(VerificationResult) with the ticket ID and attempt if verification succeeds, None otherwise
   */
  def verifyRingProof(
    signature: JamBytes,
    ringCommitment: JamBytes,
    entropy: Hash,
    attempt: UByte,
    ringSize: Int
  ): Option[VerificationResult] =
    try
      ensureInitialized(ringSize)

      // Note: The JNI function expects attempt as a long
      val result = JniBandersnatchWrapper.verifierRingVrfVerify(
        entropy.bytes.toArray,
        attempt.toLong,
        signature.toArray,
        ringCommitment.toArray,
        ringSize
      )

      // If result is all zeros or null, verification failed
      if result == null || result.length != 32 || result.forall(_ == 0) then
        None
      else
        // The result is the 32-byte ticket ID
        Some(VerificationResult(JamBytes(result), attempt))
    catch
      case _: Exception =>
        None

  /**
   * Alternative verification method that takes raw byte arrays.
   * Useful for testing and direct integration.
   */
  def verifyRingProofRaw(
    entropy: Array[Byte],
    attempt: Int,
    signature: Array[Byte],
    commitment: Array[Byte],
    ringSize: Int
  ): Option[Array[Byte]] =
    try
      ensureInitialized(ringSize)

      val result = JniBandersnatchWrapper.verifierRingVrfVerify(
        entropy,
        attempt.toLong,
        signature,
        commitment,
        ringSize
      )

      if result == null || result.length != 32 || result.forall(_ == 0) then
        None
      else
        Some(result)
    catch
      case _: Exception =>
        None

  /**
   * Check if the native library is loaded and operational.
   */
  def isAvailable: Boolean =
    try
      JniBandersnatchWrapper.ensureLibraryLoaded()
      JniBandersnatchWrapper.isLibraryLoaded()
    catch
      case _: Exception => false

  /**
   * Verify an IETF VRF signature and return the VRF output.
   *
   * @param publicKey The Bandersnatch public key (32 bytes)
   * @param vrfInput The VRF input data (signing context + entropy + optional attempt)
   * @param auxData Auxiliary data bound to the signature (e.g., encoded header)
   * @param signature The 96-byte IETF VRF signature (seal)
   * @return Some(vrfOutput) if verification succeeds, None otherwise
   */
  def ietfVrfVerify(
    publicKey: Array[Byte],
    vrfInput: Array[Byte],
    auxData: Array[Byte],
    signature: Array[Byte]
  ): Option[Array[Byte]] =
    try
      JniBandersnatchWrapper.ensureLibraryLoaded()
      val result = JniBandersnatchWrapper.ietfVrfVerify(publicKey, vrfInput, auxData, signature)
      if result == null || result.length != 32 || result.forall(_ == 0) then
        None
      else
        Some(result)
    catch
      case _: Exception =>
        None

  /**
   * Extract VRF output from an IETF VRF signature without verification.
   *
   * @param signature The 96-byte IETF VRF signature
   * @return Some(vrfOutput) with 32-byte output, None on failure
   */
  def getVrfOutput(signature: Array[Byte]): Option[Array[Byte]] =
    try
      JniBandersnatchWrapper.ensureLibraryLoaded()
      val result = JniBandersnatchWrapper.getIetfVrfOutput(signature)
      if result == null || result.length != 32 then
        None
      else
        Some(result)
    catch
      case _: Exception =>
        None

/**
 * Signing context constants for VRF input data construction.
 */
object SigningContext:
  val TicketSeal: Array[Byte] = "jam_ticket_seal".getBytes("UTF-8")
  val FallbackSeal: Array[Byte] = "jam_fallback_seal".getBytes("UTF-8")
  val Entropy: Array[Byte] = "jam_entropy".getBytes("UTF-8")

  /**
   * Build VRF input for ticket seal
   */
  def safroleTicketInputData(entropy: Array[Byte], attempt: Byte): Array[Byte] =
    TicketSeal ++ entropy ++ Array(attempt)

  /**
   * Build VRF input for fallback seal
   */
  def fallbackSealInputData(entropy: Array[Byte]): Array[Byte] =
    FallbackSeal ++ entropy

  /**
   * Build VRF input for entropy accumulation
   */
  def entropyInputData(entropy: Array[Byte]): Array[Byte] =
    Entropy ++ entropy
