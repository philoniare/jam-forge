package io.forge.jam.crypto

/**
 * Erasure coding implementation using Reed-Solomon via native JNI bindings.
 */
object ErasureCoding:

  /** Inner shard size in bytes (must be 2 for reed-solomon-simd) */
  val InnerShardSize: Int = 2

  private var libraryInitialized = false

  /** Ensure the native library is loaded */
  private def ensureInitialized(): Unit = synchronized {
    if !libraryInitialized then
      ErasureCodingWrapper.ensureLibraryLoaded()
      libraryInitialized = true
  }

  /** Check if the native library is available */
  def isAvailable: Boolean =
    try
      ensureInitialized()
      ErasureCodingWrapper.isLibraryLoaded
    catch
      case _: RuntimeException | _: LinkageError => false

  /** Shard with data and index */
  case class Shard(data: Array[Byte], index: Int)

  sealed trait ErasureCodingError extends Exception
  case class InvalidBasicSize(size: Int) extends ErasureCodingError:
    override def getMessage: String = s"Invalid basic size: $size (must be even)"
  case object InvalidShardsCount extends ErasureCodingError:
    override def getMessage: String = "Insufficient shards for reconstruction"
  case class InvalidRecoveryCount(recoveryCount: Int) extends ErasureCodingError:
    override def getMessage: String = s"Invalid recovery count: $recoveryCount (basicSize/2 exceeds validatorCount)"
  case object RaggedShards extends ErasureCodingError:
    override def getMessage: String = "Shards have inconsistent (ragged) lengths"
  case object EncodeFailed extends ErasureCodingError:
    override def getMessage: String = "Erasure coding encode failed"
  case object RecoverFailed extends ErasureCodingError:
    override def getMessage: String = "Erasure coding recover failed"

  /**
   * Split data into chunks of size n (with zero-padding if needed).
   */
  def split(data: Array[Byte], n: Int): Array[Array[Byte]] =
    if n <= 0 then return Array.empty

    val padded =
      val remainder = data.length % n
      if remainder != 0 then
        data ++ Array.fill(n - remainder)(0.toByte)
      else
        data

    val k = padded.length / n
    (0 until k).map(i => padded.slice(i * n, (i + 1) * n)).toArray

  /**
   * Join array of chunks into single array.
   */
  def join(arr: Array[Array[Byte]]): Array[Byte] =
    arr.flatten

  /**
   * Transpose a 2D array.
   */
  def transpose[T: reflect.ClassTag](matrix: Array[Array[T]]): Array[Array[T]] =
    if matrix.isEmpty || matrix(0).isEmpty then return Array.empty
    val rows = matrix.length
    val cols = matrix(0).length
    Array.tabulate(cols, rows)((col, row) => matrix(row)(col))

  /**
   * Encode original shards into recovery shards.
   *
   * @param original Array of original shards (each must have even length)
   * @param recoveryCount Number of recovery shards to generate
   * @return Array of recovery shards
   */
  def encode(original: Array[Array[Byte]], recoveryCount: Int): Either[ErasureCodingError, Array[Array[Byte]]] =
    if original.isEmpty then return Right(Array.empty)

    ensureInitialized()
    val result = ErasureCodingWrapper.encode(original, recoveryCount)
    if result == null then Left(EncodeFailed)
    else Right(result)

  /**
   * Recover original shards from a subset of shards.
   *
   * @param originalCount Total number of original shards
   * @param recoveryCount Total number of recovery shards
   * @param shards Available shards with their indices
   * @return Recovered original shards
   */
  def recover(
    originalCount: Int,
    recoveryCount: Int,
    shards: Array[Shard]
  ): Either[ErasureCodingError, Array[Array[Byte]]] =
    if shards.length < originalCount then return Left(InvalidShardsCount)

    ensureInitialized()
    val shardData = shards.map(_.data)
    val indices = shards.map(_.index)

    val result = ErasureCodingWrapper.recover(shardData, indices, originalCount, recoveryCount)
    if result == null then Left(RecoverFailed)
    else Right(result)

  /**
   * C_k: Erasure-code chunking function.
   *
   * @param data The original data
   * @param basicSize Approximately 2 * number of cores; W_E (erasureCodedPieceSize), 684 for full config
   * @param validatorCount Number of validators (total output shards); 1023 for full config
   * @return Array of erasure-coded chunks (validatorCount shards)
   */
  def chunk(data: Array[Byte], basicSize: Int, validatorCount: Int): Either[ErasureCodingError, Array[Array[Byte]]] =
    if basicSize % 2 != 0 then Left(InvalidBasicSize(basicSize))
    else if basicSize / 2 > validatorCount then Left(InvalidRecoveryCount(validatorCount - basicSize / 2))
    else if data.isEmpty then Right(Array.empty)
    else
      ensureInitialized()

      val originalCount = basicSize / 2
      val recoveryCount = validatorCount - originalCount
      // Use ceiling division to ensure k >= 1 even for small data
      val k = Math.max(1, (data.length + basicSize - 1) / basicSize)

      // One native call codes every 2-byte position; data is zero-padded to k * basicSize there.
      val result = ErasureCodingWrapper.chunk(data, originalCount, recoveryCount, k)
      if result == null then Left(EncodeFailed)
      else Right(result)

  /**
   * R_k: Erasure-code reconstruction function.
   *
   * @param shards The shards to reconstruct from (should be ordered by index)
   * @param basicSize Approximately 2 * number of cores; 684 for full config
   * @param validatorCount Number of validators (total shards); 1023 for full config
   * @return Reconstructed original data
   */
  def reconstruct(
    shards: Array[Shard],
    basicSize: Int,
    validatorCount: Int
  ): Either[ErasureCodingError, Array[Byte]] =
    if shards.isEmpty then Left(InvalidShardsCount)
    else if basicSize % 2 != 0 then Left(InvalidBasicSize(basicSize))
    else
      val originalCount = basicSize / 2
      val recoveryCount = validatorCount - originalCount

      if shards.length < originalCount then Left(InvalidShardsCount)
      else if shards.exists(_.data.length != shards(0).data.length) then Left(RaggedShards)
      else
        ensureInitialized()

        // One native call recovers every 2-byte position; odd-length shards are zero-padded to a whole word there.
        val result =
          ErasureCodingWrapper.reconstruct(shards.map(_.data), shards.map(_.index), originalCount, recoveryCount)
        if result == null then Left(RecoverFailed)
        else Right(result)
