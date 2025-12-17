package io.forge.jam.crypto

/**
 * Erasure coding implementation using Reed-Solomon via native JNI bindings.
 */
object ErasureCoding:

  /** Inner shard size in bytes (must be 2 for reed-solomon-simd) */
  val InnerShardSize: Int = 2

  private var libraryInitialized = false

  /** Ensure the native library is loaded */
  private def ensureInitialized(): Unit =
    if !libraryInitialized then
      ErasureCodingWrapper.ensureLibraryLoaded()
      libraryInitialized = true

  /** Check if the native library is available */
  def isAvailable: Boolean =
    try
      ensureInitialized()
      ErasureCodingWrapper.isLibraryLoaded
    catch
      case _: Throwable => false

  /** Shard with data and index */
  case class Shard(data: Array[Byte], index: Int)

  sealed trait ErasureCodingError extends Exception
  case class InvalidBasicSize(size: Int) extends ErasureCodingError:
    override def getMessage: String = s"Invalid basic size: $size (must be even)"
  case object InvalidShardsCount extends ErasureCodingError:
    override def getMessage: String = "Insufficient shards for reconstruction"
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
    else if data.isEmpty then Right(Array.empty)
    else
      ensureInitialized()

      val originalCount = basicSize / 2
      val recoveryCount = validatorCount - originalCount
      // Use ceiling division to ensure k >= 1 even for small data
      val k = Math.max(1, (data.length + basicSize - 1) / basicSize)

      // Pad data to k * basicSize bytes to ensure we always have originalCount shards
      val paddedLength = k * basicSize
      val paddedData = if data.length < paddedLength then
        data ++ Array.fill(paddedLength - data.length)(0.toByte)
      else
        data

      // Split padded data into originalCount chunks of 2*k bytes each
      val splitted = split(paddedData, 2 * k)

      // Split each chunk into inner shards of size InnerShardSize
      val splitted2 = splitted.map(d => split(d, InnerShardSize))

      // Transpose to get original shards grouped by position
      val originalShards = transpose(splitted2)

      // Encode each group of original shards - returns originalCount + recoveryCount shards
      val encodedResults = originalShards.map(shards => encode(shards, recoveryCount))

      // Check for errors
      encodedResults.find(_.isLeft) match
        case Some(Left(err)) => Left(err)
        case _ =>
          val result2d = encodedResults.map(_.toOption.get)
          // Transpose back and join
          val transposed = transpose(result2d)
          Right(transposed.map(join))

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
    if shards.isEmpty then Right(Array.empty)
    else if basicSize % 2 != 0 then Left(InvalidBasicSize(basicSize))
    else
      val originalCount = basicSize / 2
      val recoveryCount = validatorCount - originalCount

      if shards.length < originalCount then Left(InvalidShardsCount)
      else
        ensureInitialized()

        val shardSize = shards(0).data.length
        val k = (shardSize + InnerShardSize - 1) / InnerShardSize

        // Split each shard into inner shards
        val splitted = shards.map(s => split(s.data, InnerShardSize))

        // Recover each position
        val recoverResults = (0 until k).map { p =>
          // Collect inner shards at position p from all available shards
          val recoveryShards = shards.indices.map(i => Shard(splitted(i)(p), shards(i).index)).toArray

          recover(originalCount, recoveryCount, recoveryShards)
        }

        // Check for errors
        recoverResults.find(_.isLeft) match
          case Some(Left(err)) => Left(err)
          case _ =>
            val result2d = recoverResults.map(_.toOption.get).toArray
            // Transpose and join
            val transposed = transpose(result2d)
            Right(join(transposed.map(join)))
