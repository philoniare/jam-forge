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

  /** The shards of many segments: validator v's chunk of segment j for every v and j. */
  sealed trait SegmentShards:
    def segmentCount: Int

    /** Validator v's chunk of segment j. */
    def chunk(v: Int, j: Int): Array[Byte]

    /** Validator v's chunks of every segment, in segment order. */
    def column(v: Int): IndexedSeq[Array[Byte]] = (0 until segmentCount).map(chunk(v, _))

    /** Validator v's chunks of every segment end to end: the shard [[reconstructSegments]] takes back. */
    def joinedColumn(v: Int): Array[Byte] = column(v).toArray.flatten

  /** Chunks of `shardBytes` bytes each, validator v's chunk of segment j at `(v * segmentCount + j) * shardBytes`. */
  private final class FlatSegmentShards(flat: Array[Byte], val segmentCount: Int, shardBytes: Int)
      extends SegmentShards:
    def chunk(v: Int, j: Int): Array[Byte] =
      val at = (v * segmentCount + j) * shardBytes
      java.util.Arrays.copyOfRange(flat, at, at + shardBytes)

    override def joinedColumn(v: Int): Array[Byte] =
      val at = v * segmentCount * shardBytes
      java.util.Arrays.copyOfRange(flat, at, at + segmentCount * shardBytes)

  /** One [[chunk]] result per segment. */
  private final class PerSegmentShards(shards: IndexedSeq[Array[Array[Byte]]]) extends SegmentShards:
    def segmentCount: Int = shards.length
    def chunk(v: Int, j: Int): Array[Byte] = shards(j)(v)

  /**
   * C_k of each of `segments` 
   */
  def chunkSegments(
    segments: IndexedSeq[Array[Byte]],
    basicSize: Int,
    validatorCount: Int
  ): Either[ErasureCodingError, SegmentShards] =
    if basicSize % 2 != 0 then Left(InvalidBasicSize(basicSize))
    else if basicSize / 2 > validatorCount then Left(InvalidRecoveryCount(validatorCount - basicSize / 2))
    else if segments.isEmpty then Right(PerSegmentShards(IndexedSeq.empty))
    else
      val len = segments(0).length
      if len == 0 || segments.exists(_.length != len) then chunkEach(segments, basicSize, validatorCount)
      else
        ensureInitialized()
        val originalCount = basicSize / 2
        val k = Math.max(1, (len + basicSize - 1) / basicSize)
        val data = new Array[Byte](len * segments.length)
        var j = 0
        while j < segments.length do
          System.arraycopy(segments(j), 0, data, j * len, len)
          j += 1
        val flat = ErasureCodingWrapper.chunkSegments(data, segments.length, originalCount, validatorCount - originalCount, k)
        if flat == null then Left(EncodeFailed) else Right(FlatSegmentShards(flat, segments.length, 2 * k))

  /** [[chunkSegments]] for segments of differing lengths: one [[chunk]] each. */
  private def chunkEach(
    segments: IndexedSeq[Array[Byte]],
    basicSize: Int,
    validatorCount: Int
  ): Either[ErasureCodingError, SegmentShards] =
    val perSegment = segments.map(chunk(_, basicSize, validatorCount))
    perSegment.collectFirst { case Left(e) => e } match
      case Some(e) => Left(e)
      case None => Right(PerSegmentShards(perSegment.map(_.toOption.get)))

  /**
   * R_k of each of `segmentCount` segments at once
   */
  def reconstructSegments(
    shards: Array[Shard],
    segmentCount: Int,
    basicSize: Int,
    validatorCount: Int
  ): Either[ErasureCodingError, IndexedSeq[Array[Byte]]] =
    if segmentCount <= 0 || shards.isEmpty || shards(0).data.length % segmentCount != 0 then Left(InvalidShardsCount)
    else
      val shardBytes = shards(0).data.length / segmentCount
      if shardBytes % 2 != 0 then Left(RaggedShards)
      else
        reconstruct(shards, basicSize, validatorCount).map { joined =>
          // joined holds original shard i (every segment's chunk i in turn) for each i in order.
          val originalCount = basicSize / 2
          val row = segmentCount * shardBytes
          (0 until segmentCount).map { j =>
            val out = new Array[Byte](originalCount * shardBytes)
            var i = 0
            while i < originalCount do
              System.arraycopy(joined, i * row + j * shardBytes, out, i * shardBytes, shardBytes)
              i += 1
            out
          }
        }
