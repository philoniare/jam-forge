package io.forge.jam.protocol.refine

import io.forge.jam.core.ChainConfig
import io.forge.jam.core.merkle.BinaryMerkle
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.scodec.JamCodecs
import io.forge.jam.crypto.ErasureCoding

/** One validator's custody unit for a work package:
  * its erasure chunk of the audit bundle, its chunk column across all
  * exported + paged-proof segments, and the Merkle co-path justifying the
  * pair against the report's erasure-root.
  */
final case class ValidatorShards(
    validatorIndex: Int,
    bundleShard: Array[Byte],
    segmentShards: IndexedSeq[Array[Byte]],
    justification: List[Array[Byte]]
):
  /** The erasure-root leaf this shard set reconstructs. */
  def leaf: Array[Byte] =
    BinaryMerkle.blake2b(bundleShard) ++ BinaryMerkle.merklizeWB(segmentShards)

  /** Serialize for storage / the CE 137 wire (see [[DaShards.decode]]). */
  def encode: Array[Byte] =
    val out = new java.io.ByteArrayOutputStream()
    def writeBlob(b: Array[Byte]): Unit =
      out.write(JamCodecs.encodeCompactInteger(b.length.toLong))
      out.write(b)
    out.write(JamCodecs.encodeCompactInteger(validatorIndex.toLong))
    writeBlob(bundleShard)
    out.write(JamCodecs.encodeCompactInteger(segmentShards.length.toLong))
    segmentShards.foreach(writeBlob)
    out.write(JamCodecs.encodeCompactInteger(justification.length.toLong))
    justification.foreach(writeBlob)
    out.toByteArray

/** Per-validator shard construction, verification and reconstruction for the
  * data-availability layer. Mirrors [[AvailabilitySpecifier]]'s commitments;
  * `DaShardsSpec` asserts the two agree on the erasure-root.
  */
object DaShards:

  /** Build every validator's shards for a computed report.
    *
    * @param bundleBytes the encoded audit bundle
    * @param exportedSegments the exported segments (paged proofs are derived
    *   here, matching the availability specifier)
    */
  def buildAll(
      bundleBytes: Array[Byte],
      exportedSegments: IndexedSeq[Array[Byte]],
      config: ChainConfig
  ): Either[String, IndexedSeq[ValidatorShards]] =
    val bundleData = if bundleBytes.isEmpty then Array[Byte](0) else bundleBytes
    for
      bundleChunks <- ErasureCoding
        .chunk(bundleData, config.ecPieceSize, config.validatorCount)
        .left
        .map(e => s"bundle erasure coding failed: ${e.getMessage}")
      segmentColumns <- chunkSegments(exportedSegments, config)
    yield
      val leaves: IndexedSeq[Array[Byte]] =
        (0 until config.validatorCount).map { v =>
          BinaryMerkle.blake2b(bundleChunks(v)) ++
            BinaryMerkle.merklizeWB(segmentColumns(v))
        }
      (0 until config.validatorCount).map { v =>
        ValidatorShards(
          validatorIndex = v,
          bundleShard = bundleChunks(v),
          segmentShards = segmentColumns(v),
          justification = BinaryMerkle.trace(leaves, v)
        )
      }

  /** Verify a shard set against a report's erasure-root: recompute the leaf
    * from the shards and check the justification path.
    */
  def verify(
      erasureRoot: Hash,
      validatorCount: Int,
      shards: ValidatorShards
  ): Boolean =
    shards.validatorIndex >= 0 && shards.validatorIndex < validatorCount &&
      BinaryMerkle.verifyTrace(
        erasureRoot.bytes.toArray,
        shards.leaf,
        shards.validatorIndex,
        validatorCount,
        shards.justification
      )

  /** Reconstruct the audit bundle from ≥ ecPieceSize/2 bundle shards.
    *
    * @param bundleLength the true bundle length (avspec.length); the
    *   erasure-coded payload is zero-padded beyond it
    */
  def reconstructBundle(
      bundleLength: Int,
      shards: Seq[(Int, Array[Byte])],
      config: ChainConfig
  ): Either[String, Array[Byte]] =
    ErasureCoding
      .reconstruct(
        shards.sortBy(_._1).map((i, d) => ErasureCoding.Shard(d, i)).toArray,
        config.ecPieceSize,
        config.validatorCount
      )
      .left
      .map(e => s"bundle reconstruction failed: ${e.getMessage}")
      .flatMap { data =>
        if data.length < bundleLength then
          Left(s"reconstructed ${data.length} bytes < declared $bundleLength")
        else Right(java.util.Arrays.copyOf(data, bundleLength))
      }

  /** Decode a [[ValidatorShards.encode]] blob. */
  def decode(bytes: Array[Byte]): Either[String, ValidatorShards] =
    try
      var offset = 0
      def readCompact(): Long =
        val (v, consumed) = JamCodecs.decodeCompactInteger(bytes, offset)
        offset += consumed
        v
      def readBlob(): Array[Byte] =
        val len = readCompact().toInt
        if offset + len > bytes.length then throw new IllegalArgumentException("truncated blob")
        val b = java.util.Arrays.copyOfRange(bytes, offset, offset + len)
        offset += len
        b
      val index = readCompact().toInt
      val bundleShard = readBlob()
      val segmentShards = IndexedSeq.fill(readCompact().toInt)(readBlob())
      val justification = List.fill(readCompact().toInt)(readBlob())
      if offset != bytes.length then Left("trailing bytes in shard encoding")
      else Right(ValidatorShards(index, bundleShard, segmentShards, justification))
    catch case e: Exception => Left(s"bad shard encoding: ${e.getMessage}")

  /** Erasure-code every (exported ++ paged-proof) segment and regroup by
    * validator: column v is validator v's chunk of each segment in order.
    * Empty exports → empty columns for every validator.
    */
  private def chunkSegments(
      exportedSegments: IndexedSeq[Array[Byte]],
      config: ChainConfig
  ): Either[String, IndexedSeq[IndexedSeq[Array[Byte]]]] =
    val all = exportedSegments ++ AvailabilitySpecifier.pagedProofs(exportedSegments)
    if all.isEmpty then Right(IndexedSeq.fill(config.validatorCount)(IndexedSeq.empty))
    else
      val perSegment = all.map { seg =>
        ErasureCoding.chunk(seg, config.ecPieceSize, config.validatorCount)
      }
      perSegment.collectFirst { case Left(e) => e } match
        case Some(e) => Left(s"segment erasure coding failed: ${e.getMessage}")
        case None =>
          val matrix = perSegment.map(_.toOption.get)
          Right(
            (0 until config.validatorCount).map(v => matrix.map(row => row(v)))
          )
