package io.forge.jam.protocol.refine

import io.forge.jam.core.ChainConfig
import io.forge.jam.core.constants.Csegmentsize
import io.forge.jam.core.merkle.BinaryMerkle
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.codec.JamCodecs
import io.forge.jam.core.types.work.PackageSpec
import io.forge.jam.crypto.ErasureCoding
import spire.math.{UInt, UShort}

/** The availability specifier function: commits to the
  * audit bundle and the exported segments via the erasure-root, and to the
  * exported segments alone via the constant-depth segments-root.
  */
object AvailabilitySpecifier:

  /** Paged-proofs function P: one segment per 64 exported
    * segments, each carrying encode(var(justPath6(s, i)), var(page6(s, i)))
    * zero-padded to Csegmentsize.
    */
  def pagedProofs(segments: IndexedSeq[Array[Byte]]): IndexedSeq[Array[Byte]] =
    BinaryMerkle.pagedJustifications(segments, 6).map { (path, page) =>
      val out = new java.io.ByteArrayOutputStream(Csegmentsize.toInt)
      out.write(JamCodecs.encodeCompactInteger(path.length.toLong))
      path.foreach(out.write)
      out.write(JamCodecs.encodeCompactInteger(page.length.toLong))
      page.foreach(out.write)
      val bytes = out.toByteArray
      require(
        bytes.length <= Csegmentsize.toInt,
        s"paged proof exceeds segment size: ${bytes.length}"
      )
      java.util.Arrays.copyOf(bytes, Csegmentsize.toInt) // zero-pad
    }

  /** The constant-depth segments-root over the exported segments. */
  def segmentsRoot(segments: IndexedSeq[Array[Byte]]): Hash =
    Hash(BinaryMerkle.merklizeCD(segments))

  /** Build the availability specifier: package hash, bundle length,
    * erasure-root, segments-root, exported segment count.
    */
  def build(
      packageHash: Hash,
      bundle: Array[Byte],
      exportedSegments: IndexedSeq[Array[Byte]],
      config: ChainConfig
  ): Either[String, PackageSpec] =
    for
      bClub <- bundleChunkHashes(bundle, config)
      sClub <- segmentChunkRoots(exportedSegments, config)
    yield
      val leaves: IndexedSeq[Array[Byte]] =
        (0 until config.validatorCount).map(i => bClub(i) ++ sClub(i))
      val erasureRoot = Hash(BinaryMerkle.merklizeWB(leaves))
      PackageSpec(
        hash = packageHash,
        length = UInt(bundle.length),
        erasureRoot = erasureRoot,
        erasureShards = UShort(config.validatorCount),
        exportsRoot = segmentsRoot(exportedSegments),
        exportsCount = UShort(exportedSegments.length)
      )

  /** hash of each validator's erasure chunk of the (zero-padded) bundle. */
  private def bundleChunkHashes(
      bundle: Array[Byte],
      config: ChainConfig
  ): Either[String, IndexedSeq[Array[Byte]]] =
    // chunk() zero-pads to k·ecPieceSize internally and yields
    // validatorCount shards of 2k bytes each.
    val data = if bundle.isEmpty then Array[Byte](0) else bundle
    ErasureCoding
      .chunk(data, config.ecPieceSize, config.validatorCount)
      .left
      .map(e => s"bundle erasure coding failed: ${e.getMessage}")
      .map(shards => shards.toIndexedSeq.map(BinaryMerkle.blake2b))

  private def segmentChunkRoots(
      exportedSegments: IndexedSeq[Array[Byte]],
      config: ChainConfig
  ): Either[String, IndexedSeq[Array[Byte]]] =
    val all = exportedSegments ++ pagedProofs(exportedSegments)
    if all.isEmpty then
      Right(IndexedSeq.fill(config.validatorCount)(BinaryMerkle.ZeroHash))
    else
      // Erasure-code every segment in one call: validatorCount chunks per segment.
      ErasureCoding
        .chunkSegments(all, config.ecPieceSize, config.validatorCount)
        .left
        .map(e => s"segment erasure coding failed: ${e.getMessage}")
        .map(shards => (0 until config.validatorCount).map(v => BinaryMerkle.merklizeWB(shards.column(v))))
