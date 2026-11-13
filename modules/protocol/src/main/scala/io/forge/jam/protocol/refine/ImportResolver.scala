package io.forge.jam.protocol.refine

import io.forge.jam.core.codec.JamCodecs
import io.forge.jam.core.constants.Csegmentsize
import io.forge.jam.core.merkle.BinaryMerkle
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.workpackage.WorkPackage

/** Where a work-package's imported segments come from, and whether what was supplied for them is right.
  * Pure.
  *
  * An import names a segments-root directly, or (index top bit set) the hash of the work-package that exported
  * it; the latter is resolved through the segment-root lookup, which a guarantor builds from the packages reported in
  * recent history and the report then carries. Each imported segment comes with its justification, a path proving
  * it against its segments-root.
  */
object ImportResolver:

  /** Top bit of an import index: the root is a work-package hash. */
  val BoxplusFlag: Int = 0x8000

  /** One import: the segments-root it is read from and its index there. */
  final case class Import(segmentsRoot: Hash, index: Int)

  /** Per work item, the segments-root and index of each import, and the lookup package hashes resolved through (for the
    * report). `reported` maps the work-package hashes we can resolve to their segments-roots.
    */
  def resolve(wp: WorkPackage, reported: Map[Hash, Hash]): Either[String, (IndexedSeq[IndexedSeq[Import]], Map[Hash, Hash])] =
    val lookup = Map.newBuilder[Hash, Hash]
    val resolved = wp.items.toIndexedSeq.map { item =>
      item.importSegments.toIndexedSeq.map { s =>
        val raw = s.index.toInt
        if (raw & BoxplusFlag) == 0 then Right(Import(s.treeRoot, raw))
        else
          reported.get(s.treeRoot) match
            case Some(root) =>
              lookup += s.treeRoot -> root
              Right(Import(root, raw & ~BoxplusFlag))
            case None => Left(s"import from package ${s.treeRoot.toHex.take(18)}, which no recent report exported")
      }
    }
    resolved.flatten.collectFirst { case Left(e) => e } match
      case Some(err) => Left(err)
      case None      => Right((resolved.map(_.map(_.toOption.get)), lookup.result()))

  /** Whether `segment` is item `index` under `segmentsRoot`, by justification `proof`. */
  def verify(segmentsRoot: Hash, index: Int, segment: Array[Byte], proof: List[Array[Byte]]): Boolean =
    segment.length == Csegmentsize.toInt &&
      BinaryMerkle.verifyConstantDepth(segmentsRoot.bytes, segment, index, proof)

  /** Check every imported segment of `bundle` against its justification, imports by package hash resolved through
    * `lookup` (the report's segment-root lookup).
    */
  def verifyBundle(bundle: WorkPackageBundle, lookup: Map[Hash, Hash]): Either[String, Unit] =
    resolve(bundle.workPackage, lookup).flatMap { (imports, _) =>
      val bad = for
        (item, i) <- imports.zipWithIndex
        (imp, j) <- item.zipWithIndex
        if !(bundle.importSegments.lift(i).flatMap(_.lift(j)) zip bundle.justifications.lift(i).flatMap(_.lift(j)))
          .exists((seg, proof) => verify(imp.segmentsRoot, imp.index, seg, proof))
      yield (i, j)
      bad.headOption match
        case Some((i, j)) => Left(s"import $j of item $i does not verify against its segments-root")
        case None         => Right(())
    }

  /** Segments per page of the paged proofs (2^6). */
  val PageSize: Int = 64

  /** How many paged-proof segments follow `exports` exported segments in the erasure-coded data. */
  def pagedProofCount(exports: Int): Int = (exports + PageSize - 1) / PageSize

  /** The justification of exported segment `index` of `exports`, from the paged-proof segment of its page (what
    * [[AvailabilitySpecifier.pagedProofs]] made): the path from the root to the page, then the path within the page,
    * over its leaf hashes padded as the constant-depth tree pads them. None when the proof segment does not decode.
    * The result is to be checked with [[verify]], which is what makes it trustworthy.
    */
  def justificationFromPagedProof(pagedProof: Array[Byte], index: Int, exports: Int): Option[List[Array[Byte]]] =
    try
      var off = 0
      def compact(): Int =
        val (v, used) = JamCodecs.decodeCompactInteger(pagedProof, off)
        off += used
        if v < 0 || v > PageSize then throw new IllegalArgumentException("bad count")
        v.toInt
      def hashes(n: Int): List[Array[Byte]] =
        if off + 32 * n > pagedProof.length then throw new IllegalArgumentException("truncated")
        val out = (0 until n).map(k => java.util.Arrays.copyOfRange(pagedProof, off + 32 * k, off + 32 * (k + 1))).toList
        off += 32 * n
        out
      val path = hashes(compact())
      val leaves = hashes(compact())
      var padded = 1
      while padded < math.max(1, exports) do padded <<= 1
      val pageLeaves = math.min(PageSize, padded)
      val inPage = index % PageSize
      if index < 0 || index >= exports || leaves.size > pageLeaves || inPage >= leaves.size then None
      else
        val full = leaves.toIndexedSeq ++ IndexedSeq.fill(pageLeaves - leaves.size)(BinaryMerkle.ZeroHash)
        Some(path ++ BinaryMerkle.trace(full, inPage))
    catch case _: Exception => None
