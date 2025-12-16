package io.forge.jam.core.types

import scodec.*
import scodec.bits.*
import scodec.codecs.*
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.json.JsonHelpers.parseHex
import io.forge.jam.core.scodec.JamCodecs
import io.circe.Decoder

/**
 * Historical types.
 */
object history:

  // ============================================================================
  // Private Codec Helpers
  // ============================================================================

  private val hashCodec: Codec[Hash] = fixedSizeBytes(Hash.Size.toLong, bytes).xmap(
    bv => Hash.fromByteVectorUnsafe(bv),
    h => h.toByteVector
  )

  private def optionCodec[A](codec: Codec[A]): Codec[Option[A]] =
    discriminated[Option[A]].by(byte)
      .subcaseP(0) { case None => None }(provide(None))
      .subcaseP(1) { case Some(v) => Some(v) }(codec.xmap(Some(_), _.get))

  private def compactPrefixedList[A](codec: Codec[A]): Codec[List[A]] =
    listOfN(JamCodecs.compactInt, codec)

  /**
   * A reported work package with its hash and exports root.
   * Fixed size: 64 bytes (32-byte hash + 32-byte exports root)
   *
   * Used by: History, Report STFs
   */
  final case class ReportedWorkPackage(
    hash: Hash,
    exportsRoot: Hash
  )

  object ReportedWorkPackage:
    val Size: Int = Hash.Size * 2 // 64 bytes

    given Codec[ReportedWorkPackage] =
      (hashCodec :: hashCodec).xmap(
        { case (hash, exportsRoot) => ReportedWorkPackage(hash, exportsRoot) },
        rwp => (rwp.hash, rwp.exportsRoot)
      )

    given Decoder[ReportedWorkPackage] =
      Decoder.instance { cursor =>
        for
          hashHex <- cursor.get[String]("hash")
          exportsRootHex <- cursor.get[String]("exports_root")
        yield ReportedWorkPackage(Hash(parseHex(hashHex)), Hash(parseHex(exportsRootHex)))
      }

  /**
   * Historical block entry containing header hash, beefy root, state root,
   * and reported work packages.
   */
  final case class HistoricalBeta(
    headerHash: Hash,
    beefyRoot: Hash,
    stateRoot: Hash,
    reported: List[ReportedWorkPackage]
  )

  object HistoricalBeta:
    given Codec[HistoricalBeta] =
      (hashCodec :: hashCodec :: hashCodec :: compactPrefixedList(Codec[ReportedWorkPackage])).xmap(
        { case (headerHash, beefyRoot, stateRoot, reported) =>
          HistoricalBeta(headerHash, beefyRoot, stateRoot, reported)
        },
        hb => (hb.headerHash, hb.beefyRoot, hb.stateRoot, hb.reported.sortBy(_.hash.toHex))
      )

    given Decoder[HistoricalBeta] =
      Decoder.instance { cursor =>
        for
          headerHash <- cursor.get[String]("header_hash").map(h => Hash(parseHex(h)))
          beefyRoot <- cursor.get[String]("beefy_root").map(h => Hash(parseHex(h)))
          stateRoot <- cursor.get[String]("state_root").map(h => Hash(parseHex(h)))
          reported <- cursor.get[List[ReportedWorkPackage]]("reported")
        yield HistoricalBeta(headerHash, beefyRoot, stateRoot, reported)
      }

  /**
   * Merkle Mountain Range (MMR) with optional peaks.
   * Peaks are ordered by tree height, with None representing empty positions.
   */
  final case class HistoricalMmr(
    peaks: List[Option[Hash]]
  )

  object HistoricalMmr:
    val empty: HistoricalMmr = HistoricalMmr(List.empty)

    given Codec[HistoricalMmr] =
      compactPrefixedList(optionCodec(hashCodec)).xmap(
        peaks => HistoricalMmr(peaks),
        hmr => hmr.peaks
      )

    given Decoder[HistoricalMmr] =
      Decoder.instance { cursor =>
        cursor.get[List[Option[String]]]("peaks").map { peaks =>
          HistoricalMmr(peaks.map(_.map(h => Hash(parseHex(h)))))
        }
      }

  /**
   * Container for historical beta entries and MMR.
   */
  final case class HistoricalBetaContainer(
    history: List[HistoricalBeta] = List.empty,
    mmr: HistoricalMmr = HistoricalMmr.empty
  )

  object HistoricalBetaContainer:
    given Codec[HistoricalBetaContainer] =
      (compactPrefixedList(Codec[HistoricalBeta]) :: Codec[HistoricalMmr]).xmap(
        { case (history, mmr) => HistoricalBetaContainer(history, mmr) },
        hbc => (hbc.history, hbc.mmr)
      )

    given Decoder[HistoricalBetaContainer] =
      Decoder.instance { cursor =>
        for
          history <- cursor.get[List[HistoricalBeta]]("history")
          mmr <- cursor.get[HistoricalMmr]("mmr")
        yield HistoricalBetaContainer(history, mmr)
      }
