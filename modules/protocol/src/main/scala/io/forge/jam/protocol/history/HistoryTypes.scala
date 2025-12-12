package io.forge.jam.protocol.history

import io.forge.jam.core.{JamBytes, encoding, ChainConfig, constants}
import io.forge.jam.core.codec.{JamEncoder, JamDecoder}
import io.forge.jam.core.primitives.Hash
import io.circe.Decoder
import io.forge.jam.protocol.common.JsonHelpers.parseHash
import io.forge.jam.protocol.common.SharedTypes.ReportedWorkPackage

/**
 * Types for the History State Transition Function.
 * The History STF tracks block history using a Merkle Mountain Range (MMR)
 * for efficient beefy root calculation.
 */
object HistoryTypes:
  export io.forge.jam.protocol.common.SharedTypes.ReportedWorkPackage

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
    given JamEncoder[HistoricalBeta] with
      def encode(a: HistoricalBeta): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= a.headerHash.bytes
        builder ++= a.beefyRoot.bytes
        builder ++= a.stateRoot.bytes
        builder ++= encoding.encodeCompactInteger(a.reported.length.toLong)
        for pkg <- a.reported do
          builder ++= summon[JamEncoder[ReportedWorkPackage]].encode(pkg)
        builder.result()

    given JamDecoder[HistoricalBeta] with
      def decode(bytes: JamBytes, offset: Int): (HistoricalBeta, Int) =
        val arr = bytes.toArray
        var pos = offset
        val headerHash = Hash(arr.slice(pos, pos + Hash.Size))
        pos += Hash.Size
        val beefyRoot = Hash(arr.slice(pos, pos + Hash.Size))
        pos += Hash.Size
        val stateRoot = Hash(arr.slice(pos, pos + Hash.Size))
        pos += Hash.Size
        val (reportedLength, reportedLengthBytes) = encoding.decodeCompactInteger(arr, pos)
        pos += reportedLengthBytes
        val reported = (0 until reportedLength.toInt).map { _ =>
          val (pkg, consumed) = summon[JamDecoder[ReportedWorkPackage]].decode(bytes, pos)
          pos += consumed
          pkg
        }.toList
        (HistoricalBeta(headerHash, beefyRoot, stateRoot, reported), pos - offset)

    given Decoder[HistoricalBeta] =
      Decoder.instance { cursor =>
        for
          headerHashHex <- cursor.get[String]("header_hash")
          beefyRootHex <- cursor.get[String]("beefy_root")
          stateRootHex <- cursor.get[String]("state_root")
          reported <- cursor.get[List[ReportedWorkPackage]]("reported")
          headerHash <- parseHash(headerHashHex)
          beefyRoot <- parseHash(beefyRootHex)
          stateRoot <- parseHash(stateRootHex)
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

    given JamEncoder[HistoricalMmr] with
      def encode(a: HistoricalMmr): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= encoding.encodeCompactInteger(a.peaks.length.toLong)
        for peak <- a.peaks do
          peak match
            case None =>
              builder += 0.toByte
            case Some(hash) =>
              builder += 1.toByte
              builder ++= hash.bytes
        builder.result()

    given JamDecoder[HistoricalMmr] with
      def decode(bytes: JamBytes, offset: Int): (HistoricalMmr, Int) =
        val arr = bytes.toArray
        var pos = offset
        val (length, lengthBytes) = encoding.decodeCompactInteger(arr, pos)
        pos += lengthBytes
        val peaks = (0 until length.toInt).map { _ =>
          val optionByte = arr(pos).toInt & 0xFF
          pos += 1
          if optionByte == 0 then
            None
          else
            val hash = Hash(arr.slice(pos, pos + Hash.Size))
            pos += Hash.Size
            Some(hash)
        }.toList
        (HistoricalMmr(peaks), pos - offset)

    given Decoder[HistoricalMmr] =
      Decoder.instance { cursor =>
        cursor.get[List[Option[String]]]("peaks").flatMap { peaksOpt =>
          val peaksResult = peaksOpt.map {
            case None => Right(None)
            case Some(hex) => parseHash(hex).map(Some(_))
          }
          val (errors, successes) = peaksResult.partitionMap(identity)
          if errors.nonEmpty then
            Left(io.circe.DecodingFailure(errors.head.message, cursor.history))
          else
            Right(HistoricalMmr(successes))
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
    given JamEncoder[HistoricalBetaContainer] with
      def encode(a: HistoricalBetaContainer): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= encoding.encodeCompactInteger(a.history.length.toLong)
        for beta <- a.history do
          builder ++= summon[JamEncoder[HistoricalBeta]].encode(beta)
        builder ++= summon[JamEncoder[HistoricalMmr]].encode(a.mmr)
        builder.result()

    given JamDecoder[HistoricalBetaContainer] with
      def decode(bytes: JamBytes, offset: Int): (HistoricalBetaContainer, Int) =
        val arr = bytes.toArray
        var pos = offset
        val (historyLength, historyLengthBytes) = encoding.decodeCompactInteger(arr, pos)
        pos += historyLengthBytes
        val history = (0 until historyLength.toInt).map { _ =>
          val (beta, consumed) = summon[JamDecoder[HistoricalBeta]].decode(bytes, pos)
          pos += consumed
          beta
        }.toList
        val (mmr, mmrBytes) = summon[JamDecoder[HistoricalMmr]].decode(bytes, pos)
        pos += mmrBytes
        (HistoricalBetaContainer(history, mmr), pos - offset)

    given Decoder[HistoricalBetaContainer] =
      Decoder.instance { cursor =>
        for
          history <- cursor.get[List[HistoricalBeta]]("history")
          mmr <- cursor.get[HistoricalMmr]("mmr")
        yield HistoricalBetaContainer(history, mmr)
      }

  /**
   * Historical state containing the beta container.
   */
  final case class HistoricalState(
    beta: HistoricalBetaContainer
  )

  object HistoricalState:
    given JamEncoder[HistoricalState] with
      def encode(a: HistoricalState): JamBytes =
        summon[JamEncoder[HistoricalBetaContainer]].encode(a.beta)

    given JamDecoder[HistoricalState] with
      def decode(bytes: JamBytes, offset: Int): (HistoricalState, Int) =
        val (beta, consumed) = summon[JamDecoder[HistoricalBetaContainer]].decode(bytes, offset)
        (HistoricalState(beta), consumed)

    given Decoder[HistoricalState] =
      Decoder.instance { cursor =>
        cursor.get[HistoricalBetaContainer]("beta").map(HistoricalState(_))
      }

  /**
   * Input to the History STF.
   */
  final case class HistoricalInput(
    headerHash: Hash,
    parentStateRoot: Hash,
    accumulateRoot: Hash,
    workPackages: List[ReportedWorkPackage]
  ):
    /**
     * Validate this input against the given chain configuration.
     * @param config The chain configuration (for max cores validation).
     * @throws IllegalArgumentException if validation fails.
     */
    def validate(config: ChainConfig = ChainConfig.FULL): Unit =
      require(headerHash.size == constants.HashSize, "Header hash must be 32 bytes")
      require(parentStateRoot.size == constants.HashSize, "Parent state root must be 32 bytes")
      require(accumulateRoot.size == constants.HashSize, "Accumulate root must be 32 bytes")
      require(workPackages.size <= config.coresCount, s"Too many work packages (max ${config.coresCount})")
      workPackages.zipWithIndex.foreach { (pkg, index) =>
        require(pkg.hash.size == constants.HashSize, s"Work package $index hash must be 32 bytes")
        require(pkg.exportsRoot.size == constants.HashSize, s"Work package $index exports root must be 32 bytes")
      }

  object HistoricalInput:
    given JamEncoder[HistoricalInput] with
      def encode(a: HistoricalInput): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= a.headerHash.bytes
        builder ++= a.parentStateRoot.bytes
        builder ++= a.accumulateRoot.bytes
        builder ++= encoding.encodeCompactInteger(a.workPackages.length.toLong)
        for pkg <- a.workPackages do
          builder ++= summon[JamEncoder[ReportedWorkPackage]].encode(pkg)
        builder.result()

    given JamDecoder[HistoricalInput] with
      def decode(bytes: JamBytes, offset: Int): (HistoricalInput, Int) =
        val arr = bytes.toArray
        var pos = offset
        val headerHash = Hash(arr.slice(pos, pos + Hash.Size))
        pos += Hash.Size
        val parentStateRoot = Hash(arr.slice(pos, pos + Hash.Size))
        pos += Hash.Size
        val accumulateRoot = Hash(arr.slice(pos, pos + Hash.Size))
        pos += Hash.Size
        val (length, lengthBytes) = encoding.decodeCompactInteger(arr, pos)
        pos += lengthBytes
        val workPackages = (0 until length.toInt).map { _ =>
          val (pkg, consumed) = summon[JamDecoder[ReportedWorkPackage]].decode(bytes, pos)
          pos += consumed
          pkg
        }.toList
        (HistoricalInput(headerHash, parentStateRoot, accumulateRoot, workPackages), pos - offset)

    given Decoder[HistoricalInput] =
      Decoder.instance { cursor =>
        for
          headerHashHex <- cursor.get[String]("header_hash")
          parentStateRootHex <- cursor.get[String]("parent_state_root")
          accumulateRootHex <- cursor.get[String]("accumulate_root")
          workPackages <- cursor.get[List[ReportedWorkPackage]]("work_packages")
          headerHash <- parseHash(headerHashHex)
          parentStateRoot <- parseHash(parentStateRootHex)
          accumulateRoot <- parseHash(accumulateRootHex)
        yield HistoricalInput(headerHash, parentStateRoot, accumulateRoot, workPackages)
      }

  /**
   * Test case for History STF containing input, pre-state, and post-state.
   */
  final case class HistoricalCase(
    input: HistoricalInput,
    preState: HistoricalState,
    postState: HistoricalState
  )

  object HistoricalCase:
    given JamEncoder[HistoricalCase] with
      def encode(a: HistoricalCase): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= summon[JamEncoder[HistoricalInput]].encode(a.input)
        builder ++= summon[JamEncoder[HistoricalState]].encode(a.preState)
        builder ++= summon[JamEncoder[HistoricalState]].encode(a.postState)
        builder.result()

    given JamDecoder[HistoricalCase] with
      def decode(bytes: JamBytes, offset: Int): (HistoricalCase, Int) =
        var pos = offset
        val (input, inputBytes) = summon[JamDecoder[HistoricalInput]].decode(bytes, pos)
        pos += inputBytes
        val (preState, preStateBytes) = summon[JamDecoder[HistoricalState]].decode(bytes, pos)
        pos += preStateBytes
        val (postState, postStateBytes) = summon[JamDecoder[HistoricalState]].decode(bytes, pos)
        pos += postStateBytes
        (HistoricalCase(input, preState, postState), pos - offset)

    given Decoder[HistoricalCase] =
      Decoder.instance { cursor =>
        for
          input <- cursor.get[HistoricalInput]("input")
          preState <- cursor.get[HistoricalState]("pre_state")
          postState <- cursor.get[HistoricalState]("post_state")
        yield HistoricalCase(input, preState, postState)
      }
