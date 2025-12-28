package io.forge.jam.protocol.history

import _root_.scodec.Codec
import io.forge.jam.core.{ChainConfig, constants}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.history.{ReportedWorkPackage, HistoricalBeta, HistoricalMmr, HistoricalBetaContainer}
import io.forge.jam.core.json.JsonHelpers.parseHash
import io.forge.jam.core.scodec.JamCodecs
import io.circe.Decoder

/**
 * Types for the History State Transition Function.
 * The History STF tracks block history using a Merkle Mountain Range (MMR)
 * for efficient beefy root calculation.
 */
object HistoryTypes:
  // Re-export core types for backward compatibility
  export io.forge.jam.core.types.history.ReportedWorkPackage
  export io.forge.jam.core.types.history.HistoricalBeta
  export io.forge.jam.core.types.history.HistoricalMmr
  export io.forge.jam.core.types.history.HistoricalBetaContainer

  /**
   * Historical state containing the beta container.
   */
  final case class HistoricalState(
    beta: HistoricalBetaContainer
  )

  object HistoricalState:
    given Codec[HistoricalState] =
      Codec[HistoricalBetaContainer].xmap(
        beta => HistoricalState(beta),
        hs => hs.beta
      )


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
    given Codec[HistoricalInput] =
      (JamCodecs.hashCodec :: JamCodecs.hashCodec :: JamCodecs.hashCodec :: JamCodecs.compactPrefixedList(Codec[ReportedWorkPackage])).xmap(
        { case (headerHash, parentStateRoot, accumulateRoot, workPackages) =>
          HistoricalInput(headerHash, parentStateRoot, accumulateRoot, workPackages)
        },
        hi => (hi.headerHash, hi.parentStateRoot, hi.accumulateRoot, hi.workPackages)
      )


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
    given Codec[HistoricalCase] =
      (Codec[HistoricalInput] :: Codec[HistoricalState] :: Codec[HistoricalState]).xmap(
        { case (input, preState, postState) =>
          HistoricalCase(input, preState, postState)
        },
        hc => (hc.input, hc.preState, hc.postState)
      )


    given Decoder[HistoricalCase] =
      Decoder.instance { cursor =>
        for
          input <- cursor.get[HistoricalInput]("input")
          preState <- cursor.get[HistoricalState]("pre_state")
          postState <- cursor.get[HistoricalState]("post_state")
        yield HistoricalCase(input, preState, postState)
      }
