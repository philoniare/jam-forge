package io.forge.jam.conformance

import io.forge.jam.core.{ChainConfig, Hashing}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.scodec.JamCodecs.encode
import io.forge.jam.core.types.block.Block
import io.forge.jam.protocol.report.ReportTypes.AncestorHeader
import io.forge.jam.protocol.traces.{BlockImporter, ImportResult}

/** The single ImportBlock state transition shared by the fuzz socket server
  * ([[ProtocolHandler]]) and the in-process vector runner
  * (ConformanceTestRunner): per-chain ancestry derivation via
  * `StateStore.ancestryFor` (BP-9 — a shared global ancestry list is polluted
  * by fork mutations), import, post-state materialization, and parentage
  * recording so descendants can derive their own ancestry.
  */
object ImportBlockStep:

  def apply(
      stateStore: StateStore,
      blockImporter: BlockImporter,
      config: ChainConfig,
      block: Block
  ): Either[String, Hash] =
    val parentHash = block.header.parent
    stateStore.get(parentHash) match
      case None =>
        Left(s"Parent state not found: ${parentHash.toHex.take(16)}...")
      case Some(parentState) =>
        // Derive this block's ancestry per-chain by walking parent links
        // from its parent, so fork mutations do not pollute a shared
        // global ancestry list (header.tex eq:ancestors).
        val ancestry = stateStore.ancestryFor(parentHash).map(a =>
          AncestorHeader(a.slot.value.toLong & 0xffffffffL, a.headerHash)
        )
        blockImporter.importBlock(block, parentState, ancestry) match
          case ImportResult.Success(postStateRoot, _) =>
            val headerHash = Hashing.blake2b256(block.header.encode)
            val postState = blockImporter.materializePostState(config)
            val isOriginal = stateStore.isOriginalBlock(parentHash)
            // Record parentage for every imported block so any future
            // block can derive its own per-chain ancestry.
            stateStore.store(
              headerHash,
              postState,
              isOriginal,
              Some((parentHash, block.header.slot))
            )
            Right(postStateRoot)

          case ImportResult.Failure(error, message) =>
            Left(s"Import failed: $error - $message")
