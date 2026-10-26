package io.forge.jam.conformance

import io.forge.jam.core.{ChainConfig, Hashing, JamBytes}
import io.forge.jam.core.codec.JamCodecs.encode as jencode
import io.forge.jam.protocol.pipeline.ExtrinsicHashing
import io.forge.jam.protocol.traces.{BlockImporter, Genesis, RawState, StateMerklization, TraceStep}
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*

/** Ticket proofs are verified alongside the rest of the block import. A block carrying
  * a bad proof must still be rejected with BadTicketProof, ahead of the seal it invalidates, and the untampered block
  * then imports to the trace's root.
  */
class DeferredTicketProofSpec extends AnyFunSuite:
  test("GP: a block with a bad ticket proof is rejected with BadTicketProof (eq. 6.30)") {
    given config: ChainConfig = ChainConfig.TINY
    val dir = Paths.get("jamtestvectors/traces/safrole")
    val genesisCodec = Genesis.codec(config.epochLength)
    val genesis = genesisCodec.decodeUnsafe(Files.readAllBytes(dir.resolve("genesis.bin")))
    val stepCodec =
      TraceStep.codec(config.epochLength, config.coresCount)
    val files = Files.list(dir).iterator().asScala
      .filter(_.getFileName.toString.matches("\\d+\\.bin"))
      .toVector.sortBy(_.toString)
    val store = new StateStore(config)
    val importer = new BlockImporter(config, skipAncestryValidation = true)
    store.initialize(
      Hashing.blake2b256(genesis.header.jencode),
      RawState(StateMerklization.stateMerklize(genesis.state.keyvals), genesis.state.keyvals),
      Nil,
      Some((genesis.header.parent, genesis.header.slot))
    )
    var tampered = 0
    for file <- files do
      val step = stepCodec.decodeUnsafe(Files.readAllBytes(file))
      val block = step.block
      if block.extrinsic.tickets.nonEmpty && tampered < 3 then
        // Flip one byte of the ring proof, after the 32-byte output, so the ticket identifier is unchanged.
        val ticket = block.extrinsic.tickets.head
        val signature = ticket.signature.toArray.clone()
        signature(200) = (signature(200) ^ 1).toByte
        val extrinsic =
          block.extrinsic.copy(tickets = ticket.copy(signature = JamBytes(signature)) :: block.extrinsic.tickets.tail)
        val bad = block.copy(
          extrinsic = extrinsic,
          header = block.header.copy(extrinsicHash = ExtrinsicHashing.computeExtrinsicHash(extrinsic, config))
        )
        val result = ImportBlockStep(store, importer, bad)
        assert(result.left.exists(_.contains("BadTicketProof")), s"${file.getFileName}: $result")
        tampered += 1
      val imported = ImportBlockStep(store, importer, block)
      assert(imported == Right(step.postState.stateRoot), file.getFileName)
    assert(tampered == 3)
  }
