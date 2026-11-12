package io.forge.jam.node

import java.nio.file.{Files, Path, Paths}

import io.circe.Decoder
import io.circe.parser.decode
import io.forge.jam.core.ChainConfig
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.protocol.safrole.SafroleTypes.TicketsOrKeys
import io.forge.jam.protocol.traces.Genesis
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** A validator that restarts in the middle of a ticket-sealed epoch still seals its slots: the tickets it made
  * before the restart are no longer in memory, but their ids follow from its keys.
  */
class RestartAuthoringSpec extends AnyFunSuite with Matchers:

  private val baseDir = sys.props.get("jam.base.dir").map(Paths.get(_)).getOrElse(Paths.get("."))
  private val genesisPath = baseDir.resolve("jamtestvectors/traces/fuzzy/genesis.json")

  private def cleanup(dir: Path): Unit =
    Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p))

  test("after a restart in a ticket-sealed epoch every slot is still authored") {
    if !Files.exists(genesisPath) then cancel("dev genesis not available")
    given Decoder[Genesis] = Genesis.decoder
    val genesis = decode[Genesis](Files.readString(genesisPath)).fold(e => fail(e.getMessage), identity)
    val spec = ChainSpec("restart-devnet", ChainConfig.TINY, Some(genesis.header.encode.toArray), None, genesis.state.keyvals, Nil)
    val keys = (0 until 6).map(ValidatorKeySet.dev)
    val dir = Files.createTempDirectory("jam-restart-author")
    var node: JamNode = null
    def open(): JamNode =
      val n = new JamNode(spec, NodeConfig(dataDir = dir, slotTicking = false))
      n.chain.initializeOrRestore(spec)
      n.enableAuthoring(keys)
      n
    try
      val epochLen = spec.config.epochLength // 12 (TINY)
      node = open()
      // Epoch 0: the tickets generated (18 candidates, at most 3 per block) fill the 12-entry accumulator before
      // the cutoff at phase 10.
      var ticketsIncluded = 0
      for slot <- 1 to 20 do
        withClue(s"slot $slot: ") { node.authorSlot(slot.toLong).isDefined shouldBe true }
        if slot < epochLen then
          val block = node.chain.decodeBlock(node.chain.blockStore.getBlock(node.chain.best.hash).get).toOption.get
          ticketsIncluded += block.extrinsic.tickets.size
      ticketsIncluded should be >= epochLen
      // The epoch now running is sealed by tickets made before the restart.
      node.chain.stateView().gamma.st shouldBe a[TicketsOrKeys.Tickets]
      // So is the next one: its accumulator is already full of tickets made before the restart.
      node.chain.stateView().gamma.a.size shouldBe epochLen
      node.shutdownStorageOnly()

      node = open()
      // The rest of epoch 1, then all of epoch 2.
      for slot <- 21 to 35 do withClue(s"slot $slot after the restart: ") { node.authorSlot(slot.toLong).isDefined shouldBe true }
      node.chain.stateView().gamma.st shouldBe a[TicketsOrKeys.Tickets]
      node.chain.best.slot shouldBe 35L
    finally
      if node != null then node.shutdownStorageOnly()
      cleanup(dir)
  }
