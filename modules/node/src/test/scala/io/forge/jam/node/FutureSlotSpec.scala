package io.forge.jam.node

import java.nio.file.{Files, Path, Paths}

import io.circe.Decoder
import io.circe.parser.decode
import io.forge.jam.core.ChainConfig
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.protocol.traces.Genesis
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** A block whose slot has not started is not yet valid. On the best chain as on a side branch it is refused
  * without being executed, stored or marked, and imports once the clock reaches its slot.
  */
class FutureSlotSpec extends AnyFunSuite with Matchers:

  private val baseDir = sys.props.get("jam.base.dir").map(Paths.get(_)).getOrElse(Paths.get("."))
  private val genesisPath = baseDir.resolve("jamtestvectors/traces/fuzzy/genesis.json")
  private def cleanup(dir: Path): Unit =
    Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(f => Files.deleteIfExists(f))

  test("a best-chain block ahead of our clock is refused for now, and imports when its slot comes") {
    given Decoder[Genesis] = Genesis.decoder
    val genesis = Option.when(Files.exists(genesisPath))(decode[Genesis](Files.readString(genesisPath)).toOption).flatten
      .getOrElse(cancel("dev genesis not available"))
    val spec = ChainSpec("future-slot", ChainConfig.TINY, Some(genesis.header.encode.toArray), None, genesis.state.keyvals, Nil)
    val dirs = Seq("jam-future-a", "jam-future-b").map(Files.createTempDirectory(_))
    var nodes: Seq[JamNode] = Nil
    try
      nodes = dirs.map(d => new JamNode(spec, NodeConfig(d, slotTicking = false)).start())
      val Seq(a, b) = nodes
      a.enableAuthoring((0 until 6).map(ValidatorKeySet.dev))
      val head = a.authorSlot(3).getOrElse(fail("slot 3 not authored")) // first block, on genesis
      val bytes = a.chain.blockStore.getBlock(head.hash).get

      @volatile var now = 1L
      b.chain.useWallClock(() => now)
      b.chain.importBlock(bytes) shouldBe Left(ChainError.FutureSlot(3L, 1L)) // 3 > 1 + tolerance
      b.chain.hasBlock(head.hash) shouldBe false
      b.chain.best.slot shouldBe 0L
      now = 2L // within the tolerance of one slot
      b.chain.importBlock(bytes).map(_.hash) shouldBe Right(head.hash)
      b.chain.best.stateRoot shouldBe head.stateRoot

      a.chain.nextBlockSlot shouldBe 4L
      b.chain.nextBlockSlot shouldBe 4L
      now = 3L
      b.chain.nextBlockSlot shouldBe 4L
      now = 9L
      b.chain.nextBlockSlot shouldBe 9L
    finally
      nodes.foreach(n => if n != null then n.shutdown())
      dirs.foreach(cleanup)
  }
