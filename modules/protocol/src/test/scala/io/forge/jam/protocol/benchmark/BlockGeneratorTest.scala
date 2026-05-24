package io.forge.jam.protocol.benchmark

import io.forge.jam.core.ChainConfig
import io.forge.jam.protocol.traces.{BlockImporter, ImportResult, RawState, Genesis}
import io.forge.jam.protocol.TestFileLoader
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import io.circe.Decoder
import io.forge.jam.core.types.block.Block.given

class BlockGeneratorTest extends AnyFunSpec with Matchers:

  val config: ChainConfig = ChainConfig.TINY

  private def loadGenesis(): RawState =
    given Decoder[Genesis] = Genesis.decoder
    val genesis = TestFileLoader
      .loadJsonFromTestVectors[Genesis]("traces/fuzzy", "genesis")
      .getOrElse(throw new RuntimeException("Cannot load genesis"))
    genesis.state

  private def setup(genesis: RawState): (BlockImporter, BlockGenerator) =
    val importer = new BlockImporter(config)
    val store = importer.store
    store.bootstrap(genesis.keyvals.map(kv => (kv.key, kv.value)))
    assert(store.currentRoot == genesis.stateRoot, "genesis bootstrap mismatch")
    val validators = DevKeyStore.getAllTinyValidators()
    val generator = new BlockGenerator(config, validators, store)
    (importer, generator)

  describe("BlockGenerator"):
    it("should generate a valid block 1 from genesis"):
      val genesis = loadGenesis()
      val (importer, generator) = setup(genesis)

      val block = generator.generateBlock(genesis.stateRoot, slot = 1)

      block.header.slot.value.toInt shouldBe 1
      block.header.parentStateRoot shouldBe genesis.stateRoot
      block.header.seal.length shouldBe 96
      block.header.entropySource.length shouldBe 96

      val result = importer.importBlock(block, RawState(genesis.stateRoot, Nil))
      result shouldBe a[ImportResult.Success]

    it("should chain multiple blocks"):
      val genesis = loadGenesis()
      val (importer, generator) = setup(genesis)

      var parentRoot = genesis.stateRoot
      for slot <- 1 to 25 do // 25 blocks = 2+ epochs
        val block = generator.generateBlock(parentRoot, slot.toLong)
        val result = importer.importBlock(block, RawState(parentRoot, Nil))
        result match
          case ImportResult.Success(_, _) =>
            parentRoot = importer.currentTrieRoot
          case ImportResult.Failure(err, msg) =>
            fail(s"Block $slot failed: $err - $msg")

      info(s"Successfully chained 25 blocks across 2+ epochs")
