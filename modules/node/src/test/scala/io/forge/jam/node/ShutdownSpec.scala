package io.forge.jam.node

import io.forge.jam.core.ChainConfig
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.db.{BlockStore, RocksDbTrieBackend, ShardStore}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ShutdownSpec extends AnyFunSuite with Matchers with NodeFixture:

  test("a shutdown step that throws does not keep the later ones, or the closing of the stores, from running") {
    val genesis = loadGenesis().getOrElse(cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available"))
    val spec = ChainSpec(
      id = "shutdown",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )
    val dir = tempDir("jam-shutdown")
    try
      val node = new JamNode(spec, NodeConfig(dataDir = dir, slotTicking = false)).start()
      node.shutdownFault = () => throw new IllegalStateException("injected: a service failed to stop")
      noException should be thrownBy node.shutdown()
      node.poolsLeftRunning shouldBe Nil
      BlockStore.open(dir.resolve("blocks")).close()
      RocksDbTrieBackend.open(dir.resolve("state"), trackGarbage = false).close()
      ShardStore.open(dir.resolve("shards")).close()
    finally cleanup(dir)
  }
