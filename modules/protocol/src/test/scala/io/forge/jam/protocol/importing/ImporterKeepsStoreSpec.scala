package io.forge.jam.protocol.importing

import io.circe.Decoder
import io.forge.jam.core.ChainConfig
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.trie.{InMemoryTrieBackend, StateTrieStore}
import io.forge.jam.protocol.TestFileLoader
import io.forge.jam.protocol.benchmark.{BlockGenerator, DevKeyStore}
import io.forge.jam.protocol.state.RawState
import io.forge.jam.protocol.traces.Genesis
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ImporterKeepsStoreSpec extends AnyFunSuite with Matchers:

  private val config = ChainConfig.TINY

  private def genesisState =
    given Decoder[Genesis] = Genesis.decoder
    TestFileLoader
      .loadJsonFromTestVectors[Genesis]("traces/fuzzy", "genesis")
      .getOrElse(cancel("dev genesis not available"))
      .state

  private def bootstrapped(genesis: RawState): (StateTrieStore, InMemoryTrieBackend) =
    val backend = new InMemoryTrieBackend()
    val store = new StateTrieStore(backend)
    store.bootstrap(genesis.keyvals.map(kv => (kv.key, kv.value)))
    (store, backend)

  test("a block whose stored parent state is not the pinned root imports from that state, clearing nothing") {
    val genesis = genesisState
    val (store, backend) = bootstrapped(genesis)
    val importer = new BlockImporter(config, externalTrieStore = Some(store), gcAfterImport = false)
    val generator = new BlockGenerator(config, DevKeyStore.getAllTinyValidators(), store)

    val block = generator.generateBlock(genesis.stateRoot, slot = 1)
    val first = importer.importBlock(block, RawState(genesis.stateRoot, Nil))
    first shouldBe a[ImportResult.Success]
    val root1 = store.currentRoot
    root1 should not be genesis.stateRoot
    backend.readNode(root1).isDefined shouldBe true
    backend.readNode(genesis.stateRoot).isDefined shouldBe true

    val second = importer.importBlock(block, RawState(genesis.stateRoot, Nil))
    second shouldBe a[ImportResult.Success]
    second.asInstanceOf[ImportResult.Success].postStateRoot shouldBe root1
    store.currentRoot shouldBe root1
    backend.readNode(root1).isDefined shouldBe true
    backend.readNode(genesis.stateRoot).isDefined shouldBe true
    store.at(root1).read(genesis.keyvals.head.key) should not be empty
  }

  test("a block whose parent state is not in the store fails, not Invalid, and leaves the store as it was") {
    val genesis = genesisState

    val (other, _) = bootstrapped(genesis)
    val otherImporter = new BlockImporter(config, externalTrieStore = Some(other), gcAfterImport = false)
    val generator = new BlockGenerator(config, DevKeyStore.getAllTinyValidators(), other)
    val block1 = generator.generateBlock(genesis.stateRoot, slot = 1)
    otherImporter.importBlock(block1, RawState(genesis.stateRoot, Nil)) shouldBe a[ImportResult.Success]
    val root1 = other.currentRoot
    val block2 = generator.generateBlock(root1, slot = 2)
    block2.header.parentStateRoot shouldBe root1

    val (store, backend) = bootstrapped(genesis)
    val nodesBefore = backend.nodeCount
    val importer = new BlockImporter(config, externalTrieStore = Some(store), gcAfterImport = false)
    val result = importer.importBlock(block2, RawState(root1, Nil))
    result shouldBe a[ImportResult.Failed]
    store.currentRoot shouldBe genesis.stateRoot
    backend.nodeCount shouldBe nodesBefore
    backend.readNode(genesis.stateRoot).isDefined shouldBe true
    backend.readNode(root1).isDefined shouldBe false

    val emptyBackend = new InMemoryTrieBackend()
    val empty = new StateTrieStore(emptyBackend)
    val emptyImporter = new BlockImporter(config, externalTrieStore = Some(empty), gcAfterImport = false)
    emptyImporter.importBlock(block1, RawState(genesis.stateRoot, Nil)) shouldBe a[ImportResult.Failed]
    empty.currentRoot shouldBe Hash.zero
    emptyBackend.nodeCount shouldBe 0
  }
