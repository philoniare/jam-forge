package io.forge.jam.protocol.importing

import io.circe.Decoder
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.trie.{BackendOp, InMemoryTrieBackend, MissingTrieNodeException, StateTrie, StateTrieBackend, StateTrieStore, TrieNode}
import io.forge.jam.protocol.TestFileLoader
import io.forge.jam.protocol.benchmark.{BlockGenerator, DevKeyStore}
import io.forge.jam.protocol.traces.Genesis
import io.forge.jam.protocol.state.RawState
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class MissingStateImportSpec extends AnyFunSuite with Matchers:

  private val config = ChainConfig.TINY

  private final class LossyBackend(inner: StateTrieBackend) extends StateTrieBackend:
    @volatile var hidden: Set[Hash] = Set.empty
    def readNode(hash: Hash): Option[TrieNode] = if hidden(hash) then None else inner.readNode(hash)
    def readRawValue(hash: Hash): Option[JamBytes] = if hidden(hash) then None else inner.readRawValue(hash)
    def batchUpdate(ops: Seq[BackendOp]): Unit = inner.batchUpdate(ops)
    def gc(): Unit = inner.gc()
    def clear(): Unit = inner.clear()

  test("a block on a state with a missing trie node fails to import, as a local fault, and is not judged invalid") {
    given Decoder[Genesis] = Genesis.decoder
    val genesis = TestFileLoader
      .loadJsonFromTestVectors[Genesis]("traces/fuzzy", "genesis")
      .fold(e => fail(s"cannot load genesis: $e"), _.state)
    val keyvals = genesis.keyvals.map(kv => (kv.key, kv.value))

    val author = new BlockImporter(config)
    author.store.bootstrap(keyvals)
    val block = new BlockGenerator(config, DevKeyStore.getAllTinyValidators(), author.store)
      .generateBlock(genesis.stateRoot, slot = 1)
    author.importBlock(block, RawState(genesis.stateRoot, Nil)) shouldBe a[ImportResult.Success]

    val backend = new LossyBackend(new InMemoryTrieBackend)
    val store = new StateTrieStore(backend)
    store.bootstrap(keyvals) shouldBe genesis.stateRoot
    val root = StateTrie.at(backend, genesis.stateRoot)
    val rootNode = backend.readNode(genesis.stateRoot).getOrElse(fail("genesis root not stored"))
    val missing = Hash.wrapUnchecked(rootNode.left.toArrayUnsafe)
    missing should not be Hash.zero
    backend.hidden = Set(missing)
    a[MissingTrieNodeException] should be thrownBy root.getKeyValues(JamBytes.empty, 0)

    val importer = new BlockImporter(config, externalTrieStore = Some(store), gcAfterImport = false)
    importer.importBlock(block, RawState(genesis.stateRoot, Nil)) match
      case ImportResult.Failed(_, cause) =>
        cause shouldBe a[MissingTrieNodeException]
        cause.asInstanceOf[MissingTrieNodeException].hash shouldBe missing
      case other => fail(s"expected a local failure, got $other")
    store.currentRoot shouldBe genesis.stateRoot
  }
