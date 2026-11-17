package io.forge.jam.protocol.importing

import io.circe.Decoder
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.trie.{BackendOp, InMemoryTrieBackend, StateTrie, StateTrieBackend, StateTrieStore, TrieNode}
import io.forge.jam.protocol.TestFileLoader
import io.forge.jam.protocol.benchmark.{BlockGenerator, DevKeyStore}
import io.forge.jam.protocol.state.RawState
import io.forge.jam.protocol.traces.Genesis
import io.forge.jam.core.types.block.Block
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class FailedSaveCacheSpec extends AnyFunSuite with Matchers:

  private val config = ChainConfig.TINY

  private def genesisState =
    given Decoder[Genesis] = Genesis.decoder
    TestFileLoader
      .loadJsonFromTestVectors[Genesis]("traces/fuzzy", "genesis")
      .getOrElse(cancel("dev genesis not available"))
      .state

  private final class FailOnce(inner: InMemoryTrieBackend) extends StateTrieBackend:
    @volatile var failNext = false
    def readNode(hash: Hash): Option[TrieNode] = inner.readNode(hash)
    def readRawValue(hash: Hash): Option[JamBytes] = inner.readRawValue(hash)
    def batchUpdate(ops: Seq[BackendOp]): Unit =
      if failNext then
        failNext = false
        throw new java.io.IOException("injected save failure")
      inner.batchUpdate(ops)
    def gc(): Unit = inner.gc()
    def clear(): Unit = inner.clear()

  private def isAccountRecord(k: Array[Byte]): Boolean =
    k.length == 31 && (k(0) & 0xff) == 0xff && k(2) == 0 && k(4) == 0 && k(6) == 0 && (8 until 31).forall(k(_) == 0)

  private def accounts(backend: StateTrieBackend, root: Hash): Map[Long, JamBytes] =
    StateTrie.at(backend, root).getKeyValues(JamBytes(Array(0xff.toByte)), 8).iterator.collect {
      case (k, v) if isAccountRecord(k.toArray) =>
        val a = k.toArray
        ((a(1) & 0xffL) | ((a(3) & 0xffL) << 8) | ((a(5) & 0xffL) << 16) | ((a(7) & 0xffL) << 24)) -> v
    }.toMap

  private def cachesDescribePinnedState(store: StateTrieStore, backend: StateTrieBackend, ids: Iterable[Long]): Unit =
    val root = store.currentRoot
    val state = accounts(backend, root)
    store.cachedServiceIds(root).foreach(_ shouldBe state.keySet)
    (ids ++ state.keys).toSet.foreach { id =>
      withClue(s"service $id: ")(store.cachedServiceInfo(root, id).foreach(cached => Some(cached) shouldBe state.get(id)))
    }

  test("a block whose save fails, imported again, gives the post-state of an importer that never failed") {
    val genesis = genesisState
    val blockCount = 12

    val refBackend = new InMemoryTrieBackend()
    val refStore = new StateTrieStore(refBackend)
    refStore.bootstrap(genesis.keyvals.map(kv => (kv.key, kv.value)))
    val refImporter = new BlockImporter(config, externalTrieStore = Some(refStore), gcAfterImport = false)
    val generator = new BlockGenerator(config, DevKeyStore.getAllTinyValidators(), refStore)
    val chain = (1 to blockCount).map { slot =>
      val parent = refStore.currentRoot
      val block = generator.generateBlock(parent, slot.toLong)
      val result = refImporter.importBlock(block, RawState(parent, Nil))
      result shouldBe a[ImportResult.Success]
      (block, parent, result.asInstanceOf[ImportResult.Success].postStateRoot)
    }
    val allIds = chain.flatMap((_, _, post) => accounts(refBackend, post).keys).toSet

    val failing = chain.indexWhere((_, parent, post) => accounts(refBackend, parent) != accounts(refBackend, post))
    withClue("no block of the dev chain changes a service account: ") { failing should be >= 0 }

    val inner = new InMemoryTrieBackend()
    val backend = new FailOnce(inner)
    val store = new StateTrieStore(backend)
    store.bootstrap(genesis.keyvals.map(kv => (kv.key, kv.value)))
    val importer = new BlockImporter(config, externalTrieStore = Some(store), gcAfterImport = false)

    def importOk(block: Block, parent: Hash, post: Hash): Unit =
      val result = importer.importBlock(block, RawState(parent, Nil))
      result shouldBe a[ImportResult.Success]
      result.asInstanceOf[ImportResult.Success].postStateRoot shouldBe post
      store.currentRoot shouldBe post
      cachesDescribePinnedState(store, backend, allIds)

    chain.take(failing).foreach(importOk)

    val (block, parent, post) = chain(failing)
    backend.failNext = true
    importer.importBlock(block, RawState(parent, Nil)) shouldBe a[ImportResult.Failed]
    store.currentRoot shouldBe parent
    val before = accounts(refBackend, parent)
    val changed = accounts(refBackend, post).filter((id, v) => !before.get(id).contains(v))
    changed should not be empty
    changed.foreach { (id, _) =>
      withClue(s"service $id: ")(store.cachedServiceInfo(parent, id) shouldBe before.get(id))
    }
    cachesDescribePinnedState(store, backend, allIds)

    importOk(block, parent, post)
    chain.drop(failing + 1).foreach(importOk)
    store.currentRoot shouldBe refStore.currentRoot
  }
