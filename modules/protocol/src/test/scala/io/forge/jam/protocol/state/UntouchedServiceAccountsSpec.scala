package io.forge.jam.protocol.state

import io.circe.Decoder
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.trie.{InMemoryTrieBackend, StateTrieStore}
import io.forge.jam.core.types.block.Block.given
import io.forge.jam.protocol.TestFileLoader
import io.forge.jam.protocol.accumulation.AccumulationExecutor
import io.forge.jam.protocol.pipeline.BlockPipeline
import io.forge.jam.protocol.traces.TraceStep
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Importing a block must cost what the block touches, not what the state holds: service accounts no work report,
  * accumulation or host call names are never read.
  */
class UntouchedServiceAccountsSpec extends AnyFunSuite with Matchers:

  private val config = ChainConfig.TINY
  private given Decoder[TraceStep] = TraceStep.decoder

  private def isAccountRecord(key: Array[Byte]): Boolean =
    (key(0) & 0xff) == 0xff && key(2) == 0 && key(4) == 0 && key(6) == 0 && (8 until key.length).forall(key(_) == 0)

  private val extraIds = (0 until 50).map(0x40000000L + _)

  test("blocks with guarantees and accumulation leave the accounts they do not name unread") {
    if !TestFileLoader.canLocateTestVectors then cancel("jamtestvectors not available")
    val names = TestFileLoader.getTraceStepFilenames("storage").fold(e => fail(e), _.take(40))
    val steps = names.map(n => TestFileLoader.loadJsonFromTestVectors[TraceStep]("traces/storage", n).fold(e => fail(e), identity))

    val keyvals = steps.head.preState.keyvals.map(kv => (kv.key, kv.value))
    val template: JamBytes = keyvals.collectFirst { case (k, v) if isAccountRecord(k.toArray) => v }.get
    val store = new StateTrieStore(new InMemoryTrieBackend)
    store.bootstrap(keyvals ++ extraIds.map(id => StateKey.computeServiceAccountKey(id) -> template))
    val executor = new AccumulationExecutor(config)

    var guarantees = 0
    var accumulated = 0
    steps.foreach { step =>
      val parent = store.currentRoot
      val trie = store.at(parent)
      val storage = new ServiceStorageView(trie)
      val view = new TrieBackedJamState(trie, config, storage, Some(store))
      val result = BlockPipeline.execute(step.block, view, skipAncestryValidation = true, sharedExecutor = Some(executor))
      withClue(s"block at slot ${step.block.header.slot}: ") { result.isRight shouldBe true }
      guarantees += step.block.extrinsic.guarantees.size
      accumulated += result.toOption.get.accumulationStats.size
      val updates = view.commitUpdates()
      updates ++= storage.commitUpdates()
      trie.update(updates.toIndexedSeq)
      trie.save()
      store.commitChild(parent, trie.rootHash, view.accountRecordChanges)
    }
    guarantees should be > 0
    accumulated should be > 0

    extraIds.foreach(id => withClue(s"service $id: ")(store.cachedServiceInfo(store.currentRoot, id) shouldBe None))
    store.cachedServiceIds(store.currentRoot) shouldBe None

    // The state is the trace's own post-state plus the extra accounts, unchanged.
    val expected = steps.last.postState.keyvals ++ extraIds.map(id => KeyValue(StateKey.computeServiceAccountKey(id), template))
    store.currentRoot shouldBe StateMerklization.stateMerklize(expected)
  }
