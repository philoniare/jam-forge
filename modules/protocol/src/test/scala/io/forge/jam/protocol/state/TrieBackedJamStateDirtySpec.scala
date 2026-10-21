package io.forge.jam.protocol.state

import io.circe.Decoder
import io.forge.jam.core.ChainConfig
import io.forge.jam.core.trie.{InMemoryTrieBackend, StateTrieStore}
import io.forge.jam.core.types.block.Block.given
import io.forge.jam.protocol.TestFileLoader
import io.forge.jam.protocol.state.TrieBackedJamStateBridges.*
import io.forge.jam.protocol.traces.TraceStep
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Writing back a component that did not change must not re-encode it at commit. */
class TrieBackedJamStateDirtySpec extends AnyFunSuite with Matchers:

  private val config = ChainConfig.TINY
  private given Decoder[TraceStep] = TraceStep.decoder

  private def freshView(): (TrieBackedJamState, StateTrieStore) =
    val name = TestFileLoader.getTraceStepFilenames("storage").fold(e => fail(e.toString), _.head)
    val step = TestFileLoader.loadJsonFromTestVectors[TraceStep]("traces/storage", name).fold(e => fail(e.toString), identity)
    val store = new StateTrieStore(new InMemoryTrieBackend)
    val root = store.bootstrap(step.preState.keyvals.map(kv => (kv.key, kv.value)))
    (TrieBackedJamState.at(store, root, config), store)

  test("bridges that hand back the pre-state mark nothing dirty") {
    val (view, _) = freshView()
    SafroleBridge.apply(view, SafroleBridge.extract(view))
    DisputeBridge.apply(view, DisputeBridge.extract(view))
    AssuranceBridge.apply(view, AssuranceBridge.extract(view))
    ReportBridge.apply(view, ReportBridge.extract(view))
    AuthorizationBridge.apply(view, AuthorizationBridge.extract(view))
    HistoryBridge.apply(view, HistoryBridge.extract(view))
    StatisticsBridge.apply(view, StatisticsBridge.extract(view))
    val acc = AccumulationBridge.extract(view)
    AccumulationBridge.apply(view, acc.state, acc.initStagingSet, acc.initStagingSet, acc.initAuthQueues, acc.initAuthQueues)
    view.timeslot = view.timeslot
    view.dirtyComponents shouldBe 0
  }

  test("an equal but rebuilt value marks nothing dirty") {
    val (view, _) = freshView()
    view.psi = view.psi.copy()
    view.validators.current = view.validators.current.map(identity)
    view.accumulation.privileges = view.accumulation.privileges.copy()
    view.dirtyComponents shouldBe 0
  }

  test("a changed value is committed") {
    val (view, store) = freshView()
    val tau = view.timeslot + 1
    view.timeslot = tau
    view.dirtyComponents shouldBe 1
    val trie = store.at(store.currentRoot)
    view.commit(trie)
    new TrieBackedJamState(trie, config, new ServiceStorageView(trie)).timeslot shouldBe tau
  }

  test("a component set before it is read is always committed") {
    val (view, store) = freshView()
    val reread = TrieBackedJamState.at(store, store.currentRoot, config)
    reread.timeslot = view.timeslot // same value, but this view never loaded it
    reread.dirtyComponents shouldBe 1
  }
