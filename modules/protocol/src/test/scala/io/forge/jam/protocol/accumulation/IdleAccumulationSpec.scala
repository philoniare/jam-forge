package io.forge.jam.protocol.accumulation

import io.circe.Decoder
import io.forge.jam.core.ChainConfig
import io.forge.jam.core.trie.{InMemoryTrieBackend, StateTrieStore}
import io.forge.jam.core.types.block.Block.given
import io.forge.jam.protocol.TestFileLoader
import io.forge.jam.protocol.state.TrieBackedJamState
import io.forge.jam.protocol.traces.TraceStep
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/**
 * The shortcut for a block with nothing to accumulate must leave the same state and
 * give the same output as the full accumulation STF. Compared on the pre-states of the public traces.
 */
class IdleAccumulationSpec extends AnyFunSuite with Matchers:

  private val config = ChainConfig.TINY
  private given Decoder[TraceStep] = TraceStep.decoder

  test("GP: the idle shortcut matches full accumulation when nothing is accumulated (eq. 12.17)") {
    var idleBlocks = 0
    for trace <- Seq("fallback", "safrole", "storage", "storage_light") do
      val names = TestFileLoader.getTraceStepFilenames(trace).fold(e => fail(e.toString), identity).take(20)
      for name <- names do
        val step =
          TestFileLoader.loadJsonFromTestVectors[TraceStep](s"traces/$trace", name).fold(e => fail(e.toString), identity)
        val store = new StateTrieStore(new InMemoryTrieBackend)
        val root = store.bootstrap(step.preState.keyvals.map(kv => (kv.key, kv.value)))
        val input = AccumulationInput(slot = step.block.header.slot.toLong, reports = Nil)

        def posterior(run: TrieBackedJamState => AccumulationOutput) =
          val trie = store.at(root)
          val view = TrieBackedJamState.at(store, root, config)
          val output = run(view)
          view.commit(trie)
          view.storage.commit(trie)
          (output, trie.rootHash)

        val prevSlot = TrieBackedJamState.at(store, root, config).timeslot
        val shortcut = TrieBackedJamState.at(store, root, config)
        AccumulationTransition.idle(input, shortcut, prevSlot).foreach { _ =>
          idleBlocks += 1
          val full = posterior(v => AccumulationTransition.accumulate(input, v, prevSlot, None))
          val fast = posterior(v => AccumulationTransition.idle(input, v, prevSlot).get)
          withClue(s"$trace/$name") {
            fast shouldBe full
          }
        }
    idleBlocks should be > 0
    info(s"$idleBlocks pre-states compared")
  }
