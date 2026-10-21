package io.forge.jam.protocol.report

import io.circe.Decoder
import io.forge.jam.core.ChainConfig
import io.forge.jam.core.trie.{InMemoryTrieBackend, StateTrieStore}
import io.forge.jam.core.types.block.Block.given
import io.forge.jam.protocol.TestFileLoader
import io.forge.jam.protocol.report.ReportTypes.{ReportInput, ReportOutput}
import io.forge.jam.protocol.state.TrieBackedJamState
import io.forge.jam.protocol.state.TrieBackedJamStateBridges.ReportBridge
import io.forge.jam.protocol.traces.TraceStep
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** The Reports shortcut for a block without guarantees must match the full STF on the same state. */
class EmptyGuaranteesSpec extends AnyFunSuite with Matchers:

  private val config = ChainConfig.TINY
  private given Decoder[TraceStep] = TraceStep.decoder

  test("an empty guarantees extrinsic gives the same posterior and output as the full Reports STF") {
    for trace <- Seq("fallback", "storage") do
      val names = TestFileLoader.getTraceStepFilenames(trace).fold(e => fail(e.toString), identity).take(10)
      for name <- names do
        val step =
          TestFileLoader.loadJsonFromTestVectors[TraceStep](s"traces/$trace", name).fold(e => fail(e.toString), identity)
        val store = new StateTrieStore(new InMemoryTrieBackend)
        val root = store.bootstrap(step.preState.keyvals.map(kv => (kv.key, kv.value)))
        val input = ReportInput(guarantees = Nil, slot = step.block.header.slot.value.toLong)

        def posterior(run: TrieBackedJamState => ReportOutput) =
          val trie = store.at(root)
          val view = TrieBackedJamState.at(store, root, config)
          val output = run(view)
          view.commit(trie)
          (output, trie.rootHash)

        val full = posterior { v =>
          val (post, output) = ReportTransition.stfInternal(input, ReportBridge.extract(v), config)
          ReportBridge.apply(v, post)
          output
        }
        withClue(s"$trace/$name")(posterior(ReportTransition.stfView(input, _)) shouldBe full)
  }
