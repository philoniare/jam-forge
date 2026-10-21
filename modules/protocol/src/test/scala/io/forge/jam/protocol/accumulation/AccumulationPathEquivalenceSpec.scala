package io.forge.jam.protocol.accumulation

import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.trie.{InMemoryTrieBackend, StateTrie}
import io.forge.jam.protocol.TestFileLoader
import io.forge.jam.protocol.state.ServiceStorageView
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/**
 * The parallel accpar path (services of a batch run concurrently on forked storage views) must give the same posterior
 * as the sequential path: every service of a batch starts from the same state). Runs every
 * tiny accumulate vector through both, with the real PVM and a storage view installed as on block import.
 */
class AccumulationPathEquivalenceSpec extends AnyFunSuite with Matchers:

  private val config = ChainConfig.TINY
  private val folder = "stf/accumulate/tiny"

  private def runWith(testCase: AccumulationCase, parallel: Boolean) =
    val trie = StateTrie.empty(new InMemoryTrieBackend())
    val raw = testCase.preState.rawServiceDataByStateKey
    if raw.nonEmpty then trie.update(raw.toSeq.map((k, v) => (k, Some(v))))
    val view = new ServiceStorageView(trie)
    val executor = new AccumulationExecutor(config, parallelServices = parallel)
    executor.setStorageView(Some(view))
    val result = AccumulationTransition.stfInternal(
      testCase.input,
      testCase.preState,
      List.empty,
      List.empty,
      config,
      testCase.preState.slot,
      Some(executor)
    )
    val staged: Map[JamBytes, Option[JamBytes]] =
      view.pendingStateKeys.iterator.map(k => k -> view.getByStateKey(k)).toMap
    (result, staged)

  test("GP: parallel and sequential accumulation agree on every tiny accumulate vector (eq. 12.18)") {
    val names = TestFileLoader.getTestFilenamesFromTestVectors(folder).getOrElse(List.empty)
    names should not be empty
    var multiService = 0
    for name <- names do
      val testCase = TestFileLoader.loadJsonFromTestVectors[AccumulationCase](folder, name) match
        case Right(c)  => c
        case Left(err) => fail(s"$name: $err")
      val (seq, seqStaged) = runWith(testCase, parallel = false)
      val (par, parStaged) = runWith(testCase, parallel = true)
      withClue(name) {
        par.output shouldBe seq.output
        par.state shouldBe seq.state
        par.stagingSet shouldBe seq.stagingSet
        par.authQueues shouldBe seq.authQueues
        parStaged shouldBe seqStaged
      }
      if seq.output.toOption.exists(_.accumulationStats.size > 1) then multiService += 1
    info(s"${names.size} vectors, $multiService accumulate more than one service")
  }
