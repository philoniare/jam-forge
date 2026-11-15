package io.forge.jam.protocol.benchmark

import io.circe.Decoder
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.trie.{InMemoryTrieBackend, StateTrieStore}
import io.forge.jam.core.types.block.Block.given
import io.forge.jam.protocol.TestFileLoader
import io.forge.jam.protocol.accumulation.AccumulationExecutor
import io.forge.jam.protocol.pipeline.BlockPipeline
import io.forge.jam.protocol.state.{ServiceStorageView, StateKey, TrieBackedJamState}
import io.forge.jam.protocol.traces.TraceStep

import java.util.Locale

/** Block import cost against the number of service accounts in the state.
  *
  * Replays a jamtestvectors trace on a state that holds the trace's own accounts plus `--services=N` more, which no
  * block of the trace touches. The extra accounts change the state root, so blocks are applied through
  * [[BlockPipeline]] (which does not check it) and the state is carried from block to block as an importer would.
  * With N = 0 every root is checked against the trace.
  *
  * {{{
  * sbt "protocol/Test/runMain io.forge.jam.protocol.benchmark.ServiceScaleBench --services=0,10000,100000"
  * }}}
  * Options: `--trace=storage`, `--steps=40`, `--reps=3` timed passes after one warm-up pass.
  */
object ServiceScaleBench:

  private val config: ChainConfig = ChainConfig.TINY
  private given Decoder[TraceStep] = TraceStep.decoder

  private def opt(args: Array[String], name: String, default: String): String =
    args.collectFirst { case a if a.startsWith(s"--$name=") => a.drop(name.length + 3) }.getOrElse(default)

  private def isAccountRecord(key: Array[Byte]): Boolean =
    (key(0) & 0xff) == 0xff && key(2) == 0 && key(4) == 0 && key(6) == 0 && (8 until key.length).forall(key(_) == 0)

  private def extraAccounts(keyvals: Seq[(JamBytes, JamBytes)], n: Int): Seq[(JamBytes, JamBytes)] =
    if n == 0 then Nil
    else
      val template = keyvals
        .collectFirst { case (k, v) if isAccountRecord(k.toArray) => v }
        .getOrElse(sys.error("the trace's state holds no service account"))
      (0 until n).map(i => StateKey.computeServiceAccountKey(0x40000000L + i) -> template)

  private final case class Pass(millis: Array[Double], mismatches: Int, failures: Int)

  private def pass(steps: Vector[TraceStep], extra: Int): Pass =
    val store = new StateTrieStore(new InMemoryTrieBackend())
    val first = steps.head.preState.keyvals.map(kv => (kv.key, kv.value))
    store.bootstrap(first ++ extraAccounts(first, extra))
    val executor = new AccumulationExecutor(config)
    val millis = Array.newBuilder[Double]
    var mismatches = 0
    var failures = 0
    steps.foreach { step =>
      val parent = store.currentRoot
      val trie = store.at(parent)
      val storage = new ServiceStorageView(trie)
      val view = new TrieBackedJamState(trie, config, storage, Some(store))
      val t0 = System.nanoTime()
      val result = BlockPipeline.execute(step.block, view, skipAncestryValidation = true, sharedExecutor = Some(executor))
      if result.isRight then
        val updates = view.commitUpdates(holdStoreUpdates = true)
        updates ++= storage.commitUpdates()
        trie.update(updates.toIndexedSeq)
      val t1 = System.nanoTime()
      result match
        case Right(_) =>
          millis += (t1 - t0) / 1e6
          view.applyStoreUpdates()
          trie.save()
          store.commitChild(parent, trie.rootHash)
          if extra == 0 && trie.rootHash != step.postState.stateRoot then mismatches += 1
        case Left(_) => failures += 1
    }
    Pass(millis.result(), mismatches, failures)

  def main(args: Array[String]): Unit =
    Locale.setDefault(Locale.US)
    org.slf4j.LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME) match
      case logback: ch.qos.logback.classic.Logger => logback.setLevel(ch.qos.logback.classic.Level.OFF)
      case _                                      => ()
    val trace = opt(args, "trace", "storage")
    val maxSteps = opt(args, "steps", "40").toInt
    val reps = opt(args, "reps", "3").toInt
    val counts = opt(args, "services", "0,10000,100000").split(',').map(_.trim.toInt).toList

    val names = TestFileLoader.getTraceStepFilenames(trace).fold(e => sys.error(e), identity)
    val steps = names.take(maxSteps)
      .flatMap(n => TestFileLoader.loadJsonFromTestVectors[TraceStep](s"traces/$trace", n).toOption)
      .toVector
    println(s"trace $trace: ${steps.size} blocks, $reps timed passes per service count")

    counts.foreach { n =>
      pass(steps, n): Unit
      val passes = (1 to reps).map(_ => pass(steps, n))
      val all = passes.flatMap(_.millis).sorted.toArray
      def pct(p: Double): Double = all(math.min(all.length - 1, (p * all.length).toInt))
      val mean = all.sum / all.length
      println(
        f"RESULT services extra=$n%d blocks=${all.length}%d mean=$mean%.3f p50=${pct(0.5)}%.3f p90=${pct(0.9)}%.3f " +
          f"max=${all.last}%.3f ms mismatches=${passes.map(_.mismatches).sum}%d failures=${passes.map(_.failures).sum}%d"
      )
    }
