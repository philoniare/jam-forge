package io.forge.jam.conformance.bench

import io.forge.jam.conformance.{FuzzClient, ImportBlock, ImportBlockStep, ProtocolMessage, StateStore}
import io.forge.jam.core.{ChainConfig, Hashing, JamBytes}
import io.forge.jam.core.primitives.{Hash, Timeslot}
import io.forge.jam.core.types.block.Block
import io.forge.jam.core.types.header.Header
import io.forge.jam.protocol.traces.{BlockImporter, RawState}

import java.lang.management.ManagementFactory
import java.nio.file.{Files, Path, Paths}
import java.util.Locale
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** Block-import benchmark and heap soak over binary traces (`NNNNNNNN.bin`, with or without `genesis.bin`).
  *
  * Every block goes through what the conformance target does for an ImportBlock message: decode the framed message,
  * [[ImportBlockStep]] against a per-session [[StateStore]] and [[BlockImporter]], and wait for the post-state to be
  * read out of the trie. Each replay of a trace is a fresh session, as after an Initialize. Every resulting state
  * root is compared with the fixture, so a change that breaks results fails the run.
  *
  *   - `--mode=bench` (default): `--warmup` untimed passes over all traces, then `--reps` timed ones. Reports wall
  *     time per block (`import`) and CPU time of the importing thread per block (`import-cpu`).
  *   - `--mode=soak`: replay until `--imports` blocks are imported, sampling used heap after a forced GC every
  *     `--sample-every` imports.
  *
  * Commands and options are in BENCHMARKS.md ("Block import bench and heap soak").
  */
object ImportBench:

  private val DefaultTraces = "jam-conformance/fuzz-reports/0.8.0/traces"

  /** One trace, reduced to what a replay needs: the seed state and, per block, the framed message and both roots. */
  private final case class Step(name: String, frame: Array[Byte], preRoot: Hash, postRoot: Hash)
  private final case class Trace(
    name: String,
    seedHash: Hash,
    seedState: RawState,
    seedParent: Option[(Hash, Timeslot)],
    steps: Vector[Step]
  )

  private final class Session(config: ChainConfig):
    val store = new StateStore(config)
    // What the target negotiates with a fuzzer that offers forks but no ancestry (FuzzClient.fuzzerPeerInfo).
    store.setForksEnabled(true)
    val importer = new BlockImporter(config, skipAncestryValidation = true)

    def start(t: Trace): Unit = store.initialize(t.seedHash, t.seedState, Nil, t.seedParent)

    def decode(frame: Array[Byte]): Block =
      ProtocolMessage.decodeMessage(JamBytes.wrap(frame), 4, config)._1 match
        case ProtocolMessage.ImportBlockMsg(m) => m.block
        case other => throw new IllegalStateException(s"not an ImportBlock frame: $other")

    def importBlock(block: Block): Either[String, Hash] =
      val r = ImportBlockStep(store, importer, block)(using config)
      store.awaitPending()
      r

  def main(argv: Array[String]): Unit =
    Locale.setDefault(Locale.US)
    val opts = argv.map(_.stripPrefix("--").split("=", 2)).map {
      case Array(k, v) => k -> v
      case Array(k) => k -> "true"
    }.toMap
    val known = Set("mode", "spec", "traces", "warmup", "reps", "per-trace", "imports", "sample-every", "mutate",
      "max-growth-mb", "csv")
    opts.keys.find(!known(_)).foreach(k => fail(s"unknown option --$k (known: ${known.toSeq.sorted.mkString(", ")})"))
    quietLogging()

    val config = ChainConfig.fromName(opts.getOrElse("spec", "tiny")).fold(fail, identity)
    val baseDir = Paths.get(sys.props.getOrElse("jam.base.dir", System.getProperty("user.dir")))
    val roots = opts.getOrElse("traces", DefaultTraces).split(",").toVector.map(p => baseDir.resolve(p.trim))
    val traces = roots.flatMap(loadTraces(_, config))
    if traces.isEmpty then fail(s"no usable traces under ${roots.mkString(", ")}")
    val rt = Runtime.getRuntime
    val gcs = ManagementFactory.getGarbageCollectorMXBeans.asScala.map(_.getName).mkString("+")
    println(
      s"ImportBench spec=${ChainConfig.nameOf(config)} traces=${traces.size} blocks=${traces.map(_.steps.size).sum} " +
        s"(${traces.map(_.steps.count(s => s.postRoot == s.preRoot)).sum} of them rejected by the fixture) " +
        s"jvm=${Runtime.version()} gc=$gcs maxHeapMb=${rt.maxMemory() >> 20} cpus=${rt.availableProcessors()}"
    )

    val ok = opts.getOrElse("mode", "bench") match
      case "bench" =>
        bench(config, traces, opts.get("warmup").fold(2)(_.toInt), opts.get("reps").fold(5)(_.toInt), opts.contains("per-trace"))
      case "soak" =>
        soak(
          config,
          traces,
          imports = opts.get("imports").fold(20000L)(_.toLong),
          sampleEvery = opts.get("sample-every").fold(1000L)(_.toLong),
          mutate = opts.contains("mutate"),
          maxGrowthMb = opts.get("max-growth-mb").fold(2.0)(_.toDouble),
          csv = opts.get("csv").map(Paths.get(_))
        )
      case other => fail(s"unknown --mode=$other (bench|soak)")
    sys.exit(if ok then 0 else 1)

  // ── bench ──────────────────────────────────────────────────────────────────────────────────────────────────────

  private def bench(config: ChainConfig, traces: Vector[Trace], warmup: Int, reps: Int, perTrace: Boolean): Boolean =
    val times = traces.map(t => t.name -> ArrayBuffer.empty[Double]).toMap
    // CPU time of the importing thread only: work handed to other threads is not in it, but it is much steadier
    // than wall time on a loaded machine.
    val cpuTimes = traces.map(t => t.name -> ArrayBuffer.empty[Double]).toMap
    val threads = ManagementFactory.getThreadMXBean
    var mismatches = 0
    val t0 = System.nanoTime()
    for pass <- 0 until warmup + reps; t <- traces do
      val session = new Session(config)
      session.start(t)
      for s <- t.steps do
        val cpuStart = threads.getCurrentThreadCpuTime
        val start = System.nanoTime()
        val result = session.importBlock(session.decode(s.frame))
        val ms = (System.nanoTime() - start) / 1e6
        val cpuMs = (threads.getCurrentThreadCpuTime - cpuStart) / 1e6
        if pass >= warmup then
          times(t.name) += ms
          cpuTimes(t.name) += cpuMs
        if !matches(result, s) then
          mismatches += 1
          if mismatches <= 20 then System.err.println(s"MISMATCH ${t.name}/${s.name} pass=$pass: ${describe(result, s)}")
    val total = (System.nanoTime() - t0) / 1e9

    val blocks = traces.map(_.steps.size).sum
    if perTrace then
      traces.foreach { t =>
        println(resultLine("import", t.name, t.steps.size, times(t.name).toArray))
        println(resultLine("import-cpu", t.name, t.steps.size, cpuTimes(t.name).toArray))
      }
    println(resultLine("import", "ALL", blocks, traces.flatMap(t => times(t.name)).toArray))
    println(resultLine("import-cpu", "ALL", blocks, traces.flatMap(t => cpuTimes(t.name)).toArray))
    println(f"TOTAL $total%.1fs warmup=$warmup reps=$reps mismatches=$mismatches")
    if mismatches > 0 then System.err.println(s"FAILED: $mismatches state-root mismatches against the fixtures")
    mismatches == 0

  private def resultLine(metric: String, name: String, blocks: Int, ms: Array[Double]): String =
    val a = ms.sorted
    if a.isEmpty then s"RESULT $metric $name blocks=$blocks n=0"
    else
      def pct(p: Double) = a(math.min(a.length - 1, math.ceil(p / 100 * a.length).toInt - 1).max(0))
      f"RESULT $metric $name blocks=$blocks n=${a.length} mean=${a.sum / a.length}%.3f p50=${pct(50)}%.3f " +
        f"p90=${pct(90)}%.3f p99=${pct(99)}%.3f max=${a.last}%.3f ms"

  // ── soak ───────────────────────────────────────────────────────────────────────────────────────────────────────

  private def soak(
    config: ChainConfig,
    traces: Vector[Trace],
    imports: Long,
    sampleEvery: Long,
    mutate: Boolean,
    maxGrowthMb: Double,
    csv: Option[Path]
  ): Boolean =
    val rows = ArrayBuffer.empty[(Long, Double)]
    val out = new StringBuilder
    def emit(line: String): Unit = { println(line); out.append(line).append('\n') }
    val t0 = System.nanoTime()
    var done = 0L
    var sessions = 0L
    var mismatches = 0
    var nextSample = 0L
    def sample(): Unit =
      if done >= nextSample then
        val mb = heapAfterGcMb()
        rows += ((done, mb))
        emit(f"$done,${(System.nanoTime() - t0) / 1e9}%.1f,$mb%.2f,$sessions")
        nextSample = done + sampleEvery

    emit("imports,elapsed_s,heap_after_gc_mb,sessions")
    sample()
    while done < imports do
      for t <- traces if done < imports do
        val session = new Session(config)
        session.start(t)
        sessions += 1
        var i = 0
        while i < t.steps.length && done < imports do
          val s = t.steps(i)
          val block = session.decode(s.frame)
          val result = session.importBlock(block)
          done += 1
          if !matches(result, s) then
            mismatches += 1
            if mismatches <= 20 then System.err.println(s"MISMATCH ${t.name}/${s.name}: ${describe(result, s)}")
          if mutate then
            // What a fuzzer does between original blocks; results are not checked. A sibling with a corrupted seal
            // (rejected), and every fifth block a fork: the previous block again, then this one again.
            if i % 3 == 1 then { session.importBlock(corruptSeal(block)); done += 1 }
            if i % 5 == 2 then
              session.importBlock(session.decode(t.steps(i - 1).frame))
              session.importBlock(block)
              done += 2
          sample()
          i += 1
    nextSample = 0
    sample()
    csv.foreach(Files.writeString(_, out.toString))

    // Growth is the least-squares slope over the second half of the samples, so JIT and cache fill do not count.
    val tail = if rows.size >= 6 then rows.drop(rows.size / 2) else rows
    val slope = slopePer10k(tail.toSeq)
    val verdict = if slope <= maxGrowthMb then "FLAT" else "GROWING"
    println(
      f"SOAK imports=$done sessions=$sessions samples=${rows.size} heap_first_mb=${rows.head._2}%.1f " +
        f"heap_last_mb=${rows.last._2}%.1f growth_mb_per_10k_imports=$slope%.3f (limit $maxGrowthMb%.1f) " +
        f"mismatches=$mismatches verdict=$verdict"
    )
    if mismatches > 0 then System.err.println(s"FAILED: $mismatches state-root mismatches against the fixtures")
    mismatches == 0 && verdict == "FLAT"

  private def slopePer10k(rows: Seq[(Long, Double)]): Double =
    if rows.size < 2 then 0.0
    else
      val mx = rows.map(_._1.toDouble).sum / rows.size
      val my = rows.map(_._2).sum / rows.size
      val den = rows.map(r => (r._1 - mx) * (r._1 - mx)).sum
      if den == 0 then 0.0 else rows.map(r => (r._1 - mx) * (r._2 - my)).sum / den * 10000

  private def heapAfterGcMb(): Double =
    val mem = ManagementFactory.getMemoryMXBean
    mem.gc(); mem.gc()
    mem.getHeapMemoryUsage.getUsed / 1048576.0

  private def corruptSeal(block: Block): Block =
    val seal = block.header.seal.toArray.clone()
    seal(5) = (seal(5) ^ 0x5a).toByte
    block.copy(header = block.header.copy(seal = JamBytes(seal)))

  // ── shared ─────────────────────────────────────────────────────────────────────────────────────────────────────

  /** A rejected block must be one the fixture also leaves the state unchanged for. */
  private def matches(result: Either[String, Hash], s: Step): Boolean = result match
    case Right(root) => root == s.postRoot
    case Left(_) => s.postRoot == s.preRoot

  private def describe(result: Either[String, Hash], s: Step): String = result match
    case Right(root) => s"root ${root.toHex}, fixture ${s.postRoot.toHex}"
    case Left(err) => s"rejected ($err), fixture root ${s.postRoot.toHex}"

  /** `dir` itself if it holds step files, else each subdirectory that does, by name. */
  private def loadTraces(dir: Path, config: ChainConfig): Vector[Trace] =
    if !Files.isDirectory(dir) then fail(s"trace directory not found: $dir")
    def stepFiles(d: Path): Vector[Path] =
      Using.resource(Files.list(d))(_.iterator().asScala.filter(_.getFileName.toString.matches("\\d+\\.bin")).toVector)
        .sortBy(_.getFileName.toString)
    val own = stepFiles(dir)
    val dirs =
      if own.nonEmpty then Vector(dir)
      else Using.resource(Files.list(dir))(_.iterator().asScala.filter(Files.isDirectory(_)).toVector).sortBy(_.getFileName.toString)
    dirs.flatMap { d =>
      val files = if d == dir then own else stepFiles(d)
      if files.isEmpty then None
      else
        val name = d.getFileName.toString
        val reader = FuzzClient.TraceDir(d.resolve("genesis.bin"), files)
        var first: Option[io.forge.jam.protocol.traces.TraceStep] = None
        val steps = files.map { f =>
          val step = reader.readStep(f, config)
          if first.isEmpty then first = Some(step)
          val frame = ProtocolMessage.encodeWithFrame(ProtocolMessage.ImportBlockMsg(ImportBlock(step.block)), config)
          Step(f.getFileName.toString.stripSuffix(".bin"), frame, step.preState.stateRoot, step.postState.stateRoot)
        }
        if Files.isRegularFile(reader.genesis) then
          val g = reader.readGenesis(config)
          val hash = Hashing.blake2b256(Header.headerCodec(config.epochLength).encode(g.header))
          Some(Trace(name, hash, g.state, Some((g.header.parent, g.header.slot)), steps))
        else
          // No genesis: start from the first step's pre-state, keyed by the hash its block names as parent.
          val f0 = first.get
          if f0.preState.keyvals.isEmpty then
            System.err.println(s"skipping $name: no genesis.bin and the first step carries no pre-state")
            None
          else Some(Trace(name, f0.block.header.parent, f0.preState, None, steps))
    }

  /** The default logback.xml logs at DEBUG; a benchmark wants ERROR unless LOG_LEVEL says otherwise. */
  private def quietLogging(): Unit =
    if !sys.env.contains("LOG_LEVEL") then
      org.slf4j.LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME) match
        case l: ch.qos.logback.classic.Logger => l.setLevel(ch.qos.logback.classic.Level.ERROR)
        case _ => ()

  private def fail(msg: String): Nothing =
    System.err.println(s"ImportBench: $msg")
    sys.exit(2)
