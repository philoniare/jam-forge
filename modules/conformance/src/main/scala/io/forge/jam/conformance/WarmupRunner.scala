package io.forge.jam.conformance

import io.forge.jam.core.{ChainConfig, Hashing}
import io.forge.jam.core.primitives.Hash
import org.crac.{Context, Core, Resource}

import java.nio.file.{Files, Path, Paths}
import java.util.Comparator
import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.control.NonFatal

/** JIT warmup before the fuzz socket opens, plus the legacy CRaC checkpoint commands.  */
object WarmupRunner extends Resource:

  /** Passes over the warmup corpus unless `JAM_WARMUP_ROUNDS` says otherwise. */
  val DefaultRounds: Int = 2

  /** What a warmup run imported. */
  final case class Summary(traces: Int, imported: Int, mismatches: Int, millis: Long)

  /** Warm the JVM by replaying all available traces, take a CRaC checkpoint, and on restore launch the fuzz server.
    * Returns only after restore (the original process exits inside Core.checkpointRestore()).
    *
    * @param socketPath
    *   socket the restored server binds (must exist at restore time)
    * @return
    *   true if restore-path executed cleanly; false if checkpoint was unavailable
    */
  def warmupAndCheckpoint(socketPath: Path): Boolean =
    runWarmup(resolveTracesDir(baseDir), DefaultRounds)

    Core.getGlobalContext.register(WarmupRunner)

    println("\nTriggering CRaC checkpoint...")
    println(s"Post-restore the server will bind: $socketPath")

    try
      Core.checkpointRestore()
      // Resumes here on restore.
      println("Restored from CRaC checkpoint.")
      true
    catch
      // No CRaC JVM, no -XX:CRaCCheckpointTo, or CRIU refused (unprivileged container): the warmup already
      // happened in this process, so serve from it rather than dying before the socket opens.
      case e: Exception =>
        println(
          s"CRaC checkpoint not available (${e.getClass.getSimpleName}: ${e.getMessage}); " +
            "continuing without checkpoint (JIT is still warm in this process)."
        )
        false

  /** Warm the JIT before the fuzz server binds its socket, without a CRaC checkpoint. Used when the target is started
    * through the fuzz packaging env (`JAM_FUZZ`), which is how W3F's `target.py` launches every target. Skipped when
    * `JAM_WARMUP=0` or when no binary warmup traces are found, so a missing corpus never stops the server from
    * starting. `JAM_WARMUP_ROUNDS` sets the number of passes over the corpus.
    */
  def warmupIfEnabled(env: String => Option[String] = sys.env.get): Option[Summary] =
    if env("JAM_WARMUP").contains("0") then
      println("Warmup disabled (JAM_WARMUP=0).")
      None
    else
      val tracesDir = resolveTracesDir(baseDir, env)
      val rounds = env("JAM_WARMUP_ROUNDS").flatMap(_.toIntOption).filter(_ >= 0).getOrElse(DefaultRounds)
      if Files.isDirectory(tracesDir) then runWarmup(tracesDir, rounds)
      else
        println(s"No warmup traces at $tracesDir; starting cold.")
        None

  /** Standalone warmup for the legacy `warmup` CLI. Warms then checkpoints, then exits — does not start a server
    * post-restore.
    */
  def run(): Unit =
    runWarmup(resolveTracesDir(baseDir), DefaultRounds)
    Core.getGlobalContext.register(WarmupRunner)
    println("\nTriggering CRaC checkpoint...")
    try
      Core.checkpointRestore()
      println("Restored from checkpoint.")
    catch
      case e: UnsupportedOperationException =>
        println(s"CRaC checkpoint not available: ${e.getMessage}")
      case e: Exception =>
        println(s"Checkpoint failed: ${e.getMessage}")
        e.printStackTrace()
        sys.exit(1)

  /** The binary traces to warm on: `tracesDir` itself if it holds one, else each subdirectory that does, by name. */
  private[conformance] def findTraces(tracesDir: Path): Vector[FuzzClient.TraceDir] =
    FuzzClient.TraceDir.find(tracesDir) match
      case Some(trace) => Vector(trace)
      case None =>
        Using.resource(Files.list(tracesDir))(_.iterator().asScala.filter(Files.isDirectory(_)).toVector)
          .sortBy(_.getFileName.toString)
          .flatMap(FuzzClient.TraceDir.find)

  /** Replay every trace under `tracesDir` `rounds` times through a temporary fuzz server. Never throws: a warmup
    * problem is reported and the caller starts the real server anyway.
    */
  private[conformance] def runWarmup(tracesDir: Path, rounds: Int): Option[Summary] =
    println("=== Warmup ===")
    println(s"Traces directory: $tracesDir")
    try
      val traces = if Files.isDirectory(tracesDir) then findTraces(tracesDir) else Vector.empty
      if traces.isEmpty || rounds == 0 then
        println(s"No binary traces (genesis.bin + NNNNNNNN.bin) under $tracesDir; starting cold.")
        None
      else
        val summary = replayThroughSocket(traces, rounds)
        println(
          s"Warmup complete: ${summary.traces} trace replays, ${summary.imported} blocks in ${summary.millis}ms, " +
            s"${summary.mismatches} root mismatches"
        )
        if summary.imported == 0 then
          println("WARNING: No successful block imports during warmup; JIT may not be fully warmed.")
        Some(summary)
    catch
      case NonFatal(e) =>
        println(s"WARNING: warmup failed (${e.getClass.getSimpleName}: ${e.getMessage}); starting anyway.")
        None

  // The warmup corpus is TINY (the image ships jamtestvectors' preimages traces), whatever spec the real server uses;
  // the code paths compiled are the same.
  private val warmupConfig = ChainConfig.TINY

  private def replayThroughSocket(traces: Vector[FuzzClient.TraceDir], rounds: Int): Summary =
    val dir = Files.createTempDirectory("jam-warmup")
    val server = FuzzServer.start(dir.resolve("warmup.sock"), warmupConfig, FileLogger.noop)
    val start = System.nanoTime()
    var imported = 0
    var mismatches = 0
    try
      for _ <- 1 to rounds; trace <- traces do
        val (ok, bad) = replay(server.socketPath, trace)
        imported += ok
        mismatches += bad
    finally
      server.close()
      Using.resource(Files.walk(dir))(_.sorted(Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p)))
    Summary(traces.size * rounds, imported, mismatches, (System.nanoTime() - start) / 1_000_000)

  /** One fuzzer session: handshake, Initialize from the genesis, every step as ImportBlock, then a GetState for the
    * last imported block. Returns (blocks whose reply root matched, blocks that did not).
    */
  private def replay(socket: Path, trace: FuzzClient.TraceDir): (Int, Int) =
    Using.resource(FuzzClient.connect(socket, warmupConfig)) { client =>
      client.request(FuzzClient.fuzzerPeerInfo("jam-forge-warmup"))
      val genesis = trace.readGenesis(warmupConfig)
      client.request(ProtocolMessage.InitializeMsg(Initialize(genesis.header, genesis.state.keyvals, Nil)))
      var ok = 0
      var bad = 0
      var lastHeader = Option.empty[Hash]
      for step <- trace.steps do
        val traceStep = trace.readStep(step, warmupConfig)
        client.request(ProtocolMessage.ImportBlockMsg(ImportBlock(traceStep.block))) match
          case ProtocolMessage.StateRootMsg(root) if root.hash == traceStep.postState.stateRoot =>
            ok += 1
            lastHeader = Some(Hashing.blake2b256(io.forge.jam.core.types.header.Header.headerCodec(warmupConfig.epochLength).encode(traceStep.block.header)))
          case _ => bad += 1
      // A trace whose later blocks were rejected may have pruned that state; the server then ends the session.
      try lastHeader.foreach(h => client.request(ProtocolMessage.GetStateMsg(GetState(h))))
      catch case NonFatal(_) => ()
      (ok, bad)
    }

  private def baseDir: String = sys.props.getOrElse("jam.base.dir", System.getProperty("user.dir"))

  private def resolveTracesDir(baseDir: String, env: String => Option[String] = sys.env.get): Path =
    env("JAM_TRACES_DIR")
      .map(Paths.get(_))
      .getOrElse(
        Paths.get(baseDir, "jam-conformance", "fuzz-reports", "0.8.0", "traces")
      )

  override def beforeCheckpoint(context: Context[? <: Resource]): Unit =
    println("Preparing for CRaC checkpoint...")

  override def afterRestore(context: Context[? <: Resource]): Unit =
    println("CRaC restore: JVM resumed with warm JIT.")
