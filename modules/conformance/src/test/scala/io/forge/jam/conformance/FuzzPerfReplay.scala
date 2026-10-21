package io.forge.jam.conformance

import io.forge.jam.core.ChainConfig

import java.nio.file.{Files, Path, Paths}
import java.util.Locale

object FuzzPerfReplay:
  private final case class Args(
    socket: Path = Paths.get("/tmp/jam_fuzz/fuzz.sock"),
    trace: Option[Path] = None,
    config: ChainConfig = ChainConfig.TINY,
    out: Option[Path] = None,
    waitSecs: Int = 120
  )

  private def parse(args: List[String], acc: Args = Args()): Args = args match
    case "--socket" :: p :: rest    => parse(rest, acc.copy(socket = Paths.get(p)))
    case "--trace" :: p :: rest     => parse(rest, acc.copy(trace = Some(Paths.get(p))))
    case "--spec" :: name :: rest =>
      parse(rest, acc.copy(config = ChainConfig.fromName(name).fold(e => throw new IllegalArgumentException(e), identity)))
    case "--out" :: p :: rest       => parse(rest, acc.copy(out = Some(Paths.get(p))))
    case "--wait-secs" :: n :: rest => parse(rest, acc.copy(waitSecs = n.toInt))
    case Nil                        => acc
    case other :: _                 => throw new IllegalArgumentException(s"Unknown argument: $other")

  private def percentile(sorted: Array[Double], p: Double): Double =
    sorted(math.min(sorted.length - 1, math.ceil(p / 100.0 * sorted.length).toInt - 1).max(0))

  def main(argv: Array[String]): Unit =
    Locale.setDefault(Locale.US)
    val args = parse(argv.toList)
    val traceDir = args.trace.getOrElse(throw new IllegalArgumentException("--trace <dir> is required"))
    val config = args.config
    val trace = FuzzClient.TraceDir.find(traceDir).getOrElse(throw new IllegalArgumentException(s"No genesis.bin in $traceDir"))
    val genesis = trace.readGenesis(config)

    val client = FuzzClient.connect(args.socket, config, args.waitSecs)
    val target = client.request(FuzzClient.fuzzerPeerInfo("jam-forge-fuzz-perf")) match
      case ProtocolMessage.PeerInfoMsg(info) => info
      case other => throw new RuntimeException(s"Expected peer_info, got $other")

    client.request(ProtocolMessage.InitializeMsg(Initialize(genesis.header, genesis.state.keyvals, Nil))) match
      case ProtocolMessage.StateRootMsg(root) if root.hash == genesis.state.stateRoot => ()
      case other => throw new RuntimeException(s"Initialize: unexpected reply $other")

    val times = Array.newBuilder[Double]
    var mismatches = 0
    var maxStep = ""
    var maxMs = -1.0
    for file <- trace.steps do
      val step = trace.readStep(file, config)
      val msg = ProtocolMessage.encodeWithFrame(ProtocolMessage.ImportBlockMsg(ImportBlock(step.block)), config)
      val t0 = System.nanoTime()
      client.sendRaw(msg)
      val reply = client.readReply()
      val ms = (System.nanoTime() - t0) / 1e6
      reply match
        case ProtocolMessage.StateRootMsg(root) if root.hash == step.postState.stateRoot => ()
        case other =>
          mismatches += 1
          System.err.println(s"${file.getFileName}: unexpected reply $other")
      times += ms
      if ms > maxMs then
        maxMs = ms
        maxStep = file.getFileName.toString.stripSuffix(".bin")
    client.close()

    val all = times.result()
    val sorted = all.sorted
    val mean = all.sum / all.length
    val stdDev = math.sqrt(all.map(t => (t - mean) * (t - mean)).sum / all.length)
    val name = traceDir.getFileName.toString
    val app = s"${target.appName} ${target.appVersion.major}.${target.appVersion.minor}.${target.appVersion.patch}"
    val stats = Vector(
      "import_min" -> sorted.head, "import_max" -> sorted.last, "import_mean" -> mean,
      "import_p50" -> percentile(sorted, 50), "import_p75" -> percentile(sorted, 75),
      "import_p90" -> percentile(sorted, 90), "import_p99" -> percentile(sorted, 99), "import_std_dev" -> stdDev
    )
    stats.foreach((k, v) => println(f"RESULT $name $k $v%.3f ms"))
    println(s"RESULT $name steps ${all.length} mismatches $mismatches target $app")

    args.out.foreach { out =>
      val fields = stats.map((k, v) => f"\"$k\": $v%.6f").mkString(", ")
      Files.writeString(out,
        s"""{"info": {"app_name": "${target.appName}", "app_version": "${target.appVersion.major}.${target.appVersion.minor}.${target.appVersion.patch}"}, "stats": {"steps": ${all.length}, "imported": ${all.length - mismatches}, "import_max_step": "$maxStep", $fields}}
           |""".stripMargin)
    }
    if mismatches > 0 then sys.exit(1)
