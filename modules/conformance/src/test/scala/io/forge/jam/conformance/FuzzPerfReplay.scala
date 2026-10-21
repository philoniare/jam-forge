package io.forge.jam.conformance

import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.codec.JamCodecs
import io.forge.jam.protocol.traces.{Genesis, TraceStep}
import io.forge.jam.conformance.ConformanceCodecs.encode
import spire.math.{UByte, UInt}
import io.forge.jam.core.codec.Codec

import java.net.{StandardProtocolFamily, UnixDomainSocketAddress}
import java.nio.ByteBuffer
import java.nio.channels.SocketChannel
import java.nio.file.{Files, Path, Paths}
import java.util.Locale
import scala.jdk.CollectionConverters.*

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
    case "--spec" :: "full" :: rest => parse(rest, acc.copy(config = ChainConfig.FULL))
    case "--spec" :: "tiny" :: rest => parse(rest, acc.copy(config = ChainConfig.TINY))
    case "--out" :: p :: rest       => parse(rest, acc.copy(out = Some(Paths.get(p))))
    case "--wait-secs" :: n :: rest => parse(rest, acc.copy(waitSecs = n.toInt))
    case Nil                        => acc
    case other :: _                 => throw new IllegalArgumentException(s"Unknown argument: $other")

  private def decodeFile[A](path: Path, codec: Codec[A]): A =
    codec.decodeUnsafe(Files.readAllBytes(path))

  /** Fuzzer-to-target messages; jam-forge's ProtocolMessage only encodes the target-to-fuzzer direction. */
  private def frame(discriminant: Int, body: JamBytes): Array[Byte] =
    val payload = JamBytes(Array(discriminant.toByte)) ++ body
    (JamBytes(JamCodecs.encodeU32LE(UInt(payload.length))) ++ payload).toArray

  private def connect(socket: Path, waitSecs: Int): SocketChannel =
    val deadline = System.nanoTime() + waitSecs * 1_000_000_000L
    var channel: SocketChannel = null
    while channel == null do
      try
        val c = SocketChannel.open(StandardProtocolFamily.UNIX)
        c.connect(UnixDomainSocketAddress.of(socket))
        channel = c
      catch
        case e: java.io.IOException =>
          if System.nanoTime() > deadline then throw new RuntimeException(s"No target on $socket", e)
          Thread.sleep(100)
    channel

  private def readFully(channel: SocketChannel, n: Int): Array[Byte] =
    val buf = ByteBuffer.allocate(n)
    while buf.hasRemaining do
      if channel.read(buf) < 0 then throw new RuntimeException("Target closed the connection")
    buf.array()

  private def roundTrip(channel: SocketChannel, msg: Array[Byte], config: ChainConfig): ProtocolMessage =
    val out = ByteBuffer.wrap(msg)
    while out.hasRemaining do channel.write(out)
    val len = ByteBuffer.wrap(readFully(channel, 4)).order(java.nio.ByteOrder.LITTLE_ENDIAN).getInt
    ProtocolMessage.decodeMessage(JamBytes(readFully(channel, len)), 0, config)._1

  private def percentile(sorted: Array[Double], p: Double): Double =
    sorted(math.min(sorted.length - 1, math.ceil(p / 100.0 * sorted.length).toInt - 1).max(0))

  def main(argv: Array[String]): Unit =
    Locale.setDefault(Locale.US)
    val args = parse(argv.toList)
    val traceDir = args.trace.getOrElse(throw new IllegalArgumentException("--trace <dir> is required"))
    val config = args.config
    val genesis = decodeFile(traceDir.resolve("genesis.bin"), Genesis.codec(config.validatorCount, config.epochLength))
    val stepCodec = TraceStep.codec(config.validatorCount, config.epochLength, config.coresCount, config.votesPerVerdict)
    val stepFiles = Files.list(traceDir).iterator().asScala
      .filter(p => p.getFileName.toString.matches("\\d+\\.bin"))
      .toVector.sortBy(_.getFileName.toString)

    val channel = connect(args.socket, args.waitSecs)
    // Same handshake fuzz-perf's fuzzer sends: forks feature only, no ancestry.
    val peerInfo = PeerInfo(UByte(1), UInt(Features.FORKS), Version.JAM_VERSION, Version(UByte(0), UByte(1), UByte(0)),
      "jam-forge-fuzz-perf")
    val target = roundTrip(channel, frame(PeerInfo.DISCRIMINANT, peerInfo.encode), config) match
      case ProtocolMessage.PeerInfoMsg(info) => info
      case other => throw new RuntimeException(s"Expected peer_info, got $other")

    given Codec[Initialize] = Initialize.codec(config)
    val init = Initialize(genesis.header, genesis.state.keyvals, Nil)
    roundTrip(channel, frame(Initialize.DISCRIMINANT, init.encode), config) match
      case ProtocolMessage.StateRootMsg(root) if root.hash == genesis.state.stateRoot => ()
      case other => throw new RuntimeException(s"Initialize: unexpected reply $other")

    given Codec[ImportBlock] = ImportBlock.codec(config)
    val times = Array.newBuilder[Double]
    var mismatches = 0
    var maxStep = ""
    var maxMs = -1.0
    for file <- stepFiles do
      val step = decodeFile(file, stepCodec)
      val msg = frame(ImportBlock.DISCRIMINANT, ImportBlock(step.block).encode)
      val t0 = System.nanoTime()
      val reply = roundTrip(channel, msg, config)
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
    channel.close()

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
