package io.forge.jam.node.bench

import io.circe.Decoder
import io.circe.parser.decode
import io.forge.jam.core.{ChainConfig, Hashing, JamBytes}
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.primitives.{Gas, ServiceId, Timeslot}
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.workitem.WorkItem
import io.forge.jam.core.types.workpackage.WorkPackage
import io.forge.jam.network.StreamKind
import io.forge.jam.node.{ChainSpec, JamNode, NodeConfig, ValidatorKeySet, tinyChainConfig}
import io.forge.jam.protocol.traces.Genesis
import spire.math.UShort

import java.lang.management.ManagementFactory
import java.net.InetSocketAddress
import java.nio.file.{Files, Path, Paths}
import java.util.Locale
import java.util.concurrent.TimeUnit
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.control.NonFatal

/** Heap soak of two in-process nodes over JAMNP on loopback, each with its own RocksDB, on the tiny `fuzzy` genesis.
  *
  * Node A authors every slot with all six dev keys, assures and audits; node B guarantees and audits. Every
  * `--wp-every` slots a work package is submitted to B, so the guarantee, assurance, availability and audit paths
  * run. Slots are driven by hand (no wall-clock ticking); finality is the devnet rule at depth 5 with a quorum of 2.
  * Every `--sample-every` slots the heap is sampled after a forced GC, with the size of both data directories.
  *
  * Not exercised: GRANDPA voting, peer churn, reorgs, more than two nodes.
  *
  * Commands and options are in BENCHMARKS.md ("Block import bench and heap soak").
  */
object NodeSoak:

  def main(argv: Array[String]): Unit =
    Locale.setDefault(Locale.US)
    val opts = argv.map(_.stripPrefix("--").split("=", 2)).map {
      case Array(k, v) => k -> v
      case Array(k) => k -> "true"
    }.toMap
    val known = Set("slots", "sample-every", "wp-every", "data", "max-growth-mb", "csv")
    opts.keys.find(!known(_)).foreach { k =>
      System.err.println(s"NodeSoak: unknown option --$k (known: ${known.toSeq.sorted.mkString(", ")})")
      sys.exit(2)
    }
    if !sys.env.contains("LOG_LEVEL") then
      org.slf4j.LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME) match
        case l: ch.qos.logback.classic.Logger => l.setLevel(ch.qos.logback.classic.Level.ERROR)
        case _ => ()

    val slots = opts.get("slots").fold(1000L)(_.toLong)
    val sampleEvery = opts.get("sample-every").fold(100L)(_.toLong)
    val wpEvery = opts.get("wp-every").fold(4L)(_.toLong)
    val maxGrowthMb = opts.get("max-growth-mb").fold(2.0)(_.toDouble)
    val dataRoot = opts.get("data").map(Paths.get(_)).getOrElse(Files.createTempDirectory("jam-node-soak"))
    val baseDir = Paths.get(sys.props.getOrElse("jam.base.dir", System.getProperty("user.dir")))

    given Decoder[Genesis] = Genesis.decoder
    val genesis = decode[Genesis](Files.readString(baseDir.resolve("jamtestvectors/traces/fuzzy/genesis.json")))
      .fold(e => throw new IllegalStateException(s"fuzzy genesis: $e"), identity)
    val devKeys = (0 until 6).map(ValidatorKeySet.dev)
    // The null authorizer's code is the one 50-byte value in the fuzzy genesis.
    val authCodeHash = Hashing.blake2b256(genesis.state.keyvals.map(_.value.toArray).find(_.length == 50).get)
    val spec = ChainSpec(
      id = "soak-devnet",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil,
      devnetFinalityDepth = 5,
      devnetFinalityQuorum = Some(2)
    )
    val dirA = dataRoot.resolve("a")
    val dirB = dataRoot.resolve("b")
    Files.createDirectories(dirA)
    Files.createDirectories(dirB)
    val nodeA = new JamNode(spec, NodeConfig(dirA, slotTicking = false, ed25519Seed = Some(devKeys(0).ed25519Secret))).start()
    val nodeB = new JamNode(spec, NodeConfig(dirB, slotTicking = false, ed25519Seed = Some(devKeys(1).ed25519Secret))).start()
    nodeA.enableAuthoring(devKeys)
    nodeA.enableAssuring(devKeys)
    nodeA.enableAuditing(devKeys.take(1))
    nodeB.enableGuaranteeing(devKeys)
    nodeB.enableAuditing(devKeys.slice(1, 2))
    val connAtoB = nodeA.connectPeer(new InetSocketAddress("127.0.0.1", nodeB.listenPort))

    def awaitSync(): Unit =
      val deadline = System.currentTimeMillis() + 10000
      while nodeB.chain.best.hash != nodeA.chain.best.hash && System.currentTimeMillis() < deadline do Thread.sleep(5)

    def submitWorkPackage(slot: Long): Unit =
      val view = nodeB.chain.stateView()
      val anchor = view.beta.history(view.beta.history.size - 2)
      val serviceCodeHash = view.accumulation.serviceAccounts.find(_.id == 0).map(_.data.service.codeHash).get
      val wp = WorkPackage(
        authCodeHost = ServiceId(0),
        authCodeHash = authCodeHash,
        context = Context(
          anchor = anchor.headerHash,
          anchorSlot = Timeslot(anchor.slot.toInt),
          stateRoot = anchor.stateRoot,
          beefyRoot = anchor.beefyRoot,
          lookupAnchor = anchor.headerHash,
          lookupAnchorSlot = Timeslot(anchor.slot.toInt),
          lookupAnchorStateRoot = anchor.stateRoot,
          prerequisites = List.empty
        ),
        authorization = JamBytes.empty,
        authorizerConfig = JamBytes.empty,
        items = List(
          WorkItem(
            service = ServiceId(0),
            codeHash = serviceCodeHash,
            payload = JamBytes(s"soak $slot".getBytes("UTF-8")),
            refineGasLimit = Gas(100_000_000L),
            accumulateGasLimit = Gas(10_000_000L),
            importSegments = List.empty,
            extrinsic = List.empty,
            exportCount = UShort(0)
          )
        )
      )
      val stream = connAtoB.openStream(StreamKind.WorkPackageSubmission).get(10, TimeUnit.SECONDS)
      stream.send(Array[Byte](0, 0) ++ wp.encode.toArray)
      stream.finish()
      val deadline = System.currentTimeMillis() + 2000
      while nodeA.pools.guaranteeCount == 0 && System.currentTimeMillis() < deadline do Thread.sleep(5)

    val rt = Runtime.getRuntime
    val gcs = ManagementFactory.getGarbageCollectorMXBeans.asScala.map(_.getName).mkString("+")
    println(s"NodeSoak slots=$slots wpEvery=$wpEvery data=$dataRoot jvm=${Runtime.version()} gc=$gcs maxHeapMb=${rt.maxMemory() >> 20}")

    val rows = ArrayBuffer.empty[(Long, Double)]
    val out = new StringBuilder
    def emit(line: String): Unit = { println(line); out.append(line).append('\n') }
    var authored = 0L
    var failed = 0L
    var wpSent = 0L
    var guarantees = 0L
    var assurances = 0L
    val start = System.nanoTime()
    def sample(slot: Long): Unit =
      val mb = heapAfterGcMb()
      rows += ((slot, mb))
      emit(
        f"$slot,${(System.nanoTime() - start) / 1e9}%.1f,$mb%.2f,${dirKb(dirA)},${dirKb(dirB)},$authored,$failed," +
          s"${nodeA.chain.finalized.slot},$wpSent,$guarantees,$assurances"
      )

    emit("slot,elapsed_s,heap_after_gc_mb,db_a_kb,db_b_kb,authored,failed_slots,finalized_slot,wp_sent,guarantees_included,assurances_included")
    sample(0)
    var slot = 1L
    while slot <= slots do
      try
        if slot > 2 && slot % wpEvery == 0 then
          submitWorkPackage(slot)
          wpSent += 1
        // Give assurances for pending reports a moment to arrive before authoring.
        val deadline = System.currentTimeMillis() + 300
        while nodeA.pools.assuranceCount < 5 && System.currentTimeMillis() < deadline do Thread.sleep(5)
        if nodeA.authorSlot(slot).isDefined then
          authored += 1
          nodeA.chain.decodeBlock(nodeA.chain.blockStore.getBlock(nodeA.chain.best.hash).get).toOption.foreach { b =>
            guarantees += b.extrinsic.guarantees.size
            assurances += b.extrinsic.assurances.size
          }
        else failed += 1
        awaitSync()
      catch
        case NonFatal(e) =>
          failed += 1
          System.err.println(s"slot $slot: $e")
      if slot % sampleEvery == 0 || slot == slots then sample(slot)
      slot += 1

    val tail = if rows.size >= 6 then rows.drop(rows.size / 2) else rows
    val slope = slopePer(tail.toSeq, 1000)
    val verdict = if slope <= maxGrowthMb then "FLAT" else "GROWING"
    println(
      f"SOAK slots=$slots authored=$authored failed_slots=$failed wp_sent=$wpSent guarantees=$guarantees " +
        f"assurances=$assurances heap_first_mb=${rows.head._2}%.1f heap_last_mb=${rows.last._2}%.1f " +
        f"growth_mb_per_1k_slots=$slope%.3f (limit $maxGrowthMb%.1f) verdict=$verdict"
    )
    opts.get("csv").foreach(f => Files.writeString(Paths.get(f), out.toString))
    nodeA.shutdown()
    nodeB.shutdown()
    if !opts.contains("data") then deleteTree(dataRoot)
    sys.exit(if verdict == "FLAT" && authored > 0 then 0 else 1)

  /** Least-squares slope of heap (MB) against slot, per `per` slots. */
  private def slopePer(rows: Seq[(Long, Double)], per: Int): Double =
    if rows.size < 2 then 0.0
    else
      val mx = rows.map(_._1.toDouble).sum / rows.size
      val my = rows.map(_._2).sum / rows.size
      val den = rows.map(r => (r._1 - mx) * (r._1 - mx)).sum
      if den == 0 then 0.0 else rows.map(r => (r._1 - mx) * (r._2 - my)).sum / den * per

  private def heapAfterGcMb(): Double =
    val mem = ManagementFactory.getMemoryMXBean
    mem.gc(); mem.gc()
    mem.getHeapMemoryUsage.getUsed / 1048576.0

  private def dirKb(dir: Path): Long =
    Using.resource(Files.walk(dir))(
      _.iterator().asScala.map(p => try if Files.isRegularFile(p) then Files.size(p) else 0L catch case NonFatal(_) => 0L).sum
    ) / 1024

  private def deleteTree(dir: Path): Unit =
    try Using.resource(Files.walk(dir))(_.iterator().asScala.toVector.reverse.foreach(Files.deleteIfExists(_)))
    catch case NonFatal(_) => ()
