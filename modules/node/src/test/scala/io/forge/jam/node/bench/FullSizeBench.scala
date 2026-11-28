package io.forge.jam.node.bench

import java.lang.management.ManagementFactory
import java.nio.file.{Files, Path, Paths}

import io.forge.jam.core.{ChainConfig, Hashing, JamBytes, LittleEndian, constants}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.block.Block
import io.forge.jam.crypto.{BandersnatchVrf, Ed25519ZebraWrapper}
import io.forge.jam.db.ShardStore
import io.forge.jam.network.{Loopback, PeerConnection, PeerStreamHandler, StreamKind}
import io.forge.jam.node.*
import io.forge.jam.protocol.refine.{AvailabilitySpecifier, DaShards}
import io.forge.jam.vrfs.BandersnatchWrapper
import io.forge.jam.protocol.traces.{Genesis, TraceStep}

import scala.jdk.CollectionConverters.*
import scala.util.Random

object FullSizeBench:

  private val config = ChainConfig.FULL

  private def result(name: String, value: Double, unit: String): Unit =
    println(f"RESULT $name%-44s $value%12.3f $unit")

  private def cpuMillis(prefix: String): Double =
    val mx = ManagementFactory.getThreadMXBean
    Thread.getAllStackTraces.keySet.asScala.filter(_.getName == prefix).toList
      .map(t => math.max(0L, mx.getThreadCpuTime(t.threadId())))
      .sum / 1e6

  private val os = ManagementFactory.getOperatingSystemMXBean.asInstanceOf[com.sun.management.OperatingSystemMXBean]

  private def processCpuMillis: Double = os.getProcessCpuTime / 1e6

  private def await(what: String, seconds: Int)(done: => Boolean): Unit =
    val deadline = System.nanoTime() + seconds * 1_000_000_000L
    while !done do
      if System.nanoTime() > deadline then throw new IllegalStateException(s"timed out waiting for: $what")
      Thread.sleep(5)

  private def deleteTree(dir: Path): Unit =
    Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p))

  def main(args: Array[String]): Unit =
    val opts = args.collect { case s"--$k=$v" => k -> v }.toMap
    val trace = Paths.get(opts.getOrElse("trace", "target/bench/full-busy-trace"))
    PerState.enabled = !opts.get("per-state").contains("off")
    if !Files.exists(trace.resolve("genesis.bin")) then
      System.err.println(s"no trace in $trace: make one with\n  sbt \"benchGenTrace --spec=full --blocks=12 --busy-cores=341 --out=$trace\"")
      sys.exit(1)
    val dir = Files.createTempDirectory("jam-full-size")
    var code = 0
    try
      val keys = (0 until config.validatorCount).map(ValidatorKeySet.dev)
      val (node, auditor, blocks) = replay(trace, dir)
      auditor.verifyAside = !opts.get("verify-aside").contains("off")
      auditor.batchJudgments = !opts.get("batch-judgments").contains("off")
      try
        auditing(node, auditor, blocks, keys)
        shardLookup(node, blocks, keys, dir)
        peerOrdering(keys)
      finally node.shutdown()
    catch
      case e: Throwable =>
        e.printStackTrace()
        code = 1
    finally deleteTree(dir)
    sys.exit(code)

  private def replay(trace: Path, dir: Path): (JamNode, AuditorService, List[Block]) =
    val genesis = Genesis.codec(config.epochLength).decodeUnsafe(Files.readAllBytes(trace.resolve("genesis.bin")))
    val headerCodec = io.forge.jam.core.types.header.Header.headerCodec(config.epochLength)
    val spec = ChainSpec(
      id = "full-size-bench",
      config = config,
      genesisHeaderBytes = Some(headerCodec.encode(genesis.header)),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )
    val node = new JamNode(spec, NodeConfig(dataDir = dir.resolve("node"), slotTicking = false))
    node.chain.initializeOrRestore(spec)
    val auditor = node.enableAuditing(Nil)
    val stepCodec = TraceStep.codec(config.epochLength, config.coresCount)
    val files = Files.list(trace).iterator.asScala.filter(_.getFileName.toString.matches("\\d{8}\\.bin")).toList.sortBy(_.toString)
    val t0 = System.nanoTime()
    val blocks = files.map { f =>
      val block = stepCodec.decodeUnsafe(Files.readAllBytes(f)).block
      node.chain.importBlock(node.chain.encodeBlock(block)).fold(e => throw new IllegalStateException(s"$f: ${e.message}"), _ => ())
      block
    }
    result("replay_block_import_mean", (System.nanoTime() - t0) / 1e6 / blocks.size, "ms")
    println(s"replayed ${blocks.size} blocks: ${blocks.map(_.extrinsic.guarantees.size).sum} guarantees, " +
      s"${blocks.map(_.extrinsic.assurances.size).sum} assurances")
    (node, auditor, blocks)


  private def auditing(node: JamNode, auditor: AuditorService, blocks: List[Block], keys: IndexedSeq[ValidatorKeySet]): Unit =
    import AuditSelection.*
    val block = blocks.filter(_.extrinsic.assurances.nonEmpty).last
    val hash = node.chain.headerHashOf(block)
    await("the auditor to track the block", 60)(auditor.trackedSelection(hash).isDefined)
    val (vrfOutput, reports, epoch) = auditor.trackedSelection(hash).get
    val available = reports.flatten
    println(s"auditing block at slot ${block.header.slot.toLong}: ${available.size} reports available, ${keys.size} validators")

    val tBuild = System.nanoTime()
    val messages = keys.indices.map { v =>
      val k = keys(v)
      val s0 = BandersnatchWrapper.ietfVrfSign(k.bandersnatchSecret, initialInput(vrfOutput), Array.emptyByteArray)
      val y = BandersnatchVrf.getVrfOutput(s0).getOrElse(throw new IllegalStateException("no VRF output"))
      val items = initialCores(reports, y).sorted.flatMap(c => reports(c).map(c -> _))
      val signature = Ed25519ZebraWrapper.sign(k.ed25519Secret, announcementMessage(0, items, hash))
      val head = encodeHead(hash, 0, Announcement(items, signature))
      val judgments = items.map { (_, report) =>
        LittleEndian.encode(epoch, 4) ++ LittleEndian.encode(v.toLong, 2) ++ Array[Byte](1) ++ report.bytes.toArray ++
          Ed25519ZebraWrapper.sign(k.ed25519Secret, constants.JAM_VALID_BYTES ++ report.bytes.toArray)
      }
      (head, encodeFirstEvidence(s0), items.map(_._2), judgments)
    }
    result("audit_messages_built", (System.nanoTime() - tBuild) / 1e6, "ms")
    val pairs = messages.map(_._3.size).sum
    println(s"${messages.count(_._3.nonEmpty)} announcements naming $pairs (validator, report) pairs; $pairs judgments")

    val nodeKey = node.identity.publicKeyBytes
    val serve: Byte => Option[PeerStreamHandler] =
      kind => Option.when(kind == StreamKind.AuditAnnouncement)(auditor.auditAnnouncementHandler)
    val connections = keys.map(k => Loopback.pair(k.ed25519Public, nodeKey, _ => None, serve)._1)

    def announced: Int = available.map(r => auditor.announcersOf(r, 0).size).sum
    def settle(): Unit = await("the auditor to be idle", 120)(auditor.queuedMessages == 0)

    val auditCpu0 = cpuMillis("jam-audit")
    val cpu0 = processCpuMillis
    val t0 = System.nanoTime()
    messages.indices.foreach { v =>
      val (head, evidence, items, _) = messages(v)
      if items.nonEmpty then
        val stream = connections(v).openStream(StreamKind.AuditAnnouncement).get()
        stream.send(head)
        stream.send(evidence)
        stream.finish()
      if v % 128 == 127 then await("room in the auditor's queue", 120)(auditor.queuedMessages < 1024)
    }
    await("every announcement to be recorded", 300)(announced == pairs)
    settle()
    val t1 = System.nanoTime()
    val auditCpu1 = cpuMillis("jam-audit")
    val cpu1 = processCpuMillis
    result("audit_announcements_process_cpu", cpu1 - cpu0, "ms")
    val announcements = messages.count(_._3.nonEmpty)
    result("audit_announcements", announcements.toDouble, "messages")
    result("audit_announcements_wall", (t1 - t0) / 1e6, "ms")
    result("audit_announcement_audit_thread_cpu", (auditCpu1 - auditCpu0) * 1000 / announcements, "us/message")
    if auditor.isAudited(hash) then throw new IllegalStateException("audited before any judgment")

    var sent = 0
    messages.foreach { (_, _, _, judgments) =>
      judgments.foreach { j =>
        auditor.receiveJudgment(j)
        sent += 1
        if sent % 512 == 0 then await("room in the auditor's queue", 120)(auditor.queuedMessages < 1024)
      }
    }
    await("the block to be audited", 300)(auditor.isAudited(hash))
    settle()
    val t2 = System.nanoTime()
    val auditCpu2 = cpuMillis("jam-audit")
    result("audit_judgments_process_cpu", processCpuMillis - cpu1, "ms")
    result("audit_judgments", pairs.toDouble, "messages")
    result("audit_judgments_wall", (t2 - t1) / 1e6, "ms")
    result("audit_judgment_audit_thread_cpu", (auditCpu2 - auditCpu1) * 1000 / pairs, "us/message")
    result("audit_block_total_wall", (t2 - t0) / 1e6, "ms")
    if !auditor.auditedStateConsistent then throw new IllegalStateException("per-report audit results differ from a fresh evaluation")
    println("the block is audited, and the per-report results equal a fresh evaluation of all 341 reports")
    connections.foreach(_.close())


  private def shardLookup(node: JamNode, blocks: List[Block], keys: IndexedSeq[ValidatorKeySet], dir: Path): Unit =
    val chainIndex = new GuarantorIndex()
    blocks.foreach { b =>
      if b.extrinsic.guarantees.nonEmpty then
        val parent = node.chain.stateViewAt(b.header.parent).getOrElse(throw new IllegalStateException("no parent state"))
        chainIndex.record(List(parent.validators.current, parent.validators.previous), b.extrinsic.guarantees)
    }
    val guarantees = blocks.flatMap(_.extrinsic.guarantees)
    val indexed = guarantees.map(g => chainIndex.guarantorsOf(g.report.packageSpec.erasureRoot).size)
    result("guarantor_index_reports", guarantees.size.toDouble, "reports")
    result("guarantor_index_keys_per_report_mean", indexed.sum.toDouble / indexed.size, "keys")
    if indexed.exists(_ == 0) then throw new IllegalStateException("a report has no guarantors in the index")

    val bundle = Array.tabulate[Byte](20000)(i => (i * 31 + 7).toByte)
    val root = AvailabilitySpecifier.build(Hashing.blake2b256(bundle), bundle, IndexedSeq.empty, config)
      .fold(e => throw new IllegalStateException(e), _.erasureRoot)
    val shards = DaShards.buildAll(bundle, IndexedSeq.empty, config).fold(e => throw new IllegalStateException(e), identity)
    val model = guarantees.last
    val guarantee = model.copy(report = model.report.copy(packageSpec = model.report.packageSpec.copy(erasureRoot = root)))
    val validators = node.chain.stateView().validators
    val index = new GuarantorIndex()
    index.record(List(validators.current, validators.previous), List(guarantee))
    val holders = index.guarantorsOf(root)
    val wanted = 517 // the shard index one assurer is after

    val threads = new NodeThreads
    val holderStore = ShardStore.open(dir.resolve("holder"))
    val emptyStore = ShardStore.open(dir.resolve("empty"))
    holderStore.put(root, wanted, shards(wanted).encode)
    val holder = new ShardService(holderStore, config, threads = threads)
    val other = new ShardService(emptyStore, config, threads = threads)
    val asker = new ShardService(emptyStore, config, threads = threads)
    def serving(s: ShardService): Byte => Option[PeerStreamHandler] =
      kind => Option.when(kind == StreamKind.ShardDistribution)(s.shardDistributionHandler)
    val self = keys(0).ed25519Public
    val peers: List[PeerConnection] = keys.drop(1).map { k =>
      val has = holders.contains(io.forge.jam.core.primitives.Ed25519PublicKey(k.ed25519Public))
      Loopback.pair(self, k.ed25519Public, _ => None, serving(if has then holder else other))._1: PeerConnection
    }.toList
    println(s"${peers.size} peers, ${peers.count(c => holders.contains(c.peerPublicKey))} of them hold the shard")

    def lookups(name: String, order: List[PeerConnection] => List[PeerConnection]): Unit =
      val rng = new Random(9)
      val runs = 40
      val asked = new java.util.concurrent.atomic.AtomicInteger()
      val times = (1 to runs).map { _ =>
        val shuffled = rng.shuffle(peers) // connection order is arbitrary
        val t0 = System.nanoTime()
        val found = asker.firstFrom(order(shuffled)) { conn =>
          asked.incrementAndGet()
          asker.fetchShards(conn, root, wanted, timeoutMs = 4000)
        }
        if found.isEmpty then throw new IllegalStateException(s"$name: the shard was not found")
        (System.nanoTime() - t0) / 1e6
      }.sorted
      result(s"shard_lookup_${name}_peers_asked_mean", asked.get.toDouble / runs, "peers")
      result(s"shard_lookup_${name}_p50", times(runs / 2), "ms")
      result(s"shard_lookup_${name}_max", times.last, "ms")

    try
      lookups("connection_order", identity)
      lookups("guarantors_first", index.first(root, _))
    finally
      peers.foreach(_.close())
      threads.close()
      holderStore.close()
      emptyStore.close()


  private def peerOrdering(keys: IndexedSeq[ValidatorKeySet]): Unit =
    val peers = keys.drop(1).map(_.ed25519Public).toList
    def time(name: String, faulted: Int): Unit =
      val scores = new PeerScores(exempt = _ => true)
      peers.take(faulted).foreach(scores.report(_, Offence.Malformed))
      var sink = 0
      (1 to 2000).foreach(_ => sink += scores.leastFaultedFirst(peers)(identity).size) // warm up
      val runs = 5000
      val t0 = System.nanoTime()
      (1 to runs).foreach(_ => sink += scores.leastFaultedFirst(peers)(identity).size)
      result(s"peer_order_${name}", (System.nanoTime() - t0) / 1e3 / runs, "us/call")
      if sink == 0 then println()
    time("nobody_faulted", 0)
    time("10_faulted", 10)
    time("all_faulted", peers.size)
