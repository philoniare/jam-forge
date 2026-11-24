package io.forge.jam.node

import java.net.{InetAddress, InetSocketAddress, URI}
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.Path

import io.circe.Json
import io.circe.parser.parse as parseJson
import io.forge.jam.core.ChainConfig
import io.forge.jam.core.codec.JamCodecs.encode
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class NodeOperationsSpec extends AnyFunSuite with Matchers with NodeFixture:

  private val devKeys = (0 until 6).map(ValidatorKeySet.dev)
  private val http = HttpClient.newHttpClient()

  private def eventually(what: String, seconds: Int = 20)(cond: => Boolean): Unit =
    val deadline = System.currentTimeMillis() + seconds * 1000L
    while !cond && System.currentTimeMillis() < deadline do Thread.sleep(50)
    if !cond then fail(s"timed out waiting for: $what")

  private def get(port: Int, path: String, method: String = "GET"): HttpResponse[String] =
    val request = HttpRequest.newBuilder(URI.create(s"http://127.0.0.1:$port$path")).method(method, HttpRequest.BodyPublishers.noBody()).build()
    http.send(request, HttpResponse.BodyHandlers.ofString())

  private def json(response: HttpResponse[String]): Json =
    parseJson(response.body).fold(e => fail(s"not JSON: $e: ${response.body}"), identity)

  private def field(j: Json, name: String): Json = j.hcursor.downField(name).focus.getOrElse(fail(s"no $name in $j"))

  private def sample(text: String, name: String): Option[String] =
    text.linesIterator.collectFirst { case line if line.startsWith(name + " ") => line.drop(name.length + 1) }

  private def withSpec(id: String)(body: ChainSpec => Unit): Unit =
    val genesis = loadGenesis().getOrElse(cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available"))
    body(ChainSpec(id, ChainConfig.TINY, Some(genesis.header.encode.toArray), None, genesis.state.keyvals, Nil))

  private def withNodes(prefix: String)(body: ((NodeConfig => JamNode), String => Path) => Unit): Unit =
    withSpec(prefix) { spec =>
      var nodes = List.empty[JamNode]
      var dirs = List.empty[Path]
      def dir(name: String): Path =
        val d = tempDir(s"$prefix-$name")
        dirs ::= d
        d
      def start(config: NodeConfig): JamNode =
        val n = new JamNode(spec, config).start()
        nodes ::= n
        n
      try body(start, dir)
      finally
        nodes.foreach(_.shutdown())
        dirs.foreach(cleanup)
    }

  test("/metrics and /health report the node; health is 503 until it follows the chain with a peer") {
    withNodes("jam-metrics") { (start, dir) =>
      val a = start(NodeConfig(dir("a"), slotTicking = false))
      val server = a.enableMetrics(0)
      val port = server.boundPort
      server.boundAddress.getAddress.isLoopbackAddress shouldBe true

      val alone = get(port, "/health")
      alone.statusCode shouldBe 503
      alone.headers.firstValue("Content-Type").orElse("") should startWith("application/json")
      val aloneJson = json(alone)
      field(aloneJson, "best").asNumber.flatMap(_.toLong) shouldBe Some(0L)
      field(aloneJson, "finalized").asNumber.flatMap(_.toLong) shouldBe Some(0L)
      field(aloneJson, "peers").asNumber.flatMap(_.toInt) shouldBe Some(0)
      field(aloneJson, "syncing").asBoolean shouldBe Some(false)
      field(aloneJson, "following").asBoolean shouldBe Some(false)

      val b = start(NodeConfig(dir("b"), slotTicking = false, ed25519Seed = Some(devKeys(1).ed25519Secret)))
      b.connectPeer(new InetSocketAddress("127.0.0.1", a.listenPort))
      eventually("the peer to count")(a.distribution.peerCount == 1)
      val withPeer = get(port, "/health")
      withPeer.statusCode shouldBe 200
      field(json(withPeer), "peers").asNumber.flatMap(_.toInt) shouldBe Some(1)
      field(json(withPeer), "following").asBoolean shouldBe Some(true)

      a.enableAuthoring(devKeys)
      a.authorSlot(1).isDefined shouldBe true
      eventually("the import to be counted")(sample(get(port, "/metrics").body, "jam_blocks_imported_total").contains("1"))
      eventually("the validator peer to count")(sample(get(port, "/metrics").body, "jam_validator_peers").contains("1"))

      val metrics = get(port, "/metrics")
      metrics.statusCode shouldBe 200
      metrics.headers.firstValue("Content-Type").orElse("") should startWith("text/plain; version=0.0.4")
      val text = metrics.body
      sample(text, "jam_best_slot") shouldBe Some("1")
      sample(text, "jam_finalized_slot") shouldBe Some("0")
      sample(text, "jam_peers") shouldBe Some("1")
      sample(text, "jam_block_import_failures_total") shouldBe Some("0")
      sample(text, "jam_guarantees_made_total") shouldBe Some("0")
      sample(text, "jam_audits_done_total") shouldBe Some("0")
      sample(text, "jam_telemetry_dropped_events_total") shouldBe Some("0")
      sample(text, "jam_queued_submissions") shouldBe Some("0")
      sample(text, "jam_syncing") shouldBe Some("0")
      text should include("# TYPE jam_blocks_imported_total counter")
      text should include("# TYPE jam_best_slot gauge")
      text should include("# HELP jam_peers ")
      text should include(s"""jam_build_info{version="${BuildInfo.version}"} 1""")
      text should include("# TYPE jam_block_import_seconds histogram")
      sample(text, "jam_block_import_seconds_count") shouldBe Some("1")
      sample(text, """jam_block_import_seconds_bucket{le="+Inf"}""") shouldBe Some("1")
      sample(text, "jam_block_import_seconds_sum").map(_.toDouble).exists(_ > 0.0) shouldBe true
      text should include("# TYPE jam_trie_wal_sync_seconds histogram")
      sample(text, "jam_trie_wal_sync_seconds_count").map(_.toLong).exists(_ >= 1L) shouldBe true
      text should include("# TYPE jam_chain_meta_write_seconds histogram")
      sample(text, "jam_chain_meta_write_seconds_count").map(_.toLong).exists(_ >= 1L) shouldBe true
      sample(text, "jam_slots_behind_best_peer") shouldBe Some("0")
      sample(text, "jam_sync_blocks_total") shouldBe Some("0")
      sample(text, "jam_sync_answers_total") shouldBe Some("0")
      eventually("b to import the announced block")(b.chain.best.slot == 1L)
      eventually("b's sync counters to move") {
        val now = b.metrics.prometheusText
        sample(now, "jam_sync_blocks_total").contains("1") &&
        sample(now, "jam_sync_import_seconds_total").map(_.toDouble).exists(_ > 0.0)
      }
      val synced = b.metrics.prometheusText
      sample(synced, "jam_sync_blocks_total") shouldBe Some("1")
      sample(synced, "jam_sync_answers_total") shouldBe Some("1")
      sample(synced, "jam_sync_fetch_seconds_total").map(_.toDouble).exists(_ > 0.0) shouldBe true
      sample(synced, "jam_sync_import_seconds_total").map(_.toDouble).exists(_ > 0.0) shouldBe true
      sample(synced, "jam_block_import_seconds_count") shouldBe Some("1")

      val histograms = text.linesIterator.collect { case l if l.startsWith("# TYPE ") && l.endsWith(" histogram") => l.split(' ')(2) }.toSet
      val names = text.linesIterator.filterNot(_.startsWith("#")).filter(_.nonEmpty).map(_.takeWhile(c => c != ' ' && c != '{')).toList
      names.foreach { n =>
        n should fullyMatch regex "[a-zA-Z_:][a-zA-Z0-9_:]*"
        val family = Seq("_bucket", "_sum", "_count").collectFirst { case x if n.endsWith(x) && histograms(n.dropRight(x.length)) => n.dropRight(x.length) }
        text should include(s"# TYPE ${family.getOrElse(n)} ")
      }
      field(json(get(port, "/health")), "best").asNumber.flatMap(_.toLong) shouldBe Some(1L)

      get(port, "/nothing").statusCode shouldBe 404
      get(port, "/metricsx").statusCode shouldBe 404
      get(port, "/health", method = "POST").statusCode shouldBe 405
    }
  }

  test("/metrics reports how many slots the best peer is ahead, and the stage timers when they are on") {
    val head = ChainHead(io.forge.jam.core.primitives.Hash.zero, 7L, io.forge.jam.core.primitives.Hash.zero)
    val base = NodeMetrics(
      health = NodeHealth(head, head, peers = 1, syncing = true, following = false, validatorConnectivity = None),
      validatorPeers = 0,
      importedBlocks = 0L,
      importFailures = 0L,
      guaranteesMade = 0L,
      auditsDone = 0L,
      telemetryDroppedEvents = 0L,
      queuedSubmissions = 0,
      slotsBehindBestPeer = 93L
    )
    sample(base.prometheusText, "jam_slots_behind_best_peer") shouldBe Some("93")
    base.prometheusText should not include "jam_stage_"
    val staged = base.copy(stages = List(("accumulation", 3L, 1_500_000L), ("pvm/host-3", 1L, 250L))).prometheusText
    staged should include("# TYPE jam_stage_seconds_total counter")
    staged should include("""jam_stage_seconds_total{stage="accumulation"} 0.0015""")
    staged should include("""jam_stage_calls_total{stage="accumulation"} 3""")
    staged should include("""jam_stage_seconds_total{stage="pvm/host-3"} 2.5E-7""")
  }

  test("a validator connected to fewer than 2/3 of the others is told so, in the log and in /health") {
    val logger = org.slf4j.LoggerFactory.getLogger(classOf[LowConnectivityWatch]).asInstanceOf[ch.qos.logback.classic.Logger]
    val seen = new ch.qos.logback.core.read.ListAppender[ch.qos.logback.classic.spi.ILoggingEvent]()
    seen.start()
    logger.addAppender(seen)
    try
      withNodes("jam-low-connectivity") { (start, dir) =>
        val a = start(
          NodeConfig(dir("a"), slotTicking = false, ed25519Seed = Some(devKeys(0).ed25519Secret), lowConnectivityGraceMillis = 0L)
        )
        val port = a.enableMetrics(0).boundPort
        val b = start(NodeConfig(dir("b"), slotTicking = false, ed25519Seed = Some(devKeys(1).ed25519Secret)))
        b.connectPeer(new InetSocketAddress("127.0.0.1", a.listenPort))
        eventually("connectivity to count the validator peer") {
          a.validatorConnectivity.contains(ValidatorConnectivity(connected = 1, others = 5, low = true))
        }
        val health = json(get(port, "/health"))
        val connectivity = field(health, "validatorConnectivity")
        field(connectivity, "connected").asNumber.flatMap(_.toInt) shouldBe Some(1)
        field(connectivity, "others").asNumber.flatMap(_.toInt) shouldBe Some(5)
        field(connectivity, "low").asBoolean shouldBe Some(true)
        sample(get(port, "/metrics").body, "jam_validator_connectivity_low") shouldBe Some("1")
        val warnings = seen.list.toArray.toList.collect {
          case e: ch.qos.logback.classic.spi.ILoggingEvent if e.getLevel == ch.qos.logback.classic.Level.WARN => e.getFormattedMessage
        }
        warnings.count(_.contains("of the 5 other current validators")) shouldBe 1
      }
    finally logger.detachAppender(seen): Unit
  }

  test("a node not in the validator set reports no validator connectivity") {
    withNodes("jam-no-validator") { (start, dir) =>
      val a = start(NodeConfig(dir("a"), slotTicking = false, lowConnectivityGraceMillis = 0L))
      val port = a.enableMetrics(0).boundPort
      Thread.sleep(1200) // two connectivity ticks
      a.validatorConnectivity shouldBe None
      field(json(get(port, "/health")), "validatorConnectivity").isNull shouldBe true
      sample(get(port, "/metrics").body, "jam_validator_connectivity_low") shouldBe Some("0")
    }
  }

  test("--listen-ip binds the QUIC listener to that address, and --external-ip is the endpoint the node reports") {
    withNodes("jam-listen-ip") { (start, dir) =>
      val loopback = InetAddress.getByAddress(Array[Byte](127, 0, 0, 1))
      val external = InetAddress.getByAddress(Array[Byte](203.toByte, 0, 113, 7))
      val a = start(NodeConfig(dir("a"), slotTicking = false, listenIp = loopback, externalIp = Some(external)))
      a.boundAddress.getAddress shouldBe loopback
      a.advertisedEndpoint shouldBe Some(new InetSocketAddress(external, a.listenPort))
      val b = start(NodeConfig(dir("b"), slotTicking = false))
      b.advertisedEndpoint shouldBe None
      b.boundAddress.getAddress.isAnyLocalAddress shouldBe true
      b.connectPeer(new InetSocketAddress(loopback, a.listenPort))
      eventually("the peer to count")(a.distribution.peerCount == 1)
    }
  }

  test("an IPv6 --listen-ip binds there, where the machine has IPv6") {
    val v6Loopback = InetAddress.getByAddress(Array.fill[Byte](15)(0) :+ 1.toByte)
    val hasIpv6 =
      try
        val s = new java.net.DatagramSocket(new InetSocketAddress(v6Loopback, 0))
        s.close()
        true
      catch case _: java.io.IOException => false
    if !hasIpv6 then cancel("no IPv6 loopback on this machine")
    withNodes("jam-listen-ipv6") { (start, dir) =>
      val a = start(NodeConfig(dir("a"), slotTicking = false, listenIp = v6Loopback))
      a.boundAddress.getAddress shouldBe v6Loopback
      val b = start(NodeConfig(dir("b"), slotTicking = false))
      b.connectPeer(new InetSocketAddress(v6Loopback, a.listenPort))
      eventually("the peer to count")(a.distribution.peerCount == 1)
    }
  }

  test("endpoints are written as operators write them: IPv6 compressed and in brackets") {
    def at(ip: String, port: Int) = JamNode.endpointText(new InetSocketAddress(InetAddress.getByName(s"[$ip]"), port))
    at("::", 40000) shouldBe "[::]:40000"
    at("::1", 1) shouldBe "[::1]:1"
    at("2001:db8:0:0:0:0:0:7", 2) shouldBe "[2001:db8::7]:2"
    at("2001:db8:0:1:0:0:0:7", 3) shouldBe "[2001:db8:0:1::7]:3"
    at("fe80:0:0:0:1:0:0:1", 4) shouldBe "[fe80::1:0:0:1]:4"
    at("1:2:3:4:5:6:7:8", 5) shouldBe "[1:2:3:4:5:6:7:8]:5"
    at("1:0:3:4:5:6:7:8", 6) shouldBe "[1:0:3:4:5:6:7:8]:6"
    JamNode.endpointText(new InetSocketAddress(InetAddress.getByAddress(Array[Byte](203.toByte, 0, 113, 7)), 7)) shouldBe
      "203.0.113.7:7"
  }

  test("node information on telemetry carries the advertised address, an IPv4 one mapped into IPv6") {
    val external = InetAddress.getByAddress(Array[Byte](203.toByte, 0, 113, 7))
    val params = Array.fill[Byte](10)(9)
    def info(address: Option[InetAddress]) =
      Telemetry.nodeInformation(
        jamParameters = params,
        genesisHash = io.forge.jam.core.primitives.Hash.zero,
        peerId = Array.fill[Byte](32)(1),
        listenPort = 40000,
        recompiler = false,
        implementationVersion = "v",
        grayPaperVersion = "g",
        note = "",
        advertisedAddress = address
      )
    val at = 1 + params.length + 32 + 32
    val withAddress = info(Some(external))
    withAddress.slice(at, at + 16).toSeq shouldBe (Array.fill[Byte](10)(0) ++ Array[Byte](-1, -1, 203.toByte, 0, 113, 7)).toSeq
    Connectivity.endpoint(withAddress.slice(at, at + 18)) shouldBe Some(new InetSocketAddress(external, 40000))
    info(None).slice(at, at + 16).toSeq shouldBe Seq.fill[Byte](16)(0)
    val v6 = InetAddress.getByName("[2001:db8::7]")
    Connectivity.addressBytes(v6).toSeq shouldBe v6.getAddress.toSeq
  }
