package io.forge.jam.node

import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets

import com.sun.net.httpserver.{HttpExchange, HttpServer}
import com.typesafe.scalalogging.LazyLogging
import io.circe.Json

final case class NodeHealth(
    best: ChainHead,
    finalized: ChainHead,
    peers: Int,
    syncing: Boolean,
    following: Boolean,
    validatorConnectivity: Option[ValidatorConnectivity]
):
  def json: Json =
    Json.obj(
      "best" -> Json.fromLong(best.slot),
      "bestHash" -> Json.fromString("0x" + best.hash.toHex),
      "finalized" -> Json.fromLong(finalized.slot),
      "finalizedHash" -> Json.fromString("0x" + finalized.hash.toHex),
      "peers" -> Json.fromInt(peers),
      "syncing" -> Json.fromBoolean(syncing),
      "following" -> Json.fromBoolean(following),
      "validatorConnectivity" -> validatorConnectivity.fold(Json.Null)(v =>
        Json.obj(
          "connected" -> Json.fromInt(v.connected),
          "others" -> Json.fromInt(v.others),
          "low" -> Json.fromBoolean(v.low)
        )
      )
    )

final case class NodeMetrics(
    health: NodeHealth,
    validatorPeers: Int,
    importedBlocks: Long,
    importFailures: Long,
    guaranteesMade: Long,
    auditsDone: Long,
    telemetryDroppedEvents: Long,
    queuedSubmissions: Int
):
  def prometheusText: String =
    val out = new StringBuilder
    def metric(name: String, kind: String, help: String, value: Long, labels: String = ""): Unit =
      out ++= s"# HELP $name $help\n# TYPE $name $kind\n$name$labels $value\n"
    def flag(b: Boolean) = if b then 1L else 0L
    val h = health
    metric("jam_best_slot", "gauge", "Slot of the best block.", h.best.slot)
    metric("jam_finalized_slot", "gauge", "Slot of the finalized block.", h.finalized.slot)
    metric("jam_peers", "gauge", "Distinct peers connected.", h.peers.toLong)
    metric("jam_validator_peers", "gauge", "Connected peers that are validators of the previous, current or next set.", validatorPeers.toLong)
    metric("jam_syncing", "gauge", "1 while the node is catching up with its peers.", flag(h.syncing))
    metric("jam_following", "gauge", "1 while the node follows the chain (what /health answers 200 for).", flag(h.following))
    metric(
      "jam_validator_connectivity_low",
      "gauge",
      "1 while this validator has been connected to fewer than 2/3 of the other current validators past the grace period.",
      flag(h.validatorConnectivity.exists(_.low))
    )
    h.validatorConnectivity.foreach { v =>
      metric("jam_validators_connected", "gauge", "Other current validators this validator is connected to.", v.connected.toLong)
      metric("jam_validators_others", "gauge", "Other validators in the current set.", v.others.toLong)
    }
    metric("jam_blocks_imported_total", "counter", "Blocks imported onto the best chain since start.", importedBlocks)
    metric(
      "jam_block_import_failures_total",
      "counter",
      "Blocks found invalid, or that failed to import, since start.",
      importFailures
    )
    metric("jam_guarantees_made_total", "counter", "Guarantees this node assembled and distributed since start.", guaranteesMade)
    metric("jam_audits_done_total", "counter", "Work reports audited to a verdict since start.", auditsDone)
    metric("jam_telemetry_dropped_events_total", "counter", "JIP-3 telemetry events dropped since start.", telemetryDroppedEvents)
    metric("jam_queued_submissions", "gauge", "Work-package submissions waiting for a refine thread.", queuedSubmissions.toLong)
    metric(
      "jam_build_info",
      "gauge",
      "The node's version, as a label; always 1.",
      1L,
      s"""{version="${NodeMetrics.labelValue(BuildInfo.version)}"}"""
    )
    out.toString

object NodeMetrics:
  def labelValue(s: String): String = s.flatMap {
    case '\\' => "\\\\"
    case '"'  => "\\\""
    case '\n' => "\\n"
    case c    => c.toString
  }

final class MetricsServer(
    bind: InetSocketAddress,
    metrics: () => NodeMetrics,
    health: () => NodeHealth,
    executor: java.util.concurrent.ExecutorService
) extends LazyLogging:

  private val server = HttpServer.create(bind, 0)
  server.setExecutor(executor)
  server.createContext("/", (ex: HttpExchange) => handle(ex))

  def start(): MetricsServer =
    server.start()
    logger.info(s"metrics on ${JamNode.endpointText(boundAddress)} (GET /metrics, GET /health)")
    this

  def boundAddress: InetSocketAddress = server.getAddress

  def boundPort: Int = boundAddress.getPort

  def stop(): Unit =
    server.stop(0)
    executor.shutdownNow(): Unit

  private def handle(ex: HttpExchange): Unit =
    try
      val (status, contentType, body) =
        ex.getRequestURI.getPath match
          case path @ ("/metrics" | "/health") if ex.getRequestMethod != "GET" && ex.getRequestMethod != "HEAD" =>
            ex.getResponseHeaders.set("Allow", "GET, HEAD")
            (405, "text/plain; charset=utf-8", s"$path takes GET\n")
          case "/metrics" => (200, MetricsServer.PrometheusContentType, metrics().prometheusText)
          case "/health" =>
            val h = health()
            (if h.following then 200 else 503, "application/json", h.json.noSpaces + "\n")
          case _ => (404, "text/plain; charset=utf-8", "not found: GET /metrics or GET /health\n")
      val bytes = body.getBytes(StandardCharsets.UTF_8)
      ex.getResponseHeaders.set("Content-Type", contentType)
      ex.getResponseHeaders.set("Cache-Control", "no-store")
      if ex.getRequestMethod == "HEAD" then ex.sendResponseHeaders(status, -1)
      else
        ex.sendResponseHeaders(status, bytes.length.toLong)
        ex.getResponseBody.write(bytes)
    catch
      case e: Exception =>
        logger.warn(s"metrics request ${ex.getRequestURI} failed: $e")
        try ex.sendResponseHeaders(500, -1)
        catch case _: Exception => ()
    finally ex.close()

object MetricsServer:
  val PrometheusContentType = "text/plain; version=0.0.4; charset=utf-8"
