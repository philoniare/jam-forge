package io.forge.jam.node

import java.net.InetSocketAddress

import com.sun.net.httpserver.{HttpExchange, HttpServer}
import com.typesafe.scalalogging.LazyLogging

/** Local submission endpoint until the JIP-2 RPC exists: `POST /preimage/<service-id>` with the raw preimage as the
  * body hands it to [[PreimageService.submit]] (pooled, then announced on CE 142). Answers 200 with the hex hash, or
  * 400 with the reason. Binds to loopback only.
  */
final class SubmitServer(port: Int, preimages: PreimageService) extends LazyLogging:

  private val server = HttpServer.create(new InetSocketAddress("127.0.0.1", port), 0)
  server.createContext("/preimage/", (ex: HttpExchange) => handlePreimage(ex))

  def start(): SubmitServer =
    server.start()
    logger.info(s"submission endpoint on 127.0.0.1:$boundPort (POST /preimage/<service-id>)")
    this

  def boundPort: Int = server.getAddress.getPort

  def stop(): Unit = server.stop(0)

  private def handlePreimage(ex: HttpExchange): Unit =
    try
      val service = ex.getRequestURI.getPath.stripPrefix("/preimage/").toLongOption
      val result =
        if ex.getRequestMethod != "POST" then Left("use POST")
        else
          service match
            case None => Left("expected /preimage/<service-id>")
            case Some(id) =>
              val body = ex.getRequestBody.readNBytes(SubmitServer.MaxPreimageBytes + 1)
              if body.length > SubmitServer.MaxPreimageBytes then Left("preimage too large")
              else preimages.submit(id, body).map(_.toHex)
      val (code, text) = result.fold(400 -> _, 200 -> _)
      val bytes = (text + "\n").getBytes("UTF-8")
      ex.sendResponseHeaders(code, bytes.length.toLong)
      ex.getResponseBody.write(bytes)
    catch case e: Exception => logger.warn(s"preimage submission failed: $e")
    finally ex.close()

object SubmitServer:
  /** Upper bound on one submitted preimage: 4 MiB, above the service-code limit `W_C`. */
  val MaxPreimageBytes: Int = 4 * 1024 * 1024
