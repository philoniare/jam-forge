package io.forge.jam.node

import java.net.URI
import java.net.http.{HttpClient, WebSocket}
import java.util.concurrent.{CompletionStage, LinkedBlockingQueue, TimeUnit}

import io.circe.Json
import io.circe.parser.parse
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RpcServerSpec extends AnyFunSuite with Matchers with LoopbackFixture:

  private final class Client(port: Int, origin: Option[String] = None):
    val received = new LinkedBlockingQueue[Json]()
    private val text = new StringBuilder
    private val socket = origin
      .foldLeft(HttpClient.newHttpClient().newWebSocketBuilder())((b, o) => b.header("Origin", o))
      .buildAsync(
        URI.create(s"ws://127.0.0.1:$port"),
        new WebSocket.Listener:
          override def onText(ws: WebSocket, data: CharSequence, last: Boolean): CompletionStage[?] =
            text.append(data)
            if last then
              received.add(parse(text.toString).getOrElse(fail(s"not JSON: $text")))
              text.clear()
            ws.request(1)
            null
      )
      .get(10, TimeUnit.SECONDS)

    def send(message: String): Unit = socket.sendText(message, true).get(10, TimeUnit.SECONDS): Unit
    def next(): Json = Option(received.poll(10, TimeUnit.SECONDS)).getOrElse(fail("no message from the server"))
    def close(): Unit = socket.abort()

  private def withServer(body: (JamNode, RpcServer, Client) => Unit): Unit =
    withServerAllowing(Nil)(body)

  private def withServerAllowing(origins: Seq[String])(body: (JamNode, RpcServer, Client) => Unit): Unit =
    withNodes(1, "rpc-server") { nodes =>
      author(nodes, 1 to 1)
      val server = nodes.head.enableRpc(0, allowedOrigins = origins)
      val client = new Client(server.boundPort)
      try body(nodes.head, server, client)
      finally client.close()
    }

  test("a request is answered, and a subscription is told of each new head after its id") {
    withServer { (node, _, client) =>
      client.send("""{"jsonrpc":"2.0","id":1,"method":"bestBlock","params":[]}""")
      val best = client.next()
      best.hcursor.get[Int]("id") shouldBe Right(1)
      best.hcursor.downField("result").get[Long]("slot") shouldBe Right(1L)

      client.send("""{"jsonrpc":"2.0","id":2,"method":"subscribeBestBlock","params":[]}""")
      val subscribed = client.next()
      subscribed.hcursor.get[Int]("id") shouldBe Right(2)
      val id = subscribed.hcursor.get[Long]("result").toOption.get
      val first = client.next().hcursor
      first.get[String]("method") shouldBe Right("subscribeBestBlock")
      first.downField("params").get[Long]("subscription") shouldBe Right(id)
      first.downField("params").downField("result").get[Long]("slot") shouldBe Right(1L)

      author(List(node), 2 to 2)
      client.next().hcursor.downField("params").downField("result").get[Long]("slot") shouldBe Right(2L)
    }
  }

  test("a connection that closes takes its subscriptions with it, and the server stops with the node") {
    withServer { (node, server, client) =>
      client.send("""{"jsonrpc":"2.0","id":1,"method":"subscribeBestBlock","params":[]}""")
      client.next()
      eventually("the connection to be counted")(server.connectionCount == 1)
      client.close()
      eventually("the connection to be forgotten")(server.connectionCount == 0)
      author(List(node), 2 to 2) // nothing to notify, nothing fails
    }
  }

  test("a client pipelining large requests has at most its bound queued, and every request is answered in order") {
    withServer { (_, server, client) =>
      val release = new java.util.concurrent.CountDownLatch(1)
      server.onWorker(() => release.await(60, TimeUnit.SECONDS): Unit)
      val padding = "x" * (3 * 1024 * 1024)
      val large = 30
      val small = 100
      val total = large + small
      def request(id: Int, pad: String) = s"""{"jsonrpc":"2.0","id":$id,"method":"bestBlock","params":[],"pad":"$pad"}"""
      val sender = new Thread(() =>
        (1 to large).foreach(i => client.send(request(i, padding)))
        (large + 1 to total).foreach(i => client.send(request(i, "")))
      )
      sender.setDaemon(true)
      sender.start()
      eventually("the server to stop taking requests")(
        server.peakQueuedBytes > RpcServer.MaxQueuedRequestBytes - padding.length
      )
      Thread.sleep(1000) // what would have been read by now is read
      server.peakQueuedBytes should be <= RpcServer.MaxQueuedRequestBytes.toLong
      server.peakQueuedRequests should be <= RpcServer.MaxQueuedRequests

      release.countDown()
      val ids = (1 to total).map(_ => client.next().hcursor.get[Int]("id").toOption.get)
      ids shouldBe (1 to total)
      sender.join(10000)
      server.peakQueuedRequests should be <= RpcServer.MaxQueuedRequests
      eventually("every request to be counted out")(server.queuedBytesTotal == 0L)
    }
  }

  test("the requests of a connection closed while they wait are counted out") {
    withServer { (_, server, client) =>
      val release = new java.util.concurrent.CountDownLatch(1)
      server.onWorker(() => release.await(60, TimeUnit.SECONDS): Unit)
      (1 to 5).foreach(i => client.send(s"""{"jsonrpc":"2.0","id":$i,"method":"bestBlock","params":[]}"""))
      eventually("the requests to wait")(server.queuedBytesTotal > 0L)
      client.close()
      eventually("the connection to be forgotten")(server.connectionCount == 0)
      release.countDown()
      eventually("its requests to be counted out")(server.queuedBytesTotal == 0L)
    }
  }

  test("a handshake with an Origin not allowed is refused with 403; an allowed one, or none, is accepted") {
    withServerAllowing(List("http://good.example")) { (_, server, client) =>
      client.send("""{"jsonrpc":"2.0","id":1,"method":"bestBlock","params":[]}""")
      client.next().hcursor.get[Int]("id") shouldBe Right(1) // no Origin

      val refused = intercept[java.util.concurrent.ExecutionException](new Client(server.boundPort, Some("http://evil.example")))
      refused.getCause match
        case e: java.net.http.WebSocketHandshakeException => e.getResponse.statusCode shouldBe 403
        case other                                         => fail(s"not refused at the handshake: $other")

      val allowed = new Client(server.boundPort, Some("http://good.example"))
      try
        allowed.send("""{"jsonrpc":"2.0","id":2,"method":"bestBlock","params":[]}""")
        allowed.next().hcursor.get[Int]("id") shouldBe Right(2)
      finally allowed.close()
    }
  }
