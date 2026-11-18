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
    withServerUsing(origins)(body)

  private def withServerUsing(
      origins: Seq[String] = Nil,
      methods: RpcMethods = RpcMethods.Auto,
      limits: RpcServer.Limits = RpcServer.Limits()
  )(body: (JamNode, RpcServer, Client) => Unit): Unit =
    withNodes(1, "rpc-server") { nodes =>
      author(nodes, 1 to 1)
      val server = nodes.head.enableRpc(0, allowedOrigins = origins, methods = methods, limits = limits)
      val client = new Client(server.boundPort)
      try body(nodes.head, server, client)
      finally client.close()
    }

  private def request(id: Int, method: String, params: String = ""): String =
    s"""{"jsonrpc":"2.0","id":$id,"method":"$method","params":[$params]}"""

  private def errorCode(response: Json): Option[Int] = response.hcursor.downField("error").get[Int]("code").toOption

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

  private val unsafeCalls: List[(String, String)] =
    val blob = "\"" + java.util.Base64.getEncoder.encodeToString(Array[Byte](1, 2, 3)) + "\""
    val hash = "\"" + java.util.Base64.getEncoder.encodeToString(new Array[Byte](32)) + "\""
    List(
      "submitPreimage" -> s"0,$blob",
      "submitWorkPackage" -> s"0,$blob,[]",
      "submitWorkPackageBundle" -> s"0,$blob",
      "fetchSegments" -> s"$hash,[0]",
      "fetchWorkPackageSegments" -> s"$hash,[0]"
    )

  test("an RPC serving safe methods only refuses the unsafe ones with a JSON-RPC error and answers the rest") {
    withServerUsing(methods = RpcMethods.Safe) { (_, _, client) =>
      unsafeCalls.zipWithIndex.foreach { case ((method, params), i) =>
        client.send(request(i + 1, method, params))
        val answer = client.next()
        withClue(s"$method: ") {
          answer.hcursor.get[Int]("id") shouldBe Right(i + 1)
          errorCode(answer) shouldBe Some(RpcError.UnsafeMethodCode)
          answer.hcursor.downField("error").get[String]("message") shouldBe Right("Method not allowed")
        }
      }
      client.send(request(10, "bestBlock"))
      client.next().hcursor.downField("result").get[Long]("slot") shouldBe Right(1L)
    }
  }

  test("auto, the default, serves the unsafe methods on a loopback address only; unsafe serves them anywhere") {
    withServerUsing() { (_, _, client) =>
      unsafeCalls.zipWithIndex.foreach { case ((method, params), i) =>
        client.send(request(i + 1, method, params))
        withClue(s"$method: ")(errorCode(client.next()) should not be Some(RpcError.UnsafeMethodCode))
      }
    }
    def at(ip: String) = java.net.InetAddress.getByName(ip)
    for loopback <- List("127.0.0.1", "127.0.0.2", "::1") do
      RpcMethods.allowsUnsafe(RpcMethods.Auto, at(loopback)) shouldBe true
      RpcMethods.allowsUnsafe(RpcMethods.Safe, at(loopback)) shouldBe false
    for other <- List("0.0.0.0", "::", "10.1.2.3", "192.168.1.20", "2001:db8::1") do
      RpcMethods.allowsUnsafe(RpcMethods.Auto, at(other)) shouldBe false
      RpcMethods.allowsUnsafe(RpcMethods.Unsafe, at(other)) shouldBe true
    RpcMethods.parse("safe") shouldBe Some(RpcMethods.Safe)
    RpcMethods.parse("unsafe") shouldBe Some(RpcMethods.Unsafe)
    RpcMethods.parse("auto") shouldBe Some(RpcMethods.Auto)
    RpcMethods.parse("all") shouldBe None
  }

  test("one address may hold only so many connections; the next is refused at the handshake until one closes") {
    withServerUsing(limits = RpcServer.Limits(connectionsPerIp = 2)) { (_, server, first) =>
      val second = new Client(server.boundPort)
      try
        eventually("two connections")(server.connectionCount == 2)
        val refused = intercept[java.util.concurrent.ExecutionException](new Client(server.boundPort))
        refused.getCause match
          case e: java.net.http.WebSocketHandshakeException => e.getResponse.statusCode shouldBe 429
          case other                                         => fail(s"not refused at the handshake: $other")
        server.connectionCount shouldBe 2
        second.send(request(1, "bestBlock"))
        second.next().hcursor.get[Int]("id") shouldBe Right(1)
        first.close()
        eventually("the closed connection to be forgotten")(server.connectionCount == 1)
        val third = new Client(server.boundPort)
        try
          third.send(request(2, "bestBlock"))
          third.next().hcursor.get[Int]("id") shouldBe Right(2)
        finally third.close()
      finally second.close()
    }
  }

  test("requests from one address over its rate are answered with an error, in order, and the connection stays") {
    withServerUsing(limits = RpcServer.Limits(requestsPerSecond = 0.01, burst = 3)) { (_, server, client) =>
      (1 to 5).foreach(i => client.send(request(i, "bestBlock")))
      val answers = (1 to 5).map(_ => client.next())
      answers.map(_.hcursor.get[Int]("id").toOption.get) shouldBe (1 to 5)
      answers.take(3).foreach(_.hcursor.downField("result").get[Long]("slot") shouldBe Right(1L))
      answers.drop(3).foreach(errorCode(_) shouldBe Some(RpcError.LimitExceededCode))
      val other = new Client(server.boundPort)
      try
        other.send(request(6, "bestBlock"))
        errorCode(other.next()) shouldBe Some(RpcError.LimitExceededCode)
      finally other.close()
      server.connectionCount should be >= 1
    }
  }
