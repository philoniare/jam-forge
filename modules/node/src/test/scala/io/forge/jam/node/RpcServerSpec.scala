package io.forge.jam.node

import java.net.URI
import java.net.http.{HttpClient, WebSocket}
import java.util.concurrent.{CompletionStage, LinkedBlockingQueue, TimeUnit}

import io.circe.Json
import io.circe.parser.parse
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RpcServerSpec extends AnyFunSuite with Matchers with LoopbackFixture:

  private final class Client(port: Int):
    val received = new LinkedBlockingQueue[Json]()
    private val text = new StringBuilder
    private val socket = HttpClient
      .newHttpClient()
      .newWebSocketBuilder()
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
    withNodes(1, "rpc-server") { nodes =>
      author(nodes, 1 to 1)
      val server = nodes.head.enableRpc(0)
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
