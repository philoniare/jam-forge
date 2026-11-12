package io.forge.jam.network

import java.net.{DatagramSocket, InetAddress, InetSocketAddress}
import java.util.concurrent.TimeUnit

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** A dialing node's UDP socket is of its peer's address family. A dual-stack socket can be given a port that an
  * IPv4 socket of another program already holds; it then sends its handshake but the peer's replies go to that
  * other socket, and the connection attempt hangs until it times out.
  */
class ClientSocketFamilySpec extends AnyFunSuite with Matchers:

  test("connecting to an IPv4 peer never hangs, however many IPv4 ports other sockets hold") {
    val alpn = "abcd1234"
    val server = new JamnpNode(NodeIdentity.generate(), JamnpConfig(genesisHashPrefix = alpn))
    val client = new JamnpNode(NodeIdentity.generate(), JamnpConfig(genesisHashPrefix = alpn))
    // Other programs' IPv4 sockets: enough of the port range that a socket ignoring them soon lands on one
    // (about one connection in eleven here; a machine with a handful of such sockets sees one in thousands).
    val others = List.newBuilder[DatagramSocket]
    try
      val loopback = InetAddress.getByName("127.0.0.1")
      try for _ <- 1 to 1500 do others += new DatagramSocket(new InetSocketAddress(loopback, 0))
      catch case _: java.net.SocketException => () // out of descriptors: go on with those we have
      val held = others.result()
      if held.size < 500 then cancel(s"only ${held.size} sockets could be opened")

      server.start(new InetSocketAddress("127.0.0.1", 0))
      val address = new InetSocketAddress("127.0.0.1", server.boundPort)
      for i <- 1 to 80 do
        val conn =
          try client.connect(address).get(4, TimeUnit.SECONDS)
          catch case e: Exception => fail(s"connection $i did not complete: $e")
        conn.close()
    finally
      others.result().foreach(_.close())
      client.shutdown()
      server.shutdown()
  }
