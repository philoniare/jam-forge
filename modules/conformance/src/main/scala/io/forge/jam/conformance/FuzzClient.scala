package io.forge.jam.conformance

import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.protocol.traces.{Genesis, TraceStep}
import spire.math.{UByte, UInt}

import java.io.EOFException
import java.net.{StandardProtocolFamily, UnixDomainSocketAddress}
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.channels.SocketChannel
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** Fuzzer side of the fuzz protocol over a Unix socket, with blocking NIO: one request, one reply. Used by the warmup
  * to drive the server's own socket path, and by the fuzz-perf replay and the server tests.
  */
final class FuzzClient private (channel: SocketChannel, config: ChainConfig) extends AutoCloseable:

  /** Send `msg` and wait for the target's reply. */
  def request(msg: ProtocolMessage): ProtocolMessage =
    sendRaw(ProtocolMessage.encodeWithFrame(msg, config))
    readReply()

  /** Write bytes as they are, for tests that send malformed or split frames. */
  def sendRaw(bytes: Array[Byte]): Unit =
    val out = ByteBuffer.wrap(bytes)
    while out.hasRemaining do channel.write(out)

  /** Read one framed reply. Throws EOFException when the target closed the connection. */
  def readReply(): ProtocolMessage =
    val len = readFully(4).order(ByteOrder.LITTLE_ENDIAN).getInt(0)
    ProtocolMessage.decodeMessage(JamBytes.wrap(readFully(len).array()), 0, config)._1

  private def readFully(n: Int): ByteBuffer =
    val buf = ByteBuffer.allocate(n)
    while buf.hasRemaining do
      if channel.read(buf) < 0 then throw new EOFException("Target closed the connection")
    buf

  def close(): Unit = channel.close()

object FuzzClient:
  /** Connect to `socket`, retrying for up to `waitSecs` while the target is not listening yet. */
  def connect(socket: Path, config: ChainConfig, waitSecs: Int = 0): FuzzClient =
    val deadline = System.nanoTime() + waitSecs * 1_000_000_000L
    var client: FuzzClient = null
    while client == null do
      val c = SocketChannel.open(StandardProtocolFamily.UNIX)
      try
        c.connect(UnixDomainSocketAddress.of(socket))
        client = new FuzzClient(c, config)
      catch
        case e: java.io.IOException =>
          c.close()
          if System.nanoTime() > deadline then throw new java.io.IOException(s"No target on $socket", e)
          Thread.sleep(100)
    client

  /** The handshake fuzz-perf's fuzzer sends: forks feature only, no ancestry. */
  def fuzzerPeerInfo(appName: String): ProtocolMessage =
    ProtocolMessage.PeerInfoMsg(
      PeerInfo(UByte(1), UInt(Features.FORKS), Version.JAM_VERSION, Version(UByte(0), UByte(1), UByte(0)), appName)
    )

  /** A binary trace directory: `genesis.bin` plus one `NNNNNNNN.bin` per step (the jamtestvectors/traces layout). */
  final case class TraceDir(genesis: Path, steps: Vector[Path]):
    def readGenesis(config: ChainConfig): Genesis =
      Genesis.codec(config.validatorCount, config.epochLength).decodeUnsafe(Files.readAllBytes(genesis))
    def readStep(step: Path, config: ChainConfig): TraceStep =
      TraceStep
        .codec(config.validatorCount, config.epochLength, config.coresCount, config.votesPerVerdict)
        .decodeUnsafe(Files.readAllBytes(step))

  object TraceDir:
    /** The binary trace in `dir`, if it has a genesis.bin. Steps are sorted by file name. */
    def find(dir: Path): Option[TraceDir] =
      val genesis = dir.resolve("genesis.bin")
      Option.when(Files.isRegularFile(genesis)) {
        val steps = Using.resource(Files.list(dir))(
          _.iterator().asScala.filter(_.getFileName.toString.matches("\\d+\\.bin")).toVector
        )
        TraceDir(genesis, steps.sortBy(_.getFileName.toString))
      }
