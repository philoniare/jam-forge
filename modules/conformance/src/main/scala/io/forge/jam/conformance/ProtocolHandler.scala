package io.forge.jam.conformance

import io.forge.jam.core.{ChainConfig, JamBytes, Hashing}
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.protocol.traces.{BlockImporter, RawState, StateMerklization}

import java.io.EOFException
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.channels.{ByteChannel, ClosedChannelException}
import scala.util.control.NonFatal

/** Outcome of a single `readMessage` call.
  *
  *   - `Eof`: peer closed the socket between frames; loop terminates normally.
  *   - `Decoded`: a well-formed protocol message; loop dispatches it.
  *   - `DecodeFailed`: the wire frame was malformed.
  */
sealed trait ReadResult
object ReadResult:
  case object Eof extends ReadResult
  final case class Decoded(msg: ProtocolMessage, size: Int) extends ReadResult
  final case class DecodeFailed(reason: String, size: Int) extends ReadResult

/** Protocol handler for one fuzzer session (one connection), run on the server thread with blocking reads and
  * writes. The protocol is strictly request/response, so there is nothing to overlap and no reason to leave the
  * thread between reading a frame and writing its reply.
  *
  * Manages:
  *   - Handshake and feature negotiation
  *   - Message routing to appropriate handlers
  *   - Protocol state machine
  */
class ProtocolHandler(
    stateStore: StateStore,
    logger: FileLogger,
    config: ChainConfig
):
  // BlockImporter will be created by Initialize, once the handshake has fixed the negotiated features.
  private var blockImporter: BlockImporter = _

  // Session features after negotiation
  private var sessionFeatures: Int = 0
  private var skipAncestryValidation: Boolean = true

  private val lengthBuf = ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN)

  /** Serve one connection until the peer closes it, a frame fails to decode, or an out-of-protocol error ends the
    * session. Non-fatal errors are logged here and end only this connection.
    */
  def handleConnection(channel: ByteChannel): Unit =
    logger.logInfo("Connection established")
    try connectionLoop(channel)
    catch
      // Only this side closes the channel (FuzzServer.close); a peer that goes away shows up as EOF or an IOException.
      case _: ClosedChannelException =>
        logger.logInfo("Connection closed by server shutdown")
      case NonFatal(error) =>
        println(
          s"[JAM-FORGE ERROR] Protocol error, connection terminated: ${error.getClass.getSimpleName} - ${Option(error.getMessage).getOrElse("unknown")}"
        )
        error.printStackTrace(System.err)
        logger.logError(s"Protocol error, connection terminated: ${error.getClass.getSimpleName}", error)

  private def connectionLoop(channel: ByteChannel): Unit =
    var open = true
    while open do
      logger.logInfo("Waiting for next message...")
      readMessage(channel) match
        case ReadResult.Decoded(msg, size) =>
          logger.logInfo(s"Received message of size $size bytes, processing...")
          sendMessage(channel, handleMessage(msg, size))

        case ReadResult.DecodeFailed(reason, size) =>
          println(s"[JAM-FORGE WARN] Decode failure on $size-byte frame, terminating session: $reason")
          logger.logWarning(s"Decode failure on $size-byte frame, terminating session: $reason")
          open = false

        case ReadResult.Eof =>
          logger.logInfo("Connection closed by peer")
          open = false

  /** Read a length-prefixed message from the channel. EOF before the first length byte ends the session normally;
    * EOF anywhere else inside a frame is an error.
    */
  private def readMessage(channel: ByteChannel): ReadResult =
    lengthBuf.clear()
    if !readExactly(channel, lengthBuf, eofAllowed = true) then ReadResult.Eof
    else
      val length = lengthBuf.getInt(0)
      logger.logInfo(s"readMessage: length prefix indicates $length bytes")
      if length <= 0 then throw new IllegalArgumentException(s"Invalid message length: $length")
      if length > ProtocolHandler.MaxFrameBytes then
        throw new IllegalArgumentException(s"Message length too large: $length (max 100MB)")
      val body = ByteBuffer.allocate(length)
      readExactly(channel, body, eofAllowed = false)
      try
        // Decoded in place: nothing else holds the frame array.
        val (msg, _) = ProtocolMessage.decodeMessage(JamBytes.wrap(body.array()), 0, config)
        ReadResult.Decoded(msg, length)
      catch
        case NonFatal(e) =>
          ReadResult.DecodeFailed(s"${e.getClass.getSimpleName} - ${Option(e.getMessage).getOrElse("unknown")}", length)

  /** Fill `buf`. Returns false only when `eofAllowed` and the peer closed before sending any byte of it. */
  private def readExactly(channel: ByteChannel, buf: ByteBuffer, eofAllowed: Boolean): Boolean =
    var eof = false
    while buf.hasRemaining && !eof do
      if channel.read(buf) < 0 then eof = true
    if !eof then true
    else if eofAllowed && buf.position() == 0 then false
    else throw new EOFException(s"Expected ${buf.remaining()} more bytes (read ${buf.position()} of ${buf.limit()})")

  /** Send a protocol message to the channel.
    */
  private def sendMessage(channel: ByteChannel, msg: ProtocolMessage): Unit =
    val framed = ProtocolMessage.encodeWithFrame(msg, config)
    logger.logSent(messageType(msg), framed.length - 4)
    val out = ByteBuffer.wrap(framed)
    while out.hasRemaining do channel.write(out)
    logger.logInfo(s"sendMessage: wrote ${framed.length} bytes")

  private def messageType(msg: ProtocolMessage): String = msg match
    case _: ProtocolMessage.PeerInfoMsg  => "PeerInfo"
    case _: ProtocolMessage.StateRootMsg => "StateRoot"
    case _: ProtocolMessage.StateMsg     => "State"
    case _: ProtocolMessage.ErrorMsg     => "Error"
    case other                           => other.getClass.getSimpleName

  /** Handle a received protocol message and return the reply. Throws for out-of-protocol requests (unexpected message
    * type, GetState for an unknown block, an importer exception), which end the session.
    */
  private[conformance] def handleMessage(msg: ProtocolMessage, size: Int): ProtocolMessage =
    msg match
      case ProtocolMessage.PeerInfoMsg(info) =>
        logger.logReceived("PeerInfo", size, s"features=0x${info.fuzzFeatures.signed.toHexString}")
        handlePeerInfo(info)

      case ProtocolMessage.InitializeMsg(init) =>
        logger.logReceived("Initialize", size, s"keyvals=${init.keyvals.size}, ancestry=${init.ancestry.size}")
        handleInitialize(init)

      case ProtocolMessage.ImportBlockMsg(importBlock) =>
        logger.logReceived("ImportBlock", size, s"slot=${importBlock.block.header.slot.toInt}")
        handleImportBlock(importBlock)

      case ProtocolMessage.GetStateMsg(getState) =>
        logger.logReceived("GetState", size, s"hash=${getState.headerHash.toHex.take(16)}...")
        handleGetState(getState)

      case other =>
        logger.logWarning(s"Unexpected message type: ${other.getClass.getSimpleName}")
        throw new IllegalArgumentException(s"Unexpected message: $other")

  /** Handle PeerInfo message (handshake).
    *
    * Feature negotiation: session features are the intersection of fuzzer and target features. The target advertises
    * all features it supports (ALL_M1).
    */
  private def handlePeerInfo(info: PeerInfo): ProtocolMessage =
    // We support all M1 features
    val targetFeatures = Features.ALL_M1

    // Session features are the intersection of fuzzer and target features
    sessionFeatures = info.fuzzFeatures.signed & targetFeatures

    val hasAncestry = (sessionFeatures & Features.ANCESTRY) != 0
    val hasForks = (sessionFeatures & Features.FORKS) != 0

    // Remember negotiated flags so handleInitialize can build the BlockImporter
    skipAncestryValidation = !hasAncestry
    stateStore.setForksEnabled(hasForks)

    logger.logInfo(
      s"Handshake complete, negotiated features=0x${sessionFeatures.toHexString} (ancestry=$hasAncestry, forks=$hasForks, skipAncestryValidation=$skipAncestryValidation)"
    )
    ProtocolMessage.PeerInfoMsg(PeerInfo.forTarget(targetFeatures))

  /** Handle Initialize message.
    */
  private def handleInitialize(init: Initialize): ProtocolMessage =
    blockImporter = new BlockImporter(config, skipAncestryValidation)

    val headerHash = Hashing.blake2b256(init.header.encode)
    val stateRoot = StateMerklization.stateMerklize(init.keyvals)
    stateStore.initialize(
      headerHash,
      RawState(stateRoot, init.keyvals),
      init.ancestry,
      Some((init.header.parent, init.header.slot))
    )

    logger.logInfo(s"Initialize: stored state with root ${stateRoot.toHex.take(16)}...")
    ProtocolMessage.StateRootMsg(StateRoot(stateRoot))

  /** Handle ImportBlock message. An invalid block gets an Error reply; an exception from the importer is out of
    * protocol and ends the session.
    */
  private def handleImportBlock(importBlock: ImportBlock): ProtocolMessage =
    val result =
      try ImportBlockStep(stateStore, blockImporter, config, importBlock.block)
      catch
        case NonFatal(error) =>
          throw new IllegalStateException(
            s"Fatal import error: ${error.getClass.getSimpleName} - ${Option(error.getMessage).getOrElse("unknown")}",
            error
          )
    result match
      case Right(stateRoot) =>
        logger.logInfo(s"ImportBlock: success, root=${stateRoot.toHex.take(16)}...")
        ProtocolMessage.StateRootMsg(StateRoot(stateRoot))

      case Left(errorMsg) =>
        logger.logInfo(s"ImportBlock: failed - $errorMsg")
        ProtocolMessage.ErrorMsg(Error(errorMsg))

  /** Handle GetState message.
    */
  private def handleGetState(getState: GetState): ProtocolMessage =
    stateStore.get(getState.headerHash) match
      case Some(rawState) =>
        logger.logInfo(s"GetState: returning ${rawState.keyvals.size} keyvals")
        ProtocolMessage.StateMsg(State(rawState.keyvals))

      case None =>
        // Out-of-protocol error: close connection
        logger.logWarning(s"GetState: hash not found ${getState.headerHash.toHex.take(16)}...")
        throw new IllegalStateException(s"State not found: ${getState.headerHash.toHex}")

object ProtocolHandler:
  /** Largest frame the server will allocate for (100 MB). */
  val MaxFrameBytes: Int = 100_000_000
