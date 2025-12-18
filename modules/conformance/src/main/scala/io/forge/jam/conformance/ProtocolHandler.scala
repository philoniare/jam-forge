package io.forge.jam.conformance

import cats.effect.IO
import fs2.io.net.Socket
import io.forge.jam.core.{ChainConfig, JamBytes, Hashing}
import io.forge.jam.core.scodec.JamCodecs
import io.forge.jam.core.scodec.JamCodecs.encode
import io.forge.jam.protocol.traces.{BlockImporter, ImportResult, RawState, StateMerklization}

/**
 * Protocol handler for the conformance testing session.
 *
 * Manages:
 * - Handshake and feature negotiation
 * - Message routing to appropriate handlers
 * - Protocol state machine
 */
class ProtocolHandler(
  stateStore: StateStore,
  logger: FileLogger,
  config: ChainConfig = ChainConfig.TINY
):
  // BlockImporter will be created after handshake when we know the negotiated features
  private var blockImporter: BlockImporter = _

  // Session features after negotiation
  private var sessionFeatures: Int = 0

  /**
   * Handle a connection from the fuzzer.
   */
  def handleConnection(socket: Socket[IO]): IO[Unit] =
    for
      _ <- logger.logInfo("Connection established")
      _ <- connectionLoop(socket)
    yield ()

  private def connectionLoop(socket: Socket[IO]): IO[Unit] =
    readMessage(socket).flatMap {
      case Some((msg, size)) =>
        handleMessage(msg, size, socket) *> connectionLoop(socket)
      case None =>
        logger.logInfo("Connection closed by peer")
    }.handleErrorWith { error =>
      logger.logError("Protocol error", error) *>
        IO.raiseError(error)
    }

  /**
   * Read a length-prefixed message from the socket.
   */
  private def readMessage(socket: Socket[IO]): IO[Option[(ProtocolMessage, Int)]] =
    // Read 4-byte length prefix
    socket.read(4).flatMap {
      case None =>
        IO.pure(None)
      case Some(lengthChunk) if lengthChunk.size < 4 =>
        IO.pure(None)
      case Some(lengthChunk) =>
        val length = JamCodecs.decodeU32LE(lengthChunk.toArray, 0).signed
        if length <= 0 || length > 10_000_000 then
          IO.raiseError(new IllegalArgumentException(s"Invalid message length: $length"))
        else
          readExactly(socket, length).flatMap { bodyBytes =>
            val jamBytes = JamBytes(bodyBytes)
            val (msg, _) = ProtocolMessage.decodeMessage(jamBytes, 0, config)
            IO.pure(Some((msg, length)))
          }
    }

  /**
   * Read exactly n bytes from the socket.
   */
  private def readExactly(socket: Socket[IO], n: Int): IO[Array[Byte]] =
    def loop(remaining: Int, acc: Array[Byte]): IO[Array[Byte]] =
      if remaining <= 0 then IO.pure(acc)
      else
        socket.read(remaining).flatMap {
          case None =>
            IO.raiseError(new java.io.EOFException(s"Expected $remaining more bytes"))
          case Some(chunk) =>
            val chunkArr = chunk.toArray
            loop(remaining - chunkArr.length, acc ++ chunkArr)
        }
    loop(n, Array.empty)

  /**
   * Send a protocol message to the socket.
   */
  private def sendMessage(socket: Socket[IO], msg: ProtocolMessage): IO[Unit] =
    val framedBytes = ProtocolMessage.encodeWithFrame(msg)
    val msgType = msg match
      case _: ProtocolMessage.PeerInfoMsg => "PeerInfo"
      case _: ProtocolMessage.StateRootMsg => "StateRoot"
      case _: ProtocolMessage.StateMsg => "State"
      case _: ProtocolMessage.ErrorMsg => "Error"
      case other => other.getClass.getSimpleName

    for
      _ <- logger.logSent(msgType, framedBytes.length - 4)
      _ <- socket.write(fs2.Chunk.array(framedBytes.toArray))
    yield ()

  /**
   * Handle a received protocol message.
   */
  private def handleMessage(msg: ProtocolMessage, size: Int, socket: Socket[IO]): IO[Unit] =
    msg match
      case ProtocolMessage.PeerInfoMsg(info) =>
        logger.logReceived("PeerInfo", size, s"features=0x${info.fuzzFeatures.signed.toHexString}") *>
          handlePeerInfo(info, socket)

      case ProtocolMessage.InitializeMsg(init) =>
        logger.logReceived("Initialize", size, s"keyvals=${init.keyvals.size}, ancestry=${init.ancestry.size}") *>
          handleInitialize(init, socket)

      case ProtocolMessage.ImportBlockMsg(importBlock) =>
        logger.logReceived("ImportBlock", size, s"slot=${importBlock.block.header.slot.toInt}") *>
          handleImportBlock(importBlock, socket)

      case ProtocolMessage.GetStateMsg(getState) =>
        logger.logReceived("GetState", size, s"hash=${getState.headerHash.toHex.take(16)}...") *>
          handleGetState(getState, socket)

      case other =>
        logger.logWarning(s"Unexpected message type: ${other.getClass.getSimpleName}") *>
          IO.raiseError(new IllegalArgumentException(s"Unexpected message: $other"))

  /**
   * Handle PeerInfo message (handshake).
   *
   * Feature negotiation: session features are the intersection of fuzzer and target features.
   * The target advertises all features it supports (ALL_M1).
   */
  private def handlePeerInfo(info: PeerInfo, socket: Socket[IO]): IO[Unit] =
    // We support all M1 features
    val targetFeatures = Features.ALL_M1

    // Session features are the intersection of fuzzer and target features
    sessionFeatures = info.fuzzFeatures.signed & targetFeatures

    // Log the negotiated features
    val hasAncestry = (sessionFeatures & Features.ANCESTRY) != 0
    val hasForks = (sessionFeatures & Features.FORKS) != 0

    // Create BlockImporter with the correct skipAncestryValidation flag
    val skipAncestryValidation = !hasAncestry
    blockImporter = new BlockImporter(config, skipAncestryValidation)

    val response = ProtocolMessage.PeerInfoMsg(PeerInfo.forTarget(targetFeatures))
    sendMessage(socket, response) *>
      logger.logInfo(
        s"Handshake complete, negotiated features=0x${sessionFeatures.toHexString} (ancestry=$hasAncestry, forks=$hasForks, skipAncestryValidation=$skipAncestryValidation)"
      )

  /**
   * Handle Initialize message.
   */
  private def handleInitialize(init: Initialize, socket: Socket[IO]): IO[Unit] =
    IO {
      // Compute header hash
      val headerBytes = init.header.encode
      val headerHash = Hashing.blake2b256(headerBytes)

      // Compute state root from keyvals
      val stateRoot = StateMerklization.stateMerklize(init.keyvals)

      // Create RawState and store
      val rawState = RawState(stateRoot, init.keyvals)
      stateStore.initialize(headerHash, rawState, init.ancestry)

      stateRoot
    }.flatMap { stateRoot =>
      val response = ProtocolMessage.StateRootMsg(StateRoot(stateRoot))
      logger.logInfo(s"Initialize: stored state with root ${stateRoot.toHex.take(16)}...") *>
        sendMessage(socket, response)
    }

  /**
   * Handle ImportBlock message.
   */
  private def handleImportBlock(importBlock: ImportBlock, socket: Socket[IO]): IO[Unit] =
    IO {
      val block = importBlock.block
      val parentHash = block.header.parent

      // Look up parent state
      stateStore.get(parentHash) match
        case None =>
          Left(s"Parent state not found: ${parentHash.toHex.take(16)}...")
        case Some(parentState) =>
          // Import block using existing BlockImporter
          blockImporter.importBlock(block, parentState) match
            case ImportResult.Success(postState, _, _) =>
              // Compute header hash for this block
              val headerBytes = block.header.encode
              val headerHash = Hashing.blake2b256(headerBytes)

              // Store the new state
              val isOriginal = stateStore.isOriginalBlock(parentHash)
              stateStore.store(headerHash, postState, isOriginal)

              // Update ancestry if this is an original block
              if isOriginal then
                stateStore.addToAncestry(AncestryItem(block.header.slot, headerHash))

              Right(postState.stateRoot)

            case ImportResult.Failure(error, message) =>
              Left(s"Import failed: $error - $message")
    }.flatMap {
      case Right(stateRoot) =>
        val response = ProtocolMessage.StateRootMsg(StateRoot(stateRoot))
        logger.logInfo(s"ImportBlock: success, root=${stateRoot.toHex.take(16)}...") *>
          sendMessage(socket, response)

      case Left(errorMsg) =>
        val response = ProtocolMessage.ErrorMsg(Error(errorMsg))
        logger.logInfo(s"ImportBlock: failed - $errorMsg") *>
          sendMessage(socket, response)
    }

  /**
   * Handle GetState message.
   */
  private def handleGetState(getState: GetState, socket: Socket[IO]): IO[Unit] =
    IO(stateStore.get(getState.headerHash)).flatMap {
      case Some(rawState) =>
        val response = ProtocolMessage.StateMsg(State(rawState.keyvals))
        logger.logInfo(s"GetState: returning ${rawState.keyvals.size} keyvals") *>
          sendMessage(socket, response)

      case None =>
        // Out-of-protocol error: close connection
        logger.logWarning(s"GetState: hash not found ${getState.headerHash.toHex.take(16)}...") *>
          IO.raiseError(new IllegalStateException(s"State not found: ${getState.headerHash.toHex}"))
    }
