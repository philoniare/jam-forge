package io.forge.jam.conformance

import cats.effect.IO
import fs2.io.net.Socket
import io.forge.jam.core.{ChainConfig, JamBytes, Hashing}
import io.forge.jam.core.scodec.JamCodecs
import io.forge.jam.core.scodec.JamCodecs.encode
import io.forge.jam.protocol.report.ReportTypes.AncestorHeader
import io.forge.jam.protocol.traces.{
  BlockImporter,
  ImportResult,
  RawState,
  StateMerklization
}

/** Outcome of a single `readMessage` call.
  *
  *   - `Eof`: peer closed the socket; loop terminates normally.
  *   - `Decoded`: a well-formed protocol message; loop dispatches it.
  *   - `DecodeFailed`: the wire frame was malformed.
  */
sealed trait ReadResult
object ReadResult:
  case object Eof extends ReadResult
  final case class Decoded(msg: ProtocolMessage, size: Int) extends ReadResult
  final case class DecodeFailed(reason: String, size: Int) extends ReadResult

/** Protocol handler for the conformance testing session.
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
  // BlockImporter will be created after handshake when we know the negotiated features.
  private var blockImporter: BlockImporter = _

  // Session features after negotiation
  private var sessionFeatures: Int = 0
  private var skipAncestryValidation: Boolean = true

  /** Handle a connection from the fuzzer.
    */
  def handleConnection(socket: Socket[IO]): IO[Unit] =
    for
      _ <- logger.logInfo("Connection established")
      _ <- connectionLoop(socket)
    yield ()

  private def connectionLoop(socket: Socket[IO]): IO[Unit] =
    logger.logInfo("Waiting for next message...") *>
      readMessage(socket)
        .flatMap {
          case ReadResult.Decoded(msg, size) =>
            logger.logInfo(
              s"Received message of size $size bytes, processing..."
            ) *>
              handleMessage(msg, size, socket).handleErrorWith { error =>
                // Log error but try to send error response and continue processing
                IO.println(
                  s"[JAM-FORGE ERROR] Error handling message: ${error.getClass.getSimpleName} - ${Option(error.getMessage).getOrElse("unknown")}"
                ) *>
                  logger.logError(
                    s"Error handling message: ${error.getClass.getSimpleName}",
                    error
                  ) *>
                  (for _ <- sendMessage(
                      socket,
                      ProtocolMessage.ErrorMsg(
                        Error(
                          s"Internal error: ${error.getClass.getSimpleName} - ${Option(error.getMessage).getOrElse("unknown")}"
                        )
                      )
                    )
                  yield ()).handleErrorWith { sendError =>
                    // If we can't even send the error response, the socket is dead
                    logger.logError(
                      s"Failed to send error response, connection lost",
                      sendError
                    ) *>
                      IO.raiseError(sendError)
                  }
              } *> connectionLoop(socket)

          case ReadResult.DecodeFailed(reason, size) =>
            IO.println(
              s"[JAM-FORGE WARN] Decode failure on $size-byte frame: $reason"
            ) *>
              logger.logWarning(
                s"Decode failure on $size-byte frame: $reason"
              ) *>
              sendMessage(
                socket,
                ProtocolMessage.ErrorMsg(
                  Error(s"Decode failure: $reason")
                )
              ).handleErrorWith { sendError =>
                logger.logError(
                  "Failed to send decode-error response, connection lost",
                  sendError
                ) *> IO.raiseError(sendError)
              } *> connectionLoop(socket)

          case ReadResult.Eof =>
            logger.logInfo("Connection closed by peer (read returned None)")
        }
        .handleErrorWith { error =>
          IO.println(
            s"[JAM-FORGE ERROR] Protocol error in connectionLoop: ${error.getClass.getSimpleName} - ${Option(error.getMessage).getOrElse("unknown")}"
          ) *>
            IO.blocking { error.printStackTrace(System.err) } *>
            logger.logError(
              s"Protocol error in connectionLoop, connection terminated: ${error.getClass.getSimpleName}",
              error
            )
        }

  /** Read a length-prefixed message from the socket.
    */
  private def readMessage(socket: Socket[IO]): IO[ReadResult] =
    // Read 4-byte length prefix
    socket
      .read(4)
      .flatMap {
        case None =>
          logger.logInfo(
            "readMessage: socket.read(4) returned None - connection closed"
          ) *>
            IO.pure(ReadResult.Eof)
        case Some(lengthChunk) if lengthChunk.size < 4 =>
          logger.logWarning(
            s"readMessage: got partial length header (${lengthChunk.size} bytes instead of 4)"
          ) *>
            IO.pure(ReadResult.Eof)
        case Some(lengthChunk) =>
          val length = JamCodecs.decodeU32LE(lengthChunk.toArray, 0).signed
          logger.logInfo(
            s"readMessage: length prefix indicates $length bytes"
          ) *>
            (if length <= 0 then
               IO.raiseError(
                 new IllegalArgumentException(
                   s"Invalid message length: $length"
                 )
               )
             else if length > 100_000_000 then
               IO.raiseError(
                 new IllegalArgumentException(
                   s"Message length too large: $length (max 100MB)"
                 )
               )
             else
               readExactly(socket, length).flatMap { bodyBytes =>
                 IO.blocking {
                   try
                     val jamBytes = JamBytes(bodyBytes)
                     val (msg, _) =
                       ProtocolMessage.decodeMessage(jamBytes, 0, config)
                     ReadResult.Decoded(msg, length)
                   catch
                     case e: Throwable =>
                       val reason =
                         s"${e.getClass.getSimpleName} - ${Option(e.getMessage).getOrElse("unknown")}"
                       ReadResult.DecodeFailed(reason, length)
                 }
               })
      }
      .handleErrorWith { error =>
        logger.logError(
          s"readMessage failed: ${error.getClass.getSimpleName}",
          error
        ) *>
          IO.raiseError(error)
      }

  /** Read exactly n bytes from the socket.
    */
  private def readExactly(socket: Socket[IO], n: Int): IO[Array[Byte]] =
    val buf = new Array[Byte](n)
    def loop(offset: Int): IO[Array[Byte]] =
      val remaining = n - offset
      if remaining <= 0 then IO.pure(buf)
      else
        socket.read(remaining).flatMap {
          case None =>
            logger.logError(
              s"readExactly: early EOF - expected $remaining more bytes, got $offset so far",
              new java.io.EOFException(s"Expected $remaining more bytes")
            ) *>
              IO.raiseError(
                new java.io.EOFException(
                  s"Expected $remaining more bytes (read $offset of $n total)"
                )
              )
          case Some(chunk) =>
            chunk.copyToArray(buf, offset)
            loop(offset + chunk.size)
        }
    loop(0)

  /** Send a protocol message to the socket.
    */
  private def sendMessage(socket: Socket[IO], msg: ProtocolMessage): IO[Unit] =
    val framedBytes = ProtocolMessage.encodeWithFrame(msg)
    val msgType = msg match
      case _: ProtocolMessage.PeerInfoMsg  => "PeerInfo"
      case _: ProtocolMessage.StateRootMsg => "StateRoot"
      case _: ProtocolMessage.StateMsg     => "State"
      case _: ProtocolMessage.ErrorMsg     => "Error"
      case other                           => other.getClass.getSimpleName

    (for
      _ <- logger.logSent(msgType, framedBytes.length - 4)
      _ <- logger.logInfo(
        s"sendMessage: writing ${framedBytes.length} bytes to socket..."
      )
      _ <- socket.write(fs2.Chunk.array(framedBytes.toArray))
      _ <- logger.logInfo(s"sendMessage: write completed successfully")
    yield ()).handleErrorWith { error =>
      logger.logError(
        s"sendMessage failed (broken pipe?): ${error.getClass.getSimpleName}",
        error
      ) *>
        IO.raiseError(error)
    }

  /** Handle a received protocol message.
    */
  private def handleMessage(
      msg: ProtocolMessage,
      size: Int,
      socket: Socket[IO]
  ): IO[Unit] =
    msg match
      case ProtocolMessage.PeerInfoMsg(info) =>
        logger.logReceived(
          "PeerInfo",
          size,
          s"features=0x${info.fuzzFeatures.signed.toHexString}"
        ) *>
          handlePeerInfo(info, socket)

      case ProtocolMessage.InitializeMsg(init) =>
        logger.logReceived(
          "Initialize",
          size,
          s"keyvals=${init.keyvals.size}, ancestry=${init.ancestry.size}"
        ) *>
          handleInitialize(init, socket)

      case ProtocolMessage.ImportBlockMsg(importBlock) =>
        logger.logReceived(
          "ImportBlock",
          size,
          s"slot=${importBlock.block.header.slot.toInt}"
        ) *>
          handleImportBlock(importBlock, socket)

      case ProtocolMessage.GetStateMsg(getState) =>
        logger.logReceived(
          "GetState",
          size,
          s"hash=${getState.headerHash.toHex.take(16)}..."
        ) *>
          handleGetState(getState, socket)

      case other =>
        logger.logWarning(
          s"Unexpected message type: ${other.getClass.getSimpleName}"
        ) *>
          IO.raiseError(
            new IllegalArgumentException(s"Unexpected message: $other")
          )

  /** Handle PeerInfo message (handshake).
    *
    * Feature negotiation: session features are the intersection of fuzzer and
    * target features. The target advertises all features it supports (ALL_M1).
    */
  private def handlePeerInfo(info: PeerInfo, socket: Socket[IO]): IO[Unit] =
    // We support all M1 features
    val targetFeatures = Features.ALL_M1

    // Session features are the intersection of fuzzer and target features
    sessionFeatures = info.fuzzFeatures.signed & targetFeatures

    // Log the negotiated features
    val hasAncestry = (sessionFeatures & Features.ANCESTRY) != 0
    val hasForks = (sessionFeatures & Features.FORKS) != 0

    // Remember negotiated flags so we can rebuild the BlockImporter on every Initialize.
    skipAncestryValidation = !hasAncestry
    blockImporter = new BlockImporter(config, skipAncestryValidation)
    stateStore.setForksEnabled(hasForks)

    val response =
      ProtocolMessage.PeerInfoMsg(PeerInfo.forTarget(targetFeatures))
    sendMessage(socket, response) *>
      logger.logInfo(
        s"Handshake complete, negotiated features=0x${sessionFeatures.toHexString} (ancestry=$hasAncestry, forks=$hasForks, skipAncestryValidation=$skipAncestryValidation)"
      )

  /** Handle Initialize message.
    */
  private def handleInitialize(init: Initialize, socket: Socket[IO]): IO[Unit] =
    IO {
      blockImporter = new BlockImporter(config, skipAncestryValidation)

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
      logger.logInfo(
        s"Initialize: stored state with root ${stateRoot.toHex.take(16)}..."
      ) *>
        sendMessage(socket, response)
    }

  /** Handle ImportBlock message.
    */
  private def handleImportBlock(
      importBlock: ImportBlock,
      socket: Socket[IO]
  ): IO[Unit] =
    (IO
      .blocking {
        try
          val block = importBlock.block
          val parentHash = block.header.parent

          // Look up parent state
          stateStore.get(parentHash) match
            case None =>
              Left(s"Parent state not found: ${parentHash.toHex.take(16)}...")
            case Some(parentState) =>
              val ancestry = stateStore.getAncestry.map(a =>
                AncestorHeader(a.slot.value.toLong & 0xffffffffL, a.headerHash)
              )
              // Import block using existing BlockImporter
              blockImporter.importBlock(block, parentState, ancestry) match
                case ImportResult.Success(postStateRoot, _) =>
                  // Compute header hash for this block
                  val headerBytes = block.header.encode
                  val headerHash = Hashing.blake2b256(headerBytes)

                  val postState = blockImporter.materializePostState(config)

                  val isOriginal = stateStore.isOriginalBlock(parentHash)
                  stateStore.store(headerHash, postState, isOriginal)

                  if isOriginal then
                    stateStore.addToAncestry(
                      AncestryItem(block.header.slot, headerHash)
                    )

                  Right(postStateRoot)

                case ImportResult.Failure(error, message) =>
                  Left(s"Import failed: $error - $message")
        catch
          case e: Throwable =>
            Left(
              s"Import exception: ${e.getClass.getSimpleName} - ${Option(e.getMessage).getOrElse("unknown")}"
            )
      }
      .flatMap {
        case Right(stateRoot) =>
          val response = ProtocolMessage.StateRootMsg(StateRoot(stateRoot))
          logger.logInfo(
            s"ImportBlock: success, root=${stateRoot.toHex.take(16)}..."
          ) *>
            sendMessage(socket, response)

        case Left(errorMsg) =>
          val response = ProtocolMessage.ErrorMsg(Error(errorMsg))
          logger.logInfo(s"ImportBlock: failed - $errorMsg") *>
            sendMessage(socket, response)
      })
      .handleErrorWith { error =>
        // Catch-all: if anything escaped above (e.g. OOM, StackOverflow), still try to respond
        val msg =
          s"Fatal import error: ${error.getClass.getSimpleName} - ${Option(error.getMessage).getOrElse("unknown")}"
        IO.println(s"[JAM-FORGE ERROR] $msg") *>
          IO.blocking { error.printStackTrace(System.err) } *>
          logger.logError(msg, error) *>
          sendMessage(socket, ProtocolMessage.ErrorMsg(Error(msg)))
            .handleErrorWith(_ => IO.unit)
      }

  /** Handle GetState message.
    */
  private def handleGetState(getState: GetState, socket: Socket[IO]): IO[Unit] =
    IO(stateStore.get(getState.headerHash)).flatMap {
      case Some(rawState) =>
        val response = ProtocolMessage.StateMsg(State(rawState.keyvals))
        logger.logInfo(
          s"GetState: returning ${rawState.keyvals.size} keyvals"
        ) *>
          sendMessage(socket, response)

      case None =>
        // Out-of-protocol error: close connection
        logger.logWarning(
          s"GetState: hash not found ${getState.headerHash.toHex.take(16)}..."
        ) *>
          IO.raiseError(
            new IllegalStateException(
              s"State not found: ${getState.headerHash.toHex}"
            )
          )
    }
