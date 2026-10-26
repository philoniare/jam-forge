package io.forge.jam.conformance

import io.forge.jam.core.{ChainConfig, Hashing}
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.primitives.Hash
import io.forge.jam.protocol.traces.{StateMerklization, TraceStep}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import java.io.EOFException
import java.nio.file.{Files, Paths}
import scala.util.Using

/** Socket-level tests of the fuzz server: real Unix socket, real frames, a public trace. */
class FuzzServerSpec extends AnyFunSpec with Matchers:

  private given config: ChainConfig = ChainConfig.TINY
  private val baseDir = sys.props.getOrElse("jam.base.dir", System.getProperty("user.dir"))
  private lazy val fallback = FuzzClient.TraceDir.find(Paths.get(baseDir, "jamtestvectors", "traces", "fallback"))

  private def trace: FuzzClient.TraceDir =
    assume(fallback.isDefined, "jamtestvectors/traces/fallback not found")
    fallback.get

  private lazy val steps: Vector[TraceStep] = trace.steps.take(5).map(trace.readStep(_, config))

  private def withServer(test: FuzzServer => Unit): Unit =
    val dir = Files.createTempDirectory("jam-fuzz-server")
    val server = FuzzServer.start(dir.resolve("fuzz.sock"), config, FileLogger.noop)
    try test(server)
    finally server.close()

  private def withClient(server: FuzzServer)(test: FuzzClient => Unit): Unit =
    Using.resource(FuzzClient.connect(server.socketPath, config, waitSecs = 5))(test)

  private def handshake(client: FuzzClient): PeerInfo =
    client.request(FuzzClient.fuzzerPeerInfo("fuzz-server-spec")) match
      case ProtocolMessage.PeerInfoMsg(info) => info
      case other                            => fail(s"Expected PeerInfo, got $other")

  private def initialize(client: FuzzClient): Unit =
    val genesis = trace.readGenesis(config)
    client.request(ProtocolMessage.InitializeMsg(Initialize(genesis.header, genesis.state.keyvals, Nil))) shouldBe
      ProtocolMessage.StateRootMsg(StateRoot(genesis.state.stateRoot))

  private def importBlock(client: FuzzClient, step: TraceStep): ProtocolMessage =
    client.request(ProtocolMessage.ImportBlockMsg(ImportBlock(step.block)))

  private def headerHash(step: TraceStep): Hash = Hashing.blake2b256(step.block.header.encode)

  /** The server ended the session: the next read sees EOF instead of a reply. */
  private def assertClosedByServer(client: FuzzClient): Unit =
    an[EOFException] should be thrownBy client.readReply()

  describe("FuzzServer") {

    it("serves handshake, Initialize, ImportBlock and GetState from the fallback trace") {
      withServer { server =>
        withClient(server) { client =>
          val target = handshake(client)
          target.appName shouldBe "jam-forge"
          target.fuzzFeatures.signed shouldBe Features.ALL_M1
          initialize(client)
          for step <- steps do
            importBlock(client, step) shouldBe ProtocolMessage.StateRootMsg(StateRoot(step.postState.stateRoot))
          client.request(ProtocolMessage.GetStateMsg(GetState(headerHash(steps.last)))) match
            case ProtocolMessage.StateMsg(state) =>
              StateMerklization.stateMerklize(state.keyvals) shouldBe steps.last.postState.stateRoot
            case other => fail(s"Expected State, got $other")
        }
      }
    }

    it("reads a frame whose length prefix and body arrive in pieces") {
      withServer { server =>
        withClient(server) { client =>
          val frame = ProtocolMessage.encodeWithFrame(FuzzClient.fuzzerPeerInfo("split"), config)
          for (from, until) <- Seq((0, 1), (1, 3), (3, 6), (6, frame.length)) do
            client.sendRaw(frame.slice(from, until))
            Thread.sleep(20)
          client.readReply() shouldBe a[ProtocolMessage.PeerInfoMsg]
        }
      }
    }

    it("ends the session on a malformed frame and keeps serving new connections") {
      val unknownDiscriminant = Array[Byte](1, 0, 0, 0, 0x07)
      val truncatedImport = Array[Byte](3, 0, 0, 0, ImportBlock.DISCRIMINANT.toByte, 1, 2)
      val zeroLength = Array[Byte](0, 0, 0, 0)
      withServer { server =>
        for bad <- Seq(unknownDiscriminant, truncatedImport, zeroLength) do
          withClient(server) { client =>
            handshake(client)
            client.sendRaw(bad)
            assertClosedByServer(client)
          }
        withClient(server)(client => handshake(client).appName shouldBe "jam-forge")
      }
    }

    it("ends the session on an unexpected message type or a GetState for an unknown block") {
      withServer { server =>
        withClient(server) { client =>
          handshake(client)
          client.sendRaw(ProtocolMessage.encodeWithFrame(ProtocolMessage.StateRootMsg(StateRoot(Hash.zero)), config))
          assertClosedByServer(client)
        }
        withClient(server) { client =>
          handshake(client)
          initialize(client)
          client.sendRaw(ProtocolMessage.encodeWithFrame(ProtocolMessage.GetStateMsg(GetState(Hash.zero)), config))
          assertClosedByServer(client)
        }
      }
    }

    it("serves sequential connections as fresh sessions") {
      withServer { server =>
        withClient(server) { client =>
          handshake(client)
          initialize(client)
          for step <- steps.take(2) do
            importBlock(client, step) shouldBe ProtocolMessage.StateRootMsg(StateRoot(step.postState.stateRoot))
        }
        withClient(server) { client =>
          handshake(client)
          // Block 3's parent was imported by the previous connection; a new session must not see it.
          importBlock(client, steps(2)) shouldBe a[ProtocolMessage.ErrorMsg]
          initialize(client)
          for step <- steps do
            importBlock(client, step) shouldBe ProtocolMessage.StateRootMsg(StateRoot(step.postState.stateRoot))
        }
      }
    }

    it("stops promptly on close while a client is connected and idle") {
      val dir = Files.createTempDirectory("jam-fuzz-server")
      val server = FuzzServer.start(dir.resolve("fuzz.sock"), config, FileLogger.noop)
      Using.resource(FuzzClient.connect(server.socketPath, config, waitSecs = 5)) { client =>
        handshake(client)
        val t0 = System.nanoTime()
        server.close()
        (System.nanoTime() - t0) / 1_000_000 should be < 5000L
        server.isRunning shouldBe false
        server.failed shouldBe None
        Files.exists(server.socketPath) shouldBe false
        assertClosedByServer(client)
      }
    }
  }
