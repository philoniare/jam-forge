package io.forge.jam.conformance

import io.forge.jam.core.ChainConfig
import io.forge.jam.protocol.traces.TraceStep
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import spire.math.{UByte, UInt}

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** Replays fuzz-report traces through the same [[ProtocolHandler]] the fuzz socket server uses, so a decode failure
  * or importer exception (a target crash for the fuzzer) fails the test like a wrong state root does.
  *
  * Each trace directory holds `NNNNNNNN.bin` steps and optionally `genesis.bin`. A step whose post-state root equals
  * its pre-state root is a block the target must reject with an Error reply; any other step must reply with the
  * post-state root. Without a genesis the session starts from the first step's pre-state, with no ancestry.
  *
  * The bundled corpus is the jamforge-failing subset of the jam-conformance fuzz reports (fuzz-reports/0.8.0, branch
  * v0.8.0, plus the PR #217 batch in jam-conformance/jam-fuzzing). Set `JAM_FUZZ_REPORT_TRACES=<dir>` to replay
  * another traces directory instead.
  */
class FuzzReportRegressionSpec extends AnyFunSuite with Matchers:
  private val config = ChainConfig.TINY

  private lazy val tracesRoot: Path =
    sys.env.get("JAM_FUZZ_REPORT_TRACES").map(Paths.get(_)).getOrElse(
      Paths.get(getClass.getResource("/fuzz-reports/0.8.0/traces").toURI)
    )

  private def traceDirs: List[Path] =
    Using.resource(Files.list(tracesRoot))(_.iterator().asScala.filter(Files.isDirectory(_)).toList.sortBy(_.toString))

  private def steps(dir: Path): Vector[Path] =
    Using.resource(Files.list(dir))(
      _.iterator().asScala.filter(_.getFileName.toString.matches("\\d+\\.bin")).toVector.sortBy(_.toString)
    )

  private def peerInfo(features: Int) = ProtocolMessage.PeerInfoMsg(
    PeerInfo(UByte(1), UInt(features), Version.JAM_VERSION, Version(UByte(0), UByte(1), UByte(0)), "regression")
  )

  /** Replay one trace directory; returns the first mismatch, if any. */
  private def replay(dir: Path): Option[String] =
    val store = new StateStore()
    val handler = new ProtocolHandler(store, FileLogger.noop, config)
    val trace = FuzzClient.TraceDir(dir.resolve("genesis.bin"), steps(dir))
    val decoded = trace.steps.map(s => s.getFileName.toString -> trace.readStep(s, config))
    if Files.isRegularFile(trace.genesis) then
      val genesis = trace.readGenesis(config)
      handler.handleMessage(peerInfo(Features.ANCESTRY | Features.FORKS), 0)
      handler.handleMessage(ProtocolMessage.InitializeMsg(Initialize(genesis.header, genesis.state.keyvals, Nil)), 0)
    else
      // Mid-session excerpt: seed the parent's state directly, since its header is not in the trace.
      val first: TraceStep = decoded.head._2
      handler.handleMessage(peerInfo(Features.FORKS), 0)
      handler.handleMessage(ProtocolMessage.InitializeMsg(Initialize(first.block.header, Nil, Nil)), 0)
      store.initialize(first.block.header.parent, first.preState, Nil)

    decoded.iterator.map { (name, step) =>
      val rejected = step.postState.stateRoot == step.preState.stateRoot
      handler.handleMessage(ProtocolMessage.ImportBlockMsg(ImportBlock(step.block)), 0) match
        case ProtocolMessage.StateRootMsg(root) if !rejected && root.hash == step.postState.stateRoot => None
        case ProtocolMessage.ErrorMsg(_) if rejected                                                 => None
        case other =>
          val expected = if rejected then "Error" else s"root ${step.postState.stateRoot.toHex}"
          Some(s"$name: expected $expected, got $other")
    }.collectFirst { case Some(msg) => msg }

  test("fuzz-report traces replay without crash or mismatch") {
    val dirs = traceDirs
    dirs should not be empty
    val failures = dirs.flatMap { dir =>
      val id = dir.getFileName.toString
      try replay(dir).map(m => s"$id/$m")
      catch case e: Exception => Some(s"$id: crashed with ${e.getClass.getSimpleName}: ${e.getMessage}")
    }
    failures.foreach(f => info(f))
    withClue(s"${failures.size}/${dirs.size} traces failed:\n${failures.mkString("\n")}\n")(failures shouldBe empty)
  }
