package io.forge.jam.conformance

import io.forge.jam.core.ChainConfig
import io.forge.jam.core.types.block.Block
import io.forge.jam.protocol.importing.{BlockImporter, ImportResult}
import io.forge.jam.protocol.report.ReportTypes.Ancestry
import io.forge.jam.protocol.state.RawState
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.Paths

class ImportBlockStepSpec extends AnyFunSuite with Matchers:

  private given config: ChainConfig = ChainConfig.TINY
  private val baseDir = sys.props.getOrElse("jam.base.dir", System.getProperty("user.dir"))

  test("an import that fails on our side is thrown, not answered as a block the protocol rejects") {
    val trace = FuzzClient.TraceDir
      .find(Paths.get(baseDir, "jamtestvectors", "traces", "fallback"))
      .getOrElse(cancel("jamtestvectors/traces/fallback not found"))
    val step = trace.readStep(trace.steps.head, config)
    val store = new StateStore(config)
    store.initialize(step.block.header.parent, RawState(step.preState.stateRoot, step.preState.keyvals), Nil)
    val ours = new IllegalStateException("jump table entry size 5")
    val importer = new BlockImporter(config, skipAncestryValidation = true):
      override def importBlock(block: Block, preState: RawState, ancestry: Ancestry, extrinsicHashChecked: Boolean): ImportResult =
        ImportResult.Failed(ours.getMessage, ours)

    val thrown = the[IllegalStateException] thrownBy ImportBlockStep(store, importer, step.block)
    thrown.getCause shouldBe ours
    thrown.getMessage should include("jump table entry size 5")
    store.size shouldBe 1 // nothing was stored for the block
  }
