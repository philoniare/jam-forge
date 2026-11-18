package io.forge.jam.node

import java.util.concurrent.Executors

import io.forge.jam.core.{ChainConfig, Hashing, JamBytes}
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.primitives.{Gas, Hash, ServiceId, Timeslot}
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.workitem.WorkItem
import io.forge.jam.core.types.workpackage.WorkPackage
import io.forge.jam.protocol.state.TrieBackedJamState
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import spire.math.UShort

class AnchorWaitSpec extends AnyFunSuite with Matchers with NodeFixture:

  private val devKeys = (0 until 6).map(ValidatorKeySet.dev)

  private final class LaggingHead(real: ChainManager, heldAt: ChainHead) extends ChainReader:
    @volatile var released = false
    def config: ChainConfig = real.config
    def best: ChainHead = if released then real.best else heldAt
    def finalized: ChainHead = real.finalized
    def nextBlockSlot: Long = best.slot + 1
    def hasBlock(h: Hash): Boolean = real.hasBlock(h)
    def block(h: Hash) = real.block(h)
    def slotOf(h: Hash): Option[Long] = real.slotOf(h)
    def stateView(): TrieBackedJamState = real.stateViewOf(best)
    def stateViewOf(head: ChainHead): TrieBackedJamState = real.stateViewOf(head)
    def stateViewAtRoot(root: Hash): TrieBackedJamState = real.stateViewAtRoot(root)
    def stateViewAt(h: Hash): Option[TrieBackedJamState] = real.stateViewAt(h)
    def stateAtRoot(root: Hash) = real.stateAtRoot(root)
    def readRawState(k: JamBytes): Option[JamBytes] = real.readRawState(k)
    private[node] def ancestorsOf(parent: Hash, parentPostRoot: Hash, slot: Long) = real.ancestorsOf(parent, parentPostRoot, slot)

  test("a package anchored at a block stored but not yet the best head's waits for the head to reach it") {
    val genesis = loadGenesis().getOrElse(cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available"))
    val authCodeHash = Hashing.blake2b256(
      genesis.state.keyvals.map(_.value.toArray).find(_.length == 50).getOrElse(fail("no NULL Authorizer"))
    )
    val spec = ChainSpec(
      id = "anchor-wait",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )
    val dir = tempDir("jam-anchor-wait")
    val egress = Executors.newSingleThreadExecutor()
    var node: JamNode = null
    var guarantor: GuarantorService = null
    try
      node = new JamNode(spec, NodeConfig(dir, slotTicking = false)).start()
      node.enableAuthoring(devKeys)
      node.authorSlot(1).isDefined shouldBe true
      val before = node.chain.best
      node.authorSlot(2).isDefined shouldBe true
      val head = node.chain.best
      val entry = node.chain.stateViewOf(head).beta.history.last
      val code = node.chain.stateViewOf(head).accumulation.serviceAccounts.find(_.id == 0).get.data.service.codeHash
      val wp = WorkPackage(
        authCodeHost = ServiceId(0),
        authCodeHash = authCodeHash,
        context = Context(
          anchor = head.hash,
          anchorSlot = Timeslot(head.slot.toInt),
          stateRoot = head.stateRoot,
          beefyRoot = entry.beefyRoot,
          lookupAnchor = head.hash,
          lookupAnchorSlot = Timeslot(head.slot.toInt),
          lookupAnchorStateRoot = head.stateRoot,
          prerequisites = List.empty
        ),
        authorization = JamBytes.empty,
        authorizerConfig = JamBytes.empty,
        items = List(
          WorkItem(ServiceId(0), code, JamBytes("wait for the head".getBytes("UTF-8")), Gas(100_000_000L),
            Gas(10_000_000L), List.empty, List.empty, UShort(0))
        )
      )

      val chain = new LaggingHead(node.chain, before)
      val pools = new ExtrinsicPools
      guarantor = new GuarantorService(chain, new DistributionService(pools, coresCount = 2, egress), pools, devKeys)
      guarantor.submit(0, wp.encode.toArray, Array.emptyByteArray) shouldBe Right(())
      Thread.sleep(500) // the anchor block is stored; the head has not reached it
      chain.released = true

      val deadline = System.currentTimeMillis() + 20000
      while pools.guaranteeCount == 0 && System.currentTimeMillis() < deadline do Thread.sleep(50)
      pools.guaranteeCount shouldBe 1
    finally
      if guarantor != null then guarantor.shutdown()
      if node != null then node.shutdown()
      egress.shutdownNow()
      cleanup(dir)
  }
