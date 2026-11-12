package io.forge.jam.protocol.accumulation

import io.forge.jam.core.{ChainConfig, Hashing, JamBytes}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.trie.{InMemoryTrieBackend, StateTrie}
import io.forge.jam.core.types.service.ServiceInfo
import io.forge.jam.protocol.state.ServiceStorageView
import io.forge.jam.pvm.ExecutionMode
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import spire.math.ULong

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

/**
 * GP Ψ_A runs a_preimages[a_codehash] of the account in the current partial state. A service that `upgrade`s in
 * accseq round 1 and is accumulated again in round 2 of the same block (here: it sends itself a deferred transfer)
 * runs the code under its new code hash in round 2. Round 1 is a script driving the real host calls; round 2 runs
 * the real executor on a real PVM blob.
 */
class UpgradedCodeVisibilitySpec extends AnyFunSuite with Matchers:

  private val config = ChainConfig.TINY
  private val slot = 1000L
  private val S = 10L

  private def le(v: Int, n: Int): Array[Byte] = Array.tabulate(n)(i => (v >>> (8 * i)).toByte)

  /** Service-code preimage E(↕m, c): empty metadata, then a JAM program blob whose code is six `trap`s, so the
    * accumulate entry point (pc 5) starts a basic block. Running it panics after charging gas.
    */
  private val codePreimage: JamBytes =
    val pvm = Array[Byte](0, 1, 6) ++ Array.fill[Byte](6)(0) ++ Array[Byte](0x3f)
    JamBytes(Array[Byte](0) ++ le(0, 3) ++ le(0, 3) ++ le(0, 2) ++ le(0, 3) ++ le(pvm.length, 4) ++ pvm)
  private val newCodeHash: JamBytes = JamBytes(Hashing.blake2b256(codePreimage.toArray).bytes)
  private val blobKey = StateKey.computeServiceDataStateKey(S, 0xfffffffeL, newCodeHash)
  private val infoKey = StateKey.computePreimageInfoStateKey(S, codePreimage.length.toLong, newCodeHash)

  private final class Calls(val ctx: AccumulationContext, operands: List[AccumulationOperand]):
    private val hostCalls = new AccumulationHostCalls(ctx, operands, config)
    private val vm = new MockPvmInstance()
    private val in1 = 0x10000

    private def call(id: Int, regs: (Int, Long)*): ULong =
      regs.foreach((r, v) => vm.setReg(r, v))
      hostCalls.dispatch(id, vm)
      ULong(vm.reg(7))

    def upgrade(hash: JamBytes): ULong =
      vm.writeBytes(in1, hash.toArray)
      call(HostCall.UPGRADE, 7 -> in1.toLong, 8 -> 0L, 9 -> 0L)

    def provide(blob: JamBytes): ULong =
      vm.writeBytes(in1, blob.toArray)
      call(HostCall.PROVIDE, 7 -> -1L, 8 -> in1.toLong, 9 -> blob.length.toLong)

  /** Runs `round1` in place of the service's first run and the real executor for every later run. */
  private final class UpgradeThenRunExecutor(parallel: Boolean, round1: Calls => Unit)
      extends AccumulationExecutor(config, parallel):
    val laterRuns = TrieMap.empty[Int, AccumulationOneResult]
    @volatile private var runs = 0

    override def executeService(
        partialState: PartialState,
        timeslot: Long,
        serviceId: Long,
        gasLimit: Long,
        entropy: JamBytes,
        operands: List[AccumulationOperand],
        executionMode: ExecutionMode,
        storageView: Option[ServiceStorageView]
    ): AccumulationOneResult =
      runs += 1
      if runs == 1 then
        val ctx = new AccumulationContext(
          x = partialState.deepCopy(),
          initialY = null,
          serviceIndex = serviceId,
          timeslot = timeslot,
          entropy = entropy,
          storageView = storageView
        )
        round1(new Calls(ctx, operands))
        val selfTransfer =
          DeferredTransfer(source = S, destination = S, amount = 0L, memo = JamBytes.zeros(128), gasLimit = 10_000L)
        AccumulationOneResult(ctx.collapse(ExitReason.HALT), List(selfTransfer), None, 1L,
          ctx.getProvisions(ExitReason.HALT))
      else
        val r = super.executeService(partialState, timeslot, serviceId, gasLimit, entropy, operands, executionMode,
          storageView)
        laterRuns(runs) = r
        r

  private def account: ServiceAccount =
    ServiceAccount.empty(
      ServiceInfo(
        version = 0,
        codeHash = Hash(Array.fill[Byte](32)(S.toByte)), // no preimage: round 1 is scripted
        balance = 1_000_000_000L,
        minItemGas = 0L,
        minMemoGas = 0L,
        bytesUsed = 1000L,
        depositOffset = 0L,
        items = 10,
        creationSlot = 0L,
        lastAccumulationSlot = 0L,
        parentService = 0L
      )
    )

  private def run(
      parallel: Boolean,
      withView: Boolean,
      preBlock: Seq[(JamBytes, JamBytes)],
      round1: Calls => Unit
  ): (UpgradeThenRunExecutor, AccumulationTransition.OuterAccumulationResult) =
    val executor = new UpgradeThenRunExecutor(parallel, round1)
    if withView then
      val trie = StateTrie.empty(new InMemoryTrieBackend())
      if preBlock.nonEmpty then trie.update(preBlock.map((k, v) => (k, Some(v))))
      executor.setStorageView(Some(new ServiceStorageView(trie)))
    val state = PartialState(
      accounts = Map(S -> account),
      stagingSet = mutable.ListBuffer.empty,
      authQueue = Vector.empty,
      manager = 0L,
      assigners = mutable.ListBuffer.empty,
      delegator = 0L,
      registrar = 0L,
      alwaysAccers = mutable.Map.empty,
      rawServiceDataByStateKey = if withView then Map.empty else preBlock.toMap
    )
    val result = AccumulationTransition.outerAccumulate(
      partialState = state,
      transfers = Nil,
      workReports = Nil,
      alwaysAccers = Map(S -> 10_000L),
      gasLimit = 1_000_000L,
      timeslot = slot,
      entropy = JamBytes.zeros(32),
      executor = executor,
      config = config
    )
    (executor, result)

  for
    parallel <- Seq(false, true)
    withView <- Seq(true, false)
  do
    val path = s"${if parallel then "parallel" else "sequential"}, ${if withView then "storage view" else "raw state"}"

    test(s"GP: code upgraded in round 1 to an on-chain preimage runs in round 2 of the same block ($path)") {
      val (executor, result) = run(parallel, withView, Seq(blobKey -> codePreimage), c =>
        c.upgrade(newCodeHash) shouldBe HostCallResult.OK)
      result.postState.accounts(S).info.codeHash shouldBe Hash(newCodeHash.toArray)
      executor.laterRuns.keySet shouldBe Set(2)
      // The no-code path charges nothing; the six-trap program panics after charging gas.
      executor.laterRuns(2).gasUsed should be > 0L
    }

    test(s"GP: a code preimage provided and integrated in round 1 runs in round 2 after an upgrade ($path)") {
      val (executor, _) = run(parallel, withView, Seq(infoKey -> StateKey.encodePreimageInfoValue(Nil)), { c =>
        c.provide(codePreimage) shouldBe HostCallResult.OK
        c.upgrade(newCodeHash) shouldBe HostCallResult.OK
      })
      executor.laterRuns.keySet shouldBe Set(2)
      executor.laterRuns(2).gasUsed should be > 0L
    }

  test("GP: an upgrade to a code hash without a preimage still runs nothing in round 2") {
    val (executor, _) = run(parallel = false, withView = true, Nil, c =>
      c.upgrade(newCodeHash) shouldBe HostCallResult.OK)
    executor.laterRuns(2).gasUsed shouldBe 0L
  }
