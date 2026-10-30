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
 * What a service sees of another service's data across and within accpar batches. 
 * Batch k starts from the posterior of batch k-1, and inside a batch every service starts from the batch's
 * initial state. The services here are scripts that drive the real host calls against the real accseq/accpar code;
 * deferred transfers make the second batch. Every scenario runs on the sequential and on the parallel path.
 */
class CrossBatchVisibilitySpec extends AnyFunSuite with Matchers:

  private val config = ChainConfig.TINY
  private val slot = 1000L

  private val A = 1L // reads in batch 2 (transfer destination)
  private val B = 10L // writes in batch 1 (always-accumulate)
  private val C = 20L // runs in batch 1 next to B
  private val D = 30L // second transfer destination, so batch 2 has two services
  private val N = 40L // created during a run

  /** One service's run: sees the operands, may call host calls, returns its deferred transfers. */
  private type Script = Calls => List[DeferredTransfer]

  /** Host calls of one run, backed by a mock PVM instance. */
  private final class Calls(val ctx: AccumulationContext, operands: List[AccumulationOperand]):
    private val hostCalls = new AccumulationHostCalls(ctx, operands, config)
    private val vm = new MockPvmInstance()
    private val in1 = 0x10000
    private val in2 = 0x20000
    private val out = 0x30000

    private def call(id: Int, regs: (Int, Long)*): ULong =
      regs.foreach((r, v) => vm.setReg(r, v))
      hostCalls.dispatch(id, vm)
      ULong(vm.reg(7))

    def write(sKey: JamBytes, value: JamBytes): ULong =
      vm.writeBytes(in1, sKey.toArray)
      vm.writeBytes(in2, value.toArray)
      call(HostCall.WRITE, 7 -> in1.toLong, 8 -> sKey.length.toLong, 9 -> in2.toLong, 10 -> value.length.toLong)

    def read(service: Long, sKey: JamBytes): Option[JamBytes] =
      vm.writeBytes(in1, sKey.toArray)
      val r = call(HostCall.READ, 7 -> service, 8 -> in1.toLong, 9 -> sKey.length.toLong, 10 -> out.toLong,
        11 -> 0L, 12 -> 4096L)
      if r == HostCallResult.NONE then None else vm.readBytes(out, r.toInt).map(JamBytes(_))

    def lookup(service: Long, hash: JamBytes): Option[JamBytes] =
      vm.writeBytes(in1, hash.toArray)
      val r = call(HostCall.LOOKUP, 7 -> service, 8 -> in1.toLong, 9 -> out.toLong, 10 -> 0L, 11 -> 4096L)
      if r == HostCallResult.NONE then None else vm.readBytes(out, r.toInt).map(JamBytes(_))

    def forget(hash: JamBytes, length: Long): ULong =
      vm.writeBytes(in1, hash.toArray)
      call(HostCall.FORGET, 7 -> in1.toLong, 8 -> length)

    def provide(service: Long, blob: JamBytes): ULong =
      vm.writeBytes(in1, blob.toArray)
      call(HostCall.PROVIDE, 7 -> service, 8 -> in1.toLong, 9 -> blob.length.toLong)

  /** Runs each service's script in place of its PVM code, with the context the real executor would build. */
  private final class ScriptedExecutor(parallel: Boolean, scripts: Map[Long, Script])
      extends AccumulationExecutor(config, parallel):
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
      val ctx = new AccumulationContext(
        x = partialState.deepCopy(),
        initialY = null,
        serviceIndex = serviceId,
        timeslot = timeslot,
        entropy = entropy,
        storageView = storageView
      )
      val transfers = scripts.get(serviceId).map(_(new Calls(ctx, operands))).getOrElse(Nil)
      AccumulationOneResult(ctx.collapse(ExitReason.HALT), transfers, None, 1L, ctx.getProvisions(ExitReason.HALT))

  private def bytes(xs: Int*): JamBytes = JamBytes(xs.map(_.toByte).toArray)

  private def account(id: Long): ServiceAccount =
    ServiceAccount.empty(
      ServiceInfo(
        version = 0,
        codeHash = Hash(Array.fill[Byte](32)(id.toByte)),
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

  private def transferTo(dest: Long): DeferredTransfer =
    DeferredTransfer(source = B, destination = dest, amount = 0L, memo = JamBytes.zeros(128), gasLimit = 1000L)

  private final case class Run(result: AccumulationTransition.OuterAccumulationResult, view: ServiceStorageView)

  /** accseq with B and C always-accumulating in batch 1 over a trie holding `preBlock`. */
  private def run(parallel: Boolean, preBlock: Seq[(JamBytes, JamBytes)], scripts: Map[Long, Script]): Run =
    val trie = StateTrie.empty(new InMemoryTrieBackend())
    if preBlock.nonEmpty then trie.update(preBlock.map((k, v) => (k, Some(v))))
    val view = new ServiceStorageView(trie)
    val executor = new ScriptedExecutor(parallel, scripts)
    executor.setStorageView(Some(view))
    val state = PartialState(
      accounts = Seq(A, B, C, D).map(id => id -> account(id)).toMap,
      stagingSet = mutable.ListBuffer.empty,
      authQueue = mutable.ListBuffer.empty,
      manager = 0L,
      assigners = mutable.ListBuffer.empty,
      delegator = 0L,
      registrar = 0L,
      alwaysAccers = mutable.Map.empty
    )
    val result = AccumulationTransition.outerAccumulate(
      partialState = state,
      transfers = Nil,
      workReports = Nil,
      alwaysAccers = Map(B -> 10_000L, C -> 10_000L),
      gasLimit = 1_000_000L,
      timeslot = slot,
      entropy = JamBytes.zeros(32),
      executor = executor,
      config = config
    )
    Run(result, view)

  private val sKey = bytes(0x6b, 0x65, 0x79)
  private val storageKeyOfB = StateKey.computeStorageStateKey(B, sKey)
  private val preBlockValue = bytes(1, 1, 1)
  private val batch1Value = bytes(2, 2, 2)

  for parallel <- Seq(false, true) do
    val path = if parallel then "parallel" else "sequential"

    test(s"GP: a storage write of batch 1 is read by another service in batch 2 ($path, eq. 12.17)") {
      val seen = TrieMap.empty[Long, Option[JamBytes]]
      val scripts: Map[Long, Script] = Map(
        B -> { c => c.write(sKey, batch1Value); List(transferTo(A), transferTo(D)) },
        A -> { c => seen(A) = c.read(B, sKey); Nil },
        D -> { c => seen(D) = c.read(B, sKey); Nil }
      )
      run(parallel, Seq(storageKeyOfB -> preBlockValue), scripts)
      seen(A) shouldBe Some(batch1Value)
      seen(D) shouldBe Some(batch1Value)
    }

    test(s"GP: a storage delete of batch 1 is seen by another service in batch 2 ($path, eq. 12.17)") {
      val seen = TrieMap.empty[Long, Option[JamBytes]]
      val scripts: Map[Long, Script] = Map(
        B -> { c => c.write(sKey, JamBytes.empty); List(transferTo(A), transferTo(D)) },
        A -> { c => seen(A) = c.read(B, sKey); Nil },
        D -> { c => seen(D) = c.read(B, sKey); Nil }
      )
      val r = run(parallel, Seq(storageKeyOfB -> preBlockValue), scripts)
      r.view.getByStateKey(storageKeyOfB) shouldBe None
      seen(A) shouldBe None
      seen(D) shouldBe None
    }

    test(s"GP: a write by another service of the same batch is not visible ($path, eq. 12.18)") {
      val seen = TrieMap.empty[Long, Option[JamBytes]]
      val scripts: Map[Long, Script] = Map(
        B -> { c => c.write(sKey, batch1Value); Nil },
        C -> { c => seen(C) = c.read(B, sKey); Nil }
      )
      val r = run(parallel, Seq(storageKeyOfB -> preBlockValue), scripts)
      seen(C) shouldBe Some(preBlockValue)
      r.view.getByStateKey(storageKeyOfB) shouldBe Some(batch1Value)
    }

    val blob = bytes(7, 7, 7, 7, 7)
    val blobHash = JamBytes(Hashing.blake2b256(blob.toArray).bytes)
    val infoKeyOfB = StateKey.computePreimageInfoStateKey(B, blob.length.toLong, blobHash)
    val blobKeyOfB = StateKey.computeServiceDataStateKey(B, 0xfffffffeL, blobHash)

    test(s"GP: a preimage forgotten in batch 1 is not found by a lookup in batch 2 ($path, eq. 12.17)") {
      val seen = TrieMap.empty[Long, Option[JamBytes]]
      val scripts: Map[Long, Script] = Map(
        B -> { c =>
          c.forget(blobHash, blob.length.toLong) shouldBe HostCallResult.OK
          List(transferTo(A))
        },
        A -> { c => seen(A) = c.lookup(B, blobHash); Nil }
      )
      // Available since slot 1, unrequested since slot 2: long enough ago to expunge.
      val preBlock = Seq(infoKeyOfB -> StateKey.encodePreimageInfoValue(List(1L, 2L)), blobKeyOfB -> blob)
      val r = run(parallel, preBlock, scripts)
      r.view.getByStateKey(blobKeyOfB) shouldBe None
      seen(A) shouldBe None
    }

    test(s"GP: a provision is dropped when the request was forgotten in the same batch ($path, eq. 12.18)") {
      val provided = TrieMap.empty[Long, ULong]
      val scripts: Map[Long, Script] = Map(
        B -> { c => c.forget(blobHash, blob.length.toLong) shouldBe HostCallResult.OK; Nil },
        // The request is still there at the start of the batch, so provide accepts the blob.
        C -> { c => provided(C) = c.provide(B, blob); Nil }
      )
      val preBlock = Seq(infoKeyOfB -> StateKey.encodePreimageInfoValue(Nil))
      val r = run(parallel, preBlock, scripts)
      provided(C) shouldBe HostCallResult.OK
      r.view.getByStateKey(infoKeyOfB) shouldBe None
      r.view.getByStateKey(blobKeyOfB) shouldBe None
      r.result.postState.accounts(B).preimages shouldBe empty
    }

    test(s"GP: a provision is integrated when a later batch provides what batch 1 solicited ($path, eq. 12.17)") {
      val scripts: Map[Long, Script] = Map(
        B -> { c =>
          c.ctx.writeRawData(infoKeyOfB, StateKey.encodePreimageInfoValue(Nil)) // as solicit stores it
          List(transferTo(A))
        },
        A -> { c => c.provide(B, blob) shouldBe HostCallResult.OK; Nil }
      )
      val r = run(parallel, Nil, scripts)
      r.view.getByStateKey(infoKeyOfB) shouldBe Some(StateKey.encodePreimageInfoValue(List(slot)))
      r.view.getByStateKey(blobKeyOfB) shouldBe Some(blob)
    }

    test(s"GP: two services of one batch writing one key differently make the block invalid ($path)") {
      // Both create service N (as `new` does), so N is in n for each and its writes carry over.
      val shared = StateKey.computeStorageStateKey(N, sKey)
      def create(c: Calls, value: JamBytes): List[DeferredTransfer] =
        c.ctx.x.accounts = c.ctx.x.accounts.updated(N, account(N))
        c.ctx.writeRawData(shared, value)
        Nil
      val scripts: Map[Long, Script] = Map(B -> (create(_, bytes(1))), C -> (create(_, bytes(2))))
      val e = intercept[RuntimeException](run(parallel, Nil, scripts))
      e.getMessage should include("block invalid")
    }

    test(s"a service's writes to an account it ejects and recreates are dropped ($path, accounts' = (d ∪ n) ∖ m)") {
      val dKey = StateKey.computeStorageStateKey(D, sKey)
      val scripts: Map[Long, Script] = Map(
        B -> { c =>
          val prior = c.ctx.x.accounts(D)
          c.ctx.x.accounts = c.ctx.x.accounts.removed(D)
          c.ctx.deleteRawData(dKey)
          c.ctx.x.accounts = c.ctx.x.accounts.updated(D, prior)
          c.ctx.writeRawData(dKey, bytes(2))
          Nil
        }
      )
      val r = run(parallel, Seq(dKey -> bytes(1)), scripts)
      r.view.getByStateKey(dKey) shouldBe Some(bytes(1))
    }

  test("GP: the parallel and sequential accpar paths produce the same posterior (eq. 12.18)") {
    // B and C both write and transfer; A and D read and write in batch 2.
    def scripts(): Map[Long, Script] = Map(
      B -> { c => c.write(sKey, batch1Value); c.write(bytes(9), bytes(9, 9)); List(transferTo(A), transferTo(D)) },
      C -> { c => c.write(sKey, bytes(3)); c.read(B, sKey).foreach(v => c.write(bytes(8), v)); List(transferTo(D)) },
      A -> { c => c.read(B, sKey).foreach(v => c.write(sKey, v)); Nil },
      D -> { c => c.read(C, sKey).foreach(v => c.write(sKey, v)); c.write(bytes(5), JamBytes.empty); Nil }
    )
    val preBlock = Seq(
      storageKeyOfB -> preBlockValue,
      StateKey.computeStorageStateKey(D, bytes(5)) -> bytes(5)
    )
    val seq = run(parallel = false, preBlock, scripts())
    val par = run(parallel = true, preBlock, scripts())

    def staged(v: ServiceStorageView): Map[JamBytes, Option[JamBytes]] =
      v.pendingStateKeys.iterator.map(k => k -> v.getByStateKey(k)).toMap
    staged(par.view) shouldBe staged(seq.view)
    staged(seq.view).size shouldBe 7
    par.result.postState.accounts shouldBe seq.result.postState.accounts
    par.result.gasUsedMap shouldBe seq.result.gasUsedMap
    par.result.transferCountMap shouldBe seq.result.transferCountMap
    par.result.commitments shouldBe seq.result.commitments
  }
