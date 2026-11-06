package io.forge.jam.protocol.refine

import io.forge.jam.core.ChainConfig
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.workpackage.WorkPackage
import io.forge.jam.pvm.engine.InterpretedModule
import io.forge.jam.pvm.memory.GuestRam

import scala.collection.mutable

final class InnerPvm(
    val code: Array[Byte],
    val module: InterpretedModule,
    val ram: GuestRam,
    var pc: Long,
    var gasChargedFlag: Boolean = false,
    val handle: Long = 0L
)

/** A program module stored by compile: the program blob and its parsed form. */
final class CompiledModule(val code: Array[Byte], val module: InterpretedModule)

trait HistoricalLookupService:
  /** True when the service account exists in the drawn-upon state. */
  def serviceExists(serviceId: Long): Boolean

  /** Λ(accounts[serviceId], lookupAnchorTimeslot, hash): the preimage blob if
    * it was available at the given timeslot.
    */
  def historicalLookup(
      serviceId: Long,
      lookupAnchorTimeslot: Long,
      hash: Hash
  ): Option[Array[Byte]]

/** Mutable context threaded through a refine invocation (Psi_R): the
  * work-package being refined, the (m, c, e) host-call context triple — inner PVMs, compiled modules
  * and exported segments — and the read-only inputs the fetch host call can
  * serve.
  *
  * @param importSegments per-work-item imported segments (ī), outer index =
  *   work-item index
  * @param extrinsicData per-work-item extrinsic blobs (x̄), outer index =
  *   work-item index
  * @param exportSegmentOffset number of segments assumed already exported by
  *   preceding work items (ς)
  * @param machineModuleCache LRU cache of compiled inner-PVM modules
  */
final class RefineContext(
    val config: ChainConfig,
    val workPackage: WorkPackage,
    val workItemIndex: Int,
    val coreIndex: Int,
    val authorizerTrace: Array[Byte],
    val importSegments: IndexedSeq[IndexedSeq[Array[Byte]]],
    val extrinsicData: IndexedSeq[IndexedSeq[Array[Byte]]],
    val exportSegmentOffset: Long,
    val accounts: HistoricalLookupService,
    val machineModuleCache: Option[BoundedModuleCache] = None
):
  /** m: integrated inner-PVM instances, keyed by machine index. */
  val innerPvms: mutable.LongMap[InnerPvm] = mutable.LongMap.empty

  /** c: compiled program modules, keyed by handle. */
  val modules: mutable.LongMap[CompiledModule] = mutable.LongMap.empty

  /** e: segments exported so far by this invocation, each Csegmentsize long. */
  val exports: mutable.ArrayBuffer[Array[Byte]] = mutable.ArrayBuffer.empty

  def workItem = workPackage.items(workItemIndex)

  def lookupAnchorTimeslot: Long =
    workPackage.context.lookupAnchorSlot.toLong

  /** H: handles in use, by stored modules and by the instances made from them (Omega_Γ). */
  def usedHandles: Set[Long] =
    modules.keySet.toSet ++ innerPvms.values.map(_.handle)

  /** Lowest natural number not already used as a machine index (Omega_M's n). */
  def nextMachineIndex: Long =
    var n = 0L
    while innerPvms.contains(n) do n += 1
    n
