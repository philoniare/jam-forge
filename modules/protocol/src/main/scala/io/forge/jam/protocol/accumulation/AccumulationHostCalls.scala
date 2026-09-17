package io.forge.jam.protocol.accumulation

import io.forge.jam.core.ChainConfig
import io.forge.jam.protocol.refine.HostCallDispatcher

/** Handles host calls during accumulation PVM execution: the single dispatch
  * surface over the cohesion-based host-call families —
  * [[StorageHostCalls]] (storage/preimage/introspection) and
  * [[PrivilegedHostCalls]] (governance/service lifecycle) — which share the
  * register/memory/threshold helpers in [[HostCallSupport]].
  */
class AccumulationHostCalls(
    val context: AccumulationContext,
    val operands: List[AccumulationOperand],
    val config: ChainConfig
) extends StorageHostCalls, PrivilegedHostCalls, HostCallDispatcher:

  /** Get gas cost for a host call without executing it. Gas is charged BEFORE
    * the host call implementation runs.
    *
    * @param hostCallId
    *   The host call identifier
    * @param instance
    *   The PVM instance (used for reading gas limit for TRANSFER)
    * @return
    *   The gas cost for this host call
    */
  def getGasCost(hostCallId: Int, instance: PvmInstance): Long =
    hostCallId match
      case HostCall.GROW_HEAP => 0L
      case HostCall.GAS   => HostCallGas.CgasG
      case HostCall.FETCH =>
        HostCallGas.fetchGas(getReg(instance, 10), getReg(instance, 9))
      case HostCall.LOOKUP =>
        HostCallGas.lookupGas(getReg(instance, 11))
      case HostCall.READ =>
        HostCallGas.readGas(getReg(instance, 9), getReg(instance, 12))
      case HostCall.WRITE =>
        HostCallGas.writeGas(getReg(instance, 8), getReg(instance, 10))
      case HostCall.INFO => HostCallGas.CgasI
      case HostCall.BLESS =>
        HostCallGas.blessGas(getReg(instance, 12))
      case HostCall.ASSIGN => HostCallGas.CgasA
      case HostCall.DESIGNATE =>
        HostCallGas.designateGas(getReg(instance, 8))
      case HostCall.CHECKPOINT => HostCallGas.CgasC
      case HostCall.NEW        => HostCallGas.CgasN
      case HostCall.UPGRADE    => HostCallGas.CgasU
      case HostCall.TRANSFER =>
        HostCallGas.CgasT
      case HostCall.EJECT   => HostCallGas.CgasJ
      case HostCall.QUERY   => HostCallGas.CgasQ
      case HostCall.SOLICIT => HostCallGas.CgasS
      case HostCall.FORGET  => HostCallGas.CgasF
      case HostCall.YIELD   => HostCallGas.CgasTaurus
      case HostCall.PROVIDE =>
        HostCallGas.provideGas(getReg(instance, 9))
      case HostCall.LOG => 0L
      case _ => HostCallGas.Cgasunknown

  /** Dispatch a host call based on its identifier. Gas should be charged BEFORE
    * calling this method.
    *
    * @param hostCallId
    *   The host call identifier
    * @param instance
    *   The PVM instance
    */
  def dispatch(hostCallId: Int, instance: PvmInstance): Unit =
    hostCallId match
      case HostCall.GAS        => handleGas(instance)
      case HostCall.GROW_HEAP  => GrowHeapHostCall.handle(instance)
      case HostCall.FETCH      => handleFetch(instance)
      case HostCall.LOOKUP     => handleLookup(instance)
      case HostCall.READ       => handleRead(instance)
      case HostCall.WRITE      => handleWrite(instance)
      case HostCall.INFO       => handleInfo(instance)
      case HostCall.BLESS      => handleBless(instance)
      case HostCall.ASSIGN     => handleAssign(instance)
      case HostCall.DESIGNATE  => handleDesignate(instance)
      case HostCall.CHECKPOINT => handleCheckpoint(instance)
      case HostCall.NEW        => handleNew(instance)
      case HostCall.UPGRADE    => handleUpgrade(instance)
      case HostCall.TRANSFER   => handleTransfer(instance)
      case HostCall.EJECT      => handleEject(instance)
      case HostCall.QUERY      => handleQuery(instance)
      case HostCall.SOLICIT    => handleSolicit(instance)
      case HostCall.FORGET     => handleForget(instance)
      case HostCall.YIELD      => handleYield(instance)
      case HostCall.PROVIDE    => handleProvide(instance)
      case HostCall.LOG        => handleLog(instance)
      case _                   =>
        // Unknown host call - return WHAT
        setReg(instance, 7, HostCallResult.WHAT)

