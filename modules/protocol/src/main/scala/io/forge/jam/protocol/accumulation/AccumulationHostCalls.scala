package io.forge.jam.protocol.accumulation

import io.forge.jam.core.ChainConfig

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
) extends StorageHostCalls, PrivilegedHostCalls:

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
      case _ => 10L

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

