package io.forge.jam.protocol.pvmhost

final case class ExecCost(gas: Long, nanos: Long):
  def +(other: ExecCost): ExecCost = ExecCost(gas + other.gas, nanos + other.nanos)

object ExecCost:
  val zero: ExecCost = ExecCost(0L, 0L)

final case class IsAuthorizedCost(total: ExecCost, compileNanos: Long, hostCalls: ExecCost)

object IsAuthorizedCost:
  val zero: IsAuthorizedCost = IsAuthorizedCost(ExecCost.zero, 0L, ExecCost.zero)

final case class RefineCost(
    total: ExecCost,
    compileNanos: Long,
    historicalLookup: ExecCost,
    machineExpunge: ExecCost,
    peekPokePages: ExecCost,
    invoke: ExecCost,
    other: ExecCost
)

object RefineCost:
  val zero: RefineCost = RefineCost(ExecCost.zero, 0L, ExecCost.zero, ExecCost.zero, ExecCost.zero, ExecCost.zero, ExecCost.zero)

  def category(hostCall: Int): Int = hostCall match
    case HostCall.HISTORICAL_LOOKUP                   => 0
    case HostCall.MACHINE | HostCall.EXPUNGE          => 1
    case HostCall.PEEK | HostCall.POKE | HostCall.PAGES => 2
    case HostCall.INVOKE                              => 3
    case _                                            => 4

  def of(total: ExecCost, compileNanos: Long, meter: HostCallMeter): RefineCost =
    RefineCost(total, compileNanos, meter.cost(0), meter.cost(1), meter.cost(2), meter.cost(3), meter.cost(4))

final case class AccumulateCost(
    calls: Int,
    transfers: Int,
    items: Int,
    total: ExecCost,
    compileNanos: Long,
    readWrite: ExecCost,
    lookup: ExecCost,
    preimages: ExecCost,
    service: ExecCost,
    transfer: ExecCost,
    transferGas: Long,
    other: ExecCost
):
  def +(o: AccumulateCost): AccumulateCost =
    AccumulateCost(
      calls + o.calls,
      transfers + o.transfers,
      items + o.items,
      total + o.total,
      compileNanos + o.compileNanos,
      readWrite + o.readWrite,
      lookup + o.lookup,
      preimages + o.preimages,
      service + o.service,
      transfer + o.transfer,
      transferGas + o.transferGas,
      other + o.other
    )

object AccumulateCost:
  val zero: AccumulateCost =
    AccumulateCost(0, 0, 0, ExecCost.zero, 0L, ExecCost.zero, ExecCost.zero, ExecCost.zero, ExecCost.zero, ExecCost.zero, 0L, ExecCost.zero)

  def category(hostCall: Int): Int = hostCall match
    case HostCall.READ | HostCall.WRITE                                         => 0
    case HostCall.LOOKUP                                                        => 1
    case HostCall.QUERY | HostCall.SOLICIT | HostCall.FORGET | HostCall.PROVIDE => 2
    case HostCall.INFO | HostCall.NEW | HostCall.UPGRADE | HostCall.EJECT       => 3
    case HostCall.TRANSFER                                                      => 4
    case _                                                                      => 5

  def of(total: ExecCost, compileNanos: Long, meter: HostCallMeter): AccumulateCost =
    val transfer = meter.cost(4)
    AccumulateCost(
      calls = 1,
      transfers = 0,
      items = 0,
      total = total,
      compileNanos = compileNanos,
      readWrite = meter.cost(0),
      lookup = meter.cost(1),
      preimages = meter.cost(2),
      service = meter.cost(3),
      transfer = transfer.copy(gas = transfer.gas - meter.takenGas(4)),
      transferGas = meter.takenGas(4),
      other = meter.cost(5)
    )

final class RunProbe:
  private val started = System.nanoTime()
  @volatile private var compiled = -1L
  @volatile var meter: HostCallMeter = null

  def codeReady(): Unit = compiled = System.nanoTime() - started

  def elapsedNanos: Long = System.nanoTime() - started

  def compileNanos: Long = if compiled < 0 then elapsedNanos else compiled

  def hostCall(c: Int): ExecCost = if meter == null then ExecCost.zero else meter.cost(c)
  def hostCalls: ExecCost = if meter == null then ExecCost.zero else meter.all

final class HostCallMeter(inner: HostCallDispatcher, categoryOf: Int => Int, categories: Int) extends HostCallDispatcher:
  private val gas = new Array[Long](categories)
  private val taken = new Array[Long](categories)
  private val nanos = new Array[Long](categories)

  def getGasCost(hostCallId: Int, instance: PvmInstance): Long =
    val charge = inner.getGasCost(hostCallId, instance)
    gas(categoryOf(hostCallId)) += charge
    charge

  def dispatch(hostCallId: Int, instance: PvmInstance): Unit =
    val c = categoryOf(hostCallId)
    val before = instance.gas
    val started = System.nanoTime()
    try inner.dispatch(hostCallId, instance)
    finally
      nanos(c) += System.nanoTime() - started
      val after = math.max(instance.gas, 0L)
      if after < before then taken(c) += before - after

  def cost(c: Int): ExecCost = ExecCost(gas(c) + taken(c), nanos(c))

  def takenGas(c: Int): Long = taken(c)

  def all: ExecCost = (0 until categories).map(cost).foldLeft(ExecCost.zero)(_ + _)
