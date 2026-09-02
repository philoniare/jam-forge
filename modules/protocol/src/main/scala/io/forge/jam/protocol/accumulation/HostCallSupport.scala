package io.forge.jam.protocol.accumulation

import io.forge.jam.core.ChainConfig
import io.forge.jam.core.types.service.ServiceInfo
import spire.math.ULong

/** Shared low-level support for the accumulation host-call families:
  * register access and spec-argument decoding,
  * guest-memory read/write, little-endian codec helpers, and service
  * threshold-balance arithmetic. Mixed into [[AccumulationHostCalls]] via
  * [[StorageHostCalls]] / [[PrivilegedHostCalls]].
  */
private[accumulation] trait HostCallSupport:
  def context: AccumulationContext
  def operands: List[AccumulationOperand]
  def config: ChainConfig

  /** Get register value from PVM instance as ULong. Gray Paper register
    * mapping: r7=A0 (first arg/return), r8=A1, r9=A2, r10=A3, r11=A4, r12=A5
    */
  protected def getReg(instance: PvmInstance, reg: Int): ULong =
    ULong(instance.reg(reg))

  /** Set register value in PVM instance.
    */
  protected def setReg(instance: PvmInstance, reg: Int, value: ULong): Unit =
    instance.setReg(reg, value.signed)

  /** Read a register as a spec offset/length argument and clamp it to
    * `available` using unsigned-64-bit min(), matching the spec's
    * `f = min(f_0, len(v))` / `l = min(z, len(v) - f)` semantics. `available`
    * is a non-negative buffer length (always <= Int.MaxValue), so the clamped
    * result fits in a non-negative Int. A normal small register value <=
    * `available` is returned unchanged; a huge value (e.g. 2^32) clamps to
    * `available` instead of truncating to a wrong/negative 32-bit number.
    */
  protected def argClampedLen(
      instance: PvmInstance,
      reg: Int,
      available: Long
  ): Int =
    val v = getReg(instance, reg)
    val cap = ULong(available)
    (if v < cap then v else cap).toLong.toInt

  /** Read a register as a full-width (unsigned 64-bit) service id. Does NOT mask
    * to 32 bits; the spec compares the full register against the `2^64 - 1`
    * sentinel and against the `Nbits(32)` bound, so masking would conflate
    * distinct destinations (e.g. `2^32 + d` vs `d`).
    */
  protected def argServiceId(instance: PvmInstance, reg: Int): ULong =
    getReg(instance, reg)

  /** Read a register as a value that the spec constrains to `Nbits(32)`
    * (preimage lengths, counts, core/service indices used as array bounds).
    * Returns the value as a Long when it fits in 32 bits, or `None` when it is
    * `>= 2^32` so the caller can apply the spec's out-of-range handling (panic
    * or WHO) instead of silently truncating. A healthy small value is returned
    * unchanged.
    */
  protected def argU32(instance: PvmInstance, reg: Int): Option[Long] =
    val v = getReg(instance, reg)
    if v > ULong(0xffffffffL) then None else Some(v.toLong)

  protected def readGuestBytes(
      instance: PvmInstance,
      address: Int,
      length: Int,
      label: String
  ): Array[Byte] =
    if !instance.isMemoryReadable(address, length) then
      throw new RuntimeException(
        s"$label PANIC: Failed to read from memory at 0x${address.toHexString} len $length"
      )
    val buf = new Array[Byte](length)
    if !readMemory(instance, address, buf) then
      throw new RuntimeException(
        s"$label PANIC: Failed to read from memory at 0x${address.toHexString} len $length"
      )
    buf

  /** Calculate threshold balance for a service account. Formula: max(0, base +
    * items*itemCost + bytes*byteCost - gratisStorage)
    */
  protected def calculateThreshold(
      items: Int,
      bytesUsed: Long,
      depositOffset: Long
  ): Long =
    // ULong throughout: signed-Long add can overflow before the wrap.
    val base = ULong(config.serviceMinBalance)
    val itemsU = ULong(items.toLong & 0xffffffffL)
    val bytesU = ULong(bytesUsed)
    val perItem = ULong(config.additionalMinBalancePerStateItem)
    val perByte = ULong(config.additionalMinBalancePerStateByte)
    val cost = base + perItem * itemsU + perByte * bytesU
    val gratis = ULong(depositOffset)
    if cost > gratis then (cost - gratis).toLong else 0L

  /** Calculate threshold balance from ServiceInfo.
    */
  protected def calculateThreshold(info: ServiceInfo): Long =
    calculateThreshold(info.items, info.bytesUsed, info.depositOffset)

  /** Check if balance meets threshold requirement.
    */
  protected def meetsThreshold(balance: Long, threshold: Long): Boolean =
    ULong(balance) >= ULong(threshold)

  protected def putLE(buf: Array[Byte], offset: Int, value: Long, size: Int): Unit =
    var i = 0
    while i < size do
      buf(offset + i) = ((value >> (i * 8)) & 0xff).toByte
      i += 1

  /** Decode a little-endian integer from a byte array */
  protected def decodeLE(bytes: Array[Byte], offset: Int, size: Int): Long =
    var result = 0L
    var i = 0
    while i < size do
      result |= (bytes(offset + i).toLong & 0xff) << (i * 8)
      i += 1
    result

  /** Check if memory is writable at the given address and length.
    *
    * Tests the page write-permission bit (PvmInstance.isMemoryWritable), not
    * mere accessibility/readability: writing an output buffer to a read-only
    * page must fail this check so the caller panics, per Omega_Y/Omega_L (ACC-005).
    */
  protected def isMemoryWritable(
      instance: PvmInstance,
      address: Int,
      length: Int
  ): Boolean =
    instance.isMemoryWritable(address, length)

  /** Read memory from PVM instance, returns true on success.
    */
  protected def readMemory(
      instance: PvmInstance,
      address: Int,
      buffer: Array[Byte]
  ): Boolean =
    instance.readInto(address, buffer, 0, buffer.length)

  /** Write memory to PVM instance, returns true on success.
    */
  protected def writeMemory(
      instance: PvmInstance,
      address: Int,
      data: Array[Byte]
  ): Boolean =
    if instance.isMemoryWritable(address, data.length) then
      instance.writeBytes(address, data)
    else
      var i = 0
      while i < data.length do
        if !instance.writeByte(address + i, data(i)) then return false
        i += 1
      true
