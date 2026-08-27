package io.forge.jam.protocol.accumulation

import io.forge.jam.core.{ChainConfig, JamBytes, Hashing}
import io.forge.jam.core.scodec.JamCodecs
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.service.ServiceInfo
import spire.math.ULong

import scala.collection.mutable

/** Handles host calls during accumulation PVM execution.
  */
class AccumulationHostCalls(
    val context: AccumulationContext,
    val operands: List[AccumulationOperand],
    val config: ChainConfig
):
  /** Get register value from PVM instance as ULong. Gray Paper register
    * mapping: r7=A0 (first arg/return), r8=A1, r9=A2, r10=A3, r11=A4, r12=A5
    */
  private def getReg(instance: PvmInstance, reg: Int): ULong =
    ULong(instance.reg(reg))

  /** Set register value in PVM instance.
    */
  private def setReg(instance: PvmInstance, reg: Int, value: ULong): Unit =
    instance.setReg(reg, value.signed)

  /** Read a register as a spec offset/length argument and clamp it to
    * `available` using unsigned-64-bit min()
    */
  private def argClampedLen(
      instance: PvmInstance,
      reg: Int,
      available: Long
  ): Int =
    val v = getReg(instance, reg)
    val cap = ULong(available)
    (if v < cap then v else cap).toLong.toInt

  /** Read a register as a full-width (unsigned 64-bit) service id
    */
  private def argServiceId(instance: PvmInstance, reg: Int): ULong =
    getReg(instance, reg)

  /** Read a register as a value that the spec constrains to `Nbits(32)`
    * (preimage lengths, counts, core/service indices used as array bounds).
    */
  private def argU32(instance: PvmInstance, reg: Int): Option[Long] =
    val v = getReg(instance, reg)
    if v > ULong(0xffffffffL) then None else Some(v.toLong)

  private def readGuestBytes(
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
  private def calculateThreshold(
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
  private def calculateThreshold(info: ServiceInfo): Long =
    calculateThreshold(info.items, info.bytesUsed, info.depositOffset)

  /** Check if balance meets threshold requirement.
    */
  private def meetsThreshold(balance: Long, threshold: Long): Boolean =
    ULong(balance) >= ULong(threshold)

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

  /** gas (0): Returns remaining gas in register r7.
    */
  private def handleGas(instance: PvmInstance): Unit =
    setReg(instance, 7, ULong(instance.gas))

  /** Fetch host call sub-selectors
    */
  private object FetchSelector:
    val CONSTANTS = 0 // Protocol configuration constants
    val ENTROPY = 1 // Entropy/randomness data
    val ALL_OPERANDS = 14 // List of all work package operands
    val SINGLE_OPERAND = 15 // Individual operand at index

  /** fetch (1): Fetch various data based on register r10 selector. For
    * accumulation, supports fetching operands and constants.
    */
  private def handleFetch(instance: PvmInstance): Unit =
    val selector = getReg(instance, 10).toInt
    val outputAddr = getReg(instance, 7).toInt
    val index = argU32(instance, 11)

    val data: Option[Array[Byte]] = selector match
      case FetchSelector.CONSTANTS      => Some(getConstantsBlob())
      case FetchSelector.ENTROPY        => Some(context.entropy.toArray)
      case FetchSelector.ALL_OPERANDS   => Some(encodeOperandsList())
      case FetchSelector.SINGLE_OPERAND =>
        index match
          case Some(i) if i < operands.size =>
            Some(encodeOperand(operands(i.toInt)))
          case _ => None
      case _ => None

    data match
      case None =>
        setReg(instance, 7, HostCallResult.NONE)
      case Some(bytes) =>
        val actualOffset = argClampedLen(instance, 8, bytes.length.toLong)
        val actualLength =
          argClampedLen(instance, 9, (bytes.length - actualOffset).toLong)
        val slice = bytes.slice(actualOffset, actualOffset + actualLength)

        // Check if output address is writable - PANIC if not
        if !isMemoryWritable(instance, outputAddr, actualLength) then
          throw new RuntimeException(
            s"Fetch PANIC: Output memory not writable at 0x${outputAddr.toHexString} len $actualLength"
          )

        val writeResult = writeMemory(instance, outputAddr, slice)
        if !writeResult then setReg(instance, 7, HostCallResult.OOB)
        else setReg(instance, 7, ULong(bytes.length))

  /** lookup (2): Look up preimage by hash.
    */
  private def handleLookup(instance: PvmInstance): Unit =
    val serviceId = getReg(instance, 7).toLong
    val hashAddr = getReg(instance, 8).toInt
    val outputAddr = getReg(instance, 9).toInt

    // Read hash from memory - panic on OOB
    val hashBuffer = new Array[Byte](32)
    if !readMemory(instance, hashAddr, hashBuffer) then
      throw new RuntimeException(
        s"Lookup PANIC: Failed to read hash from memory at 0x${hashAddr.toHexString}"
      )

    val hashBytes = JamBytes(hashBuffer)
    val hash = Hash(hashBuffer)

    // Determine which account to look up from
    val targetServiceId =
      if serviceId == -1L || serviceId == context.serviceIndex then
        context.serviceIndex
      else serviceId

    val account = context.x.accounts.get(targetServiceId)

    // Get preimage if account exists - first check in-memory map
    var preimage = account.flatMap(_.preimages.get(hash))

    // If not in memory, check raw state data with discriminator 0xFFFFFFFE (preimage blob)
    if preimage.isEmpty then
      val blobStateKey = StateKey.computeServiceDataStateKey(
        targetServiceId,
        0xfffffffeL,
        hashBytes
      )
      preimage = context.readRawDataFor(targetServiceId, blobStateKey)

    // Calculate actual offset and length based on preimage data (or 0 if not found)
    val dataSize: Long = preimage.map(_.length.toLong).getOrElse(0L)
    val actualOffset = argClampedLen(instance, 10, dataSize)
    val actualLength = argClampedLen(instance, 11, dataSize - actualOffset)

    // Check if output address is writable - PANIC if not
    if !isMemoryWritable(instance, outputAddr, actualLength) then
      throw new RuntimeException(
        s"Lookup PANIC: Output memory not writable at 0x${outputAddr.toHexString} len $actualLength"
      )

    if account.isEmpty then
      setReg(instance, 7, HostCallResult.NONE)
      return

    if preimage.isEmpty then
      setReg(instance, 7, HostCallResult.NONE)
      return

    val data = preimage.get.toArray
    val slice = data.slice(actualOffset, actualOffset + actualLength)

    if !writeMemory(instance, outputAddr, slice) then
      setReg(instance, 7, HostCallResult.OOB)
      return

    setReg(instance, 7, ULong(data.length))

  /** read (3): Read from service storage.
    */
  private def handleRead(instance: PvmInstance): Unit =
    val serviceId = getReg(instance, 7).toLong
    val keyAddr = getReg(instance, 8).toInt
    val keyLen = getReg(instance, 9).toInt
    val outputAddr = getReg(instance, 10).toInt

    // Read key from memory - PANIC on memory failure
    val keyBuffer = readGuestBytes(instance, keyAddr, keyLen, "Read")

    val key = JamBytes(keyBuffer)

    // Determine which account to read from
    val targetServiceId =
      if serviceId == -1L || serviceId == context.serviceIndex then
        context.serviceIndex
      else serviceId

    val account = context.x.accounts.get(targetServiceId)

    // First check in-memory storage (for values written in this execution)
    var value = account.flatMap(_.storage.get(key))

    if value.isEmpty then
      val stateKey = StateKey.computeStorageStateKey(targetServiceId, key)
      value = context.readRawDataFor(targetServiceId, stateKey)

    if value.isEmpty then
      setReg(instance, 7, HostCallResult.NONE)
      return

    val data = value.get.toArray
    val actualOffset = argClampedLen(instance, 11, data.length.toLong)
    val actualLength = argClampedLen(instance, 12, data.length.toLong - actualOffset)
    val slice = data.slice(actualOffset, actualOffset + actualLength)

    if !writeMemory(instance, outputAddr, slice) then
      throw new RuntimeException(
        s"Read PANIC: Failed to write to output memory at 0x${outputAddr.toHexString}"
      )

    setReg(instance, 7, ULong(data.length))

  /** write (4): Write to service storage. Updates storage map and adjusts
    * bytes/items counters in ServiceInfo. Returns: old value length on success,
    * NONE if key didn't exist, FULL if threshold exceeded.
    */
  private def handleWrite(instance: PvmInstance): Unit =
    val keyAddr = getReg(instance, 7).toInt
    val valueAddr = getReg(instance, 9).toInt
    val keyLen = argU32(instance, 8).getOrElse(
      throw new RuntimeException("Write PANIC: key length out of 32-bit range")
    ).toInt
    val valueLen = argU32(instance, 10).getOrElse(
      throw new RuntimeException("Write PANIC: value length out of 32-bit range")
    ).toInt

    val account = context.x.accounts.get(context.serviceIndex)
    if account.isEmpty then
      throw new RuntimeException(
        "Write PANIC: Current service account not found"
      )

    val acc = account.get

    // Read key from memory
    val keyBuffer = readGuestBytes(instance, keyAddr, keyLen, "Write")

    val key = JamBytes(keyBuffer)

    var oldValue = acc.storage.get(key)
    if oldValue.isEmpty then
      oldValue = context.storageView match
        case Some(v) => v.get(context.serviceIndex, key)
        case None    =>
          val stateKeyForLookup =
            StateKey.computeStorageStateKey(context.serviceIndex, key)
          context.x.rawServiceDataByStateKey.get(stateKeyForLookup)

    val oldValueSize = oldValue.map(_.length).getOrElse(0)
    val keyWasPresent = oldValue.isDefined

    // Calculate new footprint to check threshold
    val newValue =
      if valueLen == 0 then None
      else Some(JamBytes(readGuestBytes(instance, valueAddr, valueLen, "Write")))

    // Calculate bytes/items delta for threshold check
    val (bytesDelta, itemsDelta): (Long, Int) = (valueLen, keyWasPresent) match
      case (0, true) =>
        // Delete: decrement bytes (key + value + 34) and items
        (-(keyLen.toLong + oldValueSize + 34), -1)
      case (0, false) =>
        // Delete non-existent key: no change
        (0L, 0)
      case (_, true) =>
        // Update: only value size changes
        ((valueLen - oldValueSize).toLong, 0)
      case (_, false) =>
        // Insert: add key + value + 34 overhead
        ((keyLen + valueLen + 34).toLong, 1)

    // Calculate new threshold balance and check against current balance
    val info = acc.info
    val newBytes = info.bytesUsed + bytesDelta
    val newItems = info.items + itemsDelta
    val newThreshold =
      calculateThreshold(newItems, newBytes, info.depositOffset)

    if !meetsThreshold(info.balance, newThreshold) then
      setReg(instance, 7, HostCallResult.FULL)
      return

    // Compute state key for raw storage updates
    val stateKey = StateKey.computeStorageStateKey(context.serviceIndex, key)

    val viewInstalled = context.storageView.isDefined
    val newStorage: Map[JamBytes, JamBytes] =
      if valueLen == 0 then
        // Delete key
        if keyWasPresent then
          if viewInstalled then
            context.storageView.foreach(_.delete(context.serviceIndex, key))
          else
            context.x.rawServiceDataByStateKey =
              context.x.rawServiceDataByStateKey.removed(stateKey)
          acc.storage.removed(key)
        else acc.storage
      else
        if viewInstalled then
          context.storageView.foreach(_.put(context.serviceIndex, key, newValue.get))
        else
          context.x.rawServiceDataByStateKey =
            context.x.rawServiceDataByStateKey.updated(stateKey, newValue.get)
        acc.storage.updated(key, newValue.get)

    // Update account info with new bytes/items
    val updatedInfo = info.copy(
      bytesUsed = newBytes,
      items = newItems
    )
    context.x.accounts = context.x.accounts.updated(
      context.serviceIndex,
      acc.copy(info = updatedInfo, storage = newStorage)
    )

    // Return old value length (or NONE if key didn't exist)
    val returnValue =
      if keyWasPresent then ULong(oldValueSize) else HostCallResult.NONE
    setReg(instance, 7, returnValue)

  /** info (5): Get service account info. Returns 96 bytes: codeHash(32) +
    * balance(8) + thresholdBalance(8) + minAccumulateGas(8) + minMemoGas(8) +
    * totalByteLength(8) + itemsCount(4) + gratisStorage(8) + createdAt(4) +
    * lastAccAt(4) + parentService(4)
    */
  private def handleInfo(instance: PvmInstance): Unit =
    val serviceId = getReg(instance, 7).toLong
    val outputAddr = getReg(instance, 8).toInt

    val targetServiceId =
      if serviceId == -1L then context.serviceIndex else serviceId
    val account = context.x.accounts.get(targetServiceId)

    if account.isEmpty then
      setReg(instance, 7, HostCallResult.NONE)
      return

    val info = account.get.info
    val thresholdBalance = calculateThreshold(info)

    // Encode all 11 fields (96 bytes total
    val data = new Array[Byte](96)
    System.arraycopy(info.codeHash.bytes.toArray, 0, data, 0, 32) // 32 bytes
    putLE(data, 32, info.balance, 8) // 8 bytes
    putLE(data, 40, thresholdBalance, 8) // 8 bytes
    putLE(data, 48, info.minItemGas, 8) // 8 bytes (minAccumulateGas)
    putLE(data, 56, info.minMemoGas, 8) // 8 bytes
    putLE(data, 64, info.bytesUsed, 8) // 8 bytes (totalByteLength)
    putLE(data, 72, info.items.toLong, 4) // 4 bytes (itemsCount)
    putLE(data, 76, info.depositOffset, 8) // 8 bytes (gratisStorage)
    putLE(data, 84, info.creationSlot.toInt.toLong, 4) // 4 bytes (createdAt)
    putLE(data, 88, info.lastAccumulationSlot.toInt.toLong, 4) // 4 bytes (lastAccAt)
    putLE(data, 92, info.parentService.toInt.toLong, 4) // 4 bytes

    // Apply offset and length slicing
    val first = argClampedLen(instance, 9, data.length.toLong)
    val len = argClampedLen(instance, 10, data.length.toLong - first)
    val slicedData = data.slice(first, first + len)

    if !writeMemory(instance, outputAddr, slicedData) then
      throw new RuntimeException(
        s"Info PANIC: Failed to write to memory at $outputAddr"
      )

    // Return the full data length (not sliced length)
    setReg(instance, 7, ULong(data.length))

  /** bless (14): Set privileged services. reg7 = manager, reg8 = assigners ptr,
    * reg9 = delegator, reg10 = registrar, reg11 = always-acc pairs ptr, reg12 =
    * always-acc pairs count
    */
  private def handleBless(instance: PvmInstance): Unit =
    val newManager = getReg(instance, 7).toLong
    val assignersPtr = getReg(instance, 8).toInt
    val newDelegator = getReg(instance, 9).toLong
    val newRegistrar = getReg(instance, 10).toLong
    val alwaysAccPtr = getReg(instance, 11).toInt
    val alwaysAccCount = argU32(instance, 12)
      .getOrElse(
        throw new RuntimeException(
          "Bless PANIC: always-acc count out of 32-bit range"
        )
      )
      .toInt

    // Read assigners array (4 bytes per core)
    val coresCount = config.coresCount
    val assignersBytes = new Array[Byte](4 * coresCount)
    if !readMemory(instance, assignersPtr, assignersBytes) then
      throw new RuntimeException(
        "Bless PANIC: Failed to read assigners from memory"
      )

    // Parse assigners
    val newAssigners = mutable.ListBuffer.empty[Long]
    var i = 0
    while i < coresCount do
      val assigner = decodeLE(assignersBytes, i * 4, 4)
      newAssigners += assigner
      i += 1

    // Read always-acc pairs (12 bytes each: 4 service + 8 gas)
    val alwaysAccMap = mutable.Map.empty[Long, Long]
    if alwaysAccCount > 0 then
      val alwaysAccBytes = new Array[Byte](12 * alwaysAccCount)
      if !readMemory(instance, alwaysAccPtr, alwaysAccBytes) then
        throw new RuntimeException(
          "Bless PANIC: Failed to read always-acc from memory"
        )

      var j = 0
      while j < alwaysAccCount do
        val off = j * 12
        val serviceId = decodeLE(alwaysAccBytes, off, 4)
        val gas = decodeLE(alwaysAccBytes, off + 4, 8)
        alwaysAccMap(serviceId) = gas
        j += 1

    // Validate service indices
    val maxUInt = 0xffffffffL
    if newManager < 0 || newManager > maxUInt ||
      newDelegator < 0 || newDelegator > maxUInt ||
      newRegistrar < 0 || newRegistrar > maxUInt
    then
      setReg(instance, 7, HostCallResult.WHO)
      return

    // Apply all changes
    context.x.manager = newManager
    context.x.delegator = newDelegator
    context.x.registrar = newRegistrar
    context.x.assigners.clear()
    context.x.assigners ++= newAssigners
    context.x.alwaysAccers.clear()
    context.x.alwaysAccers ++= alwaysAccMap

    setReg(instance, 7, HostCallResult.OK)

  /** assign (15): Set core assigner and authorization queue (privileged). reg7 =
    * targetCoreIndex, reg8 = authorizationQueue address, reg9 = new assigner
    */
  private def handleAssign(instance: PvmInstance): Unit =
    val targetCoreIndexU = getReg(instance, 7)
    val startAddr = getReg(instance, 8).toInt
    val newAssigner = getReg(instance, 9).toLong

    // Read authorization queue from memory (32 bytes * AUTH_QUEUE_SIZE)
    val queueLength = 32 * config.authQueueSize
    val queueBuffer = new Array[Byte](queueLength)
    if !readMemory(instance, startAddr, queueBuffer) then
      // PANIC if memory is not readable
      throw new RuntimeException(
        s"Assign PANIC: Failed to read authorization queue from memory at $startAddr"
      )

    // Check core index bounds (unsigned 64-bit)
    if targetCoreIndexU >= ULong(config.coresCount.toLong) then
      setReg(instance, 7, HostCallResult.CORE)
      return

    val targetCoreIndex = targetCoreIndexU.toInt

    // Check if caller is current assigner for this core
    if targetCoreIndex >= context.x.assigners.size ||
      context.x.assigners(targetCoreIndex) != context.serviceIndex
    then
      setReg(instance, 7, HostCallResult.HUH)
      return

    // Check assigner is valid service index (fits in UInt32)
    val maxUInt = 0xffffffffL
    if ULong(newAssigner) > ULong(maxUInt) then
      setReg(instance, 7, HostCallResult.WHO)
      return

    // Parse authorization queue (list of 32-byte hashes)
    val authQueueList = mutable.ListBuffer.empty[JamBytes]
    var i = 0
    while i < config.authQueueSize do
      val hash = queueBuffer.slice(i * 32, (i + 1) * 32)
      authQueueList += JamBytes(hash)
      i += 1

    // Update authorization queue for this core
    while context.x.authQueue.size <= targetCoreIndex do
      context.x.authQueue += mutable.ListBuffer.empty[JamBytes]
    context.x.authQueue(targetCoreIndex) = authQueueList

    // Update assigner for this core
    while context.x.assigners.size <= targetCoreIndex do
      context.x.assigners += 0L
    context.x.assigners(targetCoreIndex) = newAssigner

    setReg(instance, 7, HostCallResult.OK)

  /** designate (16): Set validator queue (privileged). Panics if memory is not
    * readable. Returns HUH if caller is not the delegator. Returns OK on
    * success and updates stagingSet with the new validator keys.
    */
  private def handleDesignate(instance: PvmInstance): Unit =
    val startAddr = getReg(instance, 7).toInt
    val validatorKeySize = 336
    val totalLength = validatorKeySize * config.validatorCount

    if !instance.isMemoryReadable(startAddr, totalLength) then
      throw new RuntimeException(
        s"Designate PANIC: Memory not readable at 0x${startAddr.toHexString} len $totalLength"
      )

    // Check if caller is the delegator
    if context.serviceIndex != context.x.delegator then
      setReg(instance, 7, HostCallResult.HUH)
      return

    // Read validator keys from memory and update stagingSet
    val newStagingSet = mutable.ListBuffer[JamBytes]()
    for i <- 0 until config.validatorCount do
      val offset = i * validatorKeySize
      val keyBuffer = new Array[Byte](validatorKeySize)
      if !readMemory(instance, startAddr + offset, keyBuffer) then
        throw new RuntimeException(
          s"Designate PANIC: Failed to read validator key $i from memory"
        )
      newStagingSet += JamBytes(keyBuffer)

    // Update the staging set in context
    context.x.stagingSet.clear()
    context.x.stagingSet ++= newStagingSet

    setReg(instance, 7, HostCallResult.OK)

  /** checkpoint (17): Save current state x to checkpoint y.
    */
  private def handleCheckpoint(instance: PvmInstance): Unit =
    context.checkpoint()
    setReg(instance, 7, ULong(instance.gas))

  /** upgrade (19): Upgrade service code hash.
    */
  private def handleUpgrade(instance: PvmInstance): Unit =
    val codeHashAddr = getReg(instance, 7).toInt
    val newMinAccumulateGas = getReg(instance, 8).toLong
    val newMinMemoGas = getReg(instance, 9).toLong

    val account = context.x.accounts.get(context.serviceIndex)
    if account.isEmpty then
      setReg(instance, 7, HostCallResult.WHO)
      return

    // Read new code hash from memory
    val codeHashBuffer = new Array[Byte](32)
    if !readMemory(instance, codeHashAddr, codeHashBuffer) then
      throw new RuntimeException(
        s"Upgrade PANIC: Failed to read code hash from memory at address $codeHashAddr (0x${codeHashAddr.toHexString})"
      )

    val updatedInfo = account.get.info.copy(
      codeHash = Hash(codeHashBuffer),
      minItemGas = newMinAccumulateGas,
      minMemoGas = newMinMemoGas
    )
    context.x.accounts = context.x.accounts.updated(
      context.serviceIndex,
      account.get.copy(info = updatedInfo)
    )

    setReg(instance, 7, HostCallResult.OK)

  /** new (18): Create new service account. reg7 = codeHashAddr, reg8 =
    * codeHashLength (for preimage info), reg9 = minAccumulateGas, reg10 =
    * minMemoGas, reg11 = gratisStorage, reg12 = requested service index (if
    * caller is registrar)
    */
  private def handleNew(instance: PvmInstance): Unit =
    val codeHashAddr = getReg(instance, 7).toInt
    val codeHashLength = argU32(instance, 8)
      .getOrElse(
        throw new RuntimeException(
          "New PANIC: preimage length out of 32-bit range"
        )
      )
      .toInt
    val minAccumulateGas = getReg(instance, 9).toLong
    val minMemoGas = getReg(instance, 10).toLong
    val gratisStorage = getReg(instance, 11).toLong
    val requestedServiceId = getReg(instance, 12).toLong

    // Read code hash from memory - PANIC if not readable
    val codeHashBuffer = new Array[Byte](32)
    if !readMemory(instance, codeHashAddr, codeHashBuffer) then
      throw new RuntimeException(
        s"New PANIC: Failed to read code hash from memory at $codeHashAddr"
      )

    val codeHash = Hash(codeHashBuffer)

    // Check gratisStorage permission
    if gratisStorage != 0L && context.serviceIndex != context.x.manager then
      setReg(instance, 7, HostCallResult.HUH)
      return

    val currentAccount = context.x.accounts.get(context.serviceIndex)
    if currentAccount.isEmpty then
      throw new RuntimeException("New PANIC: Current service account not found")

    val acc = currentAccount.get

    // Calculate threshold balance for new account
    // New account starts with: items = 2, bytes = 81 + codeHashLength
    val newAccountItems = 2
    val newAccountBytes = 81L + codeHashLength
    val thresholdBalance =
      calculateThreshold(newAccountItems, newAccountBytes, gratisStorage)

    // Check if caller can afford: balance >= newThreshold + callerThreshold
    val callerThreshold = calculateThreshold(acc.info)
    val requiredBalance = ULong(thresholdBalance) + ULong(callerThreshold)
    if ULong(acc.info.balance) < requiredBalance then
      setReg(instance, 7, HostCallResult.CASH)
      return

    // Determine new service ID
    val minPublicServiceIndex = context.minPublicServiceIndex
    val unsignedRequestedServiceId = requestedServiceId & 0xffffffffL
    val usedRegistrarPrivilege =
      context.serviceIndex == context.x.registrar && unsignedRequestedServiceId < minPublicServiceIndex
    val newServiceId: Long =
      if usedRegistrarPrivilege then
        // Registrar can request specific service ID below minPublicServiceIndex
        if context.x.accounts.contains(unsignedRequestedServiceId) then
          setReg(instance, 7, HostCallResult.FULL)
          return
        unsignedRequestedServiceId
      else
        // Use pre-calculated nextAccountIndex
        context.nextAccountIndex

    // Create new account with calculated threshold balance
    val newAccount = ServiceAccount(
      info = ServiceInfo(
        version = 0,
        codeHash = codeHash,
        balance = thresholdBalance,
        minItemGas = minAccumulateGas,
        minMemoGas = minMemoGas,
        bytesUsed = newAccountBytes,
        items = newAccountItems,
        depositOffset = gratisStorage,
        creationSlot = context.timeslot,
        lastAccumulationSlot = 0L,
        parentService = context.serviceIndex
      ),
      storage = Map.empty,
      preimages = Map.empty,
      preimageRequests = Map(
        // Initialize preimage info for code hash with empty requestedAt list
        PreimageKey(Hash(codeHashBuffer), codeHashLength) -> PreimageRequest(
          List.empty
        )
      )
    )

    context.x.accounts = context.x.accounts.updated(newServiceId, newAccount)

    val preimageInfoStateKey =
      StateKey.computePreimageInfoStateKey(
        newServiceId,
        codeHashLength,
        JamBytes(codeHashBuffer)
      )
    val newEncoded = StateKey.encodePreimageInfoValue(List.empty)
    context.writeRawData(preimageInfoStateKey, newEncoded)

    // Deduct balance from creator
    val updatedCreatorInfo = acc.info.copy(
      balance = acc.info.balance - thresholdBalance
    )
    context.x.accounts = context.x.accounts.updated(
      context.serviceIndex,
      acc.copy(info = updatedCreatorInfo)
    )

    // Update nextAccountIndex for next NEW call (ONLY if not using registrar privilege)
    if !usedRegistrarPrivilege then
      val s = minPublicServiceIndex
      // Calculate next candidate index per Gray Paper:
      // i^* = check(Cminpublicindex + (nextfreeid - Cminpublicindex + 42) mod (2^32 - Cminpublicindex - 2^8))
      // Use Long arithmetic throughout to avoid overflow
      val left = (context.nextAccountIndex - s + 42) & 0xffffffffL
      val modValue = 0xffffffffL - s - 255 // 2^32 - Cminpublicindex - 2^8
      val nextCandidate = s + (left % modValue)
      val newNextAccountIndex = findAvailableServiceIndex(nextCandidate, s)
      context.nextAccountIndex = newNextAccountIndex

    setReg(instance, 7, ULong(newServiceId))

  /** eject (21): Remove another service account.
    */
  private def handleEject(instance: PvmInstance): Unit =
    val ejectServiceId = getReg(instance, 7).toLong
    val preimageHashAddr = getReg(instance, 8).toInt

    val hashBuffer = new Array[Byte](32)
    if !readMemory(instance, preimageHashAddr, hashBuffer) then
      throw new RuntimeException(
        "Eject PANIC: Failed to read preimage hash from memory"
      )

    val preimageHash = JamBytes(hashBuffer)

    // 2. Get target service account
    val ejectAccount = context.x.accounts.get(ejectServiceId)

    val expectedCodeHash = encodeServiceIdAsCodeHash(context.serviceIndex)
    if ejectServiceId == context.serviceIndex then
      setReg(instance, 7, HostCallResult.WHO)
      return

    if ejectAccount.isEmpty then
      setReg(instance, 7, HostCallResult.WHO)
      return

    val acc = ejectAccount.get
    if !acc.info.codeHash.bytes.toArray.sameElements(expectedCodeHash.toArray)
    then
      setReg(instance, 7, HostCallResult.WHO)
      return

    if acc.info.items != 2 then
      setReg(instance, 7, HostCallResult.HUH)
      return

    // 5. Find preimage request by hash AND derived length
    // Per GP: l = max(81, octets) - 81 where octets is the service's totalByteLength
    // Then look for (hash, l) in requests

    val octets = acc.info.bytesUsed
    val derivedLength = math.max(81L, octets) - 81L
    val preimageKey =
      PreimageKey(Hash(preimageHash.toArray), derivedLength.toInt)
    val preimageRequest = acc.preimageRequests.get(preimageKey)

    val timeslots: List[Long] = preimageRequest match
      case Some(req) =>
        req.requestedAt
      case None =>
        val expectedKey = StateKey.computePreimageInfoStateKey(
          ejectServiceId,
          derivedLength.toInt,
          preimageHash
        )
        context.readRawDataFor(ejectServiceId, expectedKey) match
          case Some(infoValue) =>
            StateKey.decodePreimageInfoValue(infoValue)
          case None =>
            setReg(instance, 7, HostCallResult.HUH)
            return List.empty

    if timeslots.size != 2 then
      setReg(instance, 7, HostCallResult.HUH)
      return

    val expungePeriod = config.preimageExpungePeriod
    val minHoldSlot = math.max(0L, context.timeslot - expungePeriod)
    if timeslots(1) >= minHoldSlot then
      setReg(instance, 7, HostCallResult.HUH)
      return

    // 7. SUCCESS: Transfer balance to caller and remove ejected service
    val callerAccount = context.x.accounts(context.serviceIndex)
    val updatedCallerInfo = callerAccount.info.copy(
      balance = callerAccount.info.balance + acc.info.balance
    )
    context.x.accounts = context.x.accounts
      .updated(context.serviceIndex, callerAccount.copy(info = updatedCallerInfo))
      .removed(ejectServiceId)

    val serviceIdBytes =
      java.nio.ByteBuffer
        .allocate(4)
        .order(java.nio.ByteOrder.LITTLE_ENDIAN)
        .putInt(ejectServiceId.toInt)
        .array()
    def isChapterKey(arr: Array[Byte]): Boolean =
      var i = 1
      while i < arr.length do
        if arr(i) != 0 then return false
        i += 1
      true
    def isAccountRecordKey(arr: Array[Byte]): Boolean =
      if (arr(0) & 0xff) != 0xff then false
      else if arr(2) != 0 || arr(4) != 0 || arr(6) != 0 then false
      else
        var i = 8
        while i < arr.length do
          if arr(i) != 0 then return false
          i += 1
        true
    val matchesEjectedService = (key: JamBytes) =>
      val arr = key.toArray
      arr.length >= 8 &&
        arr(0) == serviceIdBytes(0) &&
        arr(2) == serviceIdBytes(1) &&
        arr(4) == serviceIdBytes(2) &&
        arr(6) == serviceIdBytes(3) &&
        !isChapterKey(arr) &&
        !isAccountRecordKey(arr)
    val byteOnePrefix = JamBytes(Array(serviceIdBytes(0)))
    val keysToRemove: List[JamBytes] = context.storageView match
      case Some(v) =>
        v.enumerate(byteOnePrefix, 8)
          .map(_._1)
          .filter(matchesEjectedService)
          .toList
      case None =>
        context.x.rawServiceDataByStateKey.keys
          .filter(matchesEjectedService)
          .toList
    keysToRemove.foreach(context.deleteRawData)

    // Also remove the service account key from rawServiceAccountsByStateKey
    val serviceAccountKey = StateKey.computeServiceAccountKey(ejectServiceId)
    context.x.rawServiceAccountsByStateKey =
      context.x.rawServiceAccountsByStateKey.removed(serviceAccountKey)

    setReg(instance, 7, HostCallResult.OK)

  /** transfer (20): Queue a deferred transfer.
    */
  private def handleTransfer(instance: PvmInstance): Unit =
    val destination = argServiceId(instance, 7).toLong
    val amount = getReg(instance, 8).toLong
    val gasLimit = getReg(instance, 9).toLong
    val memoAddr = getReg(instance, 10).toInt

    val account = context.x.accounts.get(context.serviceIndex)
    val accounts = context.x.accounts

    // 1. Read memo from memory (128 bytes) - PANIC if fails
    val memoBuffer = new Array[Byte](DeferredTransfer.MEMO_SIZE)
    if !readMemory(instance, memoAddr, memoBuffer) then
      throw new RuntimeException(
        s"Transfer PANIC: Failed to read memo from memory at $memoAddr"
      )

    // 2. Check if destination exists (WHO)
    if !accounts.contains(destination) then
      setReg(instance, 7, HostCallResult.WHO)
      return

    // 3. Check if gasLimit >= destination.minMemoGas (LOW)
    val destAccount = accounts(destination)
    if gasLimit < destAccount.info.minMemoGas then
      setReg(instance, 7, HostCallResult.LOW)
      return

    // 4. Check if caller can afford it - balance after deduction >= caller.minBalance (CASH)
    // b = caller.balance - amount
    // Check: b < caller.minBalance
    if account.isEmpty then
      setReg(instance, 7, HostCallResult.CASH)
      return

    val acc = account.get

    // Check for underflow before subtraction - if amount > balance, insufficient funds
    if ULong(amount) > ULong(acc.info.balance) then
      setReg(instance, 7, HostCallResult.CASH)
      return

    val balanceAfterTransfer = acc.info.balance - amount
    val callerMinBalance = calculateThreshold(acc.info)

    if !meetsThreshold(balanceAfterTransfer, callerMinBalance) then
      setReg(instance, 7, HostCallResult.CASH)
      return

    // 5. Success - charge additional gas on success
    instance.setGas(instance.gas - gasLimit)

    // 6. Deduct balance and queue transfer
    val updatedInfo = acc.info.copy(balance = balanceAfterTransfer)
    context.x.accounts = context.x.accounts.updated(
      context.serviceIndex,
      acc.copy(info = updatedInfo)
    )

    context.deferredTransfers += DeferredTransfer(
      source = context.serviceIndex,
      destination = destination,
      amount = amount,
      memo = JamBytes(memoBuffer),
      gasLimit = gasLimit
    )

    setReg(instance, 7, HostCallResult.OK)

  /** query (22): Return preimage request status packed in r7/r8.
    */
  private def handleQuery(instance: PvmInstance): Unit =
    val hashAddr = getReg(instance, 7).toInt
    val length = getReg(instance, 8).toInt

    val account = context.x.accounts.get(context.serviceIndex)
    if account.isEmpty then
      setReg(instance, 7, HostCallResult.WHO)
      return

    // Read hash from memory
    val hashBuffer = new Array[Byte](32)
    if !readMemory(instance, hashAddr, hashBuffer) then
      throw new RuntimeException(
        s"Query PANIC: Failed to read hash from memory at 0x${hashAddr.toHexString}"
      )

    val key = PreimageKey(Hash(hashBuffer), length)
    var request = account.flatMap(_.preimageRequests.get(key))

    if request.isEmpty then
      val infoStateKey = StateKey.computePreimageInfoStateKey(
        context.serviceIndex,
        length,
        JamBytes(hashBuffer)
      )
      val rawInfoData = context.readRawData(infoStateKey)
      if rawInfoData.isDefined then
        // Decode preimage info from raw state
        val timeslots = StateKey.decodePreimageInfoValue(rawInfoData.get)
        request = Some(PreimageRequest(timeslots))

    if request.isEmpty then
      setReg(instance, 7, HostCallResult.NONE)
      setReg(instance, 8, ULong(0))
    else
      val history = request.get.requestedAt
      val count = history.size
      val r7Value: ULong = count match
        case 0 => ULong(0)
        case _ => ULong(count) + (ULong(history.head) << 32)

      val r8Value: ULong =
        if count >= 3 then ULong(history(1)) + (ULong(history(2)) << 32)
        else if count >= 2 then ULong(history(1))
        else ULong(0)

      setReg(instance, 7, r7Value)
      setReg(instance, 8, r8Value)

  /** solicit (23): Request a preimage. Request that a preimage be made
    * available.
    *
    * Cases:
    *   - notRequestedYet (null): Create new entry with empty list []
    *   - isPreviouslyAvailable (count == 2): Append timeslot
    *   - Otherwise: Return HUH
    */
  private def handleSolicit(instance: PvmInstance): Unit =
    val hashAddr = getReg(instance, 7).toInt
    val length = (getReg(instance, 8) & ULong(0xffffffffL)).toInt

    val account = context.x.accounts.get(context.serviceIndex)
    if account.isEmpty then
      setReg(instance, 7, HostCallResult.WHO)
      return

    val acc = account.get

    // Read hash from memory - PANIC if fails
    val hashBuffer = new Array[Byte](32)
    if !readMemory(instance, hashAddr, hashBuffer) then
      throw new RuntimeException(
        "Solicit PANIC: Failed to read hash from memory"
      )

    val key = PreimageKey(Hash(hashBuffer), length)
    var existingRequest = acc.preimageRequests.get(key)

    if existingRequest.isEmpty then
      val infoStateKey = StateKey.computePreimageInfoStateKey(
        context.serviceIndex,
        length,
        JamBytes(hashBuffer)
      )
      val rawInfoData = context.readRawData(infoStateKey)
      if rawInfoData.isDefined then
        val timeslots = StateKey.decodePreimageInfoValue(rawInfoData.get)
        existingRequest = Some(PreimageRequest(timeslots))

    val notRequestedYet = existingRequest.isEmpty
    val isPreviouslyAvailable = existingRequest.exists(_.requestedAt.size == 2)
    val canSolicit = notRequestedYet || isPreviouslyAvailable

    if !canSolicit then
      setReg(instance, 7, HostCallResult.HUH)
      return

    // Calculate new footprint for threshold balance check
    val lengthUnsigned = (length.toLong & 0xffffffffL)
    val info = acc.info
    val (newItems, newBytes): (Int, Long) =
      if notRequestedYet then
        (info.items + 2, info.bytesUsed + 81 + lengthUnsigned)
      else (info.items, info.bytesUsed)

    val thresholdBalance =
      calculateThreshold(newItems, newBytes, info.depositOffset)
    if !meetsThreshold(info.balance, thresholdBalance) then
      setReg(instance, 7, HostCallResult.FULL)
      return

    // Compute preimage info state key
    val stateKey = StateKey.computePreimageInfoStateKey(
      context.serviceIndex,
      length,
      JamBytes(hashBuffer)
    )

    if notRequestedYet then
      // New request: start with empty list (preimage not yet available)
      val newTimeslots = List.empty[Long]
      context.writeRawData(stateKey, StateKey.encodePreimageInfoValue(newTimeslots))
      val updatedInfo = info.copy(items = newItems, bytesUsed = newBytes)
      context.x.accounts = context.x.accounts.updated(
        context.serviceIndex,
        acc.copy(
          info = updatedInfo,
          preimageRequests =
            acc.preimageRequests.updated(key, PreimageRequest(newTimeslots))
        )
      )
    else if isPreviouslyAvailable then
      // Re-solicit: append current timeslot (requesting again)
      val newTimeslots = existingRequest.get.requestedAt :+ context.timeslot
      context.writeRawData(stateKey, StateKey.encodePreimageInfoValue(newTimeslots))
      context.x.accounts = context.x.accounts.updated(
        context.serviceIndex,
        acc.copy(preimageRequests =
          acc.preimageRequests.updated(key, PreimageRequest(newTimeslots))
        )
      )

    setReg(instance, 7, HostCallResult.OK)

  /** forget (24): Forget a preimage request. Mark a preimage as no longer
    * needed or remove it.
    *
    * Cases:
    *   - canExpunge (count == 0 || (count == 2 && requestedAt[1] <
    *     minHoldSlot)): Remove entry
    *   - isAvailable1 (count == 1): Append timeslot
    *   - isAvailable3 (count == 3 && requestedAt[1] < minHoldSlot): Update to
    *     [requestedAt[2], timeslot]
    *   - Otherwise: Return HUH
    */
  private def handleForget(instance: PvmInstance): Unit =
    val hashAddr = getReg(instance, 7).toInt
    val length = getReg(instance, 8).toInt

    val account = context.x.accounts.get(context.serviceIndex)
    if account.isEmpty then
      setReg(instance, 7, HostCallResult.WHO)
      return

    val acc = account.get

    // Read hash from memory - PANIC if fails
    val hashBuffer = new Array[Byte](32)
    if !readMemory(instance, hashAddr, hashBuffer) then
      throw new RuntimeException(
        "Forget PANIC: Failed to read hash from memory"
      )

    val key = PreimageKey(Hash(hashBuffer), length)
    var existingRequest = acc.preimageRequests.get(key)

    if existingRequest.isEmpty then
      val infoStateKey = StateKey.computePreimageInfoStateKey(
        context.serviceIndex,
        length,
        JamBytes(hashBuffer)
      )
      val rawInfoData = context.readRawData(infoStateKey)
      if rawInfoData.isDefined then
        val timeslots = StateKey.decodePreimageInfoValue(rawInfoData.get)
        existingRequest = Some(PreimageRequest(timeslots))

    if existingRequest.isEmpty then
      setReg(instance, 7, HostCallResult.HUH)
      return

    val historyCount = existingRequest.get.requestedAt.size
    val minHoldSlot =
      math.max(0L, context.timeslot - config.preimageExpungePeriod)

    val canExpunge =
      historyCount == 0 || (historyCount == 2 && existingRequest.get
        .requestedAt(1) < minHoldSlot)
    val isAvailable1 = historyCount == 1
    val isAvailable3 =
      historyCount == 3 && existingRequest.get.requestedAt(1) < minHoldSlot

    val canForget = canExpunge || isAvailable1 || isAvailable3

    if !canForget then
      setReg(instance, 7, HostCallResult.HUH)
      return

    // Compute preimage info state key
    val stateKey = StateKey.computePreimageInfoStateKey(
      context.serviceIndex,
      length,
      JamBytes(hashBuffer)
    )

    val info = acc.info
    if canExpunge then
      // Remove the preimage info entry
      context.deleteRawData(stateKey)
      // Also remove the preimage blob if it exists
      val preimageHash = Hash(hashBuffer)
      val preimageStateKey =
        StateKey.computeServiceDataStateKey(
          context.serviceIndex,
          0xfffffffeL,
          JamBytes(preimageHash.bytes)
        )
      context.deleteRawData(preimageStateKey)
      // Update footprint: decrease items by 2 and bytes by 81 + length
      val newItems = math.max(0, info.items - 2)
      val newBytes = math.max(0L, info.bytesUsed - 81 - length)
      val updatedInfo = info.copy(items = newItems, bytesUsed = newBytes)
      context.x.accounts = context.x.accounts.updated(
        context.serviceIndex,
        acc.copy(
          info = updatedInfo,
          preimageRequests = acc.preimageRequests.removed(key),
          preimages = acc.preimages.removed(preimageHash)
        )
      )
    else if isAvailable1 then
      // Append current timeslot (marking as forgotten)
      val newTimeslots = existingRequest.get.requestedAt :+ context.timeslot
      context.writeRawData(stateKey, StateKey.encodePreimageInfoValue(newTimeslots))
      context.x.accounts = context.x.accounts.updated(
        context.serviceIndex,
        acc.copy(preimageRequests =
          acc.preimageRequests.updated(key, PreimageRequest(newTimeslots))
        )
      )
    else if isAvailable3 then
      // Update to [requestedAt[2], timeslot]
      val newTimeslots =
        List(existingRequest.get.requestedAt(2), context.timeslot)
      context.writeRawData(stateKey, StateKey.encodePreimageInfoValue(newTimeslots))
      context.x.accounts = context.x.accounts.updated(
        context.serviceIndex,
        acc.copy(preimageRequests =
          acc.preimageRequests.updated(key, PreimageRequest(newTimeslots))
        )
      )

    setReg(instance, 7, HostCallResult.OK)

  /** provide (26): Provide a preimage for another service.
    */
  private def handleProvide(instance: PvmInstance): Unit =
    val r7 = getReg(instance, 7)
    val targetServiceId =
      if r7 == ULong(0xffffffffffffffffL) then context.serviceIndex
      else r7.toLong
    val blobAddr = getReg(instance, 8).toInt
    val blobLen = getReg(instance, 9).toInt

    // Read blob from memory - PANIC on failure
    val blobBuffer = readGuestBytes(instance, blobAddr, blobLen, "Provide")

    val blob = JamBytes(blobBuffer)

    // Check if target account exists - WHO if not
    val targetAccount = context.x.accounts.get(targetServiceId)
    if targetAccount.isEmpty then
      setReg(instance, 7, HostCallResult.WHO)
      return

    // Compute preimage hash
    val preimageHash = Hashing.blake2b256(blobBuffer)

    // Check if preimage has been solicited
    val preimageKey = PreimageKey(Hash(preimageHash.bytes.toArray), blobLen)
    var preimageRequest = targetAccount.get.preimageRequests.get(preimageKey)

    if preimageRequest.isEmpty then
      val infoStateKey = StateKey.computePreimageInfoStateKey(
        targetServiceId,
        blobLen,
        JamBytes(preimageHash.bytes)
      )
      val rawInfoData = context.readRawDataFor(targetServiceId, infoStateKey)
      if rawInfoData.isDefined then
        val timeslots = StateKey.decodePreimageInfoValue(rawInfoData.get)
        preimageRequest = Some(PreimageRequest(timeslots))

    if preimageRequest.isEmpty || preimageRequest.get.requestedAt.nonEmpty then
      setReg(instance, 7, HostCallResult.HUH)
      return

    // Check if already in provisions set for this execution -> HUH
    val provisionEntry = (targetServiceId, blob)
    if context.provisions.contains(provisionEntry) then
      setReg(instance, 7, HostCallResult.HUH)
      return

    // All checks passed - OK
    context.provisions += provisionEntry
    setReg(instance, 7, HostCallResult.OK)

  /** yield (25): Set accumulation output hash.
    */
  private def handleYield(instance: PvmInstance): Unit =
    val hashAddr = getReg(instance, 7).toInt

    // Read hash from memory
    val hashBuffer = new Array[Byte](32)
    if !readMemory(instance, hashAddr, hashBuffer) then
      throw new RuntimeException(
        s"Yield PANIC: Failed to read hash from memory at 0x${hashAddr.toHexString}"
      )

    // Store the yield in the context
    context.yieldHash = Some(JamBytes(hashBuffer))
    setReg(instance, 7, HostCallResult.OK)

  /** log (100): Debug logging host call (JIP-1) Gas cost: 10, always returns
    * WHAT
    */
  private def handleLog(instance: PvmInstance): Unit =
    setReg(instance, 7, HostCallResult.WHAT)

  private var cachedConstantsBlob: Array[Byte] = null

  /** Encode protocol configuration as expected by the guest. Uses actual config
    * values for correct behavior in both tiny and full configs. Caches the
    * result since it's constant for the lifetime of this handler.
    */
  private def getConstantsBlob(): Array[Byte] =
    if cachedConstantsBlob == null then
      cachedConstantsBlob = buildConstantsBlob()
    cachedConstantsBlob

  /** Build the constants blob (called once and cached).
    */
  private def buildConstantsBlob(): Array[Byte] =
    ConstantsBlob.build(config)

  /** Cached encoding of the full operand list. Operands are immutable for the
    * lifetime of this handler
    */
  private lazy val cachedOperandsList: Array[Byte] =
    val buffer = new java.io.ByteArrayOutputStream()
    // Gray Paper natural number encode the array length
    buffer.write(JamCodecs.encodeCompactInteger(operands.size.toLong))
    // Encode each operand using its existing encode() method (includes variant)
    for operand <- operands do buffer.write(operand.encode())
    buffer.toByteArray

  /** Encode the full array of inputs.
    */
  private def encodeOperandsList(): Array[Byte] =
    cachedOperandsList

  /** Encode a single operand.
    */
  private def encodeOperand(operand: AccumulationOperand): Array[Byte] =
    operand.encode()

  /** Encode service ID as a 32-byte code hash (fixed-width little-endian
    * encoding). Used for parent-child relationship verification in eject.
    */
  private def encodeServiceIdAsCodeHash(serviceId: Long): JamBytes =
    val bytes = new Array[Byte](32)
    // Little-endian encoding of service ID in first 4 bytes
    bytes(0) = (serviceId & 0xff).toByte
    bytes(1) = ((serviceId >> 8) & 0xff).toByte
    bytes(2) = ((serviceId >> 16) & 0xff).toByte
    bytes(3) = ((serviceId >> 24) & 0xff).toByte
    JamBytes(bytes)

  /** Find the first available service index starting from a candidate.
    */
  private def findAvailableServiceIndex(
      candidate: Long,
      minPublicServiceIndex: Long
  ): Long =
    var i = candidate
    val s = minPublicServiceIndex
    val right = (0xffffffffL - s - 255).toLong

    // Loop until we find an unused service index
    while context.x.accounts.contains(i) do
      val left = i - s + 1
      i = s + (left % right)
    i

  /** Encode a value as little-endian bytes */
  private inline def encodeLE(value: Long, size: Int): Array[Byte] =
    Array.tabulate(size)(i => ((value >> (i * 8)) & 0xff).toByte)

  private def putLE(buf: Array[Byte], offset: Int, value: Long, size: Int): Unit =
    var i = 0
    while i < size do
      buf(offset + i) = ((value >> (i * 8)) & 0xff).toByte
      i += 1

  private inline def encodeShort(value: Int): Array[Byte] = encodeLE(value, 2)
  private inline def encodeInt(value: Int): Array[Byte] = encodeLE(value, 4)
  private inline def encodeIntLE(value: Int): Array[Byte] = encodeLE(value, 4)
  private inline def encodeLong(value: Long): Array[Byte] = encodeLE(value, 8)

  /** Decode a little-endian integer from a byte array */
  private def decodeLE(bytes: Array[Byte], offset: Int, size: Int): Long =
    var result = 0L
    var i = 0
    while i < size do
      result |= (bytes(offset + i).toLong & 0xff) << (i * 8)
      i += 1
    result

  /** Check if memory is writable at the given address and length */
  private def isMemoryWritable(
      instance: PvmInstance,
      address: Int,
      length: Int
  ): Boolean =
    instance.isMemoryWritable(address, length)

  /** Read memory from PVM instance, returns true on success */
  private def readMemory(
      instance: PvmInstance,
      address: Int,
      buffer: Array[Byte]
  ): Boolean =
    instance.readInto(address, buffer, 0, buffer.length)

  /** Write memory to PVM instance, returns true on success */
  private def writeMemory(
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
