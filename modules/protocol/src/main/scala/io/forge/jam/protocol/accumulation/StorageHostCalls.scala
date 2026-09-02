package io.forge.jam.protocol.accumulation

import io.forge.jam.core.{JamBytes, Hashing}
import io.forge.jam.core.scodec.JamCodecs
import io.forge.jam.core.primitives.Hash
import spire.math.ULong

/** Storage, preimage and introspection host calls: GAS, FETCH (+ the cached constants/operand
  * blobs), LOOKUP, READ, WRITE, INFO, QUERY, SOLICIT, FORGET, PROVIDE, YIELD
  * and LOG. Dispatch lives in [[AccumulationHostCalls]].
  */
private[accumulation] trait StorageHostCalls extends HostCallSupport:

  /** gas (0): Returns remaining gas in register r7.
    */
  protected def handleGas(instance: PvmInstance): Unit =
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
  protected def handleFetch(instance: PvmInstance): Unit =
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
  protected def handleLookup(instance: PvmInstance): Unit =
    val serviceId = getReg(instance, 7).toLong
    val hashAddr = getReg(instance, 8).toInt
    val outputAddr = getReg(instance, 9).toInt

    // Read hash from memory - panic on OOB
    val hashBuffer = new Array[Byte](Hash.Size)
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
  protected def handleRead(instance: PvmInstance): Unit =
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
  protected def handleWrite(instance: PvmInstance): Unit =
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
    // Functional update of the (immutable) per-account storage map; the rebuilt
    // map replaces the prior one on the account written back below.
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
  protected def handleInfo(instance: PvmInstance): Unit =
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


  /** query (22): Return preimage request status packed in r7/r8.
    */
  protected def handleQuery(instance: PvmInstance): Unit =
    val hashAddr = getReg(instance, 7).toInt
    val length = getReg(instance, 8).toInt

    val account = context.x.accounts.get(context.serviceIndex)
    if account.isEmpty then
      setReg(instance, 7, HostCallResult.WHO)
      return

    // Read hash from memory
    val hashBuffer = new Array[Byte](Hash.Size)
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
  protected def handleSolicit(instance: PvmInstance): Unit =
    val hashAddr = getReg(instance, 7).toInt
    val length = (getReg(instance, 8) & ULong(0xffffffffL)).toInt

    val account = context.x.accounts.get(context.serviceIndex)
    if account.isEmpty then
      setReg(instance, 7, HostCallResult.WHO)
      return

    val acc = account.get

    // Read hash from memory - PANIC if fails
    val hashBuffer = new Array[Byte](Hash.Size)
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
      // Update footprint and the (immutable) preimageRequests map, written back
      // as a single rebuilt account.
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
  protected def handleForget(instance: PvmInstance): Unit =
    val hashAddr = getReg(instance, 7).toInt
    val length = getReg(instance, 8).toInt

    val account = context.x.accounts.get(context.serviceIndex)
    if account.isEmpty then
      setReg(instance, 7, HostCallResult.WHO)
      return

    val acc = account.get

    // Read hash from memory - PANIC if fails
    val hashBuffer = new Array[Byte](Hash.Size)
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
  protected def handleProvide(instance: PvmInstance): Unit =
    // Spec: s = imX_id when registers_7 = 2^64-1 (self-provide sentinel),
    // otherwise s = registers_7.
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

    // Check if preimage has been solicited. The request may only live in the
    // trie (cross-block solicit), so fall back to a raw-state preimage-info
    // read when the in-memory request map misses.
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

    // Spec OK path requires the request to be present with an empty timeslot
    // list (solicited but not yet provided): HUH when the request is absent or
    // its timeslot list is non-empty (a_requests[(blake(i), z)] != []).
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
  protected def handleYield(instance: PvmInstance): Unit =
    val hashAddr = getReg(instance, 7).toInt

    // Read hash from memory
    val hashBuffer = new Array[Byte](Hash.Size)
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
  protected def handleLog(instance: PvmInstance): Unit =
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

  /** Build the constants blob (called once and cached). Delegates to the
    * shared [[ConstantsBlob]] builder so the refine dispatcher serves
    * byte-identical constants (guarded by FetchHostCallSpec).
    */
  private def buildConstantsBlob(): Array[Byte] =
    ConstantsBlob.build(config)

  /** Cached encoding of the full operand list. Operands are immutable for the
    * lifetime of this handler (a `val` constructor parameter), so the encoded
    * blob is stable and safe to compute once and reuse across repeated
    * FETCH(ALL_OPERANDS) calls.
    */
  private lazy val cachedOperandsList: Array[Byte] =
    val buffer = new java.io.ByteArrayOutputStream()
    buffer.write(JamCodecs.encodeCompactInteger(operands.size.toLong))
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
