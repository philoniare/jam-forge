package io.forge.jam.protocol.accumulation

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.service.ServiceInfo
import spire.math.ULong

import scala.collection.mutable

/** Privileged / service-lifecycle host calls:
  * BLESS, ASSIGN, DESIGNATE, CHECKPOINT, NEW, UPGRADE, TRANSFER and EJECT,
  * plus their service-index helpers. Dispatch lives in
  * [[AccumulationHostCalls]].
  */
private[accumulation] trait PrivilegedHostCalls extends HostCallSupport:

  /** bless (14): Set privileged services. reg7 = manager, reg8 = assigners ptr,
    * reg9 = delegator, reg10 = registrar, reg11 = always-acc pairs ptr, reg12 =
    * always-acc pairs count
    */
  protected def handleBless(instance: PvmInstance): Unit =
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
  protected def handleAssign(instance: PvmInstance): Unit =
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
  protected def handleDesignate(instance: PvmInstance): Unit =
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
  protected def handleCheckpoint(instance: PvmInstance): Unit =
    context.checkpoint()
    setReg(instance, 7, ULong(instance.gas))

  /** upgrade (19): Upgrade service code hash.
    */
  protected def handleUpgrade(instance: PvmInstance): Unit =
    val codeHashAddr = getReg(instance, 7).toInt
    val newMinAccumulateGas = getReg(instance, 8).toLong
    val newMinMemoGas = getReg(instance, 9).toLong

    val account = context.x.accounts.get(context.serviceIndex)
    if account.isEmpty then
      setReg(instance, 7, HostCallResult.WHO)
      return

    // Read new code hash from memory
    val codeHashBuffer = new Array[Byte](Hash.Size)
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
  protected def handleNew(instance: PvmInstance): Unit =
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
    val codeHashBuffer = new Array[Byte](Hash.Size)
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
      val left = (context.nextAccountIndex - s + 42) & 0xffffffffL
      val modValue = 0xffffffffL - s - 255 // 2^32 - Cminpublicindex - 2^8
      val nextCandidate = s + (left % modValue)
      val newNextAccountIndex = findAvailableServiceIndex(nextCandidate, s)
      context.nextAccountIndex = newNextAccountIndex

    setReg(instance, 7, ULong(newServiceId))

  /** eject (21): Remove another service account.
    */
  protected def handleEject(instance: PvmInstance): Unit =
    val ejectServiceId = getReg(instance, 7).toLong
    val preimageHashAddr = getReg(instance, 8).toInt

    val hashBuffer = new Array[Byte](Hash.Size)
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
    val matchesEjectedService = (key: JamBytes) =>
      val arr = key.toArray
      arr.length >= 8 &&
        arr(0) == serviceIdBytes(0) &&
        arr(2) == serviceIdBytes(1) &&
        arr(4) == serviceIdBytes(2) &&
        arr(6) == serviceIdBytes(3) &&
        !StateKey.isChapterKey(arr) &&
        !StateKey.isAccountRecordKey(arr)
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
  protected def handleTransfer(instance: PvmInstance): Unit =
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

    // 2. Check if destination exists (WHO). All service accounts are fully
    // materialized in the in-memory accounts map on the live path
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


  /** Encode service ID as a 32-byte code hash (fixed-width little-endian
    * encoding). Used for parent-child relationship verification in eject.
    */
  private def encodeServiceIdAsCodeHash(serviceId: Long): JamBytes =
    val bytes = new Array[Byte](Hash.Size)
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
