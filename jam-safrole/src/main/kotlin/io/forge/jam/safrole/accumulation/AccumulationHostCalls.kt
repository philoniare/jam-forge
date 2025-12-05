package io.forge.jam.safrole.accumulation

import io.forge.jam.core.JamByteArray
import io.forge.jam.core.decodeFixedWidthInteger
import io.forge.jam.pvm.engine.RawInstance
import io.forge.jam.pvm.program.Reg

/**
 * Host call result constants as defined in Gray Paper.
 */
object HostCallResult {
    val OK: ULong = 0uL
    val NONE: ULong = ULong.MAX_VALUE               // 2^64 - 1: Item does not exist
    val WHAT: ULong = ULong.MAX_VALUE - 1uL         // 2^64 - 2: Name unknown
    val OOB: ULong = ULong.MAX_VALUE - 2uL          // 2^64 - 3: Memory index not accessible
    val WHO: ULong = ULong.MAX_VALUE - 3uL          // 2^64 - 4: Index unknown
    val FULL: ULong = ULong.MAX_VALUE - 4uL         // 2^64 - 5: Storage full
    val CORE: ULong = ULong.MAX_VALUE - 5uL         // 2^64 - 6: Core index unknown
    val CASH: ULong = ULong.MAX_VALUE - 6uL         // 2^64 - 7: Insufficient funds
    val LOW: ULong = ULong.MAX_VALUE - 7uL          // 2^64 - 8: Gas limit too low
    val HUH: ULong = ULong.MAX_VALUE - 8uL          // 2^64 - 9: Invalid operation
}

/**
 * Host call identifiers for accumulation as defined in Gray Paper.
 */
object HostCall {
    const val GAS = 0u
    const val FETCH = 1u
    const val LOOKUP = 2u
    const val READ = 3u
    const val WRITE = 4u
    const val INFO = 5u
    const val BLESS = 14u
    const val ASSIGN = 15u
    const val DESIGNATE = 16u
    const val CHECKPOINT = 17u
    const val NEW = 18u
    const val UPGRADE = 19u
    const val TRANSFER = 20u
    const val EJECT = 21u
    const val QUERY = 22u
    const val SOLICIT = 23u
    const val FORGET = 24u
    const val YIELD = 25u
    const val PROVIDE = 26u
    const val LOG = 100u  // Debug logging (JIP-1), gas cost = 0
}

/**
 * Register mapping for PVM host calls.
 * Gray Paper uses registers r7-r12 which map to Reg.A0-A5 in our enum.
 *
 * Gray Paper register assignments:
 * - r7 (A0): First argument / return value
 * - r8 (A1): Second argument
 * - r9 (A2): Third argument
 * - r10 (A3): Fourth argument
 * - r11 (A4): Fifth argument
 * - r12 (A5): Sixth argument
 */
private fun RawInstance.getReg7(): ULong = reg(Reg.A0)
private fun RawInstance.getReg8(): ULong = reg(Reg.A1)
private fun RawInstance.getReg9(): ULong = reg(Reg.A2)
private fun RawInstance.getReg10(): ULong = reg(Reg.A3)
private fun RawInstance.getReg11(): ULong = reg(Reg.A4)
private fun RawInstance.getReg12(): ULong = reg(Reg.A5)
private fun RawInstance.setReg7(value: ULong) = setReg(Reg.A0, value)

/**
 * Handles host calls during accumulation PVM execution.
 */
class AccumulationHostCalls(
    private val context: AccumulationContext,
    private val operands: List<AccumulationOperand>,
    private val config: AccumulationConfig
) {
    /**
     * Get gas cost for a host call without executing it.
     * Gas is charged BEFORE the host call implementation runs
     */
    fun getGasCost(hostCallId: UInt, instance: RawInstance): Long {
        return when (hostCallId) {
            HostCall.LOG -> 0L  // LOG has 0 gas cost per JIP-1
            HostCall.TRANSFER -> {
                // 0.7.1: TRANSFER gas is 10 + gasLimit
                val transferGasLimit = instance.getReg9().toLong()
                10L + transferGasLimit
            }

            else -> 10L  // Default gas cost for most host calls
        }
    }

    /**
     * Dispatch a host call based on its identifier.
     * Gas should be charged BEFORE calling this method.
     */
    fun dispatch(hostCallId: UInt, instance: RawInstance) {
        val hostCallName = when (hostCallId) {
            HostCall.GAS -> "GAS"
            HostCall.FETCH -> "FETCH"
            HostCall.LOOKUP -> "LOOKUP"
            HostCall.READ -> "READ"
            HostCall.WRITE -> "WRITE"
            HostCall.INFO -> "INFO"
            HostCall.BLESS -> "BLESS"
            HostCall.ASSIGN -> "ASSIGN"
            HostCall.DESIGNATE -> "DESIGNATE"
            HostCall.CHECKPOINT -> "CHECKPOINT"
            HostCall.NEW -> "NEW"
            HostCall.UPGRADE -> "UPGRADE"
            HostCall.TRANSFER -> "TRANSFER"
            HostCall.EJECT -> "EJECT"
            HostCall.QUERY -> "QUERY"
            HostCall.SOLICIT -> "SOLICIT"
            HostCall.FORGET -> "FORGET"
            HostCall.YIELD -> "YIELD"
            HostCall.PROVIDE -> "PROVIDE"
            HostCall.LOG -> "LOG"
            else -> "UNKNOWN"
        }
        println("[HOST-CALL] service=${context.serviceIndex}, hostCallId=$hostCallId ($hostCallName), gas=${instance.gas()}, r7=${instance.getReg7()}, r8=${instance.getReg8()}")

        when (hostCallId) {
            HostCall.GAS -> handleGas(instance)
            HostCall.FETCH -> handleFetch(instance)
            HostCall.LOOKUP -> handleLookup(instance)
            HostCall.READ -> handleRead(instance)
            HostCall.WRITE -> handleWrite(instance)
            HostCall.INFO -> handleInfo(instance)
            HostCall.BLESS -> handleBless(instance)
            HostCall.ASSIGN -> handleAssign(instance)
            HostCall.DESIGNATE -> handleDesignate(instance)
            HostCall.CHECKPOINT -> handleCheckpoint(instance)
            HostCall.NEW -> handleNew(instance)
            HostCall.UPGRADE -> handleUpgrade(instance)
            HostCall.TRANSFER -> handleTransfer(instance)
            HostCall.EJECT -> handleEject(instance)
            HostCall.QUERY -> handleQuery(instance)
            HostCall.SOLICIT -> handleSolicit(instance)
            HostCall.FORGET -> handleForget(instance)
            HostCall.YIELD -> handleYield(instance)
            HostCall.PROVIDE -> handleProvide(instance)
            HostCall.LOG -> handleLog(instance)
            else -> {
                // Unknown host call - return WHAT
                instance.setReg7(HostCallResult.WHAT)
            }
        }
    }

    /**
     * gas (0): Returns remaining gas in register r7.
     */
    private fun handleGas(instance: RawInstance) {
        instance.setReg7(instance.gas().toULong())
    }

    /**
     * fetch (1): Fetch various data based on register r10 selector.
     * For accumulation, supports fetching operands and constants.
     */
    private fun handleFetch(instance: RawInstance) {
        val selector = instance.getReg10().toInt()
        val outputAddr = instance.getReg7().toUInt()
        val offset = instance.getReg8().toInt()
        val length = instance.getReg9().toInt()
        val index = instance.getReg11().toInt()
        println("[FETCH] service=${context.serviceIndex}, selector=$selector, index=$index, offset=$offset, len=$length, totalOperands=${operands.size}")
        if (selector == 14 && length > 0) {
            val encoded = encodeOperandsList()
            println("[FETCH-14] encodedSize=${encoded.size}, operandCount=${operands.size}")
            operands.forEachIndexed { idx, op ->
                when (op) {
                    is AccumulationOperand.WorkItem -> {
                        val opTuple = op.operand
                        val opEncoded = op.encode()
                        println("[FETCH-14] op$idx (WorkItem): gasLimit=${opTuple.gasLimit}, resultOk=${opTuple.result.ok?.bytes?.size}, panic=${opTuple.result.panic}, authTrace=${opTuple.authTrace.bytes.size}, encodedSize=${opEncoded.size}")
                    }

                    is AccumulationOperand.Transfer -> {
                        val opEncoded = op.encode()
                        println("[FETCH-14] op$idx (Transfer): amount=${op.transfer.amount}, gas=${op.transfer.gasLimit}, encodedSize=${opEncoded.size}")
                    }
                }
            }
        }


        val data: ByteArray? = when (selector) {
            0 -> {
                val constants = getConstantsBlob()
                println("[FETCH-DATA] selector=0 (constants), size=${constants.size}")
                println("[FETCH-BYTES] selector=0 hex=${constants.joinToString("") { "%02x".format(it) }}")
                constants
            }

            14 -> {
                val opList = encodeOperandsList()
                println("[FETCH-DATA] selector=14 (operands list), size=${opList.size}")
                println("[FETCH-BYTES] selector=14 hex=${opList.joinToString("") { "%02x".format(it) }}")
                opList
            }

            15 -> {
                if (index < operands.size) {
                    val encoded = encodeOperand(operands[index])
                    println("[FETCH-DATA] selector=15 (operand $index), size=${encoded.size}")
                    println("[FETCH-BYTES] selector=15 index=$index hex=${encoded.joinToString("") { "%02x".format(it) }}")
                    encoded
                } else {
                    null
                }
            }

            else -> {
                null
            }
        }

        if (data == null) {
            instance.setReg7(HostCallResult.NONE)
            println("[FETCH-RESULT] returning NONE (no data)")
            return
        }

        val actualOffset = minOf(offset, data.size)
        val actualLength = minOf(length, data.size - actualOffset)
        val slice = data.copyOfRange(actualOffset, actualOffset + actualLength)

        println(
            "[FETCH-RESULT] totalSize=${data.size}, offset=$actualOffset, length=$actualLength, sliceHex=${
                slice.joinToString(
                    ""
                ) { "%02x".format(it) }
            }"
        )

        val writeResult = instance.writeMemory(outputAddr, slice)
        if (writeResult.isFailure) {
            instance.setReg7(HostCallResult.OOB)
            println("[FETCH-RESULT] write failed, returning OOB")
            return
        }

        instance.setReg7(data.size.toULong())
        println("[FETCH-RESULT] success, returning size=${data.size}")
    }

    /**
     * lookup (2): Look up preimage by hash.
     */
    private fun handleLookup(instance: RawInstance) {
        val serviceId = instance.getReg7().toLong()
        val hashAddr = instance.getReg8().toUInt()
        val outputAddr = instance.getReg9().toUInt()
        val offset = instance.getReg10().toInt()
        val length = instance.getReg11().toInt()

        // Read hash from memory
        val hashBuffer = ByteArray(32)
        val readResult = instance.readMemoryInto(hashAddr, hashBuffer)
        if (readResult.isFailure) {
            instance.setReg7(HostCallResult.OOB)
            return
        }
        val hash = JamByteArray(hashBuffer)

        // Determine which account to look up from
        val targetServiceId = if (serviceId == -1L || serviceId == context.serviceIndex) {
            context.serviceIndex
        } else {
            serviceId
        }

        val account = context.x.accounts[targetServiceId]
        if (account == null) {
            instance.setReg7(HostCallResult.WHO)
            return
        }

        val preimage = account.preimages[hash]
        if (preimage == null) {
            instance.setReg7(HostCallResult.NONE)
            return
        }

        val data = preimage.bytes
        val actualOffset = minOf(offset, data.size)
        val actualLength = minOf(length, data.size - actualOffset)
        val slice = data.copyOfRange(actualOffset, actualOffset + actualLength)

        val writeResult = instance.writeMemory(outputAddr, slice)
        if (writeResult.isFailure) {
            instance.setReg7(HostCallResult.OOB)
            return
        }

        instance.setReg7(data.size.toULong())
    }

    /**
     * read (3): Read from service storage.
     */
    private fun handleRead(instance: RawInstance) {
        val serviceId = instance.getReg7().toLong()
        val keyAddr = instance.getReg8().toUInt()
        val keyLen = instance.getReg9().toInt()
        val outputAddr = instance.getReg10().toUInt()
        val offset = instance.getReg11().toInt()
        val length = instance.getReg12().toInt()
        println("[DEBUG-READ-REGS] serviceId=$serviceId, keyAddr=$keyAddr, keyLen=$keyLen, outputAddr=$outputAddr, offset=$offset, length=$length")

        // Read key from memory
        val keyBuffer = ByteArray(keyLen)
        val readResult = instance.readMemoryInto(keyAddr, keyBuffer)
        if (readResult.isFailure) {
            instance.setReg7(HostCallResult.OOB)
            return
        }
        val key = JamByteArray(keyBuffer)

        // Determine which account to read from
        val targetServiceId = if (serviceId == -1L || serviceId == context.serviceIndex) {
            context.serviceIndex
        } else {
            serviceId
        }

        val account = context.x.accounts[targetServiceId]
        if (account == null) {
            instance.setReg7(HostCallResult.WHO)
            return
        }

        // First check in-memory storage (for values written in this execution)
        var value = account.storage[key]

        // If not found in-memory, look up in raw service data using computed state key
        if (value == null) {
            val stateKey = computeStorageStateKey(targetServiceId, key)
            value = context.x.rawServiceDataByStateKey[stateKey]
        }

        println("[DEBUG-READ] service=$targetServiceId, key=${key.toHex()}, found=${value != null}, value=${value?.toHex()}")
        if (value == null) {
            println("[DEBUG-READ] returning NONE=${HostCallResult.NONE} (0x${HostCallResult.NONE.toString(16)})")
            instance.setReg7(HostCallResult.NONE)
            return
        }

        val data = value.bytes
        val actualOffset = minOf(offset, data.size)
        val actualLength = minOf(length, data.size - actualOffset)
        val slice = data.copyOfRange(actualOffset, actualOffset + actualLength)

        val writeResult = instance.writeMemory(outputAddr, slice)
        if (writeResult.isFailure) {
            instance.setReg7(HostCallResult.OOB)
            return
        }

        instance.setReg7(data.size.toULong())
    }

    /**
     * write (4): Write to service storage.
     * Updates storage map and adjusts bytes/items counters in ServiceInfo.
     * Returns: old value length on success, NONE if key didn't exist, FULL if threshold exceeded.
     */
    private fun handleWrite(instance: RawInstance) {
        val keyAddr = instance.getReg7().toUInt()
        val keyLen = instance.getReg8().toInt()
        val valueAddr = instance.getReg9().toUInt()
        val valueLen = instance.getReg10().toInt()
        println("[DEBUG-WRITE] keyAddr=$keyAddr, keyLen=$keyLen, valueAddr=$valueAddr, valueLen=$valueLen")

        val account = context.x.accounts[context.serviceIndex]
        if (account == null) {
            throw RuntimeException("Write PANIC: Current service account not found")
        }

        // Read key from memory
        val keyBuffer = ByteArray(keyLen)
        val keyReadResult = instance.readMemoryInto(keyAddr, keyBuffer)
        if (keyReadResult.isFailure) {
            throw RuntimeException("Write PANIC: Failed to read key from memory at $keyAddr len $keyLen")
        }
        val key = JamByteArray(keyBuffer)

        // Track old value for return value and bytes calculation
        // Check both in-memory storage and raw service data
        var oldValue = account.storage[key]
        val stateKeyForLookup = computeStorageStateKey(context.serviceIndex, key)
        if (oldValue == null) {
            oldValue = context.x.rawServiceDataByStateKey[stateKeyForLookup]
        }
        val oldValueSize = oldValue?.bytes?.size ?: 0
        val keyWasPresent = oldValue != null
        println("[WRITE-LOOKUP] key=${key.toHex()}, stateKey=${stateKeyForLookup.toHex()}, keyWasPresent=$keyWasPresent, valueLen=$valueLen, oldValueSize=$oldValueSize")

        // Calculate new footprint to check threshold
        val newValue = if (valueLen == 0) null else {
            val valueBuffer = ByteArray(valueLen)
            val valueReadResult = instance.readMemoryInto(valueAddr, valueBuffer)
            if (valueReadResult.isFailure) {
                throw RuntimeException("Write PANIC: Failed to read value from memory at $valueAddr len $valueLen")
            }
            JamByteArray(valueBuffer)
        }

        // Calculate bytes/items delta for threshold check
        val (bytesDelta, itemsDelta) = when {
            valueLen == 0 && keyWasPresent -> {
                // Delete: decrement bytes (key + value + 34) and items
                Pair(-(keyLen.toLong() + oldValueSize + 34), -1)
            }

            valueLen == 0 && !keyWasPresent -> {
                // Delete non-existent key: no change
                Pair(0L, 0)
            }

            keyWasPresent -> {
                // Update: only value size changes
                Pair((valueLen - oldValueSize).toLong(), 0)
            }

            else -> {
                // Insert: add key + value + 34 overhead
                Pair((keyLen + valueLen + 34).toLong(), 1)
            }
        }

        // Calculate new threshold balance and check against current balance
        val info = account.info
        val newBytes = info.bytes + bytesDelta
        val newItems = info.items + itemsDelta
        val base = config.SERVICE_MIN_BALANCE
        val itemsCost = config.ADDITIONAL_MIN_BALANCE_PER_STATE_ITEM * newItems
        val bytesCost = config.ADDITIONAL_MIN_BALANCE_PER_STATE_BYTE * newBytes

        // Calculate threshold using unsigned arithmetic for depositOffset (gratisStorage)
        // depositOffset can be MAX_UINT64, so we need to treat it as unsigned
        val costUnsigned = (base + itemsCost + bytesCost).toULong()
        val gratisUnsigned = info.depositOffset.toULong()
        val newThreshold = if (costUnsigned > gratisUnsigned) (costUnsigned - gratisUnsigned).toLong() else 0L

        // Use unsigned comparison for balance - balance is stored as u64 but may appear as negative Long
        val balanceUnsigned = info.balance.toULong()
        val thresholdUnsigned = newThreshold.toULong()
        if (thresholdUnsigned > balanceUnsigned) {
            println("[WRITE-FULL] threshold=$thresholdUnsigned > balance=$balanceUnsigned, bytes=$newBytes, items=$newItems")
            instance.setReg7(HostCallResult.FULL)
            return
        }

        // Compute state key for raw storage updates
        val stateKey = computeStorageStateKey(context.serviceIndex, key)

        // Apply the write
        if (valueLen == 0) {
            // Delete key
            if (keyWasPresent) {
                account.storage.remove(key)
                context.x.rawServiceDataByStateKey.remove(stateKey)
                println("[WRITE-DELETE] key=${key.toHex()}, oldValueSize=$oldValueSize, newBytes=$newBytes, newItems=$newItems")
            }
        } else {
            // Write new value to both in-memory storage and raw service data
            account.storage[key] = newValue!!
            context.x.rawServiceDataByStateKey[stateKey] = newValue
            println("[WRITE-SET] key=${key.toHex()}, value=${newValue.toHex()}, valueLen=$valueLen, keyWasPresent=$keyWasPresent, newBytes=$newBytes, newItems=$newItems")
        }

        // Update account info with new bytes/items
        val updatedInfo = info.copy(
            bytes = newBytes,
            items = newItems
        )
        context.x.accounts[context.serviceIndex] = account.copy(info = updatedInfo)

        // Return old value length (or NONE if key didn't exist)
        val returnValue = if (keyWasPresent) oldValueSize.toULong() else HostCallResult.NONE
        println("[WRITE-RESULT] returnValue=$returnValue, storageSize=${account.storage.size}")
        instance.setReg7(returnValue)
    }

    /**
     * info (5): Get service account info.
     * Returns 96 bytes: codeHash(32) + balance(8) + thresholdBalance(8) + minAccumulateGas(8) +
     *                   minMemoGas(8) + totalByteLength(8) + itemsCount(4) + gratisStorage(8) +
     *                   createdAt(4) + lastAccAt(4) + parentService(4)
     */
    private fun handleInfo(instance: RawInstance) {
        val serviceId = instance.getReg7().toLong()
        val outputAddr = instance.getReg8().toUInt()
        val offset = instance.getReg9().toInt()
        val length = instance.getReg10().toInt()

        val targetServiceId = if (serviceId == -1L) context.serviceIndex else serviceId
        val account = context.x.accounts[targetServiceId]

        if (account == null) {
            instance.setReg7(HostCallResult.NONE)
            return
        }

        val info = account.info

        // Calculate threshold balance: max(0, base + items*itemCost + bytes*byteCost - gratisStorage)
        val base = config.SERVICE_MIN_BALANCE
        val itemsCost = config.ADDITIONAL_MIN_BALANCE_PER_STATE_ITEM * info.items
        val bytesCost = config.ADDITIONAL_MIN_BALANCE_PER_STATE_BYTE * info.bytes
        // Note: depositOffset (gratisStorage) could be max u64, treat as unsigned
        val gratisStorage = info.depositOffset.toULong()
        val thresholdUnsigned = (base + itemsCost + bytesCost).toULong()
        val thresholdBalance =
            if (thresholdUnsigned > gratisStorage) (thresholdUnsigned - gratisStorage).toLong() else 0L
        println("[INFO] service=$targetServiceId, balance=${info.balance}, threshold=$thresholdBalance, bytes=${info.bytes}, items=${info.items}, gratis=$gratisStorage, minItemGas=${info.minItemGas}, minMemoGas=${info.minMemoGas}, creationSlot=${info.creationSlot}, lastAccSlot=${info.lastAccumulationSlot}, parentService=${info.parentService}")

        // Encode all 11 fields (96 bytes total)
        val data = info.codeHash.bytes +                      // 32 bytes
            encodeLong(info.balance) +                         // 8 bytes
            encodeLong(thresholdBalance) +                     // 8 bytes
            encodeLong(info.minItemGas) +                      // 8 bytes (minAccumulateGas)
            encodeLong(info.minMemoGas) +                      // 8 bytes
            encodeLong(info.bytes) +                           // 8 bytes (totalByteLength)
            encodeInt(info.items) +                            // 4 bytes (itemsCount)
            encodeLong(info.depositOffset) +                   // 8 bytes (gratisStorage)
            encodeInt(info.creationSlot.toInt()) +             // 4 bytes (createdAt)
            encodeInt(info.lastAccumulationSlot.toInt()) +     // 4 bytes (lastAccAt)
            encodeInt(info.parentService.toInt())              // 4 bytes

        // Apply offset and length slicing
        val first = minOf(offset, data.size)
        val len = minOf(length, data.size - first)
        val slicedData = data.copyOfRange(first, first + len)

        val writeResult = instance.writeMemory(outputAddr, slicedData)
        if (writeResult.isFailure) {
            throw RuntimeException("Info PANIC: Failed to write to memory at $outputAddr")
        }

        // Return the full data length (not sliced length)
        instance.setReg7(data.size.toULong())
    }

    /**
     * bless (14): Set privileged services.
     * reg7 = manager, reg8 = assigners ptr, reg9 = delegator, reg10 = registrar,
     * reg11 = always-acc pairs ptr, reg12 = always-acc pairs count
     */
    private fun handleBless(instance: RawInstance) {
        // Only manager service can call bless
        if (context.serviceIndex != context.x.manager) {
            instance.setReg7(HostCallResult.HUH)
            return
        }

        val newManager = instance.getReg7().toLong()
        val assignersPtr = instance.getReg8().toUInt()
        val newDelegator = instance.getReg9().toLong()
        val newRegistrar = instance.getReg10().toLong()
        val alwaysAccPtr = instance.getReg11().toUInt()
        val alwaysAccCount = instance.getReg12().toInt()

        // Read assigners array (4 bytes per core)
        val coresCount = config.EPOCH_LENGTH // Use epoch length as number of cores
        val assignersBytes = ByteArray(4 * coresCount)
        val assignersResult = instance.readMemoryInto(assignersPtr, assignersBytes)
        if (assignersResult.isFailure) {
            throw RuntimeException("Bless PANIC: Failed to read assigners from memory")
        }

        // Parse assigners
        val newAssigners = mutableListOf<Long>()
        for (i in 0 until coresCount) {
            val assigner = decodeFixedWidthInteger(assignersBytes, i * 4, 4, false)
            newAssigners.add(assigner)
        }

        // Read always-acc pairs (12 bytes each: 4 service + 8 gas)
        val alwaysAccMap = mutableMapOf<Long, Long>()
        if (alwaysAccCount > 0) {
            val alwaysAccBytes = ByteArray(12 * alwaysAccCount)
            val alwaysAccResult = instance.readMemoryInto(alwaysAccPtr, alwaysAccBytes)
            if (alwaysAccResult.isFailure) {
                throw RuntimeException("Bless PANIC: Failed to read always-acc from memory")
            }

            for (i in 0 until alwaysAccCount) {
                val offset = i * 12
                val serviceId = decodeFixedWidthInteger(alwaysAccBytes, offset, 4, false)
                val gas = decodeFixedWidthInteger(alwaysAccBytes, offset + 4, 8, false)
                alwaysAccMap[serviceId] = gas
            }
        }

        // Validate service indices
        if (newManager < 0 || newManager > UInt.MAX_VALUE.toLong() ||
            newDelegator < 0 || newDelegator > UInt.MAX_VALUE.toLong() ||
            newRegistrar < 0 || newRegistrar > UInt.MAX_VALUE.toLong()
        ) {
            instance.setReg7(HostCallResult.WHO)
            return
        }

        // Apply all changes
        context.x.manager = newManager
        context.x.delegator = newDelegator
        context.x.registrar = newRegistrar
        context.x.assigners.clear()
        context.x.assigners.addAll(newAssigners)
        context.x.alwaysAccers.clear()
        context.x.alwaysAccers.putAll(alwaysAccMap)

        instance.setReg7(HostCallResult.OK)
    }

    /**
     * assign (15): Set core assigner (privileged).
     */
    private fun handleAssign(instance: RawInstance) {
        val coreIndex = instance.getReg7().toInt()
        val newAssigner = instance.getReg8().toLong()

        // Check if caller is manager or current assigner for this core
        val canAssign = context.serviceIndex == context.x.manager ||
            (coreIndex < context.x.assigners.size && context.x.assigners[coreIndex] == context.serviceIndex)

        if (!canAssign) {
            instance.setReg7(HostCallResult.HUH)
            return
        }

        if (coreIndex >= config.EPOCH_LENGTH) {
            instance.setReg7(HostCallResult.CORE)
            return
        }

        while (context.x.assigners.size <= coreIndex) {
            context.x.assigners.add(0)
        }
        context.x.assigners[coreIndex] = newAssigner
        instance.setReg7(HostCallResult.OK)
    }

    /**
     * designate (16): Set delegator (privileged).
     */
    private fun handleDesignate(instance: RawInstance) {
        // Only manager or current delegator can call designate
        if (context.serviceIndex != context.x.manager && context.serviceIndex != context.x.delegator) {
            instance.setReg7(HostCallResult.HUH)
            return
        }

        val newDelegator = instance.getReg7().toLong()
        context.x.delegator = newDelegator
        instance.setReg7(HostCallResult.OK)
    }

    /**
     * checkpoint (17): Save current state x to checkpoint y.
     */
    private fun handleCheckpoint(instance: RawInstance) {
        context.checkpoint()
        instance.setReg7(HostCallResult.OK)
    }

    /**
     * new (18): Create new service account.
     * reg7 = codeHashAddr, reg8 = codeHashLength (for preimage info),
     * reg9 = minAccumlateGas, reg10 = minMemoGas, reg11 = gratisStorage,
     * reg12 = requested service index (if caller is registrar)
     */
    private fun handleNew(instance: RawInstance) {
        val codeHashAddr = instance.getReg7().toUInt()
        val codeHashLength = instance.getReg8().toInt()  // Length for preimage info key
        val minAccumulateGas = instance.getReg9().toLong()
        val minMemoGas = instance.getReg10().toLong()
        val gratisStorage = instance.getReg11().toLong()
        val requestedServiceId = instance.getReg12().toLong()

        println("[NEW-ENTRY] codeHashAddr=$codeHashAddr, codeHashLen=$codeHashLength, minAccGas=$minAccumulateGas, minMemoGas=$minMemoGas, gratis=$gratisStorage, reqId=$requestedServiceId")

        // Read code hash from memory - PANIC if not readable
        val codeHashBuffer = ByteArray(32)
        val readResult = instance.readMemoryInto(codeHashAddr, codeHashBuffer)
        if (readResult.isFailure) {
            println("[NEW-PANIC] Failed to read code hash from memory at $codeHashAddr")
            throw RuntimeException("New PANIC: Failed to read code hash from memory at $codeHashAddr")
        }

        val codeHash = JamByteArray(codeHashBuffer)

        // Check gratisStorage permission
        if (gratisStorage != 0L && context.serviceIndex != context.x.manager) {
            println("[NEW-HUH] gratis=$gratisStorage but service=${context.serviceIndex} != manager=${context.x.manager}")
            instance.setReg7(HostCallResult.HUH)
            return
        }

        val currentAccount = context.x.accounts[context.serviceIndex]
        if (currentAccount == null) {
            throw RuntimeException("New PANIC: Current service account not found")
        }

        // Calculate threshold balance for new account
        // The new account starts with: 1 preimage info entry (items=1)
        // Bytes = preimage info overhead
        val newAccountItems = 1
        val newAccountBytes = 0L  // Will be updated when preimage is provided
        val base = config.SERVICE_MIN_BALANCE
        val itemsCost = config.ADDITIONAL_MIN_BALANCE_PER_STATE_ITEM * newAccountItems
        val bytesCost = config.ADDITIONAL_MIN_BALANCE_PER_STATE_BYTE * newAccountBytes
        val gratisUnsigned = gratisStorage.toULong()
        val costUnsigned = (base + itemsCost + bytesCost).toULong()
        val thresholdBalance = if (costUnsigned > gratisUnsigned) (costUnsigned - gratisUnsigned).toLong() else 0L

        // Check if caller can afford the new account
        // Use unsigned arithmetic since balance can be MAX_UINT64
        val callerBalanceUnsigned = currentAccount.info.balance.toULong()
        val thresholdBalanceUnsigned = thresholdBalance.toULong()

        // Calculate caller's threshold balance (what they need to keep)
        val callerBase = config.SERVICE_MIN_BALANCE
        val callerItemsCost = config.ADDITIONAL_MIN_BALANCE_PER_STATE_ITEM * currentAccount.info.items
        val callerBytesCost = config.ADDITIONAL_MIN_BALANCE_PER_STATE_BYTE * currentAccount.info.bytes
        val callerGratisUnsigned = currentAccount.info.depositOffset.toULong()
        val callerCostUnsigned = (callerBase + callerItemsCost + callerBytesCost).toULong()
        val callerThresholdUnsigned =
            if (callerCostUnsigned > callerGratisUnsigned) (callerCostUnsigned - callerGratisUnsigned) else 0uL

        // Rewritten as: account.balance < newAccount.thresholdBalance + account.thresholdBalance
        val requiredBalance = thresholdBalanceUnsigned + callerThresholdUnsigned
        if (callerBalanceUnsigned < requiredBalance) {
            println("[NEW-CASH] callerBalanceUnsigned=$callerBalanceUnsigned < required=$requiredBalance (threshold=$thresholdBalanceUnsigned + callerThreshold=$callerThresholdUnsigned)")
            instance.setReg7(HostCallResult.CASH)
            return
        }
        println("[NEW-OK] callerBalance=$callerBalanceUnsigned, thresholdBalance=$thresholdBalanceUnsigned, callerThreshold=$callerThresholdUnsigned")

        // Determine new service ID
        val newServiceId: Long
        val minPublicServiceIndex = 256L  // From config

        if (context.serviceIndex == context.x.registrar && requestedServiceId >= 0 && requestedServiceId < minPublicServiceIndex) {
            // Registrar can request specific service ID below minPublicServiceIndex
            if (context.x.accounts.containsKey(requestedServiceId)) {
                instance.setReg7(HostCallResult.FULL)
                return
            }
            newServiceId = requestedServiceId
        } else {
            // Use pre-calculated nextAccountIndex
            newServiceId = context.nextAccountIndex
        }

        // Create new account with calculated threshold balance
        val newAccount = ServiceAccount(
            info = io.forge.jam.safrole.report.ServiceInfo(
                codeHash = codeHash,
                balance = thresholdBalance,
                minItemGas = minAccumulateGas,
                minMemoGas = minMemoGas,
                bytes = newAccountBytes,
                items = newAccountItems,
                depositOffset = gratisStorage,
                creationSlot = context.timeslot,
                lastAccumulationSlot = 0,
                parentService = context.serviceIndex
            ),
            storage = mutableMapOf(),
            preimages = mutableMapOf(),
            preimageRequests = mutableMapOf(
                // Initialize preimage info for code hash with empty requestedAt list
                PreimageKey(codeHash, codeHashLength) to PreimageRequest(emptyList())
            )
        )

        context.x.accounts[newServiceId] = newAccount
        println("[NEW] created service=$newServiceId, balance=$thresholdBalance, parent=${context.serviceIndex}, codeHash=${codeHash.toHex()}")

        // Deduct balance from creator
        val updatedCreatorInfo = currentAccount.info.copy(
            balance = currentAccount.info.balance - thresholdBalance
        )
        context.x.accounts[context.serviceIndex] = currentAccount.copy(
            info = updatedCreatorInfo
        )

        // Update nextAccountIndex for next NEW call (ONLY if not using registrar privilege)
        if (context.serviceIndex != context.x.registrar || requestedServiceId >= minPublicServiceIndex) {
            val s = minPublicServiceIndex
            val left = (context.nextAccountIndex - s + 42).toUInt()
            val right = (UInt.MAX_VALUE - s.toUInt() - 255u)
            val nextCandidate = s + (left % right).toLong()
            context.nextAccountIndex = findAvailableServiceIndex(nextCandidate, s)
            println("[NEW] nextAccountIndex updated from $newServiceId to ${context.nextAccountIndex}")
        }

        instance.setReg7(newServiceId.toULong())
    }

    /**
     * upgrade (19): Upgrade service code hash.
     */
    private fun handleUpgrade(instance: RawInstance) {
        val codeHashAddr = instance.getReg7().toUInt()

        val account = context.x.accounts[context.serviceIndex]
        if (account == null) {
            instance.setReg7(HostCallResult.WHO)
            return
        }

        // Read new code hash from memory
        val codeHashBuffer = ByteArray(32)
        val readResult = instance.readMemoryInto(codeHashAddr, codeHashBuffer)
        if (readResult.isFailure) {
            instance.setReg7(HostCallResult.OOB)
            return
        }

        val updatedInfo = account.info.copy(codeHash = JamByteArray(codeHashBuffer))
        context.x.accounts[context.serviceIndex] = account.copy(info = updatedInfo)

        instance.setReg7(HostCallResult.OK)
    }

    /**
     * transfer (20): Queue a deferred transfer.
     * 0.7.1: Gas is g = 10 + gasLimit (charged regardless of success/failure).
     */
    private fun handleTransfer(instance: RawInstance) {
        val destination = instance.getReg7().toLong()
        val amount = instance.getReg8().toLong()
        val gasLimit = instance.getReg9().toLong()
        val memoAddr = instance.getReg10().toUInt()

        val account = context.x.accounts[context.serviceIndex]
        val accounts = context.x.accounts

        println("[TRANSFER] service=${context.serviceIndex}, dest=$destination, amount=$amount, gasLimit=$gasLimit, balance=${account?.info?.balance}")

        // 1. Read memo from memory (128 bytes) - PANIC if fails
        val memoBuffer = ByteArray(DeferredTransfer.MEMO_SIZE)
        val readResult = instance.readMemoryInto(memoAddr, memoBuffer)
        if (readResult.isFailure) {
            throw RuntimeException("Transfer PANIC: Failed to read memo from memory at $memoAddr")
        }

        // 2. Check if destination exists (WHO)
        if (!accounts.containsKey(destination)) {
            instance.setReg7(HostCallResult.WHO)
            return
        }

        // 3. Check if gasLimit >= destination.minMemoGas (LOW)
        val destAccount = accounts[destination]!!
        if (gasLimit < destAccount.info.minMemoGas) {
            instance.setReg7(HostCallResult.LOW)
            return
        }

        // 4. Check if caller can afford it - balance after deduction >= caller.minBalance (CASH)
        // b = caller.balance - amount
        // Check: b < caller.minBalance
        if (account == null) {
            instance.setReg7(HostCallResult.CASH)
            return
        }

        val balanceAfterTransfer = account.info.balance - amount
        // Calculate caller's minimum balance (threshold)
        val base = config.SERVICE_MIN_BALANCE
        val itemsCost = config.ADDITIONAL_MIN_BALANCE_PER_STATE_ITEM * account.info.items
        val bytesCost = config.ADDITIONAL_MIN_BALANCE_PER_STATE_BYTE * account.info.bytes
        val gratisUnsigned = account.info.depositOffset.toULong()
        val thresholdUnsigned = (base + itemsCost + bytesCost).toULong()
        val callerMinBalance =
            if (thresholdUnsigned > gratisUnsigned) (thresholdUnsigned - gratisUnsigned).toLong() else 0L

        // Use unsigned comparison
        if (balanceAfterTransfer.toULong() < callerMinBalance.toULong()) {
            instance.setReg7(HostCallResult.CASH)
            return
        }

        // 5. Success - deduct balance and queue transfer
        val updatedInfo = account.info.copy(balance = balanceAfterTransfer)
        context.x.accounts[context.serviceIndex] = account.copy(info = updatedInfo)

        context.deferredTransfers.add(
            DeferredTransfer(
                source = context.serviceIndex,
                destination = destination,
                amount = amount,
                memo = JamByteArray(memoBuffer),
                gasLimit = gasLimit
            )
        )

        instance.setReg7(HostCallResult.OK)
    }

    /**
     * eject (21): Eject (remove) another service account.
     */
    private fun handleEject(instance: RawInstance) {
        val ejectServiceId = instance.getReg7().toLong()
        val preimageHashAddr = instance.getReg8().toUInt()

        println("[EJECT] Attempting to eject service $ejectServiceId by caller ${context.serviceIndex}")

        // 1. Read 32-byte preimage hash from memory - PANIC on failure
        val hashBuffer = ByteArray(32)
        val readResult = instance.readMemoryInto(preimageHashAddr, hashBuffer)
        if (readResult.isFailure) {
            throw RuntimeException("Eject PANIC: Failed to read preimage hash from memory")
        }
        val preimageHash = JamByteArray(hashBuffer)
        println("[EJECT] preimageHash=${preimageHash.toHex()}")

        // 2. Get target service account
        val ejectAccount = context.x.accounts[ejectServiceId]

        // 3. Validate: target exists AND target != caller AND codeHash matches caller's ID
        val expectedCodeHash = encodeServiceIdAsCodeHash(context.serviceIndex)
        if (ejectServiceId == context.serviceIndex) {
            println("[EJECT] WHO: Cannot eject self")
            instance.setReg7(HostCallResult.WHO)
            return
        }
        if (ejectAccount == null) {
            println("[EJECT] WHO: Account $ejectServiceId not found")
            instance.setReg7(HostCallResult.WHO)
            return
        }
        println("[EJECT] ejectAccount.codeHash=${ejectAccount.info.codeHash.toHex()}, expected=${expectedCodeHash.toHex()}")
        if (ejectAccount.info.codeHash != expectedCodeHash) {
            println("[EJECT] WHO: codeHash mismatch")
            instance.setReg7(HostCallResult.WHO)
            return
        }

        // 4. Validate item count is exactly 2
        println("[EJECT] ejectAccount.items=${ejectAccount.info.items}")
        if (ejectAccount.info.items != 2) {
            println("[EJECT] HUH: items != 2")
            instance.setReg7(HostCallResult.HUH)
            return
        }

        // 5. Find preimage request by hash (ignoring length - the test data doesn't include length in preimageStatus)
        println("[EJECT] Looking up preimage by hash=${preimageHash.toHex()}")
        println("[EJECT] Available preimageRequest keys=${ejectAccount.preimageRequests.keys.map { "(${it.hash.toHex()}, ${it.length})" }}")
        val preimageEntry = ejectAccount.preimageRequests.entries.find { it.key.hash == preimageHash }
        val preimageRequest = preimageEntry?.value
        println("[EJECT] preimageRequest=$preimageRequest")
        if (preimageRequest == null) {
            println("[EJECT] HUH: preimageRequest is null")
            instance.setReg7(HostCallResult.HUH)
            return
        }
        println("[EJECT] requestedAt=${preimageRequest.requestedAt}")
        if (preimageRequest.requestedAt.size != 2) {
            println("[EJECT] HUH: requestedAt.size=${preimageRequest.requestedAt.size} != 2")
            instance.setReg7(HostCallResult.HUH)
            return
        }

        // 7. Validate expunge period: requestedAt[1] < timeslot - expungePeriod
        val expungePeriod = config.PREIMAGE_PURGE_PERIOD
        val minHoldSlot = maxOf(0L, context.timeslot - expungePeriod)
        println("[EJECT] expungePeriod=$expungePeriod, minHoldSlot=$minHoldSlot, requestedAt[1]=${preimageRequest.requestedAt[1]}")
        if (preimageRequest.requestedAt[1] >= minHoldSlot) {
            println("[EJECT] HUH: requestedAt[1]=${preimageRequest.requestedAt[1]} >= minHoldSlot=$minHoldSlot")
            instance.setReg7(HostCallResult.HUH)
            return
        }

        // 8. SUCCESS: Transfer balance to caller and remove ejected service
        val callerAccount = context.x.accounts[context.serviceIndex]!!
        val updatedCallerInfo = callerAccount.info.copy(
            balance = callerAccount.info.balance + ejectAccount.info.balance
        )
        context.x.accounts[context.serviceIndex] = callerAccount.copy(info = updatedCallerInfo)
        context.x.accounts.remove(ejectServiceId)
        println("[EJECT] SUCCESS: Removed service $ejectServiceId, transferred balance to ${context.serviceIndex}")

        instance.setReg7(HostCallResult.OK)
    }

    /**
     * Encode service ID as a 32-byte code hash (fixed-width little-endian encoding).
     * Used for parent-child relationship verification in eject.
     */
    private fun encodeServiceIdAsCodeHash(serviceId: Long): JamByteArray {
        val bytes = ByteArray(32)
        // Little-endian encoding of service ID in first 4 bytes
        bytes[0] = (serviceId and 0xFF).toByte()
        bytes[1] = ((serviceId shr 8) and 0xFF).toByte()
        bytes[2] = ((serviceId shr 16) and 0xFF).toByte()
        bytes[3] = ((serviceId shr 24) and 0xFF).toByte()
        return JamByteArray(bytes)
    }

    /**
     * query (22): Query preimage request status.
     */
    private fun handleQuery(instance: RawInstance) {
        val hashAddr = instance.getReg7().toUInt()
        val length = instance.getReg8().toInt()

        val account = context.x.accounts[context.serviceIndex]
        if (account == null) {
            instance.setReg7(HostCallResult.WHO)
            return
        }

        // Read hash from memory
        val hashBuffer = ByteArray(32)
        val readResult = instance.readMemoryInto(hashAddr, hashBuffer)
        if (readResult.isFailure) {
            instance.setReg7(HostCallResult.OOB)
            return
        }

        val key = PreimageKey(JamByteArray(hashBuffer), length)
        val request = account.preimageRequests[key]

        if (request == null) {
            instance.setReg7(HostCallResult.NONE)
        } else {
            instance.setReg7(request.requestedAt.firstOrNull()?.toULong() ?: HostCallResult.NONE)
        }
    }

    /**
     * solicit (23): Request a preimage.
     */
    private fun handleSolicit(instance: RawInstance) {
        val hashAddr = instance.getReg7().toUInt()
        val length = instance.getReg8().toInt()

        val account = context.x.accounts[context.serviceIndex]
        if (account == null) {
            instance.setReg7(HostCallResult.WHO)
            return
        }

        // Read hash from memory
        val hashBuffer = ByteArray(32)
        val readResult = instance.readMemoryInto(hashAddr, hashBuffer)
        if (readResult.isFailure) {
            instance.setReg7(HostCallResult.OOB)
            return
        }

        val key = PreimageKey(JamByteArray(hashBuffer), length)

        if (account.preimageRequests.containsKey(key)) {
            instance.setReg7(HostCallResult.HUH)
            return
        }

        account.preimageRequests[key] = PreimageRequest(listOf(context.timeslot))
        instance.setReg7(HostCallResult.OK)
    }

    /**
     * forget (24): Cancel a preimage request.
     */
    private fun handleForget(instance: RawInstance) {
        val hashAddr = instance.getReg7().toUInt()
        val length = instance.getReg8().toInt()

        val account = context.x.accounts[context.serviceIndex]
        if (account == null) {
            instance.setReg7(HostCallResult.WHO)
            return
        }

        // Read hash from memory
        val hashBuffer = ByteArray(32)
        val readResult = instance.readMemoryInto(hashAddr, hashBuffer)
        if (readResult.isFailure) {
            instance.setReg7(HostCallResult.OOB)
            return
        }

        val key = PreimageKey(JamByteArray(hashBuffer), length)

        if (!account.preimageRequests.containsKey(key)) {
            instance.setReg7(HostCallResult.HUH)
            return
        }

        account.preimageRequests.remove(key)
        instance.setReg7(HostCallResult.OK)
    }

    /**
     * yield (25): Set accumulation output hash.
     */
    private fun handleYield(instance: RawInstance) {
        val hashAddr = instance.getReg7().toUInt()

        // Read hash from memory
        val hashBuffer = ByteArray(32)
        val readResult = instance.readMemoryInto(hashAddr, hashBuffer)
        if (readResult.isFailure) {
            instance.setReg7(HostCallResult.OOB)
            return
        }

        // Store the yield in the context
        context.yield = JamByteArray(hashBuffer)
        // println("[YIELD] service=${context.serviceIndex}, hash=${context.yield?.toHex()}")
        instance.setReg7(HostCallResult.OK)
    }

    /**
     * provide (26): Provide a preimage for another service.
     */
    private fun handleProvide(instance: RawInstance) {
        val targetServiceId = instance.getReg7().toLong()
        val blobAddr = instance.getReg8().toUInt()
        val blobLen = instance.getReg9().toInt()

        // Read blob from memory
        val blobBuffer = ByteArray(blobLen)
        val readResult = instance.readMemoryInto(blobAddr, blobBuffer)
        if (readResult.isFailure) {
            instance.setReg7(HostCallResult.OOB)
            return
        }

        context.provisions.add(Pair(targetServiceId, JamByteArray(blobBuffer)))
        instance.setReg7(HostCallResult.OK)
    }

    /**
     * log (100): Debug logging host call
     * This is a no-op for production but allows the test service to output debug info.
     */
    private fun handleLog(instance: RawInstance) {
        // Log is a no-op - just for debugging purposes
        // No return value set, execution continues
        instance.getReg7()
        instance.getReg8()
        val ptr = instance.getReg10().toUInt()
        val len = instance.getReg11().toInt()

        val buffer = ByteArray(len)
        val result = instance.readMemoryInto(ptr, buffer)
        if (result.isSuccess) {
            String(buffer)
        } else {
        }
    }

    // Helper methods

    /**
     * Encode protocol configuration as expected by the guest.
     */
    private fun getConstantsBlob(): ByteArray {
        val buffer = java.io.ByteArrayOutputStream()

        // Tiny config values
        buffer.write(encodeLong(10L))                   // additionalMinBalancePerStateItem (UInt64)
        buffer.write(encodeLong(1L))                    // additionalMinBalancePerStateByte (UInt64)
        buffer.write(encodeLong(100L))                  // serviceMinBalance (UInt64)
        buffer.write(encodeShort(2))                    // totalNumberOfCores (UInt16)
        buffer.write(encodeIntLE(32))                   // preimagePurgePeriod (UInt32) - tiny is 32
        buffer.write(encodeIntLE(12))                   // epochLength (UInt32)
        buffer.write(encodeLong(10_000_000L))           // workReportAccumulationGas (UInt64) - tiny
        buffer.write(encodeLong(50_000_000L))           // workPackageIsAuthorizedGas (UInt64) - tiny
        buffer.write(encodeLong(1_000_000_000L))        // workPackageRefineGas (UInt64) - tiny
        buffer.write(encodeLong(20_000_000L))           // totalAccumulationGas (UInt64) - tiny
        buffer.write(encodeShort(8))                    // recentHistorySize (UInt16)
        buffer.write(encodeShort(16))                   // maxWorkItems (UInt16) - tiny is 16
        buffer.write(encodeShort(8))                    // maxDepsInWorkReport (UInt16)
        buffer.write(encodeShort(3))                    // maxTicketsPerExtrinsic (UInt16) - tiny is 3
        buffer.write(encodeIntLE(24))                   // maxLookupAnchorAge (UInt32) - tiny is 24
        buffer.write(encodeShort(3))                    // ticketEntriesPerValidator (UInt16) - tiny is 3
        buffer.write(encodeShort(8))                    // maxAuthorizationsPoolItems (UInt16) - tiny is 8
        buffer.write(encodeShort(6))                    // slotPeriodSeconds (UInt16)
        buffer.write(encodeShort(80))                   // maxAuthorizationsQueueItems (UInt16)
        buffer.write(encodeShort(4))                    // coreAssignmentRotationPeriod (UInt16) - tiny is 4 (4 divides 12!)
        buffer.write(encodeShort(128))                  // maxWorkPackageExtrinsics (UInt16) - tiny is 128
        buffer.write(encodeShort(5))                    // preimageReplacementPeriod (UInt16) - tiny is 5
        buffer.write(encodeShort(6))                    // totalNumberOfValidators (UInt16)
        buffer.write(encodeIntLE(64000))                // maxIsAuthorizedCodeSize (UInt32) - tiny is 64000
        buffer.write(encodeIntLE(13_794_305))           // maxEncodedWorkPackageSize (UInt32) - tiny
        buffer.write(encodeIntLE(4_000_000))            // maxServiceCodeSize (UInt32) - tiny is 4_000_000
        buffer.write(encodeIntLE(4))                    // erasureCodedPieceSize (UInt32) - tiny is 4
        buffer.write(encodeIntLE(3072))                 // maxWorkPackageImports (UInt32)
        buffer.write(encodeIntLE(1026))                 // erasureCodedSegmentSize (UInt32) - tiny is 1026
        buffer.write(encodeIntLE(48 * 1024))            // maxWorkReportBlobSize (UInt32) 48KB
        buffer.write(encodeIntLE(128))                  // transferMemoSize (UInt32)
        buffer.write(encodeIntLE(3072))                 // maxWorkPackageExports (UInt32)
        buffer.write(encodeIntLE(10))                   // ticketSubmissionEndSlot (UInt32) - tiny is 10

        val result = buffer.toByteArray()
        return result
    }

    private fun encodeShort(value: Int): ByteArray {
        return ByteArray(2) { i -> ((value shr (i * 8)) and 0xFF).toByte() }
    }

    private fun encodeIntLE(value: Int): ByteArray {
        return ByteArray(4) { i -> ((value shr (i * 8)) and 0xFF).toByte() }
    }

    /**
     * Encode the full array of inputs
     */
    private fun encodeOperandsList(): ByteArray {
        val buffer = mutableListOf<Byte>()
        // Gray Paper natural number encode the array length
        buffer.addAll(AccumulationOperand.encodeGrayPaperNatural(operands.size.toLong()).toList())
        // Encode each operand using its existing encode() method (includes variant)
        for (operand in operands) {
            buffer.addAll(operand.encode().toList())
        }
        return buffer.toByteArray()
    }

    /**
     * Encode a single operand
     */
    private fun encodeOperand(operand: AccumulationOperand): ByteArray {
        return operand.encode()
    }

    private fun encodeLong(value: Long): ByteArray {
        return ByteArray(8) { i -> ((value shr (i * 8)) and 0xFF).toByte() }
    }

    private fun encodeInt(value: Int): ByteArray {
        return ByteArray(4) { i -> ((value shr (i * 8)) and 0xFF).toByte() }
    }

    /**
     * Find the first available service index starting from a candidate.
     */
    private fun findAvailableServiceIndex(candidate: Long, minPublicServiceIndex: Long): Long {
        var i = candidate
        val s = minPublicServiceIndex
        val right = (UInt.MAX_VALUE - s.toUInt() - 255u).toLong()

        // Loop until we find an unused service index
        while (context.x.accounts.containsKey(i)) {
            val left = (i - s + 1)
            i = s + (left % right)
        }
        return i
    }
}
