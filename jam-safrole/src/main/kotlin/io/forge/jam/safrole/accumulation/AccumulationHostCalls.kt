package io.forge.jam.safrole.accumulation

import io.forge.jam.core.JamByteArray
import io.forge.jam.core.encodeCompactInteger
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
     * Dispatch a host call based on its identifier.
     * Returns the gas cost for the operation.
     */
    fun dispatch(hostCallId: UInt, instance: RawInstance): Long {
        // LOG host call has 0 gas cost per JIP-1
        if (hostCallId == HostCall.LOG) {
            handleLog(instance)
            return 0L
        }

        val gasCost = 10L // Default gas cost for most host calls

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
            else -> {
                // Unknown host call - return WHAT
                instance.setReg7(HostCallResult.WHAT)
            }
        }

        return gasCost
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


        val data: ByteArray? = when (selector) {
            0 -> getConstantsBlob()
            14 -> {
                encodeOperandsList()
            }

            15 -> {
                if (index < operands.size) {
                    val encoded = encodeOperand(operands[index])
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
            return
        }

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

        val value = account.storage[key]
        if (value == null) {
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
     */
    private fun handleWrite(instance: RawInstance) {
        val keyAddr = instance.getReg7().toUInt()
        val keyLen = instance.getReg8().toInt()
        val valueAddr = instance.getReg9().toUInt()
        val valueLen = instance.getReg10().toInt()


        val account = context.x.accounts[context.serviceIndex]
        if (account == null) {
            instance.setReg7(HostCallResult.WHO)
            return
        }

        // Read key from memory
        val keyBuffer = ByteArray(keyLen)
        val keyReadResult = instance.readMemoryInto(keyAddr, keyBuffer)
        if (keyReadResult.isFailure) {
            instance.setReg7(HostCallResult.OOB)
            return
        }
        val key = JamByteArray(keyBuffer)

        // Track old value size for bytes calculation
        val oldValue = account.storage[key]
        val oldValueSize = oldValue?.bytes?.size ?: 0
        val keyWasPresent = oldValue != null

        if (valueLen == 0) {
            // Delete key
            if (keyWasPresent) {
                account.storage.remove(key)
                // Decrement items and bytes
                val updatedInfo = account.info.copy(
                    bytes = account.info.bytes - keyLen - oldValueSize,
                    items = account.info.items - 1
                )
                context.x.accounts[context.serviceIndex] = account.copy(info = updatedInfo)
            }
        } else {
            // Read value from memory
            val valueBuffer = ByteArray(valueLen)
            val valueReadResult = instance.readMemoryInto(valueAddr, valueBuffer)
            if (valueReadResult.isFailure) {
                instance.setReg7(HostCallResult.OOB)
                return
            }
            account.storage[key] = JamByteArray(valueBuffer)

            // Update bytes and items in ServiceInfo
            val byteDelta = if (keyWasPresent) {
                valueLen - oldValueSize
            } else {
                keyLen + valueLen + 34
            }
            val itemsDelta = if (keyWasPresent) 0 else 1

            val updatedInfo = account.info.copy(
                bytes = account.info.bytes + byteDelta,
                items = account.info.items + itemsDelta
            )
            context.x.accounts[context.serviceIndex] = account.copy(info = updatedInfo)
        }

        instance.setReg7(HostCallResult.OK)
    }

    /**
     * info (5): Get service account info.
     */
    private fun handleInfo(instance: RawInstance) {
        val serviceId = instance.getReg7().toLong()
        val outputAddr = instance.getReg8().toUInt()

        val targetServiceId = if (serviceId == -1L) context.serviceIndex else serviceId
        val account = context.x.accounts[targetServiceId]

        if (account == null) {
            instance.setReg7(HostCallResult.WHO)
            return
        }

        // Encode service info: code_hash (32) + balance (8) + min_item_gas (8) + min_memo_gas (8) + bytes (8) + items (4)
        val info = account.info
        val data = info.codeHash.bytes +
            encodeLong(info.balance) +
            encodeLong(info.minItemGas) +
            encodeLong(info.minMemoGas) +
            encodeLong(info.bytes) +
            encodeInt(info.items)

        val writeResult = instance.writeMemory(outputAddr, data)
        if (writeResult.isFailure) {
            instance.setReg7(HostCallResult.OOB)
            return
        }

        instance.setReg7(HostCallResult.OK)
    }

    /**
     * bless (14): Set manager service (privileged).
     */
    private fun handleBless(instance: RawInstance) {
        // Only manager service can call bless
        if (context.serviceIndex != context.x.manager) {
            instance.setReg7(HostCallResult.HUH)
            return
        }

        val newManager = instance.getReg7().toLong()
        context.x.manager = newManager
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
     */
    private fun handleNew(instance: RawInstance) {
        val codeHashAddr = instance.getReg7().toUInt()
        val balance = instance.getReg8().toLong()
        val minItemGas = instance.getReg9().toLong()
        val minMemoGas = instance.getReg10().toLong()

        // Read code hash from memory
        val codeHashBuffer = ByteArray(32)
        val readResult = instance.readMemoryInto(codeHashAddr, codeHashBuffer)
        if (readResult.isFailure) {
            instance.setReg7(HostCallResult.OOB)
            return
        }

        val currentAccount = context.x.accounts[context.serviceIndex]
        if (currentAccount == null || currentAccount.info.balance < balance) {
            instance.setReg7(HostCallResult.CASH)
            return
        }

        // Generate new service ID (simplified)
        val newServiceId = generateNewServiceId()

        // Create new account
        val newAccount = ServiceAccount(
            info = io.forge.jam.safrole.report.ServiceInfo(
                codeHash = JamByteArray(codeHashBuffer),
                balance = balance,
                minItemGas = minItemGas,
                minMemoGas = minMemoGas,
                bytes = 0,
                items = 0
            ),
            storage = mutableMapOf(),
            preimages = mutableMapOf(),
            preimageRequests = mutableMapOf()
        )

        context.x.accounts[newServiceId] = newAccount

        // Deduct balance from creator
        val updatedCreatorInfo = currentAccount.info.copy(
            balance = currentAccount.info.balance - balance
        )
        context.x.accounts[context.serviceIndex] = currentAccount.copy(
            info = updatedCreatorInfo
        )

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
     */
    private fun handleTransfer(instance: RawInstance) {
        val destination = instance.getReg7().toLong()
        val amount = instance.getReg8().toLong()
        val gasLimit = instance.getReg9().toLong()
        val memoAddr = instance.getReg10().toUInt()

        val account = context.x.accounts[context.serviceIndex]
        if (account == null || account.info.balance < amount) {
            instance.setReg7(HostCallResult.CASH)
            return
        }

        // Read memo from memory (128 bytes)
        val memoBuffer = ByteArray(DeferredTransfer.MEMO_SIZE)
        val readResult = instance.readMemoryInto(memoAddr, memoBuffer)
        if (readResult.isFailure) {
            instance.setReg7(HostCallResult.OOB)
            return
        }

        // Deduct balance
        val updatedInfo = account.info.copy(balance = account.info.balance - amount)
        context.x.accounts[context.serviceIndex] = account.copy(info = updatedInfo)

        // Queue transfer
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

        // 1. Read 32-byte preimage hash from memory - PANIC on failure
        val hashBuffer = ByteArray(32)
        val readResult = instance.readMemoryInto(preimageHashAddr, hashBuffer)
        if (readResult.isFailure) {
            throw RuntimeException("Eject PANIC: Failed to read preimage hash from memory")
        }
        val preimageHash = JamByteArray(hashBuffer)

        // 2. Get target service account
        val ejectAccount = context.x.accounts[ejectServiceId]

        // 3. Validate: target exists AND target != caller AND codeHash matches caller's ID
        val expectedCodeHash = encodeServiceIdAsCodeHash(context.serviceIndex)
        if (ejectServiceId == context.serviceIndex) {
            instance.setReg7(HostCallResult.WHO)
            return
        }
        if (ejectAccount == null) {
            instance.setReg7(HostCallResult.WHO)
            return
        }
        if (ejectAccount.info.codeHash != expectedCodeHash) {
            instance.setReg7(HostCallResult.WHO)
            return
        }

        // 4. Validate item count is exactly 2
        if (ejectAccount.info.items != 2) {
            instance.setReg7(HostCallResult.HUH)
            return
        }

        // 5. Calculate length: l = max(81, d.bytes) - 81
        val length = (maxOf(81L, ejectAccount.info.bytes) - 81).toInt()
        val preimageKey = PreimageKey(preimageHash, length)

        // 6. Validate preimage request exists with exactly 2 timestamps
        val preimageRequest = ejectAccount.preimageRequests[preimageKey]
        if (preimageRequest == null) {
            instance.setReg7(HostCallResult.HUH)
            return
        }
        if (preimageRequest.requestedAt.size != 2) {
            instance.setReg7(HostCallResult.HUH)
            return
        }

        // 7. Validate expunge period: requestedAt[1] < timeslot - expungePeriod
        val expungePeriod = config.PREIMAGE_PURGE_PERIOD
        val minHoldSlot = maxOf(0L, context.timeslot - expungePeriod)
        if (preimageRequest.requestedAt[1] >= minHoldSlot) {
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
        // Compact-encode the array length
        buffer.addAll(encodeCompactInteger(operands.size.toLong()).toList())
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

    private var nextServiceIdCounter = 0L

    private fun generateNewServiceId(): Long {
        nextServiceIdCounter++
        return context.serviceIndex * 1000000 + nextServiceIdCounter
    }
}
