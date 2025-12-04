package io.forge.jam.safrole.traces

import io.forge.jam.core.JamByteArray
import io.forge.jam.core.decodeCompactInteger
import io.forge.jam.core.decodeFixedWidthInteger
import io.forge.jam.safrole.ValidatorKey
import io.forge.jam.safrole.safrole.SafroleState

/**
 * Unified JAM state that holds all state components.
 * Can be constructed from raw key-value pairs or typed state structures.
 */
class JamState {
    // Raw key-value storage organized by component
    private val componentData = mutableMapOf<StateComponent, MutableList<KeyValue>>()

    // Typed state components (lazy decoded)
    private var _timeslot: Long? = null
    private var _entropy: List<JamByteArray>? = null
    private var _currentValidators: List<ValidatorKey>? = null
    private var _previousValidators: List<ValidatorKey>? = null
    private var _pendingValidators: List<ValidatorKey>? = null

    /**
     * Loads state from raw key-value pairs.
     */
    fun loadFromKeyvals(keyvals: List<KeyValue>) {
        componentData.clear()

        for (kv in keyvals) {
            val component = StateKeys.getComponent(kv.key)
            componentData.getOrPut(component.normalize()) { mutableListOf() }.add(kv)
        }
    }

    /**
     * Gets the timeslot (τ).
     */
    val timeslot: Long
        get() {
            if (_timeslot == null) {
                val kvList = componentData[StateComponent.Timeslot]
                if (kvList != null && kvList.isNotEmpty()) {
                    val value = kvList[0].value.bytes
                    _timeslot = decodeFixedWidthInteger(value, 0, 4, false)
                } else {
                    _timeslot = 0L
                }
            }
            return _timeslot!!
        }

    /**
     * Gets the entropy pool (η).
     */
    val entropy: List<JamByteArray>
        get() {
            if (_entropy == null) {
                val kvList = componentData[StateComponent.EntropyPool]
                if (kvList != null && kvList.isNotEmpty()) {
                    _entropy = decodeEntropyPool(kvList[0].value)
                } else {
                    _entropy = listOf(
                        JamByteArray(ByteArray(32)),
                        JamByteArray(ByteArray(32)),
                        JamByteArray(ByteArray(32)),
                        JamByteArray(ByteArray(32))
                    )
                }
            }
            return _entropy!!
        }

    /**
     * Gets the current validators (κ).
     */
    val currentValidators: List<ValidatorKey>
        get() {
            if (_currentValidators == null) {
                val kvList = componentData[StateComponent.CurrentValidators]
                if (kvList != null && kvList.isNotEmpty()) {
                    _currentValidators = decodeValidatorList(kvList[0].value)
                } else {
                    _currentValidators = emptyList()
                }
            }
            return _currentValidators!!
        }

    /**
     * Gets the previous validators (λ).
     */
    val previousValidators: List<ValidatorKey>
        get() {
            if (_previousValidators == null) {
                val kvList = componentData[StateComponent.PreviousValidators]
                if (kvList != null && kvList.isNotEmpty()) {
                    _previousValidators = decodeValidatorList(kvList[0].value)
                } else {
                    _previousValidators = emptyList()
                }
            }
            return _previousValidators!!
        }

    /**
     * Gets all key-values for a component.
     */
    fun getComponentData(component: StateComponent): List<KeyValue> {
        return componentData[component.normalize()] ?: emptyList()
    }

    /**
     * Gets all service accounts.
     */
    fun getServiceAccounts(): Map<Int, KeyValue> {
        val result = mutableMapOf<Int, KeyValue>()
        for ((component, kvList) in componentData) {
            if (component is StateComponent.ServiceAccount) {
                kvList.firstOrNull()?.let { result[component.serviceIndex] = it }
            }
        }
        return result
    }

    /**
     * Converts to a SafroleState (for use with SafroleStateTransition).
     */
    fun toSafroleState(): SafroleState {
        val safroleData = componentData[StateComponent.SafroleState]?.firstOrNull()

        return SafroleState(
            tau = timeslot,
            eta = io.forge.jam.core.JamByteArrayList().apply {
                entropy.forEach { add(it) }
            },
            kappa = currentValidators,
            lambda = previousValidators,
            gammaK = pendingValidators,
            iota = pendingValidators,  // TODO: Decode separately
            gammaA = emptyList(),  // TODO: Decode from safrole state
            gammaZ = JamByteArray(ByteArray(144))  // TODO: Decode from safrole state
        )
    }

    /**
     * Gets pending validators (ι).
     */
    private val pendingValidators: List<ValidatorKey>
        get() {
            if (_pendingValidators == null) {
                val kvList = componentData[StateComponent.ValidatorQueue]
                if (kvList != null && kvList.isNotEmpty()) {
                    _pendingValidators = decodeValidatorList(kvList[0].value)
                } else {
                    _pendingValidators = emptyList()
                }
            }
            return _pendingValidators!!
        }

    /**
     * Decodes entropy pool from value bytes.
     * Entropy pool is 4 x 32-byte hashes.
     */
    private fun decodeEntropyPool(value: JamByteArray): List<JamByteArray> {
        val result = mutableListOf<JamByteArray>()
        var offset = 0

        // First decode the length
        val (length, bytesRead) = decodeCompactInteger(value.bytes, offset)
        offset += bytesRead

        for (i in 0 until length.toInt()) {
            if (offset + 32 <= value.size) {
                result.add(JamByteArray(value.bytes.copyOfRange(offset, offset + 32)))
                offset += 32
            }
        }
        return result
    }

    /**
     * Decodes a list of validators from value bytes.
     */
    private fun decodeValidatorList(value: JamByteArray): List<ValidatorKey> {
        val result = mutableListOf<ValidatorKey>()
        var offset = 0

        // First decode the length
        val (length, bytesRead) = decodeCompactInteger(value.bytes, offset)
        offset += bytesRead

        // Each validator key is: bandersnatch (32) + ed25519 (32) + bls (144) + metadata (128) = 336 bytes
        val validatorSize = 336

        for (i in 0 until length.toInt()) {
            if (offset + validatorSize <= value.size) {
                val bandersnatch = JamByteArray(value.bytes.copyOfRange(offset, offset + 32))
                val ed25519 = JamByteArray(value.bytes.copyOfRange(offset + 32, offset + 64))
                val bls = JamByteArray(value.bytes.copyOfRange(offset + 64, offset + 208))
                val metadata = JamByteArray(value.bytes.copyOfRange(offset + 208, offset + 336))

                result.add(ValidatorKey(bandersnatch, ed25519, bls, metadata))
                offset += validatorSize
            }
        }
        return result
    }

    /**
     * Normalizes a StateComponent for use as a map key.
     * Converts specific service indices to a canonical form.
     */
    private fun StateComponent.normalize(): StateComponent {
        return when (this) {
            is StateComponent.ServiceData -> this  // Keep service data separate by index
            is StateComponent.ServiceAccount -> this  // Keep service accounts separate by index
            else -> this
        }
    }
}
