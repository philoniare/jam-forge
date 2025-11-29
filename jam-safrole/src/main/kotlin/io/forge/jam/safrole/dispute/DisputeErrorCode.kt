package io.forge.jam.safrole.dispute

import io.forge.jam.core.Encodable
import kotlinx.serialization.KSerializer
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import kotlinx.serialization.SerializationException
import kotlinx.serialization.descriptors.PrimitiveKind
import kotlinx.serialization.descriptors.PrimitiveSerialDescriptor
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder

/**
 * Error codes for the Disputes STF module.
 * These match the error codes defined in disputes.asn.
 */
@Serializable
enum class DisputeErrorCode : Encodable {
    @SerialName("already_judged")
    ALREADY_JUDGED,           // 0

    @SerialName("bad_vote_split")
    BAD_VOTE_SPLIT,           // 1

    @SerialName("verdicts_not_sorted_unique")
    VERDICTS_NOT_SORTED_UNIQUE, // 2

    @SerialName("judgements_not_sorted_unique")
    JUDGEMENTS_NOT_SORTED_UNIQUE, // 3

    @SerialName("culprits_not_sorted_unique")
    CULPRITS_NOT_SORTED_UNIQUE, // 4

    @SerialName("faults_not_sorted_unique")
    FAULTS_NOT_SORTED_UNIQUE,  // 5

    @SerialName("not_enough_culprits")
    NOT_ENOUGH_CULPRITS,       // 6

    @SerialName("not_enough_faults")
    NOT_ENOUGH_FAULTS,         // 7

    @SerialName("culprits_verdict_not_bad")
    CULPRITS_VERDICT_NOT_BAD,  // 8

    @SerialName("fault_verdict_wrong")
    FAULT_VERDICT_WRONG,       // 9

    @SerialName("offender_already_reported")
    OFFENDER_ALREADY_REPORTED, // 10

    @SerialName("bad_judgement_age")
    BAD_JUDGEMENT_AGE,         // 11

    @SerialName("bad_validator_index")
    BAD_VALIDATOR_INDEX,       // 12

    @SerialName("bad_signature")
    BAD_SIGNATURE,             // 13

    @SerialName("bad_guarantor_key")
    BAD_GUARANTOR_KEY,         // 14

    @SerialName("bad_auditor_key")
    BAD_AUDITOR_KEY;           // 15

    override fun encode(): ByteArray {
        return byteArrayOf(ordinal.toByte())
    }
}

object DisputeErrorCodeSerializer : KSerializer<DisputeErrorCode> {
    override val descriptor: SerialDescriptor = PrimitiveSerialDescriptor("DisputeErrorCode", PrimitiveKind.STRING)

    override fun serialize(encoder: Encoder, value: DisputeErrorCode) {
        val serialName = value.name.lowercase()
        encoder.encodeString(serialName)
    }

    override fun deserialize(decoder: Decoder): DisputeErrorCode {
        val string = decoder.decodeString()
        return DisputeErrorCode.values().find { errorCode ->
            val underscoreName = errorCode.name.lowercase()
            val hyphenName = underscoreName.replace('_', '-')
            string == underscoreName || string == hyphenName
        } ?: throw SerializationException("Unknown dispute error code: $string")
    }
}
