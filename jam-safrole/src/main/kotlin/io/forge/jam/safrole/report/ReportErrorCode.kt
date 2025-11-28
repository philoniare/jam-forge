package io.forge.jam.safrole.report

import io.forge.jam.core.Encodable
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
enum class ReportErrorCode : Encodable {
    @SerialName("anchor_not_recent")
    ANCHOR_NOT_RECENT,

    @SerialName("bad_service_id")
    BAD_SERVICE_ID,

    @SerialName("bad_beefy_mmr_root")
    BAD_BEEFY_MMR_ROOT,

    @SerialName("bad_code_hash")
    BAD_CODE_HASH,

    @SerialName("bad_core_index")
    BAD_CORE_INDEX,

    @SerialName("bad_signature")
    BAD_SIGNATURE,

    @SerialName("bad_state_root")
    BAD_STATE_ROOT,

    @SerialName("bad_validator_index")
    BAD_VALIDATOR_INDEX,

    @SerialName("banned_validator")
    BANNED_VALIDATOR,

    @SerialName("service_item_gas_too_low")
    SERVICE_ITEM_GAS_TOO_LOW,

    @SerialName("core_engaged")
    CORE_ENGAGED,

    @SerialName("dependency_missing")
    DEPENDENCY_MISSING,

    @SerialName("duplicate_package")
    DUPLICATE_PACKAGE,

    @SerialName("future_report_slot")
    FUTURE_REPORT_SLOT,

    @SerialName("insufficient_guarantees")
    INSUFFICIENT_GUARANTEES,

    @SerialName("core_unauthorized")
    CORE_UNAUTHORIZED,

    @SerialName("duplicate_guarantors")
    DUPLICATE_GUARANTORS,

    @SerialName("out_of_order_guarantee")
    OUT_OF_ORDER_GUARANTEE,

    @SerialName("not_sorted_or_unique_guarantors")
    NOT_SORTED_OR_UNIQUE_GUARANTORS,

    @SerialName("report_epoch_before_last")
    REPORT_EPOCH_BEFORE_LAST,

    @SerialName("segment_root_lookup_invalid")
    SEGMENT_ROOT_LOOKUP_INVALID,

    @SerialName("work_report_gas_too_high")
    WORK_REPORT_GAS_TOO_HIGH,

    @SerialName("work_report_too_big")
    WORK_REPORT_TOO_BIG,

    @SerialName("too_many_dependencies")
    TOO_MANY_DEPENDENCIES,

    @SerialName("wrong_assignment")
    WRONG_ASSIGNMENT;

    override fun encode(): ByteArray {
        return byteArrayOf(ordinal.toByte())
    }
}
