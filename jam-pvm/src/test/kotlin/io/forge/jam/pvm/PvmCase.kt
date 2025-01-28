package io.forge.jam.pvm

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class PvmCase(
    val name: String,
    @SerialName("initial-regs")
    val initialRegs: List<ULong>,
    @SerialName("initial-pc")
    val initialPc: UInt,
    @SerialName("initial-page-map")
    val initialPageMap: List<PageMap>,
    @SerialName("initial-memory")
    val initialMemory: List<Memory>,
    @SerialName("initial-gas")
    val initialGas: Long,
    val program: UByteArray,
    @SerialName("expected-status")
    val expectedStatus: PvmStatus,
    @SerialName("expected-regs")
    val expectedRegs: List<ULong>,
    @SerialName("expected-pc")
    val expectedPc: UInt,
    @SerialName("expected-memory")
    val expectedMemory: List<Memory>,
    @SerialName("expected-gas")
    val expectedGas: Long,
    @SerialName("expected-page-fault-address")
    val expectedPageFaultAddress: UInt? = 0u
)
