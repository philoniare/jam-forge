package io.forge.jam.core.encoding

import io.forge.jam.core.GuaranteeExtrinsic
import kotlin.test.Test
import kotlin.test.assertContentEquals

class GuaranteesExtrinsicTest {

    private fun testEncodeGuaranteesExtrinsics(configPath: String) {
        val (inputGuarantees, expectedOutputBytes) = TestFileLoader.loadTestDataFromTestVectors<List<GuaranteeExtrinsic>>(configPath, "guarantees_extrinsic")

        val encodedGuarantees = inputGuarantees.map { guarantee ->
            val extrinsic =
                GuaranteeExtrinsic(guarantee.report, guarantee.slot, guarantee.signatures)
            extrinsic.encode()
        }

        val versionByte = byteArrayOf(0x01.toByte())
        val concatenatedEncodedAssurances = versionByte + encodedGuarantees.reduce { acc, bytes -> acc + bytes }

        assertContentEquals(
            expectedOutputBytes,
            concatenatedEncodedAssurances,
            "Encoded bytes do not match expected output"
        )
    }

    @Test
    fun testEncodeGuaranteesExtrinsicsTiny() {
        testEncodeGuaranteesExtrinsics("codec/tiny")
    }

    @Test
    fun testEncodeGuaranteesExtrinsicsFull() {
        testEncodeGuaranteesExtrinsics("codec/full")
    }
}
