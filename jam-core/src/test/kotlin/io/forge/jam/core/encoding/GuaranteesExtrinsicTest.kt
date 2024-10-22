package io.forge.jam.core.encoding

import io.forge.jam.core.GuaranteeExtrinsic
import kotlin.test.Test
import kotlin.test.assertContentEquals

class GuaranteesExtrinsicTest {
    @Test
    fun testEncodeGuaranteesExtrinsics() {
        val (inputGuarantees, expectedOutputBytes) = TestFileLoader.loadTestData<List<GuaranteeExtrinsic>>("guarantees_extrinsic")

        // Process each guarantee
        val encodedGuarantees = inputGuarantees.map { guarantee ->
            val extrinsic =
                GuaranteeExtrinsic(guarantee.report, guarantee.slot, guarantee.signatures)
            extrinsic.encode()
        }

        // Version byte
        val versionByte = byteArrayOf(0x01.toByte())

        // Concatenate all encoded guarantees
        val concatenatedEncodedAssurances = versionByte + encodedGuarantees.reduce { acc, bytes -> acc + bytes }

        // Compare the concatenated encoded bytes with the expected output bytes
        assertContentEquals(
            expectedOutputBytes,
            concatenatedEncodedAssurances,
            "Encoded bytes do not match expected output"
        )
    }
}
