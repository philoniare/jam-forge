package io.forge.jam.core.encoding

import io.forge.jam.core.AssuranceExtrinsic
import kotlin.test.Test
import kotlin.test.assertContentEquals

class AssuranceExtrinsicTest {
    @Test
    fun testEncodeAssuranceExtrinsics() {
        val (inputAssurances, expectedOutputBytes) = TestFileLoader.loadTestDataFromTestVectors<List<AssuranceExtrinsic>>("codec/tiny", "assurances_extrinsic")

        // Process each assurance
        val encodedAssurances = inputAssurances.map { assurance ->
            val extrinsic =
                AssuranceExtrinsic(assurance.anchor, assurance.bitfield, assurance.validatorIndex, assurance.signature)
            extrinsic.encode()
        }

        // Version byte
        val versionByte = byteArrayOf(0x02.toByte())

        // Concatenate all encoded assurances
        val concatenatedEncodedAssurances = versionByte + encodedAssurances.reduce { acc, bytes -> acc + bytes }

        // Compare the concatenated encoded bytes with the expected output bytes
        assertContentEquals(
            expectedOutputBytes,
            concatenatedEncodedAssurances,
            "Encoded bytes do not match expected output"
        )
    }
}
