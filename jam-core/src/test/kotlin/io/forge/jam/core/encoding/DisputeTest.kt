package io.forge.jam.core.encoding

import io.forge.jam.core.Dispute
import kotlin.test.Test
import kotlin.test.assertContentEquals

class DisputeTest {

    private fun testEncodeDispute(configPath: String) {
        val (inputDispute, expectedOutputBytes) = TestFileLoader.loadTestDataFromTestVectors<Dispute>(configPath, "disputes_extrinsic")

        val encodedDispute = inputDispute.encode()

        assertContentEquals(
            expectedOutputBytes,
            encodedDispute,
            "Encoded bytes do not match expected output"
        )
    }

    @Test
    fun testEncodeDisputeTiny() {
        testEncodeDispute("codec/tiny")
    }

    @Test
    fun testEncodeDisputeFull() {
        testEncodeDispute("codec/full")
    }
}
