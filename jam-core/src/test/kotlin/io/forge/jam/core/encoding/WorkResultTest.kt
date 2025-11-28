package io.forge.jam.core.encoding

import io.forge.jam.core.WorkResult
import kotlin.test.Test
import kotlin.test.assertContentEquals

class WorkResultTest {

    private fun testEncodeWorkResultOk(configPath: String) {
        val (inputWorkResult, expectedOutputBytes) = TestFileLoader.loadTestDataFromTestVectors<WorkResult>(configPath, "work_result_0")

        assertContentEquals(
            expectedOutputBytes,
            inputWorkResult.encode(),
            "Encoded bytes do not match expected output"
        )
    }

    private fun testEncodeWorkResultPanic(configPath: String) {
        val (inputWorkResult, expectedOutputBytes) = TestFileLoader.loadTestDataFromTestVectors<WorkResult>(configPath, "work_result_1")

        assertContentEquals(
            expectedOutputBytes,
            inputWorkResult.encode(),
            "Encoded bytes do not match expected output"
        )
    }

    @Test
    fun testEncodeWorkResultOkTiny() {
        testEncodeWorkResultOk("codec/tiny")
    }

    @Test
    fun testEncodeWorkResultOkFull() {
        testEncodeWorkResultOk("codec/full")
    }

    @Test
    fun testEncodeWorkResultPanicTiny() {
        testEncodeWorkResultPanic("codec/tiny")
    }

    @Test
    fun testEncodeWorkResultPanicFull() {
        testEncodeWorkResultPanic("codec/full")
    }
}
