package io.forge.jam.core.encoding

import io.forge.jam.core.WorkReport
import kotlin.test.Test
import kotlin.test.assertContentEquals

class WorkReportTest {

    private fun testEncodeWorkReport(configPath: String) {
        val (inputWorkReport, expectedOutputBytes) = TestFileLoader.loadTestDataFromTestVectors<WorkReport>(configPath, "work_report")

        assertContentEquals(
            expectedOutputBytes,
            inputWorkReport.encode(),
            "Encoded bytes do not match expected output"
        )
    }

    @Test
    fun testEncodeWorkReportTiny() {
        testEncodeWorkReport("codec/tiny")
    }

    @Test
    fun testEncodeWorkReportFull() {
        testEncodeWorkReport("codec/full")
    }
}
