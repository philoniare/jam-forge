package io.forge.jam.core.encoding

import io.forge.jam.core.WorkReport
import kotlin.test.Test
import kotlin.test.assertContentEquals

class WorkReportTest {
    @Test
    fun testEncodeWorkReport() {
        // Load JSON data from resources using the class loader
        val (inputWorkReport, expectedOutputBytes) = TestFileLoader.loadTestDataFromTestVectors<WorkReport>("codec/tiny", "work_report")

        // Compare the concatenated encoded bytes with the expected output bytes
        assertContentEquals(
            expectedOutputBytes,
            inputWorkReport.encode(),
            "Encoded bytes do not match expected output"
        )
    }
}
