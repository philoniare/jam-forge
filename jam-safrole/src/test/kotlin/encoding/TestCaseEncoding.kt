package io.forge.jam.core.encoding.encoding

import io.forge.jam.core.encoding.TestFileLoader
import io.forge.jam.safrole.report.ReportCase
import kotlin.test.Test
import kotlin.test.assertContentEquals

class TestCaseEncoding {
    @Test
    fun testEncodeWorkReport() {
        // Load JSON data from resources using the class loader
        val (testCase, expectedOutputBytes) = TestFileLoader.loadTestData<ReportCase>("reports/tiny/big_work_report_output-1")

        // Compare the concatenated encoded bytes with the expected output bytes
//        testCase.encode()
//        assert(false)
        assertContentEquals(
            expectedOutputBytes,
            testCase.encode(),
            "Encoded bytes do not match expected output"
        )
    }
}

