package io.forge.jam.core.encoding

import io.forge.jam.safrole.report.*
import kotlin.test.Test
import kotlin.test.assertContentEquals
import kotlin.test.assertEquals
import kotlin.test.assertTrue
import kotlin.test.fail

class ReportsJsonTest {
    fun assertListOfByteArraysEqualsIgnoreOrder(
        expected: List<ByteArray>,
        actual: List<ByteArray>,
        message: String? = null
    ) {
        if (expected.size != actual.size) {
            fail(message ?: "Expected size ${expected.size} but got ${actual.size}")
        }

        val expectedCounts = expected.groupingBy { it.contentHashCode() }.eachCount()
        val actualCounts = actual.groupingBy { it.contentHashCode() }.eachCount()

        assertEquals(expectedCounts, actualCounts, message)
    }

    fun assertReportOutputEquals(expected: ReportOutput, actual: ReportOutput, testCase: String) {
        when {
            expected.err != null && actual.err == null -> {
                fail("$testCase: Expected error ${expected.err} but got success")
            }

            expected.err == null && actual.err != null -> {
                fail("$testCase: Expected success but got error ${actual.err}")
            }

            expected.err != null && actual.err != null -> {
                assertEquals(expected.err, actual.err, "$testCase: Error codes don't match")
                return
            }
        }

        if (expected.ok == null && actual.ok != null) {
            fail("$testCase: Expected null ok but got ${actual.ok}")
        }
        if (expected.ok != null && actual.ok == null) {
            fail("$testCase: Expected ${expected.ok} but got null ok")
        }

        expected.ok?.let { expectedMarks ->
            actual.ok?.let { actualMarks ->
                // Compare reported packages
                assertEquals(
                    expectedMarks.reported.size,
                    actualMarks.reported.size,
                    "$testCase: Mismatch in reported packages list size. Reported: ${actualMarks.reported}"
                )

                // Compare each ReportPackage
                for (i in expectedMarks.reported.indices) {
                    val expectedPackage = expectedMarks.reported[i]
                    val actualPackage = actualMarks.reported[i]

                    assertTrue(
                        expectedPackage.workPackageHash.contentEquals(actualPackage.workPackageHash),
                        "$testCase: Mismatch in workPackageHash at package index $i"
                    )

                    assertTrue(
                        expectedPackage.segmentTreeRoot.contentEquals(actualPackage.segmentTreeRoot),
                        "$testCase: Mismatch in segmentTreeRoot at package index $i"
                    )
                }

                // Compare reporters list
                assertEquals(
                    expectedMarks.reporters.size,
                    actualMarks.reporters.size,
                    "$testCase: Mismatch in reporters list size"
                )

                for (i in expectedMarks.reporters.indices) {
                    assertTrue(
                        expectedMarks.reporters[i].contentEquals(actualMarks.reporters[i]),
                        "$testCase: Mismatch in reporter at index $i"
                    )
                }
            }
        }
    }

    fun assertReportStateEquals(expected: ReportState, actual: ReportState, testCase: String) {
        // Assert availability assignments
        assertEquals(
            expected.availAssignments.size,
            actual.availAssignments.size,
            "$testCase: Mismatch in availAssignments size"
        )
        for (i in expected.availAssignments.indices) {
            assertEquals(
                expected.availAssignments[i],
                actual.availAssignments[i],
                "$testCase: Mismatch in availAssignments at index $i"
            )
        }

        // Assert current validators
        assertEquals(
            expected.currValidators.size,
            actual.currValidators.size,
            "$testCase: Mismatch in currValidators size"
        )
        for (i in expected.currValidators.indices) {
            assertEquals(
                expected.currValidators[i],
                actual.currValidators[i],
                "$testCase: Mismatch in currValidators at index $i"
            )
        }

        // Assert previous validators
        assertEquals(
            expected.prevValidators.size,
            actual.prevValidators.size,
            "$testCase: Mismatch in prevValidators size"
        )
        for (i in expected.prevValidators.indices) {
            assertEquals(
                expected.prevValidators[i],
                actual.prevValidators[i],
                "$testCase: Mismatch in prevValidators at index $i"
            )
        }

        // Assert recent blocks
        assertEquals(
            expected.recentBlocks.history.size,
            actual.recentBlocks.history.size,
            "$testCase: Mismatch in recentBlocks history size"
        )
        for (i in expected.recentBlocks.history.indices) {
            assertEquals(
                expected.recentBlocks.history[i],
                actual.recentBlocks.history[i],
                "$testCase: Mismatch in recentBlocks history at index $i"
            )
        }

        // Assert auth pools (nested ByteArray lists)
        assertEquals(
            expected.authPools.size,
            actual.authPools.size,
            "$testCase: Mismatch in authPools size"
        )
        for (i in expected.authPools.indices) {
            assertEquals(
                expected.authPools[i].size,
                actual.authPools[i].size,
                "$testCase: Mismatch in authPools inner list size at index $i"
            )
            for (j in expected.authPools[i].indices) {
                assertEquals(
                    expected.authPools[i][j],
                    actual.authPools[i][j],
                    "$testCase: Mismatch in authPools at indices [$i][$j]"
                )
            }
        }

        // Assert services
        assertEquals(
            expected.accounts.size,
            actual.accounts.size,
            "$testCase: Mismatch in accounts size"
        )
        for (i in expected.accounts.indices) {
            assertEquals(
                expected.accounts[i],
                actual.accounts[i],
                "$testCase: Mismatch in account at index $i"
            )
        }
    }


    @Test
    fun testTinyReports() {
        val folderPath = "stf/reports/tiny"
        val testCaseNames = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)

        for (testCaseName in testCaseNames) {
            val (testCase, expectedBinaryData) = TestFileLoader.loadTestDataFromTestVectors<ReportCase>(folderPath, testCaseName)
            assertContentEquals(expectedBinaryData, testCase.encode(), "Encoding mismatch for $testCaseName")

            val report = ReportStateTransition(
                ReportStateConfig(
                    MAX_LOOKUP_ANCHOR_AGE = 14_000L,
                    MAX_CORES = 2,
                    MAX_DEPENDENCIES = 8,
                    ROTATION_PERIOD = 4,
                    MAX_VALIDATORS = 6,
                    EPOCH_LENGTH = 12
                )
            )
            val (postState, output) = report.transition(testCase.input, testCase.preState)
            assertReportOutputEquals(testCase.output, output, testCaseName)

            assertReportStateEquals(testCase.postState, postState, testCaseName)
        }
    }

    @Test
    fun testFullReports() {
        val folderPath = "stf/reports/full"
        val testCaseNames = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)

        for (testCaseName in testCaseNames) {
            val (testCase, expectedBinaryData) = TestFileLoader.loadTestDataFromTestVectors<ReportCase>(folderPath, testCaseName)
            assertContentEquals(expectedBinaryData, testCase.encode(), "Encoding mismatch for $testCaseName")

            val report = ReportStateTransition(
                ReportStateConfig(
                    MAX_LOOKUP_ANCHOR_AGE = 14_000L,
                    MAX_CORES = 341,
                    MAX_DEPENDENCIES = 8,
                    ROTATION_PERIOD = 10,
                    MAX_VALIDATORS = 1023,
                    EPOCH_LENGTH = 600
                )
            )
            val (postState, output) = report.transition(testCase.input, testCase.preState)
            assertReportOutputEquals(testCase.output, output, testCaseName)
            assertReportStateEquals(testCase.postState, postState, testCaseName)
        }
    }

    @Test
    fun testTinyReportsDecoding() {
        val folderPath = "stf/reports/tiny"
        val testCaseNames = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)

        for (testCaseName in testCaseNames) {
            val (testCase, binaryData) = TestFileLoader.loadTestDataFromTestVectors<ReportCase>(folderPath, testCaseName)

            // Decode from binary
            val (decodedCase, bytesConsumed) = ReportCase.fromBytes(binaryData, 0, coresCount = 2, validatorsCount = 6)

            assertEquals(binaryData.size, bytesConsumed, "Bytes consumed mismatch for $testCaseName")
            assertEquals(testCase.input.slot, decodedCase.input.slot, "Input slot mismatch for $testCaseName")

            // Verify round-trip
            assertContentEquals(binaryData, decodedCase.encode(), "Round-trip encoding mismatch for $testCaseName")
        }
    }

    @Test
    fun testFullReportsDecoding() {
        val folderPath = "stf/reports/full"
        val testCaseNames = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)

        for (testCaseName in testCaseNames) {
            val (testCase, binaryData) = TestFileLoader.loadTestDataFromTestVectors<ReportCase>(folderPath, testCaseName)

            // Decode from binary
            val (decodedCase, bytesConsumed) = ReportCase.fromBytes(binaryData, 0, coresCount = 341, validatorsCount = 1023)

            assertEquals(binaryData.size, bytesConsumed, "Bytes consumed mismatch for $testCaseName")
            assertEquals(testCase.input.slot, decodedCase.input.slot, "Input slot mismatch for $testCaseName")

            // Verify round-trip
            assertContentEquals(binaryData, decodedCase.encode(), "Round-trip encoding mismatch for $testCaseName")
        }
    }
}
