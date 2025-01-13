package io.forge.jam.core.encoding

import io.forge.jam.safrole.report.*
import kotlin.test.Test
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
        println("Testing case: $testCase")
        println("Expected: $expected")
        println("Actual: $actual")

        // First verify that both outputs are in the same state (error or success)
        when {
            expected.err != null && actual.err == null -> {
                fail("$testCase: Expected error ${expected.err} but got success")
            }

            expected.err == null && actual.err != null -> {
                fail("$testCase: Expected success but got error ${actual.err}")
            }

            expected.err != null && actual.err != null -> {
                // Both have errors - compare them
                assertEquals(expected.err, actual.err, "$testCase: Error codes don't match")
                // Early return since we don't need to check marks when we have errors
                return
            }
        }

        // At this point, both should have ok values (no errors)
        if (expected.ok == null && actual.ok != null) {
            fail("$testCase: Expected null ok but got ${actual.ok}")
        }
        if (expected.ok != null && actual.ok == null) {
            fail("$testCase: Expected ${expected.ok} but got null ok")
        }

        // Compare output marks if present
        expected.ok?.let { expectedMarks ->
            actual.ok?.let { actualMarks ->
                // Compare reported packages
                assertEquals(
                    expectedMarks.reported.size,
                    actualMarks.reported.size,
                    "$testCase: Mismatch in reported packages list size"
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
            expected.recentBlocks.size,
            actual.recentBlocks.size,
            "$testCase: Mismatch in recentBlocks size"
        )
        for (i in expected.recentBlocks.indices) {
            assertEquals(
                expected.recentBlocks[i],
                actual.recentBlocks[i],
                "$testCase: Mismatch in recentBlocks at index $i"
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
            expected.services.size,
            actual.services.size,
            "$testCase: Mismatch in services size"
        )
        for (i in expected.services.indices) {
            assertEquals(
                expected.services[i],
                actual.services[i],
                "$testCase: Mismatch in service at index $i"
            )
        }
    }


    @Test
    fun testTinyReports() {
        val folderName = "reports/tiny"
        val testCases = TestFileLoader.getTestFilenamesFromResources(folderName)

        for (testCase in testCases) {
            val (inputCase) = TestFileLoader.loadTestData<ReportCase>(
                "$folderName/$testCase",
                ".bin"
            )

            val report = ReportStateTransition(
                ReportStateConfig(
                    MAX_LOOKUP_ANCHOR_AGE = 14_000L,
                    MAX_AUTH_POOL_ITEMS = 8,
                    MAX_ACCUMULATION_GAS = 100_000L,
                    MAX_CORES = 2,
                    MAX_DEPENDENCIES = 100,
                    MIN_GUARANTORS = 3,
                    ROTATION_PERIOD = 4,
                    MAX_VALIDATORS = 6,
                    EPOCH_LENGTH = 12
                )
            )
            val (postState, output) = report.transition(inputCase.input, inputCase.preState)
            assertReportOutputEquals(inputCase.output, output, testCase)

            assertReportStateEquals(inputCase.postState, postState, testCase)
        }
    }

//    @Test
//    fun testFullDisputes() {
//        val folderName = "disputes/full"
//        val testCases = TestFileLoader.getTestFilenamesFromResources(folderName)
//
//        for (testCase in testCases) {
//            val (inputCase) = TestFileLoader.loadTestData<SafroleCase>(
//                "$folderName/$testCase",
//                ".bin"
//            )
//
//            val safrole = SafroleStateTransition(
//                SafroleConfig(
//                    epochLength = 600,
//                    ticketCutoff = 500,
//                    ringSize = 6,
//                    validatorCount = 1023
//                )
//            )
//            val (postState, output) = safrole.transition(inputCase.input, inputCase.preState)
//
//            assertDisputeOutputEquals(inputCase.output, output, testCase)
//
//            assertDisputeStateEquals(inputCase.postState, postState)
//        }
//    }
}
