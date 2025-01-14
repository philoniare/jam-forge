package io.forge.jam.core.encoding

import io.forge.jam.safrole.AvailabilityAssignment
import io.forge.jam.safrole.ValidatorKey
import io.forge.jam.safrole.assurance.*
import kotlin.test.*

class AssuranceJsonTest {

    private fun assertAvailabilityAssignmentEquals(
        expected: AvailabilityAssignment,
        actual: AvailabilityAssignment,
        context: String
    ) {
        assertEquals(
            expected.report,
            actual.report,
            "Work report mismatch in $context"
        )
        assertEquals(
            expected.timeout,
            actual.timeout,
            "Timeout mismatch in $context"
        )
    }

    private fun assertValidatorKeyEquals(expected: ValidatorKey, actual: ValidatorKey, context: String) {
        assertEquals(
            expected.bandersnatch,
            actual.bandersnatch,
            "Bandersnatch key mismatch in $context"
        )
        assertEquals(
            expected.ed25519,
            actual.ed25519,
            "Ed25519 key mismatch in $context"
        )
        assertEquals(
            expected.bls,
            actual.bls,
            "BLS key mismatch in $context"
        )
        assertEquals(
            expected.metadata,
            actual.metadata,
            "Metadata mismatch in $context"
        )
    }

    private fun assertAssuranceStateEquals(expected: AssuranceState, actual: AssuranceState, testCase: String) {
        // Compare availability assignments
        assertEquals(
            expected.availAssignments.size,
            actual.availAssignments.size,
            "Availability assignments size mismatch in test case: $testCase"
        )

        expected.availAssignments.zip(actual.availAssignments).forEachIndexed { index, (exp, act) ->
            when {
                exp == null && act == null -> {} // Both null is fine
                exp == null -> fail("Expected null assignment at index $index but got non-null in test case: $testCase")
                act == null -> fail("Expected non-null assignment at index $index but got null in test case: $testCase")
                else -> assertAvailabilityAssignmentEquals(exp, act, "$testCase - Assignment[$index]")
            }
        }

        // Compare validator keys
        assertEquals(
            expected.currValidators.size,
            actual.currValidators.size,
            "Current validators size mismatch in test case: $testCase"
        )

        expected.currValidators.zip(actual.currValidators).forEachIndexed { index, (exp, act) ->
            assertValidatorKeyEquals(exp, act, "$testCase - Validator[$index]")
        }
    }

    private fun assertAssuranceOutputEquals(expected: AssuranceOutput, actual: AssuranceOutput, testCase: String) {
        when {
            expected.ok != null -> {
                assertNotNull(actual.ok, "Expected OK output but got error in test case: $testCase")
                assertNull(actual.err, "Expected OK output but got both OK and error in test case: $testCase")

                assertEquals(
                    expected.ok!!.reported.size,
                    actual.ok!!.reported.size,
                    "Reported work reports size mismatch in test case: $testCase"
                )

                expected.ok!!.reported.zip(actual.ok!!.reported).forEachIndexed { index, (exp, act) ->
                    assertEquals(
                        exp,
                        act,
                        "Work report mismatch at index $index in test case: $testCase"
                    )
                }
            }

            expected.err != null -> {
                assertNotNull(actual.err, "Expected error output but got OK in test case: $testCase")
                assertNull(actual.ok, "Expected error output but got both OK and error in test case: $testCase")
                assertEquals(
                    expected.err,
                    actual.err,
                    "Error code mismatch in test case: $testCase"
                )
            }

            else -> fail("Invalid AssuranceOutput - both ok and err are null in test case: $testCase")
        }
    }

    @Test
    fun testTinyAssurances() {
        val folderName = "assurances/tiny"
        val testCases = TestFileLoader.getTestFilenamesFromResources(folderName)

        for (testCase in testCases) {
            val (inputCase) = TestFileLoader.loadTestData<AssuranceCase>(
                "$folderName/$testCase",
                ".bin"
            )

            val stf = AssuranceStateTransition(AssuranceConfig(VALIDATOR_COUNT = 6, CORE_COUNT = 2))
            val (postState, output) = stf.transition(inputCase.input, inputCase.preState)
            assertAssuranceOutputEquals(inputCase.output, output, testCase)
            assertAssuranceStateEquals(inputCase.postState, postState, testCase)
        }
    }

//    @Test
//    fun testFullAssurances() {
//        val folderName = "assurances/full"
//        val testCases = TestFileLoader.getTestFilenamesFromResources(folderName)
//
//        for (testCase in testCases) {
//            val (inputCase) = TestFileLoader.loadTestData<AssuranceCase>(
//                "$folderName/$testCase",
//                ".bin"
//            )
//
//            val stf = AssuranceStateTransition(AssuranceConfig(VALIDATOR_COUNT = 1023, CORE_COUNT = 341))
//            val (postState, output) = stf.transition(inputCase.input, inputCase.preState)
//            assertAssuranceStateEquals(inputCase.postState, postState, testCase)
//            assertAssuranceOutputEquals(inputCase.output, output, testCase)
//        }
//    }
}
