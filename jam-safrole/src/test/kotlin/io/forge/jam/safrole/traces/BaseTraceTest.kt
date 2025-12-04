package io.forge.jam.safrole.traces

import io.forge.jam.core.encoding.TestFileLoader
import io.forge.jam.safrole.safrole.SafroleState
import kotlin.test.assertEquals
import kotlin.test.assertContentEquals
import kotlin.test.assertTrue
import kotlin.test.fail

/**
 * Base class for all trace tests.
 * Provides common functionality for loading and validating block import traces.
 */
abstract class BaseTraceTest {
    /**
     * The name of the trace folder (e.g., "fallback", "safrole").
     */
    protected abstract val traceName: String

    /**
     * Configuration for the trace tests.
     */
    protected open val config: ImporterConfig = ImporterConfig()

    /**
     * Maximum number of blocks to test (for limiting test runtime).
     * Set to null to test all blocks.
     */
    protected open val maxBlocks: Int? = null

    /**
     * Runs all trace tests for the configured trace.
     * This validates that the full STF pipeline produces correct post-state.
     * Loads blocks one at a time to avoid OOM with large traces.
     */
    protected fun runTraceTests() {
        // Load genesis
        val (genesis, _) = TestFileLoader.loadTraceGenesis<Genesis>(traceName)

        println("[$traceName] Loaded genesis with state root: ${genesis.state.stateRoot.toHex()}")
        println("[$traceName] Genesis has ${genesis.state.keyvals.size} key-value pairs")

        // Get filenames and limit if needed
        val allFilenames = TestFileLoader.getTraceStepFilenames(traceName)
        val filenamesToTest = if (maxBlocks != null) {
            allFilenames.take(maxBlocks!!)
        } else {
            allFilenames
        }

        println("[$traceName] Testing ${filenamesToTest.size} blocks (of ${allFilenames.size} total)")

        val importer = BlockImporter(config)
        var currentStateRoot = genesis.state.stateRoot

        // Load and test trace steps one at a time to avoid OOM
        for ((index, filename) in filenamesToTest.withIndex()) {
            val blockNum = index + 1
            val (step, _) = TestFileLoader.loadTestDataFromTestVectors<TraceStep>(
                "traces/$traceName", filename
            )

            // Verify pre-state root matches current state (state chain validation)
            assertEquals(
                currentStateRoot.toHex(),
                step.preState.stateRoot.toHex(),
                "[$traceName] Pre-state root mismatch at block $blockNum"
            )

            // Validate block structure can be parsed
            assertTrue(
                step.block.header.slot >= 0,
                "[$traceName] Invalid slot at block $blockNum"
            )

            // Run block import with full STF pipeline
            val result = importer.importBlock(step.block, step.preState)

            // Debug: print expected post-state entropy for block 6
            if (blockNum == 6) {
                val expectedFullState = StateCodec.decodeFullState(step.postState.keyvals)
                println("[DEBUG-EXPECTED] Block 6: expected post-state entropy[0]=${expectedFullState.entropyPool.getOrNull(0)?.toHex()}")
                // Also print the pre-state entropy
                val preFullState = StateCodec.decodeFullState(step.preState.keyvals)
                println("[DEBUG-EXPECTED] Block 6: pre-state entropy[0]=${preFullState.entropyPool.getOrNull(0)?.toHex()}")
            }

            when (result) {
                is ImportResult.Success -> {
                    // Compare computed state root with expected
                    val computedStateRoot = result.postState.stateRoot.toHex()
                    val expectedStateRoot = step.postState.stateRoot.toHex()

                    if (computedStateRoot != expectedStateRoot) {
                        // For debugging: compare individual keyvals
                        println("[$traceName] Block $blockNum: State root mismatch")
                        println("  Expected: $expectedStateRoot")
                        println("  Computed: $computedStateRoot")

                        // Compare keyvals for debugging
                        compareKeyvals(step.postState.keyvals, result.postState.keyvals, "[$traceName] Block $blockNum")

                        // Also compare Safrole state for detailed debugging
                        val expectedSafroleState = StateCodec.decodeSafroleState(step.postState.keyvals)
                        if (result.safroleState != null) {
                            assertSafroleStateEquals(
                                expectedSafroleState,
                                result.safroleState!!,
                                "[$traceName] Block $blockNum"
                            )
                        }

                        fail("[$traceName] Block $blockNum: State root mismatch - expected: $expectedStateRoot, computed: $computedStateRoot")
                    }
                }
                is ImportResult.Failure -> {
                    fail("[$traceName] Block $blockNum import failed: ${result.error} - ${result.message}")
                }
            }

            // Update current state root for next iteration using expected post-state
            currentStateRoot = step.postState.stateRoot

            if (blockNum % 10 == 0) {
                println("[$traceName] Processed $blockNum blocks...")
            }
        }

        println("[$traceName] Completed testing ${filenamesToTest.size} blocks successfully")
    }

    /**
     * Compare keyvals for debugging.
     */
    private fun compareKeyvals(expected: List<KeyValue>, actual: List<KeyValue>, context: String) {
        val expectedMap = expected.associateBy { it.key.toHex() }
        val actualMap = actual.associateBy { it.key.toHex() }

        // Find missing keys
        val missingKeys = expectedMap.keys - actualMap.keys
        if (missingKeys.isNotEmpty()) {
            println("$context: Missing ${missingKeys.size} keys:")
            missingKeys.take(5).forEach { key ->
                println("  - $key")
            }
        }

        // Find extra keys
        val extraKeys = actualMap.keys - expectedMap.keys
        if (extraKeys.isNotEmpty()) {
            println("$context: Extra ${extraKeys.size} keys:")
            extraKeys.take(5).forEach { key ->
                println("  + $key")
            }
        }

        // Find differing values
        val commonKeys = expectedMap.keys.intersect(actualMap.keys)
        val differingKeys = commonKeys.filter { key ->
            expectedMap[key]!!.value.toHex() != actualMap[key]!!.value.toHex()
        }
        if (differingKeys.isNotEmpty()) {
            println("$context: ${differingKeys.size} keys with differing values:")
            differingKeys.take(5).forEach { key ->
                val expectedHex = expectedMap[key]!!.value.toHex()
                val actualHex = actualMap[key]!!.value.toHex()
                println("  Key: $key")
                println("    Expected (${expectedHex.length} chars): ${expectedHex.take(200)}...")
                println("    Actual   (${actualHex.length} chars): ${actualHex.take(200)}...")
                // Find first difference
                val minLen = minOf(expectedHex.length, actualHex.length)
                for (i in 0 until minLen) {
                    if (expectedHex[i] != actualHex[i]) {
                        println("    First diff at char $i: expected '${expectedHex[i]}', actual '${actualHex[i]}'")
                        // Show some context around the difference
                        val start = maxOf(0, i - 10)
                        val end = minOf(minLen, i + 50)
                        println("    Expected around diff: ...${expectedHex.substring(start, end)}...")
                        println("    Actual around diff:   ...${actualHex.substring(start, minOf(actualHex.length, end))}...")
                        break
                    }
                }
                // If lengths differ, show the extra part
                if (expectedHex.length > actualHex.length) {
                    println("    Expected has ${expectedHex.length - actualHex.length} extra chars at end:")
                    println("    Extra: ${expectedHex.substring(actualHex.length)}")
                }
            }
        }
    }

    /**
     * Compares two SafroleState objects and asserts they are equal.
     */
    private fun assertSafroleStateEquals(expected: SafroleState, actual: SafroleState, context: String) {
        assertEquals(expected.tau, actual.tau, "$context: tau mismatch")

        // Compare eta (entropy pool)
        assertEquals(expected.eta.size, actual.eta.size, "$context: eta size mismatch")
        for (i in expected.eta.indices) {
            assertEquals(
                expected.eta[i].toHex(),
                actual.eta[i].toHex(),
                "$context: eta[$i] mismatch"
            )
        }

        // Compare validator sets
        assertValidatorListEquals(expected.kappa, actual.kappa, "$context: kappa")
        assertValidatorListEquals(expected.lambda, actual.lambda, "$context: lambda")
        assertValidatorListEquals(expected.gammaK, actual.gammaK, "$context: gammaK")
        assertValidatorListEquals(expected.iota, actual.iota, "$context: iota")

        // Compare ticket accumulator
        assertEquals(expected.gammaA.size, actual.gammaA.size, "$context: gammaA size mismatch")
        for (i in expected.gammaA.indices) {
            assertEquals(
                expected.gammaA[i].id.toHex(),
                actual.gammaA[i].id.toHex(),
                "$context: gammaA[$i].id mismatch"
            )
            assertEquals(
                expected.gammaA[i].attempt,
                actual.gammaA[i].attempt,
                "$context: gammaA[$i].attempt mismatch"
            )
        }

        // Compare sealing sequence (gammaS)
        val expectedHasTickets = expected.gammaS.tickets != null
        val actualHasTickets = actual.gammaS.tickets != null
        assertEquals(expectedHasTickets, actualHasTickets, "$context: gammaS type mismatch")

        if (expectedHasTickets) {
            val expectedTickets = expected.gammaS.tickets!!
            val actualTickets = actual.gammaS.tickets!!
            assertEquals(expectedTickets.size, actualTickets.size, "$context: gammaS tickets size mismatch")
            for (i in expectedTickets.indices) {
                assertEquals(
                    expectedTickets[i].id.toHex(),
                    actualTickets[i].id.toHex(),
                    "$context: gammaS.tickets[$i].id mismatch"
                )
            }
        } else {
            val expectedKeys = expected.gammaS.keys ?: emptyList()
            val actualKeys = actual.gammaS.keys ?: emptyList()
            assertEquals(expectedKeys.size, actualKeys.size, "$context: gammaS keys size mismatch")
            for (i in expectedKeys.indices) {
                assertEquals(
                    expectedKeys[i].toHex(),
                    actualKeys[i].toHex(),
                    "$context: gammaS.keys[$i] mismatch"
                )
            }
        }

        // Compare ring root
        assertEquals(
            expected.gammaZ.toHex(),
            actual.gammaZ.toHex(),
            "$context: gammaZ mismatch"
        )
    }

    /**
     * Compares two lists of ValidatorKey.
     */
    private fun assertValidatorListEquals(
        expected: List<io.forge.jam.safrole.ValidatorKey>,
        actual: List<io.forge.jam.safrole.ValidatorKey>,
        context: String
    ) {
        assertEquals(expected.size, actual.size, "$context: size mismatch")
        for (i in expected.indices) {
            assertEquals(
                expected[i].bandersnatch.toHex(),
                actual[i].bandersnatch.toHex(),
                "$context[$i].bandersnatch mismatch"
            )
            assertEquals(
                expected[i].ed25519.toHex(),
                actual[i].ed25519.toHex(),
                "$context[$i].ed25519 mismatch"
            )
            assertEquals(
                expected[i].bls.toHex(),
                actual[i].bls.toHex(),
                "$context[$i].bls mismatch"
            )
            assertEquals(
                expected[i].metadata.toHex(),
                actual[i].metadata.toHex(),
                "$context[$i].metadata mismatch"
            )
        }
    }

    /**
     * Runs encoding-only tests for the trace.
     * This validates that all trace data can be correctly parsed and re-encoded.
     * Loads blocks one at a time to avoid OOM with large traces.
     */
    protected fun runEncodingTests() {
        // Load genesis
        val (genesis, genesisBinary) = TestFileLoader.loadTraceGenesis<Genesis>(traceName)

        // Verify genesis encoding
        assertContentEquals(
            genesisBinary,
            genesis.encode(),
            "[$traceName] Genesis binary encoding mismatch"
        )

        // Get filenames and limit if needed
        val allFilenames = TestFileLoader.getTraceStepFilenames(traceName)
        val filenamesToTest = if (maxBlocks != null) {
            allFilenames.take(maxBlocks!!)
        } else {
            allFilenames
        }

        // Load and verify trace steps one at a time to avoid OOM
        for ((index, filename) in filenamesToTest.withIndex()) {
            val blockNum = index + 1
            val (step, expectedBinary) = TestFileLoader.loadTestDataFromTestVectors<TraceStep>(
                "traces/$traceName", filename
            )

            assertContentEquals(
                expectedBinary,
                step.encode(),
                "[$traceName] Binary encoding mismatch at block $blockNum"
            )
        }

        println("[$traceName] Encoding tests passed for ${filenamesToTest.size} blocks")
    }

    /**
     * Runs state validation tests.
     * Validates that pre-state of block N+1 matches post-state of block N.
     * Loads blocks one at a time to avoid OOM with large traces.
     */
    protected fun runStateChainTests() {
        val (genesis, _) = TestFileLoader.loadTraceGenesis<Genesis>(traceName)

        // Get filenames and limit if needed
        val allFilenames = TestFileLoader.getTraceStepFilenames(traceName)
        val filenamesToTest = if (maxBlocks != null) {
            allFilenames.take(maxBlocks!!)
        } else {
            allFilenames
        }

        var previousPostStateRoot = genesis.state.stateRoot

        // Load and verify trace steps one at a time to avoid OOM
        for ((index, filename) in filenamesToTest.withIndex()) {
            val blockNum = index + 1
            val (step, _) = TestFileLoader.loadTestDataFromTestVectors<TraceStep>(
                "traces/$traceName", filename
            )

            // Verify chain: pre-state of this block matches post-state of previous
            assertEquals(
                previousPostStateRoot.toHex(),
                step.preState.stateRoot.toHex(),
                "[$traceName] State chain broken at block $blockNum: " +
                    "previous post-state != current pre-state"
            )

            previousPostStateRoot = step.postState.stateRoot
        }

        println("[$traceName] State chain validation passed for ${filenamesToTest.size} blocks")
    }

    /**
     * Runs decoding tests.
     * Decodes binary data using fromBytes() methods and compares with JSON-parsed data.
     * This validates that the decode logic matches the encode logic (round-trip).
     */
    protected fun runDecodingTests() {
        // Load genesis
        val (genesis, genesisBinary) = TestFileLoader.loadTraceGenesis<Genesis>(traceName)

        // Decode genesis from binary
        val (decodedGenesis, _) = Genesis.fromBytes(genesisBinary)

        // Compare key fields
        assertEquals(
            genesis.header.slot,
            decodedGenesis.header.slot,
            "[$traceName] Genesis header.slot mismatch after decode"
        )
        assertEquals(
            genesis.state.stateRoot.toHex(),
            decodedGenesis.state.stateRoot.toHex(),
            "[$traceName] Genesis state.stateRoot mismatch after decode"
        )
        assertEquals(
            genesis.state.keyvals.size,
            decodedGenesis.state.keyvals.size,
            "[$traceName] Genesis state.keyvals size mismatch after decode"
        )

        // Verify round-trip: decoded genesis encodes back to original binary
        assertContentEquals(
            genesisBinary,
            decodedGenesis.encode(),
            "[$traceName] Genesis round-trip encoding mismatch"
        )

        // Get filenames and limit if needed
        val allFilenames = TestFileLoader.getTraceStepFilenames(traceName)
        val filenamesToTest = if (maxBlocks != null) {
            allFilenames.take(maxBlocks!!)
        } else {
            allFilenames
        }

        // Load and verify trace steps one at a time
        for ((index, filename) in filenamesToTest.withIndex()) {
            val blockNum = index + 1
            val (step, expectedBinary) = TestFileLoader.loadTestDataFromTestVectors<TraceStep>(
                "traces/$traceName", filename
            )

            // Decode from binary
            val (decodedStep, _) = TraceStep.fromBytes(expectedBinary)

            // Compare key fields
            assertEquals(
                step.block.header.slot,
                decodedStep.block.header.slot,
                "[$traceName] Block $blockNum: slot mismatch after decode"
            )
            assertEquals(
                step.preState.stateRoot.toHex(),
                decodedStep.preState.stateRoot.toHex(),
                "[$traceName] Block $blockNum: preState root mismatch after decode"
            )
            assertEquals(
                step.postState.stateRoot.toHex(),
                decodedStep.postState.stateRoot.toHex(),
                "[$traceName] Block $blockNum: postState root mismatch after decode"
            )

            // Verify round-trip: decoded step encodes back to original binary
            assertContentEquals(
                expectedBinary,
                decodedStep.encode(),
                "[$traceName] Block $blockNum: round-trip encoding mismatch"
            )
        }

        println("[$traceName] Decoding tests passed for ${filenamesToTest.size} blocks")
    }
}
