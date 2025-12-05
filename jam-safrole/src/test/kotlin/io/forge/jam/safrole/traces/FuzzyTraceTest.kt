package io.forge.jam.safrole.traces

import kotlin.test.Test

/**
 * Tests for fuzzy service work report traces.
 * These traces test no-safrole block authoring with fuzzy service
 * (random fuzzy service profile), maximum 6 work items per report.
 */
class FuzzyTraceTest : BaseTraceTest() {
    override val traceName = "fuzzy"

    @Test
    fun testFuzzyStateChain() = runStateChainTests()

    @Test
    fun testFuzzyEncoding() = runEncodingTests()

    @Test
    fun testFuzzyBlockImport() = runTraceTests()

    @Test
    fun testFuzzyDecoding() = runDecodingTests()
}
