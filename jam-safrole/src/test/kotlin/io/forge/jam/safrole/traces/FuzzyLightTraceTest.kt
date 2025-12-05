package io.forge.jam.safrole.traces

import kotlin.test.Test

/**
 * Tests for fuzzy light service work report traces.
 * These traces test no-safrole block authoring with fuzzy service
 * (empty fuzzy service profile), maximum 1 work item per report.
 */
class FuzzyLightTraceTest : BaseTraceTest() {
    override val traceName = "fuzzy_light"

    @Test
    fun testFuzzyLightStateChain() = runStateChainTests()

    @Test
    fun testFuzzyLightEncoding() = runEncodingTests()

    @Test
    fun testFuzzyLightBlockImport() = runTraceTests()

    @Test
    fun testFuzzyLightDecoding() = runDecodingTests()
}
