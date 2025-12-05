package io.forge.jam.safrole.traces

import kotlin.test.Test

/**
 * Tests for preimages light work report traces.
 * These traces test no-safrole block authoring with preimages
 * related work reports, maximum 1 work item per report.
 */
class PreimagesLightTraceTest : BaseTraceTest() {
    override val traceName = "preimages_light"

    @Test
    fun testPreimagesLightStateChain() = runStateChainTests()

    @Test
    fun testPreimagesLightEncoding() = runEncodingTests()

    @Test
    fun testPreimagesLightBlockImport() = runStateChainTests()

    @Test
    fun testPreimagesLightDecoding() = runDecodingTests()
}
