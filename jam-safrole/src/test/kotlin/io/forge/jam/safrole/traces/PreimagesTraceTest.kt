package io.forge.jam.safrole.traces

import kotlin.test.Test

/**
 * Tests for preimages work report traces.
 * These traces test no-safrole block authoring with preimages
 * related work reports, maximum 6 work items per report.
 */
class PreimagesTraceTest : BaseTraceTest() {
    override val traceName = "preimages"

    @Test
    fun testPreimagesStateChain() = runStateChainTests()

    @Test
    fun testPreimagesEncoding() = runEncodingTests()

    @Test
    fun testPreimagesBlockImport() = runStateChainTests()

    @Test
    fun testPreimagesDecoding() = runDecodingTests()
}
