package io.forge.jam.safrole.traces

import kotlin.test.Test

/**
 * Tests for fallback block authoring traces.
 * These traces test fallback block authoring without work reports.
 */
class FallbackTraceTest : BaseTraceTest() {
    override val traceName = "fallback"

    @Test
    fun testFallbackStateChain() = runStateChainTests()

    @Test
    fun testFallbackEncoding() = runEncodingTests()

    @Test
    fun testFallbackBlockImport() = runTraceTests()

    @Test
    fun testFallbackDecoding() = runDecodingTests()
}
