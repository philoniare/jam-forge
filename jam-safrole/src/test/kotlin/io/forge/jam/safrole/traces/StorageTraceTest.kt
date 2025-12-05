package io.forge.jam.safrole.traces

import kotlin.test.Test

/**
 * Tests for service storage work report traces.
 * These traces test no-safrole block authoring with service storage
 * related work reports, maximum 6 work items per report.
 */
class StorageTraceTest : BaseTraceTest() {
    override val traceName = "storage"

    @Test
    fun testStorageStateChain() = runStateChainTests()

    @Test
    fun testStorageEncoding() = runEncodingTests()

    @Test
    fun testStorageBlockImport() = runStateChainTests()

    @Test
    fun testStorageDecoding() = runDecodingTests()
}
