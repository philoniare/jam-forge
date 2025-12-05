package io.forge.jam.safrole.traces

import kotlin.test.Test

/**
 * Tests for service storage light work report traces.
 * These traces test no-safrole block authoring with service
 * related work reports, maximum 1 work item per report.
 */
class StorageLightTraceTest : BaseTraceTest() {
    override val traceName = "storage_light"

//    @Test
//    fun testStorageLightStateChain() = runStateChainTests()
//
//    @Test
//    fun testStorageLightEncoding() = runEncodingTests()

    @Test
    fun testStorageLightBlockImport() = runSingleBlockTest(9)

//    @Test
//    fun testStorageLightDecoding() = runDecodingTests()
}
