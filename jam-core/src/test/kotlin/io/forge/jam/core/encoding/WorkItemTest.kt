package io.forge.jam.core.encoding

import io.forge.jam.core.WorkItem
import kotlin.test.Test
import kotlin.test.assertContentEquals

class WorkItemTest {

    private fun testEncodeWorkItem(configPath: String) {
        val (inputWorkItem, expectedOutputBytes) = TestFileLoader.loadTestDataFromTestVectors<WorkItem>(configPath, "work_item")

        assertContentEquals(
            expectedOutputBytes,
            inputWorkItem.encode(),
            "Encoded bytes do not match expected output"
        )
    }

    @Test
    fun testEncodeWorkItemTiny() {
        testEncodeWorkItem("codec/tiny")
    }

    @Test
    fun testEncodeWorkItemFull() {
        testEncodeWorkItem("codec/full")
    }
}
