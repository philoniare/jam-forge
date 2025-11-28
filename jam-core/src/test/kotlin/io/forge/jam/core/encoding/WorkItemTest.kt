package io.forge.jam.core.encoding

import io.forge.jam.core.WorkItem
import kotlin.test.Test
import kotlin.test.assertContentEquals

class WorkItemTest {
    @Test
    fun testEncodeWorkItem() {
        // Load JSON data from resources using the class loader
        val (inputWorkItem, expectedOutputBytes) = TestFileLoader.loadTestDataFromTestVectors<WorkItem>("codec/tiny", "work_item")

        // Compare the concatenated encoded bytes with the expected output bytes
        assertContentEquals(
            expectedOutputBytes,
            inputWorkItem.encode(),
            "Encoded bytes do not match expected output"
        )
    }
}



