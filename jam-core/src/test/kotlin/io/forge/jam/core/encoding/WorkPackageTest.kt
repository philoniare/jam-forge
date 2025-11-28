package io.forge.jam.core.encoding

import io.forge.jam.core.WorkPackage
import kotlin.test.Test
import kotlin.test.assertContentEquals

class WorkPackageTest {
    @Test
    fun testEncodeWorkPackage() {
        // Load JSON data from resources using the class loader
        val (inputWorkPackage, expectedOutputBytes) = TestFileLoader.loadTestDataFromTestVectors<WorkPackage>("codec/tiny", "work_package")

        // Compare the concatenated encoded bytes with the expected output bytes
        assertContentEquals(
            expectedOutputBytes,
            inputWorkPackage.encode(),
            "Encoded bytes do not match expected output"
        )
    }
}



