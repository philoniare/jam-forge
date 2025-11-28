package io.forge.jam.core.encoding

import io.forge.jam.core.WorkPackage
import kotlin.test.Test
import kotlin.test.assertContentEquals

class WorkPackageTest {

    private fun testEncodeWorkPackage(configPath: String) {
        val (inputWorkPackage, expectedOutputBytes) = TestFileLoader.loadTestDataFromTestVectors<WorkPackage>(configPath, "work_package")

        assertContentEquals(
            expectedOutputBytes,
            inputWorkPackage.encode(),
            "Encoded bytes do not match expected output"
        )
    }

    @Test
    fun testEncodeWorkPackageTiny() {
        testEncodeWorkPackage("codec/tiny")
    }

    @Test
    fun testEncodeWorkPackageFull() {
        testEncodeWorkPackage("codec/full")
    }
}
