package io.forge.jam.core.encoding

import io.forge.jam.core.Preimage
import kotlin.test.Test
import kotlin.test.assertContentEquals

class PreimageTest {
    @Test
    fun testEncodePreimage() {
        // Load JSON data from resources using the class loader
        val (inputPreimages, expectedOutputBytes) = TestFileLoader.loadTestDataFromTestVectors<List<Preimage>>("codec/tiny", "preimages_extrinsic")

        val encodedPreimages = inputPreimages.map { preimage ->
            val extrinsic =
                Preimage(preimage.requester, preimage.blob)
            extrinsic.encode()
        }

        // Process each assurance
        val versionByte = byteArrayOf(0x03)
        val concatenatedEncodedPreimages = versionByte + encodedPreimages.reduce { acc, bytes -> acc + bytes }

        // Compare the concatenated encoded bytes with the expected output bytes
        assertContentEquals(
            expectedOutputBytes,
            concatenatedEncodedPreimages,
            "Encoded bytes do not match expected output"
        )
    }
}

