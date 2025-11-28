package io.forge.jam.core.encoding

import io.forge.jam.core.Preimage
import kotlin.test.Test
import kotlin.test.assertContentEquals

class PreimageTest {

    private fun testEncodePreimage(configPath: String) {
        val (inputPreimages, expectedOutputBytes) = TestFileLoader.loadTestDataFromTestVectors<List<Preimage>>(configPath, "preimages_extrinsic")

        val encodedPreimages = inputPreimages.map { preimage ->
            val extrinsic =
                Preimage(preimage.requester, preimage.blob)
            extrinsic.encode()
        }

        val versionByte = byteArrayOf(0x03)
        val concatenatedEncodedPreimages = versionByte + encodedPreimages.reduce { acc, bytes -> acc + bytes }

        assertContentEquals(
            expectedOutputBytes,
            concatenatedEncodedPreimages,
            "Encoded bytes do not match expected output"
        )
    }

    @Test
    fun testEncodePreimageTiny() {
        testEncodePreimage("codec/tiny")
    }

    @Test
    fun testEncodePreimageFull() {
        testEncodePreimage("codec/full")
    }
}
