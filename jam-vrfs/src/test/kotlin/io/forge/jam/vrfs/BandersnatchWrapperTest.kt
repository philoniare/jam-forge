// RustLibraryTest.kt
import io.forge.jam.vrfs.RustLibrary
import io.forge.jam.vrfs.use
import kotlin.test.Test
import kotlin.test.assertNotNull

class RustLibraryTest {
    private fun generateTestPublicKeys(size: Int): List<ByteArray> {
        return List(size) { i ->
            // Generate some deterministic test keys
            ByteArray(32) { j -> (i * j % 256).toByte() }
        }
    }

    @Test
    fun testRingVrfSignAndVerify() {
        val ringSize = 1023
        val proverKeyIndex = 3
        val publicKeys = generateTestPublicKeys(ringSize)

        RustLibrary.use(publicKeys, ringSize, proverKeyIndex) { (proverPtr, verifierPtr) ->
            val vrfInputData = "test input".toByteArray()
            val auxData = "test aux".toByteArray()

            val signature = RustLibrary.proverRingVrfSign(
                proverPtr,
                vrfInputData,
                auxData
            )

            assertNotNull(signature) { "Signature should not be null" }

            val outputHash = RustLibrary.verifierRingVrfVerify(
                verifierPtr,
                vrfInputData,
                auxData,
                signature
            )

            assertNotNull(outputHash) { "Output hash should not be null" }
            assert(outputHash.size == 32) { "Output hash should be 32 bytes" }
        }
    }

    @Test
    fun testMultipleSignatures() {
        val ringSize = 1023
        val proverKeyIndex = 3
        val publicKeys = generateTestPublicKeys(ringSize)

        RustLibrary.use(publicKeys, ringSize, proverKeyIndex) { (proverPtr, verifierPtr) ->
            repeat(3) { i ->
                val vrfInputData = "test input $i".toByteArray()
                val auxData = "test aux $i".toByteArray()

                val signature = RustLibrary.proverRingVrfSign(
                    proverPtr,
                    vrfInputData,
                    auxData
                )

                assertNotNull(signature) { "Signature should not be null for iteration $i" }

                val outputHash = RustLibrary.verifierRingVrfVerify(
                    verifierPtr,
                    vrfInputData,
                    auxData,
                    signature
                )

                assertNotNull(outputHash) { "Output hash should not be null for iteration $i" }
                assert(outputHash.size == 32) { "Output hash should be 32 bytes for iteration $i" }
            }
        }
    }
}
