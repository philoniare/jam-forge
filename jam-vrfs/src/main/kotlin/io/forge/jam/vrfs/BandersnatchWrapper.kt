package io.forge.jam.vrfs

import java.io.File

class BandersnatchWrapper(ringSize: Int) {
    companion object {
        init {
            val libraryName = "bandersnatch_vrfs_wrapper"
            val osNameProperty = System.getProperty("os.name").lowercase()
            val osName = when {
                osNameProperty.contains("mac") -> "mac"
                osNameProperty.contains("linux") -> "linux"
                osNameProperty.contains("windows") -> "windows"
                else -> throw RuntimeException("Unsupported operating system: $osNameProperty")
            }

            val libFileName = when (osName) {
                "mac" -> "lib$libraryName.dylib"
                "linux" -> "lib$libraryName.so"
                "windows" -> "$libraryName.dll"
                else -> throw RuntimeException("Unsupported operating system: $osName")
            }

            val projectDir = File(System.getProperty("user.dir"))
            val parentBuildLib = projectDir.parentFile
                ?.resolve("build")
                ?.resolve("native-libs")
                ?.resolve(osName)
                ?.resolve(libFileName)

            if (parentBuildLib?.exists() == true) {
                System.load(parentBuildLib.absolutePath)
            } else {
                println("Native library not found in parent build directory")
            }
        }

        @JvmStatic
        external fun initializeContext(srsData: ByteArray, ringSize: Int): ByteArray

        @JvmStatic
        external fun getVerifierCommitment(ringSize: Int, keys: ByteArray): ByteArray?

        @JvmStatic
        external fun verifierRingVrfVerify(
            entropy: ByteArray,
            attempt: Long,
            signature: ByteArray,
            commitment: ByteArray,
            ringSize: Int
        ): ByteArray

        @JvmStatic
        external fun getIetfVrfOutput(signature: ByteArray): ByteArray

        @JvmStatic
        external fun secretFromSeed(seed: ByteArray): ByteArray

        @JvmStatic
        external fun publicFromSecret(secretBytes: ByteArray): ByteArray

        @JvmStatic
        external fun ietfVrfSign(secretBytes: ByteArray, vrfInput: ByteArray, auxData: ByteArray): ByteArray

        @JvmStatic
        external fun ietfVrfVerify(
            publicBytes: ByteArray,
            vrfInput: ByteArray,
            auxData: ByteArray,
            signature: ByteArray
        ): ByteArray

        @JvmStatic
        external fun getVrfOutput(secretBytes: ByteArray, vrfInput: ByteArray): ByteArray
    }

    init {
        val srsData = loadSrsData()
        initializeContext(srsData, ringSize)
    }

    private fun loadSrsData(): ByteArray {
        // Load from resources
        val resourcePath = "/zcash-srs-2-11-uncompressed.bin"
        return BandersnatchWrapper::class.java.getResourceAsStream(resourcePath)?.use { stream ->
            stream.readBytes()
        } ?: throw RuntimeException("SRS file not found in resources: $resourcePath")
    }


    fun verifyRingProof(
        entropy: ByteArray,
        attempt: Long,
        signature: ByteArray,
        commitment: ByteArray,
        ringSize: Int
    ): ByteArray {
        // If result is all zeros, verification failed
        try {
            val result = verifierRingVrfVerify(entropy, attempt, signature, commitment, ringSize)
            return result
        } catch (e: Exception) {
            return byteArrayOf(0)
        }
    }

    fun generateRingRoot(
        publicKeys: List<ByteArray>,
        ringSize: Int,
    ): ByteArray? {
        val srsData = loadSrsData()
        initializeContext(srsData, ringSize)
        val concatenatedKeys: ByteArray = publicKeys.flatMap { it.toList() }.toByteArray()
        return getVerifierCommitment(ringSize, concatenatedKeys)
    }
}
