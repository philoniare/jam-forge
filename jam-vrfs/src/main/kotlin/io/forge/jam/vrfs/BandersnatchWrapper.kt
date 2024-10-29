// src/main/kotlin/io/forge/jam/vrfs/RustLibrary.kt
package io.forge.jam.vrfs

import java.io.File

object RustLibrary {
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

        // Initialize the context as before
        val srsData = loadSrsData()
        initializeContext(srsData)
    }

    private fun loadSrsData(): ByteArray {
        // Load from resources
        val resourcePath = "/zcash-srs-2-11-uncompressed.bin"
        return RustLibrary::class.java.getResourceAsStream(resourcePath)?.use { stream ->
            stream.readBytes()
        } ?: throw RuntimeException("SRS file not found in resources: $resourcePath")
    }

    @JvmStatic
    external fun initializeContext(srsData: ByteArray): ByteArray

    @JvmStatic
    external fun getVerifierCommitment(ringSize: Int, keys: ByteArray): ByteArray?

    @JvmStatic
    external fun verifierRingVrfVerify(
        entropy: ByteArray,
        attempt: Long,
        signature: ByteArray,
        commitment: ByteArray
    ): ByteArray

    @JvmStatic
    fun verifyRingProof(
        entropy: ByteArray,
        attempt: Long,
        signature: ByteArray,
        commitment: ByteArray
    ): ByteArray {
        // If result is all zeros, verification failed
        try {
            val result = verifierRingVrfVerify(entropy, attempt, signature, commitment)
            return result
        } catch (e: Exception) {
            return byteArrayOf(0)
        }
    }

    @JvmStatic
    fun generateRingRoot(
        publicKeys: List<ByteArray>,
        ringSize: Int,
    ): ByteArray? {
        val concatenatedKeys: ByteArray = publicKeys.flatMap { it.toList() }.toByteArray()
        return getVerifierCommitment(ringSize, concatenatedKeys)
    }
}
