// src/main/kotlin/io/forge/jam/vrfs/RustLibrary.kt
package io.forge.jam.vrfs

import java.io.File

object RustLibrary {
    init {
        val libraryName = "bandersnatch_vrfs_wrapper"

        try {
            // First try the system property way
            System.loadLibrary(libraryName)
            val srsData = loadSrsData()
            initializeContext(srsData)
        } catch (e: UnsatisfiedLinkError) {
            // If that fails, try to find it in the build directory
            val buildDir = File("build/native-libs")
            val libFileName = when {
                System.getProperty("os.name").lowercase().contains("mac") -> "lib$libraryName.dylib"
                System.getProperty("os.name").lowercase().contains("linux") -> "lib$libraryName.so"
                System.getProperty("os.name").lowercase().contains("windows") -> "$libraryName.dll"
                else -> throw RuntimeException("Unsupported operating system")
            }

            val libFile = buildDir.resolve(libFileName)
            if (libFile.exists()) {
                System.load(libFile.absolutePath)
            } else {
                // If still not found, try to locate it relative to the working directory
                val workingDir = File(System.getProperty("user.dir"))
                val alternativeLibFile = workingDir.resolve("build/native-libs/$libFileName")
                if (alternativeLibFile.exists()) {
                    System.load(alternativeLibFile.absolutePath)
                } else {
                    throw RuntimeException(
                        """
                        Cannot find native library. Tried:
                        1. System library path: $libraryName
                        2. Build directory: ${libFile.absolutePath}
                        3. Working directory: ${alternativeLibFile.absolutePath}
                        Current working directory: ${workingDir.absolutePath}
                        Library path: ${System.getProperty("java.library.path")}
                    """.trimIndent()
                    )
                }
            }
        }
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
    external fun createProver(ringSize: Int, proverKeyIndex: Int): Long

    @JvmStatic
    external fun destroyProver(proverPtr: Long)

    @JvmStatic
    external fun createVerifier(ringSize: Int): Long

    @JvmStatic
    external fun destroyVerifier(verifierPtr: Long)

    @JvmStatic
    external fun proverRingVrfSign(
        proverPtr: Long,
        vrfInputData: ByteArray,
        auxData: ByteArray
    ): ByteArray?

    @JvmStatic
    external fun verifierRingVrfVerify(
        verifierPtr: Long,
        vrfInputData: ByteArray,
        auxData: ByteArray,
        signature: ByteArray
    ): ByteArray?

    @JvmStatic
    external fun rustFree(ptr: Long, len: Long)
}

// Extensions to make the API more Kotlin-friendly
fun RustLibrary.use(ringSize: Int, proverKeyIndex: Int, block: (Pair<Long, Long>) -> Unit) {
    val proverPtr = createProver(ringSize, proverKeyIndex)
    val verifierPtr = createVerifier(ringSize)
    try {
        block(Pair(proverPtr, verifierPtr))
    } finally {
        destroyProver(proverPtr)
        destroyVerifier(verifierPtr)
    }
}
