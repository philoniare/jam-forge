package io.forge.jam.core.encoding

import kotlinx.serialization.decodeFromString
import kotlinx.serialization.json.Json
import java.io.InputStream

class TestFileLoader {
    companion object {
        /**
         * Loads JSON data from the specified resource file.
         * @param filename The name of the JSON file (without extension) to load.
         * @return The JSON data as a string.
         */
        inline fun <reified T> loadJsonData(filename: String): T {
            val json = Json { ignoreUnknownKeys = true }
            val jsonInputStream: InputStream = this::class.java.getResourceAsStream("/$filename.json")
                ?: throw IllegalArgumentException("File not found: $filename.json")
            val jsonData = jsonInputStream.bufferedReader().use { it.readText() }
            val parsedJson = json.decodeFromString<T>(jsonData)
            return parsedJson
        }

        /**
         * Loads expected binary data from the specified resource file.
         * @param filename The name of the binary file (without extension) to load.
         * @return The binary data as a ByteArray.
         */
        fun loadExpectedBinaryData(filename: String, fileExtension: String): ByteArray {
            val binInputStream: InputStream = this::class.java.getResourceAsStream("/$filename$fileExtension")
                ?: throw IllegalArgumentException("File not found: $filename.bin")

            return binInputStream.readBytes()
        }

        /**
         * Loads both JSON and expected binary data from the specified resource files.
         * The JSON data is parsed into the provided generic type [T].
         *
         * @param T The type to which the JSON data should be parsed.
         * @param filename The name of the resource file (without extension) to load.
         * @return A pair containing the parsed JSON data as type [T] and the binary data as a ByteArray.
         */
        inline fun <reified T> loadTestData(filename: String, fileExtension: String = ".bin"): Pair<T, ByteArray> {
            return Pair(loadJsonData<T>(filename), loadExpectedBinaryData(filename, fileExtension))
        }

        fun getTestFilenamesFromResources(folderName: String): List<String> {
            val classLoader = TestFileLoader::class.java.classLoader
            val resource = classLoader.getResource(folderName)
                ?: throw IllegalStateException("Resources directory not found")

            return when (resource.protocol) {
                "file" -> {
                    java.io.File(resource.path)
                        .walk()
                        .filter { it.isFile && it.name.endsWith(".json") }
                        .map { it.nameWithoutExtension }
                        .toList()
                }

                "jar" -> {
                    val jarPath = resource.path.substringBefore("!")
                    val fs =
                        java.nio.file.FileSystems.newFileSystem(java.net.URI(jarPath), mutableMapOf<String, String>())
                    fs.use { fileSystem ->
                        java.nio.file.Files.walk(fileSystem.getPath("/"))
                            .filter { it.toString().endsWith(".json") }
                            .map { it.fileName.toString().removeSuffix(".json") }
                            .toList()
                    }
                }

                else -> throw IllegalStateException("Unsupported protocol: ${resource.protocol}")
            }
        }
    }
}
