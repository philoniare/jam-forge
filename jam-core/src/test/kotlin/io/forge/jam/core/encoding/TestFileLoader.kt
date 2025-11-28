package io.forge.jam.core.encoding

import kotlinx.serialization.decodeFromString
import kotlinx.serialization.json.Json
import java.io.File
import java.io.InputStream

class TestFileLoader {
    companion object {
        @PublishedApi
        internal val JAM_TEST_VECTORS_PATH: File by lazy {
            val cwd = System.getProperty("user.dir")
            // Handle both running from project root or from module directory
            if (cwd.endsWith("jam-core")) {
                File(cwd).parentFile.resolve("jamtestvectors")
            } else {
                File(cwd).resolve("jamtestvectors")
            }
        }
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

                else -> throw IllegalStateException("Unsupported protocol: ${resource.protocol}")
            }
        }

        /**
         * Loads JSON data from the jamtestvectors submodule.
         * @param subPath The path within jamtestvectors (e.g., "codec/tiny")
         * @param filename The name of the JSON file (without extension) to load.
         * @return The JSON data parsed into type [T].
         */
        inline fun <reified T> loadJsonFromTestVectors(subPath: String, filename: String): T {
            val json = Json { ignoreUnknownKeys = true }
            val file = JAM_TEST_VECTORS_PATH.resolve(subPath).resolve("$filename.json")
            require(file.exists()) { "File not found: ${file.absolutePath}" }
            val jsonData = file.readText()
            return json.decodeFromString<T>(jsonData)
        }

        /**
         * Loads binary data from the jamtestvectors submodule.
         * @param subPath The path within jamtestvectors (e.g., "codec/tiny")
         * @param filename The name of the binary file (without extension) to load.
         * @param fileExtension The file extension (default ".bin").
         * @return The binary data as a ByteArray.
         */
        fun loadBinaryFromTestVectors(subPath: String, filename: String, fileExtension: String = ".bin"): ByteArray {
            val file = JAM_TEST_VECTORS_PATH.resolve(subPath).resolve("$filename$fileExtension")
            require(file.exists()) { "File not found: ${file.absolutePath}" }
            return file.readBytes()
        }

        /**
         * Loads both JSON and binary data from the jamtestvectors submodule.
         * @param subPath The path within jamtestvectors (e.g., "codec/tiny")
         * @param filename The name of the file (without extension) to load.
         * @param fileExtension The binary file extension (default ".bin").
         * @return A pair containing the parsed JSON data as type [T] and the binary data as a ByteArray.
         */
        inline fun <reified T> loadTestDataFromTestVectors(
            subPath: String,
            filename: String,
            fileExtension: String = ".bin"
        ): Pair<T, ByteArray> {
            return Pair(
                loadJsonFromTestVectors<T>(subPath, filename),
                loadBinaryFromTestVectors(subPath, filename, fileExtension)
            )
        }

        /**
         * Gets all test filenames (without extension) from a directory in jamtestvectors.
         * @param subPath The path within jamtestvectors (e.g., "codec/tiny")
         * @return List of filenames without extensions that have both .json and .bin files.
         */
        fun getTestFilenamesFromTestVectors(subPath: String): List<String> {
            val dir = JAM_TEST_VECTORS_PATH.resolve(subPath)
            require(dir.exists() && dir.isDirectory) { "Directory not found: ${dir.absolutePath}" }
            return dir.listFiles()
                ?.filter { it.isFile && it.name.endsWith(".json") }
                ?.map { it.nameWithoutExtension }
                ?: emptyList()
        }
    }
}
