#!/usr/bin/env kotlin
import io.forge.ProgramBlob
import java.io.File
import java.nio.ByteBuffer
import java.nio.file.Paths


println("PVM Example Script")

fun readFileBytes(relativePath: String): ByteArray {
    val currentDir = Paths.get("").toAbsolutePath()
    val filePath = currentDir.resolve(relativePath)
    return File(filePath.toString()).readBytes()
}

// Read the program blob from file
val rawBlob = readFileBytes("./example-hello-world.polkavm")
println("Raw blob: ${rawBlob}")

// Create a ByteBuffer from the raw bytes
val programBytes = ByteBuffer.wrap(rawBlob)

// Parse the program
// Commented out for now as ProgramBlob might not be in the classpath
val programResult = ProgramBlob.parse(programBytes)

println("Raw blob size: ${rawBlob.size} bytes")

// when (programResult) {
//     is Result.Success -> {
//         val program = programResult.value
//         println("Program parsed successfully")
//         println("RO Data Size: ${program.roDataSize()}")
//         println("RW Data Size: ${program.rwDataSize()}")
//         println("Stack Size: ${program.stackSize()}")

//         // Example of using the program
//         val instructions = program.instructions()
//         println("First instruction offset: ${instructions.offset}")

//         // Add more examples of using the ProgramBlob methods here
//     }
//     is Result.Failure -> {
//         println("Failed to parse program: ${programResult.exceptionOrNull()?.message}")
//     }
// }

println("PVM Example Script completed")
