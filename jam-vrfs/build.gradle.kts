plugins {
    kotlin("jvm")
    id("io.gitlab.arturbosch.detekt")
}

dependencies {
    implementation(kotlin("stdlib"))
    testImplementation(kotlin("test"))
}

val os = org.gradle.internal.os.OperatingSystem.current()!!
val libPrefix = if (os.isWindows) "" else "lib"
val libSuffix = when {
    os.isMacOsX -> "dylib"
    os.isLinux -> "so"
    os.isWindows -> "dll"
    else -> throw GradleException("Unsupported operating system")
}

val nativeLibName = "${libPrefix}bandersnatch_vrfs_wrapper.$libSuffix"
val rustProjectDir = project.projectDir.resolve("bandersnatch-vrfs-wrapper")
val nativeLibs = project.buildDir.resolve("native-libs")

tasks.register<Copy>("copyNativeLib") {
    dependsOn("buildRust")

    from(rustProjectDir.resolve("target/release/$nativeLibName"))
    into(nativeLibs)

    doFirst {
        // Ensure directory exists
        nativeLibs.mkdirs()
    }

    doLast {
        val libFile = nativeLibs.resolve(nativeLibName)
        if (libFile.exists()) {
            libFile.setExecutable(true, false)
        } else {
            throw GradleException("Failed to copy native library to: ${libFile.absolutePath}")
        }
    }
}

tasks.register<Exec>("buildRust") {
    workingDir(rustProjectDir)
    println("$rustProjectDir")
    commandLine("cargo", "build", "--release")
}

dependencies {
    testImplementation(kotlin("test"))
    testImplementation("org.junit.jupiter:junit-jupiter:5.8.1")
}

tasks.test {
    dependsOn("copyNativeLib")
    useJUnitPlatform()

    doFirst {
        // Set library path to include our native libs directory
        val libraryPath = System.getProperty("java.library.path")
        val updatedPath = "${nativeLibs.absolutePath}${File.pathSeparator}$libraryPath"
        systemProperty("java.library.path", updatedPath)
    }
}

tasks.clean {
    doLast {
        // Clean Rust build artifacts
        exec {
            workingDir(rustProjectDir)
            commandLine("cargo", "clean")
        }
    }
}
