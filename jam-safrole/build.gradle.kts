plugins {
    kotlin("jvm")
    id("io.gitlab.arturbosch.detekt")
    kotlin("plugin.serialization") version "1.9.10"
}

dependencies {
    implementation(kotlin("stdlib"))
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:1.5.0")
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.7.3")
    implementation("org.bouncycastle:bcprov-jdk15on:1.70")
    implementation(project(":jam-core"))
    implementation(project(":jam-vrfs"))
    implementation(project(":jam-pvm"))

    testImplementation(kotlin("test"))
    testImplementation(project(":jam-core", configuration = "testArtifacts"))
}

tasks.test {
    maxHeapSize = "4g"
    testLogging {
        events("passed", "skipped", "failed", "standardOut", "standardError")
        showStandardStreams = true
        showStackTraces = true
    }
}
