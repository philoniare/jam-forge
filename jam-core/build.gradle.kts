plugins {
    kotlin("jvm")
    id("io.gitlab.arturbosch.detekt")
    kotlin("plugin.serialization") version "1.9.10"
}

dependencies {
    implementation(kotlin("stdlib"))
    testImplementation(kotlin("test"))
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:1.5.0")
    implementation("org.bouncycastle:bcprov-jdk15on:1.70")
}

sourceSets {
    test {
        java.srcDirs("src/test/kotlin")
    }
}

// Create a test JAR that contains test classes
tasks.register<Jar>("testJar") {
    archiveClassifier.set("tests")
    from(sourceSets.test.get().output)
}

// Make test artifacts available
configurations {
    create("testArtifacts")
}

artifacts {
    add("testArtifacts", tasks["testJar"])
}
