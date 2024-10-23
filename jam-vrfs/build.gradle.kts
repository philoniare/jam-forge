plugins {
    kotlin("vrfs")
    id("io.gitlab.arturbosch.detekt")
}

dependencies {
    implementation(kotlin("stdlib"))
    testImplementation(kotlin("test"))
}

