plugins {
    kotlin("jvm")
    id("io.gitlab.arturbosch.detekt")
}

dependencies {
    implementation(kotlin("stdlib"))
    testImplementation(kotlin("test"))
    implementation("org.bouncycastle:bcprov-jdk15on:1.70")
}
