plugins {
    id("org.gradle.toolchains.foojay-resolver-convention") version "0.8.0"
}
rootProject.name = "jam"
include("jam-core", "jam-pvm", "jam-vrfs")

// Configure project directories
project(":jam-vrfs").projectDir = file("jam-vrfs")
