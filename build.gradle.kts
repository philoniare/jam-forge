import io.gitlab.arturbosch.detekt.Detekt
import io.gitlab.arturbosch.detekt.DetektCreateBaselineTask

plugins {
    kotlin("jvm") version "2.0.0"
    id("io.gitlab.arturbosch.detekt") version "1.23.6"
}

allprojects {
    repositories {
        mavenCentral()
    }

    dependencies {
        implementation(kotlin("stdlib"))
        testImplementation(kotlin("test"))
        detektPlugins("io.gitlab.arturbosch.detekt:detekt-formatting:1.23.6")
        testImplementation("commons-codec:commons-codec:1.14")
        testImplementation("org.mockito:mockito-core:3.+")
    }

    tasks.test {
        useJUnitPlatform()
    }

    tasks.withType<Detekt>().configureEach {
        jvmTarget = "1.8"
        enabled = false
    }
    tasks.withType<DetektCreateBaselineTask>().configureEach {
        jvmTarget = "1.8"
        enabled = false
    }
}

subprojects {
    apply(plugin = "kotlin")
    apply(plugin = "io.gitlab.arturbosch.detekt")

    detekt {
        buildUponDefaultConfig = true
        allRules = false
        config.setFrom("$rootDir/config/detekt.yml")
    }
}

tasks.register<Detekt>("detektAll") {
    description = "Runs detekt on the whole project at once."
    parallel = true
    config.setFrom(files("$rootDir/config/detekt.yml"))
    setSource(files(projectDir))
    include("**/*.kt")
    include("**/*.kts")
    exclude("**/resources/**")
    exclude("**/build/**")
    reports {
        xml.required.set(false)
        html.required.set(true)
        txt.required.set(false)
    }
}
