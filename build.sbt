import scala.sys.process._

ThisBuild / scalaVersion := "3.3.7"
ThisBuild / organization := "io.forge.jam"
ThisBuild / version := "0.1.0-SNAPSHOT"

// Common compiler options for all modules
ThisBuild / scalacOptions ++= Seq(
  "-Wunused:all"  // Warn on unused imports, privates, locals, params, etc.
)

// Common dependency versions
val catsVersion = "2.10.0"
val scodecVersion = "2.2.1"

// Detect OS for native library paths
val osName = System.getProperty("os.name").toLowerCase
val osDirName = if (osName.contains("mac")) "mac"
else if (osName.contains("linux")) "linux"
else if (osName.contains("win")) "windows"
else "mac"

val libSuffix = if (osName.contains("mac")) "dylib"
else if (osName.contains("linux")) "so"
else if (osName.contains("win")) "dll"
else "dylib"

// Task keys for building native libraries (defined before use)
lazy val buildNativeLib = taskKey[Unit]("Build native Bandersnatch VRF library")
lazy val buildEd25519ZebraLib = taskKey[Unit]("Build native Ed25519-Zebra library")

lazy val root = (project in file("."))
  .aggregate(core, crypto, pvm, protocol, conformance)
  .settings(
    name := "jam",
    publish / skip := true
  )

lazy val core = (project in file("modules/core"))
  .settings(
    name := "jam-core",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.typelevel" %% "spire" % "0.18.0",
      "org.scodec" %% "scodec-core" % scodecVersion,
      "org.bouncycastle" % "bcprov-jdk18on" % "1.77",
      "io.circe" %% "circe-core" % "0.14.6",
      "io.circe" %% "circe-parser" % "0.14.6",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
      "ch.qos.logback" % "logback-classic" % "1.4.11",
      "org.scalatest" %% "scalatest" % "3.2.17" % Test
    ),
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-language:implicitConversions"
    )
  )

lazy val crypto = (project in file("modules/crypto"))
  .dependsOn(core)
  .settings(
    name := "jam-crypto",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.typelevel" %% "spire" % "0.18.0",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
      "ch.qos.logback" % "logback-classic" % "1.4.11",
      "org.scalatest" %% "scalatest" % "3.2.17" % Test
    ),
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-language:implicitConversions"
    ),
    // Native library build task implementation
    buildNativeLib := {
      val baseDir = (ThisBuild / baseDirectory).value
      val rustProjectDir = baseDir / "modules" / "crypto" / "native" / "bandersnatch-vrfs-wrapper"
      val targetDir = baseDir / "modules" / "crypto" / "native" / "build" / osDirName
      val libName = s"libbandersnatch_vrfs_wrapper.$libSuffix"

      val targetLib = targetDir / libName
      val sourceLib = rustProjectDir / "target" / "release" / libName

      if (!targetLib.exists()) {
        println(s"Building native library for $osDirName...")
        targetDir.mkdirs()

        val cargoPath =
          try
            if (osName.contains("win")) "where cargo".!!.trim
            else "which cargo".!!.trim
          catch {
            case _: Exception => "cargo"
          }

        val buildResult = Process(Seq(cargoPath, "build", "--release"), rustProjectDir).!
        if (buildResult != 0) {
          sys.error("Failed to build native library with cargo")
        }

        if (sourceLib.exists()) {
          IO.copyFile(sourceLib, targetLib)
          if (!osName.contains("win")) {
            s"chmod +x ${targetLib.absolutePath}".!
          }
          println(s"Native library built: ${targetLib.absolutePath}")
        } else {
          sys.error(s"Native library not found at: ${sourceLib.absolutePath}")
        }
      } else {
        println(s"Native library already exists: ${targetLib.absolutePath}")
      }
    },
    // Ed25519-Zebra native library build task
    buildEd25519ZebraLib := {
      val baseDir = (ThisBuild / baseDirectory).value
      val rustProjectDir = baseDir / "modules" / "crypto" / "native" / "ed25519-zebra-wrapper"
      val targetDir = baseDir / "modules" / "crypto" / "native" / "build" / osDirName
      val libName = s"libed25519_zebra_wrapper.$libSuffix"

      val targetLib = targetDir / libName
      val sourceLib = rustProjectDir / "target" / "release" / libName

      if (!targetLib.exists()) {
        println(s"Building Ed25519-Zebra native library for $osDirName...")
        targetDir.mkdirs()

        val cargoPath =
          try
            if (osName.contains("win")) "where cargo".!!.trim
            else "which cargo".!!.trim
          catch {
            case _: Exception => "cargo"
          }

        val buildResult = Process(Seq(cargoPath, "build", "--release"), rustProjectDir).!
        if (buildResult != 0) {
          sys.error("Failed to build Ed25519-Zebra native library with cargo")
        }

        if (sourceLib.exists()) {
          IO.copyFile(sourceLib, targetLib)
          if (!osName.contains("win")) {
            s"chmod +x ${targetLib.absolutePath}".!
          }
          println(s"Ed25519-Zebra native library built: ${targetLib.absolutePath}")
        } else {
          sys.error(s"Ed25519-Zebra native library not found at: ${sourceLib.absolutePath}")
        }
      } else {
        println(s"Ed25519-Zebra native library already exists: ${targetLib.absolutePath}")
      }
    },
    // Run both native lib builds before compile
    Compile / compile := (Compile / compile).dependsOn(buildNativeLib, buildEd25519ZebraLib).value,
    Test / fork := true,
    Test / baseDirectory := (ThisBuild / baseDirectory).value,
    Test / javaOptions ++= Seq(
      s"-Djava.library.path=${(ThisBuild / baseDirectory).value}/modules/crypto/native/build/$osDirName:${(ThisBuild / baseDirectory).value}/modules/crypto/src/main/resources",
      s"-Djam.base.dir=${(ThisBuild / baseDirectory).value}"
    )
  )

lazy val pvm = (project in file("modules/pvm"))
  .dependsOn(core)
  .settings(
    name := "jam-pvm",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.typelevel" %% "spire" % "0.18.0",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
      "ch.qos.logback" % "logback-classic" % "1.4.11",
      "org.scalatest" %% "scalatest" % "3.2.17" % Test,
      "org.scalacheck" %% "scalacheck" % "1.17.0" % Test,
      "org.scalatestplus" %% "scalacheck-1-17" % "3.2.17.0" % Test,
      "io.circe" %% "circe-core" % "0.14.6" % Test,
      "io.circe" %% "circe-generic" % "0.14.6" % Test,
      "io.circe" %% "circe-parser" % "0.14.6" % Test
    ),
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-language:implicitConversions",
      "-language:higherKinds"
    )
  )

lazy val protocol = (project in file("modules/protocol"))
  .dependsOn(core, crypto, pvm)
  .settings(
    name := "jam-protocol",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.typelevel" %% "spire" % "0.18.0",
      "org.bouncycastle" % "bcprov-jdk18on" % "1.77",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
      "ch.qos.logback" % "logback-classic" % "1.4.11",
      "io.circe" %% "circe-core" % "0.14.6",
      "io.circe" %% "circe-generic" % "0.14.6",
      "io.circe" %% "circe-parser" % "0.14.6",
      "dev.optics" %% "monocle-core" % "3.2.0",
      "dev.optics" %% "monocle-macro" % "3.2.0",
      "org.scalatest" %% "scalatest" % "3.2.17" % Test
    ),
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-language:implicitConversions",
      "-language:higherKinds"
    ),
    Test / fork := true,
    Test / baseDirectory := (ThisBuild / baseDirectory).value,
    Test / javaOptions ++= Seq(
      s"-Djava.library.path=${(ThisBuild / baseDirectory).value}/modules/crypto/native/build/$osDirName:${(ThisBuild / baseDirectory).value}/modules/crypto/src/main/resources",
      s"-Djam.base.dir=${(ThisBuild / baseDirectory).value}"
    )
  )

lazy val conformance = (project in file("modules/conformance"))
  .dependsOn(core, crypto, protocol)
  .settings(
    name := "jam-conformance",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.typelevel" %% "spire" % "0.18.0",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
      "ch.qos.logback" % "logback-classic" % "1.4.11",
      // cats-effect for functional async IO
      "org.typelevel" %% "cats-effect" % "3.5.4",
      // fs2-io for Unix domain socket support
      "co.fs2" %% "fs2-io" % "3.10.2",
      // Monocle for lens-based state access (needed by protocol module at runtime)
      "dev.optics" %% "monocle-core" % "3.2.0",
      "dev.optics" %% "monocle-macro" % "3.2.0",
      "org.scalatest" %% "scalatest" % "3.2.17" % Test
    ),
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-language:implicitConversions",
      "-language:higherKinds"
    ),
    Test / fork := true,
    Test / baseDirectory := (ThisBuild / baseDirectory).value,
    Test / javaOptions ++= Seq(
      s"-Djava.library.path=${(ThisBuild / baseDirectory).value}/modules/crypto/native/build/$osDirName:${(ThisBuild / baseDirectory).value}/modules/crypto/src/main/resources",
      s"-Djam.base.dir=${(ThisBuild / baseDirectory).value}"
    ),
    // Assembly settings for creating fat JAR
    assembly / mainClass := Some("io.forge.jam.conformance.ConformanceServerApp"),
    assembly / assemblyJarName := "jam-conformance.jar",
    assembly / assemblyMergeStrategy := {
      // Discard signature files from signed JARs (e.g., Bouncy Castle)
      case x if x.endsWith(".SF") => MergeStrategy.discard
      case x if x.endsWith(".DSA") => MergeStrategy.discard
      case x if x.endsWith(".RSA") => MergeStrategy.discard
      case PathList("META-INF", "versions", _*) => MergeStrategy.first
      case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
      case PathList("META-INF", _*) => MergeStrategy.first
      case "module-info.class" => MergeStrategy.discard
      case x if x.endsWith(".class") => MergeStrategy.first
      case _ => MergeStrategy.first
    }
  )
