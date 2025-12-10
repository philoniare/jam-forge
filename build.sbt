ThisBuild / scalaVersion := "3.3.3"
ThisBuild / organization := "io.forge.jam"
ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val root = (project in file("."))
    .aggregate(core, pvm)
    .settings(
        name := "jam",
        publish / skip := true
    )

lazy val core = (project in file("modules/core"))
    .settings(
        name := "jam-core",
        libraryDependencies ++= Seq(
            "org.typelevel" %% "spire" % "0.18.0",
            "org.scalatest" %% "scalatest" % "3.2.17" % Test
        ),
        scalacOptions ++= Seq(
            "-deprecation",
            "-feature",
            "-unchecked",
            "-language:implicitConversions"
        )
    )

lazy val pvm = (project in file("modules/pvm"))
    .dependsOn(core)
    .settings(
        name := "jam-pvm",
        libraryDependencies ++= Seq(
            "org.typelevel" %% "spire" % "0.18.0",
            "org.scalatest" %% "scalatest" % "3.2.17" % Test,
            "org.scalacheck" %% "scalacheck" % "1.17.0" % Test,
            "org.scalatestplus" %% "scalacheck-1-17" % "3.2.17.0" % Test
        ),
        scalacOptions ++= Seq(
            "-deprecation",
            "-feature",
            "-unchecked",
            "-language:implicitConversions",
            "-language:higherKinds"
        )
    )
