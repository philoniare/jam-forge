package io.forge.jam.node

import java.nio.file.{Files, Paths}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** The node image's entrypoint and the arguments given after it are read as one command line, so an option both give
  * is refused and the node does not start. The testnet deploy of 18 November 2026 failed that way: the entrypoint gave
  * `--data /data` and the service `--data /data/db`.
  */
class NodeImageSpec extends AnyFunSuite with Matchers:

  private def read(path: String) = Files.readString(Paths.get(path))

  /** The Dockerfile stage that runs jam-node. */
  private lazy val nodeStage: String =
    read("Dockerfile").split("(?m)^FROM ").find(_.contains("""-jar", "/app/jam-node.jar"""")).getOrElse(fail("no node stage"))

  /** What the image's entrypoint gives the node, after the jar. */
  private lazy val entrypointArgs: List[String] =
    val line = nodeStage.linesIterator.find(_.startsWith("ENTRYPOINT")).getOrElse(fail("no ENTRYPOINT"))
    "\"([^\"]*)\"".r.findAllMatchIn(line).map(_.group(1)).toList.dropWhile(_ != "/app/jam-node.jar").drop(1)

  /** The image's environment (`ENV NAME=value`). */
  private lazy val imageEnv: Map[String, String] =
    "(?m)^ENV\\s+(\\w+)=(\\S+)".r.findAllMatchIn(nodeStage).map(m => m.group(1) -> m.group(2)).toMap

  /** What jam-node.service gives the image, with JAM_ARGS as setup.sh sets it by default. */
  private lazy val serviceArgs: List[String] =
    val serviceGives = "(?s)\"\\$JAM_IMAGE\"\\s*\\\\\\s*(.*?)\\$JAM_ARGS".r
      .findFirstMatchIn(read("deploy/azure/jam-node.service")).getOrElse(fail("no ExecStart arguments")).group(1)
    val jamArgs = "JAM_ARGS=\"\\$\\{JAM_ARGS:-(.*?)\\}\"".r
      .findFirstMatchIn(read("deploy/azure/setup.sh")).getOrElse(fail("no JAM_ARGS default")).group(1)
    (serviceGives + " " + jamArgs).trim.split("\\s+").toList

  private def parsed(args: List[String]): Main.CommandLine =
    Main.parseCommandLine(args).fold(e => fail(s"${args.mkString(" ")}: $e"), identity)

  test("the testnet service's arguments after the image's entrypoint are a command line the node takes") {
    val cl = parsed(entrypointArgs ++ serviceArgs)
    Main.dataDirOf(cl, imageEnv.get) shouldBe Some(Paths.get("/data/db"))
  }

  test("the image keeps the node's data in /data when no --data is given, and takes --temp") {
    Main.dataDirOf(parsed(entrypointArgs ++ List("--bandersnatch-key", "0x" + "11" * 32)), imageEnv.get) shouldBe
      Some(Paths.get("/data"))
    Main.dataDirOf(parsed(entrypointArgs :+ "--temp"), imageEnv.get) shouldBe None
  }
