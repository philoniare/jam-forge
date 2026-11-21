package io.forge.jam.node

import java.nio.file.{Files, Paths}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class NodeImageSpec extends AnyFunSuite with Matchers:

  private def read(path: String) = Files.readString(Paths.get(path))

  /** The Dockerfile stage that runs jam-node. */
  private lazy val nodeStage: String =
    read("Dockerfile").split("(?m)^FROM ").find(_.contains("""-jar", "/app/jam-node.jar"""")).getOrElse(fail("no node stage"))

  /** What the image's entrypoint gives the node, after the jar. */
  private lazy val entrypointArgs: List[String] =
    val line = nodeStage.linesIterator.find(_.startsWith("ENTRYPOINT")).getOrElse(fail("no ENTRYPOINT"))
    "\"([^\"]*)\"".r.findAllMatchIn(line).map(_.group(1)).toList.dropWhile(_ != "/app/jam-node.jar").drop(1)

  private lazy val imageEnv: Map[String, String] =
    nodeStage.linesIterator.filter(_.startsWith("ENV ")).flatMap { line =>
      "(\\w+)=(\\S+)".r.findAllMatchIn(line.drop(4)).map(m => m.group(1) -> m.group(2))
    }.toMap

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
    entrypointArgs shouldBe Nil
    val cl = parsed(serviceArgs)
    cl.command shouldBe Some("run")
    cl.global shouldBe Map("config-path" -> "/data", "chain" -> "/data/spec.json")
    cl.options.get("data-path") shouldBe Some("/data")
  }

  test("without -c and --data-path the image keeps a chain's data and keys in /data/jam-forge/<chain id>") {
    Main.defaultBase(config = false, imageEnv.get, "/home/none") shouldBe Paths.get("/data/jam-forge")
    Main.defaultBase(config = true, imageEnv.get, "/home/none") shouldBe Paths.get("/data/jam-forge")
    parsed(List("--chain", "toaster", "run", "--temp")).flags shouldBe Set("--temp")
  }
