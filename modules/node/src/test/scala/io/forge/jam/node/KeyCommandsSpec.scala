package io.forge.jam.node

import java.nio.file.Files

import io.circe.parser.parse as parseJson
import io.forge.jam.core.JamBytes
import io.forge.jam.network.NodeIdentity
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class KeyCommandsSpec extends AnyFunSuite with Matchers with NodeFixture:

  private def json(text: String): Map[String, String] =
    parseJson(text).fold(e => fail(s"not JSON: $e\n$text"), identity).asObject
      .getOrElse(fail(s"not a JSON object: $text"))
      .toMap
      .map((k, v) => k -> v.asString.getOrElse(fail(s"$k is not a string: $v")))

  private def hex(bytes: Array[Byte]) = "0x" + JamBytes(bytes).toHex

  private def commandLine(args: String*) = Main.parseCommandLine(args.toList).fold(e => fail(e), identity)

  private def keysOf(keys: ValidatorKeySet): Map[String, String] = Map(
    "bandersnatch" -> hex(keys.bandersnatchPublic),
    "ed25519" -> hex(keys.ed25519Public),
    "bls" -> hex(keys.blsPublic),
    "peer_id" -> NodeIdentity.altName(keys.ed25519Public)
  )

  test("gen-keys prints a new random seed and the session keys it derives (JIP-5)") {
    val first = json(Main.genKeys())
    val second = json(Main.genKeys())
    first("seed") should not be second("seed")
    for printed <- List(first, second) do
      val seed = JamBytes.fromHexUnsafe(printed("seed").stripPrefix("0x")).toArray
      seed.length shouldBe 32
      printed - "seed" shouldBe keysOf(ValidatorKeySet.fromSeed(seed))
  }

  test("list-keys prints the keys each seed option gives the node, and creates no file") {
    val dir = tempDir("jam-list-keys")
    try
      val seed = Array.tabulate[Byte](32)(i => (i * 7 + 1).toByte)
      val seedFile = dir.resolve("validator.seed")
      Files.writeString(seedFile, JamBytes(seed).toHex + "\n")
      val validator = keysOf(ValidatorKeySet.fromSeed(seed))
      def listed(args: String*) = Main.listKeys(commandLine(("list-keys" +: args)*)).fold(e => fail(e), json)

      listed("--validator-seed", seedFile.toString) shouldBe validator
      listed("--validator-seed", JamBytes(seed).toHex) shouldBe validator
      listed("--author", "2") shouldBe keysOf(ValidatorKeySet.dev(2))
      val identity = NodeIdentity.fromSeed(seed).publicKeyBytes
      val network = Map("ed25519" -> hex(identity), "peer_id" -> NodeIdentity.altName(identity))
      listed("--seed", JamBytes(seed).toHex) shouldBe network
      listed("--seed-file", seedFile.toString) shouldBe network

      val missing = dir.resolve("none.seed")
      Main.listKeys(commandLine("list-keys", "--validator-seed", missing.toString)).left.toOption.get should include(
        "no such file"
      )
      Files.exists(missing) shouldBe false
      Files.writeString(dir.resolve("short.seed"), "abcd")
      Main.listKeys(commandLine("list-keys", "--seed-file", dir.resolve("short.seed").toString)).left.toOption.get should
        include("32-byte")
    finally cleanup(dir)
  }

  test("a temporary data directory is made new, and deleted with everything in it") {
    val dir = Main.newTempDataDir()
    val other = Main.newTempDataDir()
    try
      dir should not be other
      Files.isDirectory(dir) shouldBe true
      Files.list(dir).count() shouldBe 0L
      Files.createDirectories(dir.resolve("state/sub"))
      Files.writeString(dir.resolve("state/sub/file"), "x")
      Files.writeString(dir.resolve("spec.json"), "{}")
      Main.deleteTree(dir)
      Files.exists(dir) shouldBe false
      Main.deleteTree(dir) // gone already: nothing to do
    finally
      cleanup(dir)
      cleanup(other)
  }
