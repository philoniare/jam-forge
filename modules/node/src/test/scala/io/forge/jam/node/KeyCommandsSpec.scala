package io.forge.jam.node

import java.nio.file.{Files, Paths}
import java.time.LocalDateTime

import io.forge.jam.core.JamBytes
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class KeyCommandsSpec extends AnyFunSuite with Matchers with NodeFixture:

  private val knownSeed = JamBytes.fromHexUnsafe("a1af1d61c53e246a917830a94f46ddc55a753a633175baad212fb38a56d1931f").toArray
  private val knownKeys = List(
    "Peer ID: escwcf65sw3wjyfdjy6xsngu6bg5qzdnincbu4ddzbdwara66tmib",
    "Bandersnatch key: 97b6ce46667a97b3a414f4fbbb8be5174002e757188f30ca109e20a81676df94"
  )

  test("a known seed gives its known keys") {
    val dir = tempDir("jam-keystore")
    try
      val file = dir.resolve("2026-11-21-10-18-47.seed")
      Files.write(file, knownSeed)
      val seed = Keystore.read(file).fold(e => fail(e), identity)
      Keystore.describe(seed) shouldBe knownKeys
    finally cleanup(dir)
  }

  test("gen-keys writes a new raw seed, owner-only, in the keystore, and never replaces one") {
    val keys = tempDir("jam-keystore").resolve("dev/keys")
    try
      val now = LocalDateTime.of(2026, 11, 21, 10, 18, 47)
      Keystore.fileFor(keys, None, now) shouldBe keys.resolve("2026-11-21-10-18-47.seed")
      Keystore.fileFor(keys, Some("mykey"), now) shouldBe keys.resolve("mykey.seed")
      Keystore.fileFor(keys, Some("other.seed"), now) shouldBe keys.resolve("other.seed")
      Keystore.fileFor(keys, Some("/abs/v.seed"), now) shouldBe Paths.get("/abs/v.seed")

      val first = Keystore.create(keys.resolve("mykey.seed")).fold(e => fail(e), identity)
      Files.readAllBytes(first.path).toSeq shouldBe first.secret.toSeq
      first.secret.length shouldBe 32
      Main.exposedTo(first.path) shouldBe None
      Keystore.create(keys.resolve("mykey.seed")).left.toOption.get should include("not replacing a secret key")
      Files.readAllBytes(first.path).toSeq shouldBe first.secret.toSeq
      val second = Keystore.create(keys.resolve("other.seed")).fold(e => fail(e), identity)
      second.secret.toSeq should not be first.secret.toSeq
    finally cleanup(keys.getParent.getParent)
  }

  test("list-keys reads the *.seed files by name, and names a file that is not a seed") {
    val keys = tempDir("jam-keystore")
    try
      Files.write(keys.resolve("b.seed"), knownSeed)
      Files.write(keys.resolve("a.seed"), Array.fill[Byte](32)(7))
      Files.writeString(keys.resolve("notes.txt"), "not a seed")
      val (good, bad) = Keystore.load(keys, required = true)
      good.map(_.path.getFileName.toString) shouldBe List("a.seed", "b.seed")
      bad shouldBe Nil
      Keystore.describe(good(1)) shouldBe knownKeys

      Files.writeString(keys.resolve("hex.seed"), JamBytes(knownSeed).toHex.stripPrefix("0x") + "\n")
      val (_, errors) = Keystore.load(keys, required = true)
      errors shouldBe List(s"${keys.resolve("hex.seed")}: Seed file is the wrong size: expected 32 bytes, read 65 bytes")

      Keystore.load(keys.resolve("none"), required = true)._2 shouldBe List(s"${keys.resolve("none")}: No such file or directory")
      Keystore.load(keys.resolve("none"), required = false) shouldBe (Nil, Nil)
      Keystore.read(keys.resolve("missing.seed")).left.toOption.get should include("No such file or directory")
    finally cleanup(keys)
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
      Main.deleteTree(dir)
      Files.exists(dir) shouldBe false
      Main.deleteTree(dir) // gone already: nothing to do
    finally
      cleanup(dir)
      cleanup(other)
  }
