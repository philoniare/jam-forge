package io.forge.jam.node

import java.nio.file.Files
import java.nio.file.attribute.PosixFilePermissions

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class KeyFileExposureSpec extends AnyFunSuite with Matchers:

  test("a key file readable by its group or by everyone is reported, an owner-only one is not") {
    val file = Files.createTempFile("jam-key", ".hex")
    try
      def as(perms: String) =
        try Files.setPosixFilePermissions(file, PosixFilePermissions.fromString(perms))
        catch case _: UnsupportedOperationException => cancel("no POSIX permissions on this file system")
        Main.exposedTo(file)
      as("rw-------") shouldBe None
      as("r--------") shouldBe None
      as("rw-r-----") shouldBe Some("its group")
      as("rw----r--") shouldBe Some("everyone")
      as("rw-r--r--") shouldBe Some("its group and everyone")
      as("rw--w--w-") shouldBe None // writable is another problem; the secret is not readable
    finally Files.deleteIfExists(file)
  }
