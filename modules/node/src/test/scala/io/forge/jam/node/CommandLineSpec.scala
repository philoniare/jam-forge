package io.forge.jam.node

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CommandLineSpec extends AnyFunSuite with Matchers:

  private def parse(args: String*) = Main.parseCommandLine(args.toList)
  private def refused(args: String*): String = parse(args*).left.getOrElse(fail(s"accepted: ${args.mkString(" ")}"))

  test("a whole command line is read: values, repeated options and flags") {
    val cl = parse("--data", "/d", "--author", "3", "--peer", "a@h:1", "--peer", "b@h:2", "--grandpa", "--rpc-port", "9944")
      .fold(e => fail(e), identity)
    cl.options shouldBe Map("data" -> "/d", "author" -> "3", "rpc-port" -> "9944")
    cl.repeated("--peer") shouldBe List("a@h:1", "b@h:2")
    cl.repeated("--rpc-allow-origin") shouldBe Nil
    cl.flags shouldBe Set("--grandpa")
    parse("--help").map(_.flags) shouldBe Right(Set("--help"))
  }

  test("an unknown option, a missing value, a repeated value and a stray argument are named and refused") {
    refused("--data", "/d", "--auther", "3") should include("unknown option: --auther")
    refused("--data", "/d", "-x") should include("unknown option: -x")
    refused("--data", "/d", "--author") should include("--author needs a value")
    refused("--data", "--author", "3") should include("--data needs a value")
    refused("--data", "/d", "--peer") should include("--peer needs a value")
    refused("--data", "/d", "--port", "1", "--port", "2") should include("--port is given twice")
    refused("--data", "/d", "stray") should include("unexpected argument: stray")
  }

  test("numbers and the seed are checked") {
    refused("--port", "abc") should include("--port takes a whole number")
    refused("--port", "70000") should include("--port takes a whole number")
    refused("--author", "-1") should include("--author")
    refused("--seed", "1234") should include("--seed takes a 32-byte Ed25519 seed")
    refused("--seed", "zz" * 32) should include("--seed takes a 32-byte Ed25519 seed")
    parse("--seed", "0x" + "ab" * 32).isRight shouldBe true
  }

  test("an author's network identity is its own key: no other seed may be given with it") {
    refused("--author", "2", "--seed", "ab" * 32) should include("--author i runs as dev validator i")
    refused("--author", "2", "--seed-file", "/tmp/x") should include("--author i runs as dev validator i")
    refused("--seed", "ab" * 32, "--seed-file", "/tmp/x") should include("give one")
    refused("--author", "2", "--validator-seed", "/tmp/x") should include("exclusive")
    refused("--validator-seed", "/tmp/x", "--seed", "ab" * 32) should include("--validator-seed sets the network identity")
    parse("--author", "2", "--ed25519-key", "/tmp/k").isRight shouldBe true
  }
