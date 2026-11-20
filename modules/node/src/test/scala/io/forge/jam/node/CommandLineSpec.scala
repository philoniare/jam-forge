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

  test("the data directory is --data, else JAM_DATA_DIR; --temp takes neither") {
    def dir(env: Map[String, String], args: String*) =
      Main.dataDirOf(parse(args*).fold(e => fail(e), identity), env.get)
    dir(Map.empty, "--data", "/d") shouldBe Some(java.nio.file.Paths.get("/d"))
    dir(Map("JAM_DATA_DIR" -> "/data"), "--data", "/d") shouldBe Some(java.nio.file.Paths.get("/d"))
    dir(Map("JAM_DATA_DIR" -> "/data")) shouldBe Some(java.nio.file.Paths.get("/data"))
    dir(Map("JAM_DATA_DIR" -> "")) shouldBe None
    dir(Map.empty) shouldBe None
    dir(Map("JAM_DATA_DIR" -> "/data"), "--temp") shouldBe None
    Main.Usage should include("JAM_DATA_DIR")
  }

  test("--inline-judgment-guarantee is a flag, and --help names it") {
    parse("--data", "/d", "--inline-judgment-guarantee").map(_.flags) shouldBe Right(Set("--inline-judgment-guarantee"))
    Main.Usage should include("[--inline-judgment-guarantee]")
  }

  test("--rpc-methods takes safe, unsafe or auto, and --help names it") {
    for value <- List("safe", "unsafe", "auto") do
      parse("--rpc-port", "9944", "--rpc-methods", value).map(_.options.get("rpc-methods")) shouldBe Right(Some(value))
    refused("--rpc-methods", "all") should include("--rpc-methods takes safe, unsafe or auto")
    refused("--rpc-methods") should include("--rpc-methods needs a value")
    Main.Usage should include("--rpc-methods safe|unsafe|auto")
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

  test("--version prints the node's name and the version the build gave it") {
    parse("--version").map(_.flags) shouldBe Right(Set("--version"))
    val built = sys.props.getOrElse("jam.build.version", cancel("-Djam.build.version not given (sbt passes it)"))
    BuildInfo.version shouldBe built
    Main.VersionText.linesIterator.next() should startWith(s"jam-forge $built")
  }

  test("--listen-ip and --external-ip take an IPv4 or IPv6 address, never a host name") {
    def ip(option: String, value: String) = Main.parseIp(option, value).fold(e => fail(e), identity)
    parse("--listen-ip", "0.0.0.0").isRight shouldBe true
    parse("--listen-ip", "::").isRight shouldBe true
    parse("--listen-ip", "[::1]").isRight shouldBe true
    parse("--external-ip", "203.0.113.7").isRight shouldBe true
    parse("--external-ip", "2001:db8::7").isRight shouldBe true
    ip("--listen-ip", "::").isAnyLocalAddress shouldBe true
    ip("--listen-ip", "[::1]") shouldBe java.net.InetAddress.getByAddress(Array.fill[Byte](15)(0) :+ 1.toByte)
    ip("--external-ip", "::ffff:10.0.0.1") shouldBe java.net.InetAddress.getByAddress(Array[Byte](10, 0, 0, 1))
    refused("--listen-ip", "localhost") should include("--listen-ip takes an IPv4 or IPv6 address")
    refused("--listen-ip", "256.1.1.1") should include("--listen-ip takes an IPv4 or IPv6 address")
    refused("--listen-ip", "8080") should include("--listen-ip takes an IPv4 or IPv6 address")
    refused("--listen-ip", "224.0.0.1") should include("multicast")
    refused("--external-ip", "2001:db8::zz") should include("--external-ip takes an IPv4 or IPv6 address")
    refused("--external-ip", "0.0.0.0") should include("--external-ip is the address peers reach this node at")
    refused("--external-ip", "::") should include("--external-ip is the address peers reach this node at")
  }

  test("--pvm-backend names the interpreter or the recompiler, as the execution mode property says it") {
    def properties(args: String*) = Main.systemProperties(parse(args*).fold(e => fail(e), identity))
    properties("--pvm-backend", "interpreter") shouldBe Map("jam.pvm.executionMode" -> "interpreted")
    properties("--pvm-backend", "recompiler") shouldBe Map("jam.pvm.executionMode" -> "recompiled")
    properties("--data", "/d") shouldBe Map.empty
    refused("--pvm-backend", "jit") should include("--pvm-backend takes interpreter or recompiler, not 'jit'")
  }

  test("--temp runs on a temporary data directory, so it cannot be given with --data") {
    parse("--temp").map(_.flags) shouldBe Right(Set("--temp"))
    refused("--temp", "--data", "/d") should include("--temp and --data are exclusive")
  }

  test("--metrics-port takes a port, and --metrics-listen-ip an address only with it") {
    parse("--metrics-port", "9615").map(_.options) shouldBe Right(Map("metrics-port" -> "9615"))
    parse("--metrics-port", "9615", "--metrics-listen-ip", "0.0.0.0").isRight shouldBe true
    refused("--metrics-port", "x") should include("--metrics-port takes a whole number")
    refused("--metrics-listen-ip", "0.0.0.0") should include("--metrics-listen-ip needs --metrics-port")
    refused("--metrics-port", "1", "--metrics-listen-ip", "example.org") should include(
      "--metrics-listen-ip takes an IPv4 or IPv6 address"
    )
  }

  test("gen-keys and list-keys are commands, each taking only its own options") {
    parse("gen-keys").map(_.command) shouldBe Right(Some("gen-keys"))
    parse("--data", "/data", "gen-keys").map(_.command) shouldBe Right(Some("gen-keys"))
    parse("--data", "/d").map(_.command) shouldBe Right(None)
    parse("list-keys", "--validator-seed", "/k").map(_.command) shouldBe Right(Some("list-keys"))
    refused("gen-keys", "--author", "1") should include("gen-keys takes no options but --data: drop --author")
    refused("gen-keys", "--grandpa") should include("gen-keys takes no options but --data: drop --grandpa")
    refused("list-keys") should include("list-keys needs one of --validator-seed, --seed, --seed-file or --author")
    refused("list-keys", "--seed", "ab" * 32, "--validator-seed", "/k") should include("list-keys takes one of")
    refused("list-keys", "--validator-seed", "/k", "--port", "1") should include("list-keys does not take --port")
    refused("gen-keys", "list-keys") should include("one command at a time")
    refused("stray") should include("unexpected argument: stray")
  }
