package io.forge.jam.node

import java.nio.file.Paths

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CommandLineSpec extends AnyFunSuite with Matchers:

  private def parse(args: String*) = Main.parseCommandLine(args.toList)
  private def parsed(args: String*) = parse(args*).fold(e => fail(e), identity)
  private def refused(args: String*): String = parse(args*).left.getOrElse(fail(s"accepted: ${args.mkString(" ")}"))

  test("a validator's command line under jamboree is read") {
    val cl = parsed(
      "--chain", "dev:41000", "run", "--temp", "--dev-validator", "3", "--pvm-backend", "interpreter",
      "--finality-mode", "grandpa", "--rpc", "--rpc-port", "19803", "--telemetry", "127.0.0.1:9000"
    )
    cl.global shouldBe Map("chain" -> "dev:41000")
    cl.command shouldBe Some("run")
    cl.options shouldBe Map(
      "dev-validator" -> "3", "pvm-backend" -> "interpreter", "finality-mode" -> "grandpa", "rpc-port" -> "19803",
      "telemetry" -> "127.0.0.1:9000"
    )
    cl.flags shouldBe Set("--temp", "--rpc")
  }

  test("short spellings, --name=value, and repeatable options") {
    val (a, b) = ("ehqvuedkevxxqaxxpofqczeyyq75qcsmklg35bfxwhz3wgspvdlba@195.154.91.169:5556", "ab" * 32 + "@127.0.0.1:40001")
    val cl = parsed(
      "-c", "/cfg", "--chain=toaster", "run", "-d", "/data", "--bootnode", a, s"--bootnode=$b",
      "--key-seed-file", "/k1", "--key-seed-file", "/k2", "--rpc-allow-origin", "http://localhost:3000"
    )
    cl.global shouldBe Map("config-path" -> "/cfg", "chain" -> "toaster")
    cl.options shouldBe Map("data-path" -> "/data")
    cl.repeated("bootnode") shouldBe List(a, b)
    cl.repeated("key-seed-file") shouldBe List("/k1", "/k2")
    cl.repeated("rpc-allow-origin") shouldBe List("http://localhost:3000")
    parsed("run", "-t").flags shouldBe Set("--temp")
  }

  test("global options come before the command, the command's after it") {
    refused("run", "--chain", "dev") should include("'--chain' comes before the command")
    refused("run", "-c", "/cfg") should include("unexpected argument '-c' found")
    refused("--temp", "run") should include("'--temp' is an option of run")
    refused("--dev-validator", "1", "run") should include("'--dev-validator' is an option of run")
  }

  test("an unknown option or command, a missing value, a repeated value and a stray argument are named and refused") {
    refused("run", "--auther", "3") should include("unexpected argument '--auther' found")
    refused("run", "-x") should include("unexpected argument '-x' found")
    refused("run", "--port") should include("a value is required for '--port'")
    refused("run", "--port", "--temp") should include("a value is required for '--port'")
    refused("run", "--bootnode") should include("a value is required for '--bootnode'")
    refused("run", "--port", "1", "--port", "2") should include("'--port' cannot be used multiple times")
    refused("run", "stray") should include("unexpected argument 'stray' found")
    refused("start") should include("unrecognized subcommand 'start'")
    refused("list-keys", "extra") should include("unexpected argument 'extra' found")
    refused("gen-keys", "a", "b") should include("unexpected argument 'b' found")
  }

  test("the old option names are gone") {
    for old <- List("--data", "--spec", "--author", "--validator-seed", "--seed", "--seed-file", "--peer", "--grandpa", "--print-keys") do
      refused("run", old, "x") should include(s"unexpected argument '$old")
    refused("--data", "/d") should include("unexpected argument '--data' found")
  }

  test("proxy mode is not implemented: --mode proxy, --proxy and --no-proxy are refused") {
    refused("run", "--mode", "proxy") should include("[possible values: ordinary, validator]")
    refused("run", "--proxy", "e@h:1") should include("unexpected argument '--proxy' found")
    refused("run", "--no-proxy") should include("unexpected argument '--no-proxy' found")
  }

  test("values are checked: modes, backends, finality, numbers, peer IDs, bootnodes, telemetry") {
    for mode <- List("ordinary", "validator") do parse("run", "--mode", mode).isRight shouldBe true
    for backend <- List("interpreter", "compiler") do parse("run", "--pvm-backend", backend).isRight shouldBe true
    refused("run", "--pvm-backend", "recompiler") should include("[possible values: interpreter, compiler]")
    refused("run", "--finality-mode", "babe") should include("[possible values: dummy, grandpa]")
    refused("run", "--port", "abc") should include("invalid value 'abc' for '--port'")
    refused("run", "--port", "70000") should include("invalid value '70000' for '--port'")
    refused("run", "--dev-validator", "-1") should include("--dev-validator")
    refused("run", "--peer-id", "xyz") should include("invalid value 'xyz' for '--peer-id <PEER_ID>'")
    parse("run", "--peer-id", "escwcf65sw3wjyfdjy6xsngu6bg5qzdnincbu4ddzbdwara66tmib").isRight shouldBe true
    refused("run", "--bootnode", "abc") should include("invalid value 'abc' for '--bootnode <BOOTNODE>'")
    parse("run", "--bootnode", "ehqvuedkevxxqaxxpofqczeyyq75qcsmklg35bfxwhz3wgspvdlba@195.154.91.169:5556").isRight shouldBe true
    refused("run", "--telemetry", "nohost") should include("--telemetry takes <HOST:PORT>")
    refused("run", "--rpc-methods", "all") should include("[possible values: safe, unsafe, auto]")
  }

  test("options that cannot be used together are refused") {
    refused("run", "--temp", "--data-path", "/d") should include("'--temp' cannot be used with '--data-path <DATA_PATH>'")
    refused("run", "--rpc", "--no-rpc") should include("'--rpc' cannot be used with '--no-rpc'")
    refused("run", "--finality-mode", "grandpa", "--finality-peers", "1") should include("--finality-mode cannot be used")
    refused("run", "--finality-mode", "dummy", "--announced-finality") should include("--finality-mode cannot be used")
    refused("run", "--mode", "ordinary", "--dev-validator", "1") should include("--mode ordinary runs no validator")
    refused("run", "--metrics-listen-ip", "0.0.0.0") should include("--metrics-listen-ip needs --metrics-port")
  }

  test("finality is the dummy rule unless --finality-mode grandpa, or peers' when asked for") {
    def finality(args: String*) = Main.finalityOf(parsed(("run" +: args)*))
    finality() shouldBe Main.Finality.Dummy
    finality("--finality-mode", "dummy") shouldBe Main.Finality.Dummy
    finality("--finality-mode", "grandpa") shouldBe Main.Finality.Grandpa
    finality("--finality-peers", "1") shouldBe Main.Finality.Peers
    finality("--announced-finality") shouldBe Main.Finality.Peers
  }

  test("the RPC listens where the node does, unless --rpc-listen-ip names another address") {
    val any = java.net.InetAddress.getByName("0.0.0.0")
    val one = java.net.InetAddress.getByName("10.0.0.5")
    Main.rpcListenHost(Map.empty, any) shouldBe "0.0.0.0"
    Main.rpcListenHost(Map("listen-ip" -> "10.0.0.5"), one) shouldBe "10.0.0.5"
    Main.rpcListenHost(Map("listen-ip" -> "10.0.0.5", "rpc-listen-ip" -> "127.0.0.1"), one) shouldBe "127.0.0.1"
    Main.rpcListenHost(Map("rpc-listen-ip" -> "::"), one) shouldBe "::"
  }

  test("--listen-ip, --rpc-listen-ip and --external-ip take an IPv4 or IPv6 address, never a host name") {
    def ip(option: String, value: String) = Main.parseIp(option, value).fold(e => fail(e), identity)
    parse("run", "--listen-ip", "0.0.0.0").isRight shouldBe true
    parse("run", "--listen-ip", "::").isRight shouldBe true
    parse("run", "--rpc-listen-ip", "[::1]").isRight shouldBe true
    parse("run", "--external-ip", "203.0.113.7").isRight shouldBe true
    ip("--listen-ip", "::").isAnyLocalAddress shouldBe true
    ip("--external-ip", "::ffff:10.0.0.1") shouldBe java.net.InetAddress.getByAddress(Array[Byte](10, 0, 0, 1))
    refused("run", "--listen-ip", "localhost") should include("--listen-ip takes an IPv4 or IPv6 address")
    refused("run", "--rpc-listen-ip", "256.1.1.1") should include("--rpc-listen-ip takes an IPv4 or IPv6 address")
    refused("run", "--listen-ip", "224.0.0.1") should include("multicast")
    refused("run", "--external-ip", "0.0.0.0") should include("--external-ip is the address peers reach this node at")
  }

  test("--pvm-backend's compiler is the recompiler, as the execution mode property says it") {
    def properties(args: String*) = Main.systemProperties(parsed(("run" +: args)*))
    properties("--pvm-backend", "interpreter") shouldBe Map("jam.pvm.executionMode" -> "interpreted")
    properties("--pvm-backend", "compiler") shouldBe Map("jam.pvm.executionMode" -> "recompiled")
    properties() shouldBe Map.empty
  }

  test("the other run options are read") {
    val cl = parsed(
      "run", "--sync-from", "0x" + "ab" * 32, "--prune-history", "--metrics-port", "9615", "--metrics-listen-ip", "0.0.0.0",
      "--rpc-methods", "safe", "--finality-peers", "1", "--beefy", "--submit-port", "8080", "--reuse-port",
      "--era-start", "1700000000", "--inline-judgment-guarantee", "--fault-tamper-reports",
      "--bandersnatch-key", "/k", "--ed25519-key", "/e"
    )
    cl.flags shouldBe Set("--prune-history", "--beefy", "--reuse-port", "--inline-judgment-guarantee", "--fault-tamper-reports")
    cl.options.keySet shouldBe Set(
      "sync-from", "metrics-port", "metrics-listen-ip", "rpc-methods", "finality-peers", "submit-port", "era-start",
      "bandersnatch-key", "ed25519-key"
    )
  }

  test("commands, help and version") {
    parsed("gen-keys").command shouldBe Some("gen-keys")
    parsed("gen-keys", "mykey").arguments shouldBe List("mykey")
    parsed("--chain", "dev", "list-keys").command shouldBe Some("list-keys")
    parsed("--chain", "dev", "dump-spec", "/tmp/dev.json").arguments shouldBe List("/tmp/dev.json")
    refused("dump-spec") should include("<SPEC_FILE>")
    parsed("help", "run").arguments shouldBe List("run")
    refused("help", "start") should include("unrecognized subcommand 'start'")
    parsed("run", "--help").flags shouldBe Set("--help")
    parsed("-h").flags shouldBe Set("--help")
    parsed("--help", "run").command shouldBe None
    parsed("-V").flags shouldBe Set("-V")
    parsed().command shouldBe None
    Main.Usage should include("Usage: jam-node [OPTIONS] <COMMAND>")
  }

  test("--version prints the node's name and the version the build gave it, then the Gray Paper's") {
    val built = sys.props.getOrElse("jam.build.version", cancel("-Djam.build.version not given (sbt passes it)"))
    BuildInfo.version shouldBe built
    Main.VersionText.linesIterator.toList.head should startWith(s"jam-forge $built")
    Main.VersionText.linesIterator.toList.last shouldBe s"GP ${JamNode.GrayPaperVersion}"
    Main.ShortVersionText shouldBe s"jam-forge $built"
  }

  test("a chain's data and config default to the XDG directories, under jam-forge") {
    def base(config: Boolean, env: Map[String, String] = Map.empty) = Main.defaultBase(config, env.get, "/home/u")
    base(config = false) shouldBe Paths.get("/home/u/.local/share/jam-forge")
    base(config = true) shouldBe Paths.get("/home/u/.config/jam-forge")
    base(config = false, Map("XDG_DATA_HOME" -> "/data")) shouldBe Paths.get("/data/jam-forge")
    base(config = true, Map("XDG_CONFIG_HOME" -> "/data")) shouldBe Paths.get("/data/jam-forge")
    base(config = true, Map("XDG_CONFIG_HOME" -> "")) shouldBe Paths.get("/home/u/.config/jam-forge")
  }
