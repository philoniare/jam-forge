package io.forge.jam.node

import java.net.InetSocketAddress
import java.nio.file.Files

import io.circe.parser.parse as parseJson
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ChainsSpec extends AnyFunSuite with Matchers with NodeFixture:

  private def resolved(chain: String, fetch: String => Either[String, String] = _ => Left("no network in tests")) =
    Chains.resolve(chain, fetch).fold(e => fail(e), identity)

  private def specOf(chain: String) = ChainSpec.fromJson(resolved(chain)).fold(e => fail(e), identity)

  private def state(json: String): Map[String, String] =
    parseJson(json).toOption.flatMap(_.hcursor.downField("genesis_state").as[Map[String, String]].toOption).get

  test("dev is the dev chain: its genesis, and validator i at 127.0.0.1:40000 + i") {
    val dev = specOf("dev")
    dev.id shouldBe "dev"
    dev.genesisHeaderHash.toHex.stripPrefix("0x") should startWith("245becfe7f091e38")
    dev.genesisValidatorEndpoints shouldBe (0 until 6).map(i => Some(new InetSocketAddress("127.0.0.1", 40000 + i)))
  }

  test("dev:<BASE_PORT> moves the validators to BASE_PORT + i and changes nothing else") {
    val moved = resolved("dev:41000")
    val spec = ChainSpec.fromJson(moved).fold(e => fail(e), identity)
    spec.genesisValidatorEndpoints shouldBe (0 until 6).map(i => Some(new InetSocketAddress("127.0.0.1", 41000 + i)))
    spec.genesisHeaderHash shouldBe specOf("dev").genesisHeaderHash
    val (before, after) = (state(resolved("dev")), state(moved))
    after.keySet shouldBe before.keySet
    before.keySet.filter(k => before(k) != after(k)).map(_.take(2)) shouldBe Set("04", "07", "08", "09")
    for k <- before.keySet do
      val differing = before(k).grouped(2).zip(after(k).grouped(2)).count(_ != _)
      differing should be <= 12
    state(resolved("dev:40000")) shouldBe before
    Chains.resolve("dev:0").left.toOption.get should include("not a port")
    Chains.resolve("dev:x").left.toOption.get should include("not a port")
  }

  test("polkadot is a placeholder: a chain without validators yet") {
    val polkadot = specOf("polkadot")
    polkadot.id shouldBe "polkadot"
    polkadot.genesisValidatorKeys._1 shouldBe empty
    Chains.Default shouldBe "polkadot"
  }

  test("toaster fetches the network's spec from jamtoaster.network, public by default") {
    val asked = scala.collection.mutable.ListBuffer.empty[String]
    def fetch(url: String) = { asked += url; Right("""{"id": "x"}""") }
    Chains.resolve("toaster", fetch) shouldBe Right("""{"id": "x"}""")
    Chains.resolve("toaster:stress-255", fetch).isRight shouldBe true
    asked.toList shouldBe List(
      "https://jamtoaster.network/chainspec/public.json",
      "https://jamtoaster.network/chainspec/stress-255.json"
    )
    Chains.resolve("toaster:nosuch", _ => Left("HTTP 404")).left.toOption.get should include("HTTP 404")
    Chains.resolve("toaster:../x", fetch).isLeft shouldBe true
  }

  test("any other value is the path of a spec file, and dump-spec writes one that reads back the same") {
    val dir = tempDir("jam-chains")
    try
      Chains.resolve(dir.resolve("none.json").toString).left.toOption.get should include("Error loading chain spec file")
      val file = dir.resolve("out/dev-41000.json")
      Chains.write(resolved("dev:41000"), file) shouldBe Right(())
      val back = specOf(file.toString)
      back.genesisHeaderHash shouldBe specOf("dev:41000").genesisHeaderHash
      back.genesisValidatorEndpoints shouldBe specOf("dev:41000").genesisValidatorEndpoints
      state(Files.readString(file)) shouldBe state(resolved("dev:41000"))
    finally cleanup(dir)
  }
