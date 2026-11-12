package io.forge.jam.node

import java.nio.file.{Files, Path, Paths}

import io.circe.Decoder
import io.circe.parser.decode
import io.forge.jam.core.ChainConfig
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.protocol.traces.Genesis
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Block authoring on the dev genesis. A node holding all six tiny validators authoring through the full import
  * pipeline, with a keyless peer following over JAMNP, is covered by `LiveDevnetSpec` and the `Devnet*Spec`s; ticket
  * generation and ticket sealing by `RestartAuthoringSpec`.
  */
class AuthoringSpec extends AnyFunSuite with Matchers:

  private val baseDir =
    sys.props.get("jam.base.dir").map(Paths.get(_)).getOrElse(Paths.get("."))
  private val genesisPath =
    baseDir.resolve("jamtestvectors/traces/fuzzy/genesis.json")

  private def loadGenesis(): Option[Genesis] =
    if !Files.exists(genesisPath) then None
    else
      given Decoder[Genesis] = Genesis.decoder
      decode[Genesis](Files.readString(genesisPath)).toOption

  private def specFor(genesis: Genesis): ChainSpec =
    ChainSpec(
      id = "authoring-devnet",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )

  private def tempDir(prefix: String): Path = Files.createTempDirectory(prefix)
  private def cleanup(dir: Path): Unit =
    Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p))

  test("a node without the sealing key does not author") {
    val genesis = loadGenesis().getOrElse(
      cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available")
    )
    val dir = tempDir("jam-author-nokeys")
    var node: JamNode = null
    try
      node = new JamNode(specFor(genesis), NodeConfig(dataDir = dir, slotTicking = false))
      node.chain.initializeOrRestore(node.spec)
      // Keys from an index outside the tiny validator set.
      node.enableAuthoring(Seq(ValidatorKeySet.dev(17)))
      node.authorSlot(genesis.header.slot.toLong + 1) shouldBe None
    finally
      if node != null then node.shutdownStorageOnly()
      cleanup(dir)
  }
