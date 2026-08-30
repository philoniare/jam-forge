package io.forge.jam.node

import java.nio.file.{Files, Path, Paths}

import io.circe.Decoder
import io.circe.parser.decode
import io.forge.jam.core.ChainConfig
import io.forge.jam.core.scodec.JamCodecs.encode
import io.forge.jam.protocol.traces.Genesis
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Fork choice: side branches are stored unvalidated; a strictly longer
  * branch triggers a reorg that rewinds to the common ancestor and replays
  * with full validation.
  *
  * Setup: node A and node B share block 1. A extends with its own block 2
  * (with tickets); B — authoring without a ticket pool — produces a
  * different block 2' and a block 3'. Feeding 2' to A parks it on a side
  * branch (equal height); 3' makes B's branch longer and A reorgs onto it.
  */
class ForkChoiceSpec extends AnyFunSuite with Matchers:

  private val baseDir =
    sys.props.get("jam.base.dir").map(Paths.get(_)).getOrElse(Paths.get("."))
  private val genesisPath =
    baseDir.resolve("jamtestvectors/traces/fuzzy/genesis.json")

  private def loadGenesis(): Option[Genesis] =
    if !Files.exists(genesisPath) then None
    else
      given Decoder[Genesis] = Genesis.decoder
      decode[Genesis](Files.readString(genesisPath)).toOption

  private def tempDir(p: String): Path = Files.createTempDirectory(p)
  private def cleanup(dir: Path): Unit =
    Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(f => Files.deleteIfExists(f))

  private val devKeys = (0 until 6).map(ValidatorKeySet.dev)

  test("a longer side branch triggers a validated reorg") {
    val genesis = loadGenesis().getOrElse(
      cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available")
    )
    val spec = ChainSpec(
      id = "fork-devnet",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )

    val dirA = tempDir("jam-fork-a")
    val dirB = tempDir("jam-fork-b")
    var nodeA: JamNode = null
    var nodeB: JamNode = null
    try
      nodeA = new JamNode(spec, NodeConfig(dataDir = dirA, slotTicking = false))
      nodeB = new JamNode(spec, NodeConfig(dataDir = dirB, slotTicking = false))
      nodeA.chain.initializeOrRestore(spec)
      nodeB.chain.initializeOrRestore(spec)

      // A authors with tickets; B authors bare (no ticket service) so the
      // branches diverge in content and therefore in hash.
      nodeA.enableAuthoring(devKeys)
      val bareAuthorB = new BlockAuthor(nodeB.chain, devKeys) // no shared pools/tickets

      // Shared block 1 (authored bare so both chains start identically).
      val bareAuthorShared = new BlockAuthor(nodeA.chain, devKeys)
      val block1 = bareAuthorShared.tryAuthor(1).getOrElse(fail("author block 1"))
      val block1Bytes = nodeA.chain.encodeBlock(block1)
      nodeA.chain.importBlock(block1Bytes).isRight shouldBe true
      nodeB.chain.importBlock(block1Bytes).isRight shouldBe true

      // A's own block 2 (with tickets → different from B's).
      nodeA.authorSlot(2).isDefined shouldBe true
      val headA2 = nodeA.chain.best

      // B's competing chain: 2' and 3'.
      val block2b = bareAuthorB.tryAuthor(2).getOrElse(fail("author 2'"))
      val block2bBytes = nodeB.chain.encodeBlock(block2b)
      nodeB.chain.importBlock(block2bBytes).isRight shouldBe true
      val block3b = bareAuthorB.tryAuthor(3).getOrElse(fail("author 3'"))
      val block3bBytes = nodeB.chain.encodeBlock(block3b)
      nodeB.chain.importBlock(block3bBytes).isRight shouldBe true

      nodeB.chain.best.hash should not be headA2.hash

      // Sanity: 2' imports linearly on a fresh node C (isolates replay bugs
      // from block validity).
      val dirC = tempDir("jam-fork-c")
      val nodeC = new JamNode(spec, NodeConfig(dataDir = dirC, slotTicking = false))
      nodeC.chain.initializeOrRestore(spec)
      nodeC.chain.importBlock(block1Bytes).isRight shouldBe true
      val cRes = nodeC.chain.importBlock(block2bBytes)
      withClue(s"fresh-node import of 2': $cRes") { cRes.isRight shouldBe true }
      nodeC.shutdownStorageOnly()
      cleanup(dirC)

      // Feed B's branch to A: 2' parks on a side branch (equal height)...
      val sideResult = nodeA.chain.importBlock(block2bBytes)
      sideResult.isLeft shouldBe true
      sideResult.left.toOption.get should include("side branch")
      nodeA.chain.best.hash shouldBe headA2.hash

      // ...and 3' makes the branch longer → reorg onto B's chain.
      val reorged = nodeA.chain.importBlock(block3bBytes)
      withClue(s"reorg result: $reorged") {
        reorged.isRight shouldBe true
      }
      nodeA.chain.best.hash shouldBe nodeB.chain.best.hash
      nodeA.chain.best.stateRoot shouldBe nodeB.chain.best.stateRoot
      nodeA.chain.best.slot shouldBe 3L

      // The abandoned block is still stored but no longer canonical; the new
      // chain keeps extending.
      nodeA.chain.hasBlock(headA2.hash) shouldBe true
      val block4 = new BlockAuthor(nodeA.chain, devKeys).tryAuthor(4).getOrElse(fail("author 4"))
      nodeA.chain.importBlock(nodeA.chain.encodeBlock(block4)).isRight shouldBe true
      nodeA.chain.best.slot shouldBe 4L
    finally
      if nodeA != null then nodeA.shutdownStorageOnly()
      if nodeB != null then nodeB.shutdownStorageOnly()
      cleanup(dirA)
      cleanup(dirB)
  }
