package io.forge.jam.node


import io.forge.jam.core.ChainConfig
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.protocol.traces.Genesis
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Block authoring on the dev genesis. A node holding all six tiny validators authoring through the full import
  * pipeline, with a keyless peer following over JAMNP, is covered by `LiveDevnetSpec` and the `Devnet*Spec`s; ticket
  * generation and ticket sealing by `RestartAuthoringSpec`.
  */
class AuthoringSpec extends AnyFunSuite with Matchers with NodeFixture:

  private def specFor(genesis: Genesis): ChainSpec =
    ChainSpec(
      id = "authoring-devnet",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )

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

  test("GP eq:blacklistfilter: the first block of an epoch after a dispute carries the epoch mark with the offender nulled, and imports elsewhere") {
    val genesis = loadGenesis().getOrElse(
      cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available")
    )
    val spec = specFor(genesis)
    val keys = (0 until 6).map(ValidatorKeySet.dev)
    val dirs = List(tempDir("jam-author-offender-a"), tempDir("jam-author-offender-b"))
    val nodes = dirs.map(d => new JamNode(spec, NodeConfig(dataDir = d, slotTicking = false)))
    try
      nodes.foreach(_.chain.initializeOrRestore(spec))
      val List(a, b) = nodes: @unchecked
      val author = new BlockAuthor(a.chain, keys, a.pools)
      def authorAndImport(slot: Long): io.forge.jam.core.types.block.Block =
        val block = author.tryAuthor(slot).getOrElse(fail(s"slot $slot not authored"))
        val bytes = a.chain.encodeBlock(block)
        for n <- nodes do
          n.chain.importBlock(bytes) match
            case Left(err) => fail(s"slot $slot did not import: $err")
            case Right(_)  => ()
        block
      val target = io.forge.jam.core.Hashing.blake2b256("judged report".getBytes("UTF-8"))
      (0 until 6).foreach { i =>
        val valid = i != 5
        val context = if valid then io.forge.jam.core.constants.JAM_VALID_BYTES else io.forge.jam.core.constants.JAM_INVALID_BYTES
        a.pools.addJudgment(target, 0L, i, valid, io.forge.jam.crypto.Ed25519ZebraWrapper.sign(keys(i).ed25519Secret, context ++ target.bytes))
      }
      val disputed = authorAndImport(1)
      disputed.extrinsic.disputes.faults.map(_.key) shouldBe List(keys(5).ed25519Key)
      b.chain.stateView().psi.offenders shouldBe List(keys(5).ed25519Key)
      (2L to 11L).foreach(authorAndImport)
      val first = authorAndImport(12)
      val mark = first.header.epochMark.getOrElse(fail("no epoch mark"))
      mark.validators(5).ed25519.bytes.forall(_ == 0) shouldBe true
      mark.validators(5).bandersnatch.bytes.forall(_ == 0) shouldBe true
      b.chain.best.slot shouldBe 12L
    finally
      nodes.foreach(_.shutdownStorageOnly())
      dirs.foreach(cleanup)
  }
