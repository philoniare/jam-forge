package io.forge.jam.node

import java.nio.file.{Files, Path, Paths}

import io.circe.Decoder
import io.circe.parser.decode as jsonDecode
import io.forge.jam.core.{ChainConfig, Hashing}
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.node.BeefyMessages.*
import io.forge.jam.protocol.state.{ServiceStorageView, TrieBackedJamState}
import io.forge.jam.protocol.traces.Genesis
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

class BeefySpec extends AnyFunSuite with Matchers:

  private val devKeys = (0 until 6).map(ValidatorKeySet.dev)
  private val peak = Hashing.blake2b256("peak".getBytes("UTF-8"))
  private val block = Hashing.blake2b256("block".getBytes("UTF-8"))

  test("BEEFY signs $jam_beefy ++ super-peak with the 144-byte BLS key") {
    payload(peak).toSeq shouldBe ("$jam_beefy".getBytes("US-ASCII") ++ peak.bytes).toSeq
    devKeys.foreach(_.blsPublic.length shouldBe 144)
    devKeys.map(_.blsPublic.toSeq).distinct.size shouldBe 6
    val c = BeefyMessages.sign(block, 7, 2, peak, devKeys(2))
    verify(c) shouldBe true
    verify(c.copy(superPeak = block)) shouldBe false
    verify(c.copy(blsKey = devKeys(3).blsPublic)) shouldBe false
  }

  test("signed commitments and proofs round-trip") {
    val c = BeefyMessages.sign(block, 7, 2, peak, devKeys(2))
    val bytes = BeefyMessages.encode(c)
    bytes.length shouldBe SignedCommitmentSize
    val d = decode(bytes).toOption.get
    (d.block, d.slot, d.validatorIndex, d.superPeak) shouldBe (block, 7L, 2, peak)
    d.signature.toSeq shouldBe c.signature.toSeq
    decode(bytes.dropRight(1)).isLeft shouldBe true

    val p = aggregate(6, (0 until 5).map(i => BeefyMessages.sign(block, 7, i, peak, devKeys(i)))).get
    val q = decodeProof(encodeProof(p)).toOption.get
    q.signerIndices shouldBe List(0, 1, 2, 3, 4)
    q.aggregate.toSeq shouldBe p.aggregate.toSeq
  }

  test("a proof needs a super-majority (5 of 6) of κ' BLS keys") {
    val keys = devKeys.map(_.blsPublic)
    val sigs = (0 until 6).map(i => BeefyMessages.sign(block, 7, i, peak, devKeys(i)))
    verifyProof(aggregate(6, sigs.take(5)).get, keys) shouldBe true
    verifyProof(aggregate(6, sigs).get, keys) shouldBe true
    verifyProof(aggregate(6, sigs.take(4)).get, keys) shouldBe false
    // Signer bits that do not match the aggregated signatures fail.
    val p = aggregate(6, sigs.take(5)).get
    verifyProof(p.copy(signers = Array(0x3e.toByte)), keys) shouldBe false
    verifyProof(p, keys.reverse) shouldBe false
  }

  private val baseDir = sys.props.get("jam.base.dir").map(Paths.get(_)).getOrElse(Paths.get("."))
  private val genesisPath = baseDir.resolve("jamtestvectors/traces/fuzzy/genesis.json")

  private def cleanup(dir: Path): Unit =
    Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(f => Files.deleteIfExists(f))

  test("six validators sign finalized blocks and aggregate a proof over the block's super-peak") {
    val genesis =
      if !Files.exists(genesisPath) then cancel("dev genesis not available")
      else
        given Decoder[Genesis] = Genesis.decoder
        jsonDecode[Genesis](Files.readString(genesisPath)).toOption.getOrElse(cancel("dev genesis unreadable"))
    val spec = ChainSpec(
      id = "beefy-devnet",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )
    val dir = Files.createTempDirectory("jam-beefy")
    var node: JamNode = null
    try
      node = new JamNode(spec, NodeConfig(dataDir = dir, slotTicking = false))
      node.chain.initializeOrRestore(spec)
      val author = new BlockAuthor(node.chain, devKeys)
      for slot <- 1 to 3 do
        val b = author.tryAuthor(slot).getOrElse(fail(s"author block $slot"))
        node.chain.importBlock(node.chain.encodeBlock(b)).isRight shouldBe true
      val head = node.chain.best
      val parent = node.chain.parentHash(head.hash).get

      // Commitments go through a queue, tampered on the way for validator 5.
      val queue = mutable.Queue.empty[SignedCommitment]
      val services = devKeys.map(k =>
        new BeefyService(node.chain, node.distribution, Some(k), outbound = Some(c => { queue.enqueue(c); () }))
      )
      node.chain.finalize(parent).isRight shouldBe true
      services.foreach(_.step())
      queue.size shouldBe 6 * 2 // blocks 1 and 2 by every validator
      // Validator 5 first, so its bad signatures reach the others before they hold five good ones.
      val sent = queue.dequeueAll(_ => true).sortBy(-_.validatorIndex)
      for c <- sent; (s, i) <- services.zipWithIndex if i != c.validatorIndex do
        s.onCommitment(if c.validatorIndex == 5 then c.copy(signature = sent.last.signature) else c)

      val trie = node.chain.stateAt(parent).get
      val state = new TrieBackedJamState(trie, spec.config, new ServiceStorageView(trie))
      val superPeak = state.beta.history.last.beefyRoot
      services.foreach { s =>
        val p = s.proof(parent).getOrElse(fail("no proof for the finalized block"))
        p.superPeak shouldBe superPeak
        verifyProof(p, devKeys.map(_.blsPublic)) shouldBe true
      }
      // Validator 5's tampered commitments were rejected, so 0-4 aggregate exactly 0-4.
      services.take(5).map(_.proof(parent).get.signerIndices).distinct shouldBe Seq(List(0, 1, 2, 3, 4))
      services.head.latestProof.map(_.block) shouldBe Some(parent)
      node.chain.meta("beefy:latest").flatMap(decodeProof(_).toOption).map(_.block) shouldBe Some(parent)

      // The head is not finalized yet, so nobody signed it.
      services.head.proof(head.hash) shouldBe None
      node.chain.finalize(head.hash).isRight shouldBe true
      services.foreach(_.step())
      queue.map(_.block).distinct shouldBe Seq(head.hash)
    finally
      if node != null then node.shutdownStorageOnly()
      cleanup(dir)
  }
