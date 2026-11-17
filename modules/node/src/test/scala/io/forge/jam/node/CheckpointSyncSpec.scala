package io.forge.jam.node

import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.primitives.Hash
import io.forge.jam.network.{PeerStreamHandler, StreamKind}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CheckpointSyncSpec extends AnyFunSuite with Matchers with LoopbackFixture:

  private val first = JamBytes(new Array[Byte](31))
  private val last = JamBytes(Array.fill[Byte](31)(0xff.toByte))

  private def serving(node: JamNode): Byte => Option[PeerStreamHandler] =
    case StreamKind.BlockRequest => Some(node.sync.blockRequestHandler)
    case StreamKind.StateRequest => Some(node.sync.stateRequestHandler)
    case _                       => None

  private def pairs(node: JamNode, h: Hash) = node.chain.stateAt(h).get.range(first, last, Int.MaxValue)._2

  test("a node joins at a checkpoint, follows the chain from there, finalizes, prunes and restarts") {
    val genesis = loadGenesis().getOrElse(cancel("dev genesis not available"))
    val spec = ChainSpec(
      id = "checkpoint-sync",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )
    val dirA = tempDir("jam-checkpoint-a")
    val dirB = tempDir("jam-checkpoint-b")
    def node(dir: java.nio.file.Path, i: Int) =
      new JamNode(spec, NodeConfig(dataDir = dir, slotTicking = false, ed25519Seed = Some(devKeys(i).ed25519Secret)))
    val a = node(dirA, 0)
    var b = node(dirB, 1)
    try
      a.chain.initializeOrRestore(spec)
      b.chain.initializeOrRestore(spec)
      author(Seq(a), 1 to 40)
      val chainOfA = a.chain.blocksAscending(a.chain.blockStore.getHead(io.forge.jam.db.BlockStore.GenesisHead).get, 100)
      chainOfA.size shouldBe 40
      val blocks = chainOfA.map(bytes => a.chain.decodeBlock(bytes).toOption.get)
      val checkpoint = blocks(29) // slot 30
      val start = blocks(28)
      val (_, bToA) = join(a, b, serving)

      b.chain.atGenesis shouldBe true
      b.sync.hold()
      val job = new CheckpointSync(b.chain, b.sync, a.chain.headerHashOf(checkpoint), () => List(bToA))
      val head = job.attempt().fold(why => fail(why), identity)

      head.hash shouldBe a.chain.headerHashOf(start)
      head.slot shouldBe start.header.slot.toLong
      b.chain.best shouldBe head
      b.chain.finalized.hash shouldBe head.hash
      b.chain.atGenesis shouldBe false
      pairs(b, head.hash) shouldBe pairs(a, head.hash)
      b.chain.hasBlock(a.chain.headerHashOf(blocks(27))) shouldBe true // an ancestor: header only
      b.chain.stateAt(a.chain.headerHashOf(blocks(27))) shouldBe None
      b.chain.blocksDescending(head.hash, 10).size shouldBe 1 // older bodies are not kept, so not served
      val rpc = new RpcApi(b.chain, b.spec.genesisHeaderHash, () => 0, () => false, b.preimages.submit)
      def b64(h: Hash) = java.util.Base64.getEncoder.encodeToString(h.bytes.toArray)
      val status = rpc.handle(
        s"""{"jsonrpc":"2.0","id":1,"method":"workPackageStatus","params":["${b64(head.hash)}","${b64(Hash(Array.fill[Byte](32)(7)))}","${b64(a.chain.headerHashOf(blocks(26)))}"]}""",
        new RpcSession(_ => ())
      ).get
      status.hcursor.downField("error").get[Int]("code") shouldBe Right(1)

      chainOfA.drop(29).foreach(bytes => b.chain.importBlock(bytes).isRight shouldBe true)
      b.chain.best.hash shouldBe a.chain.best.hash
      b.chain.best.stateRoot shouldBe a.chain.best.stateRoot

      b.chain.importBlock(chainOfA(10)).isLeft shouldBe true

      b.chain.finalizeAtDepth(2).isDefined shouldBe true
      b.chain.finalized.slot shouldBe 38L

      b.chain.startAt(chainOfA(35), a.chain.best.stateRoot, Nil).isLeft shouldBe true
      val bestBefore = b.chain.best
      b.shutdown()
      b = node(dirB, 1)
      b.chain.initializeOrRestore(spec)
      b.chain.best shouldBe bestBefore
      b.chain.atGenesis shouldBe false
      author(Seq(a), 41 to 44)
      a.chain.blocksAscending(bestBefore.hash, 10).foreach(bytes => b.chain.importBlock(bytes).isRight shouldBe true)
      b.chain.best.stateRoot shouldBe a.chain.best.stateRoot
      pairs(b, b.chain.best.hash) shouldBe pairs(a, a.chain.best.hash)
    finally
      a.shutdown()
      b.shutdown()
      cleanup(dirA)
      cleanup(dirB)
  }

  test("after a checkpoint start GRANDPA starts with the checkpoint's voters and the given or an unknown set id, across restarts") {
    val genesis = loadGenesis().getOrElse(cancel("dev genesis not available"))
    val spec = ChainSpec(
      id = "checkpoint-grandpa",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )
    val dirs = (0 until 4).map(i => tempDir(s"jam-checkpoint-grandpa-$i"))
    def node(i: Int) =
      new JamNode(spec, NodeConfig(dataDir = dirs(i), slotTicking = false, ed25519Seed = Some(devKeys(i).ed25519Secret)))
    val a = node(0)
    var b = node(1)
    var c = node(2)
    var d = node(3)
    try
      Seq(a, b, c, d).foreach(_.chain.initializeOrRestore(spec))
      author(Seq(a), 1 to 20)
      val genesisHash = a.chain.blockStore.getHead(io.forge.jam.db.BlockStore.GenesisHead).get
      val blocks = a.chain.blocksAscending(genesisHash, 100).map(bytes => a.chain.decodeBlock(bytes).toOption.get)
      val checkpoint = a.chain.headerHashOf(blocks(16)) // slot 17
      val epochStart = blocks(11) // slot 12: begins epoch 1, which the start block (slot 16) is in
      epochStart.header.epochMark.isDefined shouldBe true
      def expectRebased(g: GrandpaService, node: JamNode): Unit =
        g.voterSet.base.hash shouldBe a.chain.headerHashOf(epochStart)
        g.voterSet.members shouldBe node.chain.stateView().validators.current.map(v => io.forge.jam.core.primitives.Ed25519PublicKey(v.ed25519.bytes)).toSet

      val (_, bToA) = join(a, b, serving)
      b.sync.hold()
      new CheckpointSync(b.chain, b.sync, checkpoint, () => List(bToA)).attempt().isRight shouldBe true
      val gb = b.enableGrandpa(Some(devKeys(1)))
      gb.voterSet.setId shouldBe GrandpaVoter.UnknownSet
      expectRebased(gb, b)

      val gc = c.enableGrandpa(Some(devKeys(2)))
      gc.voterSet.setId shouldBe 0L
      val (_, cToA) = join(a, c, serving)
      c.sync.hold()
      new CheckpointSync(c.chain, c.sync, checkpoint, () => List(cToA)).attempt().isRight shouldBe true
      gc.restartAtFinalized()
      eventually("c's voter to be re-based")(gc.voterSet.setId == GrandpaVoter.UnknownSet)
      expectRebased(gc, c)

      val (_, dToA) = join(a, d, serving)
      d.sync.hold()
      new CheckpointSync(d.chain, d.sync, checkpoint, () => List(dToA), grandpaSetId = Some(5L)).attempt().isRight shouldBe true
      d.chain.meta.grandpaCheckpoint shouldBe Some(5L)
      val gd = d.enableGrandpa(Some(devKeys(3)))
      gd.voterSet.setId shouldBe 5L
      expectRebased(gd, d)
      d.chain.meta.grandpaCheckpoint shouldBe None

      b.shutdown(); c.shutdown(); d.shutdown()
      b = node(1); c = node(2); d = node(3)
      Seq(b, c, d).foreach(_.chain.initializeOrRestore(spec))
      Seq((b, 1, GrandpaVoter.UnknownSet), (c, 2, GrandpaVoter.UnknownSet), (d, 3, 5L)).foreach { (n, i, setId) =>
        val g = n.enableGrandpa(Some(devKeys(i)))
        g.voterSet.setId shouldBe setId
        expectRebased(g, n)
      }
    finally
      Seq(a, b, c, d).foreach(_.shutdown())
      dirs.foreach(cleanup)
  }

  test("--sync-from takes a header hash with an optional GRANDPA set id and refuses anything else") {
    val hex = "ab" * 32
    val hash = Hash(JamBytes.fromHexUnsafe(hex).toArray)
    Main.parseSyncFrom(hex) shouldBe Right((hash, None))
    Main.parseSyncFrom(s"0x$hex:12") shouldBe Right((hash, Some(12L)))
    Main.parseSyncFrom(s"$hex:0") shouldBe Right((hash, Some(0L)))
    Seq(s"$hex:", s"$hex:-1", s"$hex:x", s"$hex:1:2", "ab" * 31, "zz" * 32 + ":1").foreach { bad =>
      Main.parseSyncFrom(bad).isLeft shouldBe true
    }
  }

  test("a checkpoint no peer has leaves the node at genesis, and the first block as checkpoint means genesis") {
    withNodes(2, "checkpoint-missing") { nodes =>
      val Seq(a, b) = nodes.toSeq
      author(Seq(a), 1 to 5)
      val (_, bToA) = join(a, b, serving)
      val unknown = Hash(Array.fill[Byte](32)(0x42))
      new CheckpointSync(b.chain, b.sync, unknown, () => List(bToA)).attempt().isLeft shouldBe true
      new CheckpointSync(b.chain, b.sync, unknown, () => Nil).attempt().isLeft shouldBe true
      b.chain.atGenesis shouldBe true

      val genesisHash = a.chain.blockStore.getHead(io.forge.jam.db.BlockStore.GenesisHead).get
      val firstBlock = a.chain.decodeBlock(a.chain.blocksAscending(genesisHash, 1).head).toOption.get
      new CheckpointSync(b.chain, b.sync, a.chain.headerHashOf(firstBlock), () => List(bToA)).attempt().isRight shouldBe true
      b.chain.atGenesis shouldBe true
    }
  }

  test("a start block served with a body that is not its own fails the attempt; an honest peer's then succeeds") {
    withNodes(2, "checkpoint-body") { nodes =>
      val Seq(a, b) = nodes.toSeq
      author(Seq(a), 1 to 10)
      val genesisHash = a.chain.blockStore.getHead(io.forge.jam.db.BlockStore.GenesisHead).get
      val chainOfA = a.chain.blocksAscending(genesisHash, 100)
      val blocks = chainOfA.map(bytes => a.chain.decodeBlock(bytes).toOption.get)
      val checkpoint = blocks(5) // slot 6
      val start = blocks(4)
      val checkpointHash = a.chain.headerHashOf(checkpoint)
      val startBytes = chainOfA(4)
      val extra = io.forge.jam.core.types.extrinsic.Preimage(
        io.forge.jam.core.primitives.ServiceId(0),
        JamBytes(Array[Byte](1, 2, 3))
      )
      val alteredStart =
        a.chain.encodeBlock(start.copy(extrinsic = start.extrinsic.copy(preimages = List(extra))))
      val doctored: PeerStreamHandler = (_, stream) =>
        stream.onMessage { msg =>
          val from = Hash(msg.take(32))
          val answer = a.chain.blocksDescending(from, 2).map(bs =>
            if java.util.Arrays.equals(bs, startBytes) then alteredStart else bs
          )
          stream.send(answer.toArray.flatten)
          stream.finish()
        }
      val (_, bToLiar) = join(a, b, _ => kind => Option.when(kind == StreamKind.BlockRequest)(doctored))
      var charged = List.empty[Offence]
      b.sync.hold()
      val failed = new CheckpointSync(b.chain, b.sync, checkpointHash, () => List(bToLiar), (_, o) => charged ::= o).attempt()
      failed.isLeft shouldBe true
      charged shouldBe List(Offence.InvalidBlock)
      b.chain.atGenesis shouldBe true
      b.chain.hasBlock(a.chain.headerHashOf(start)) shouldBe false

      b.chain.startAt(alteredStart, checkpoint.header.parentStateRoot, Nil).left.map(_.contains(ChainError.BodyMismatch.message)) shouldBe Left(true)
      b.chain.atGenesis shouldBe true

      val (_, bToA) = join(a, b, serving)
      val head = new CheckpointSync(b.chain, b.sync, checkpointHash, () => List(bToA)).attempt().fold(why => fail(why), identity)
      head.hash shouldBe a.chain.headerHashOf(start)
      b.chain.blocksDescending(head.hash, 1).map(_.toSeq) shouldBe List(startBytes.toSeq)
      chainOfA.drop(5).foreach(bytes => b.chain.importBlock(bytes).isRight shouldBe true)
      b.chain.best.stateRoot shouldBe a.chain.best.stateRoot
    }
  }

  private def prunedServing(keptFrom: Long): JamNode => Byte => Option[PeerStreamHandler] = node =>
    case StreamKind.BlockRequest =>
      Some((_, stream) =>
        stream.onMessage { msg =>
          val (from, _, max) = SyncCodec.decodeBlockRequest(msg).toOption.get
          val kept = node.chain.blocksDescending(from, max.toInt)
            .takeWhile(bs => node.chain.decodeBlock(bs).toOption.get.header.slot.toLong >= keptFrom)
          if kept.isEmpty then stream.reset()
          else
            stream.send(kept.toArray.flatten)
            stream.finish()
        }
      )
    case StreamKind.StateRequest => Some(node.sync.stateRequestHandler)
    case _                       => None

  test("an ancestor set one peer answers short is completed from another; with only short peers the attempt fails") {
    withNodes(2, "checkpoint-ancestors") { nodes =>
      val Seq(a, b) = nodes.toSeq
      author(Seq(a), 1 to 40)
      val genesisHash = a.chain.blockStore.getHead(io.forge.jam.db.BlockStore.GenesisHead).get
      val blocks = a.chain.blocksAscending(genesisHash, 100).map(bytes => a.chain.decodeBlock(bytes).toOption.get)
      val checkpointHash = a.chain.headerHashOf(blocks(29)) // slot 30: starts at slot 29, ancestors back to slot 5
      var charged = List.empty[Offence]
      val (_, bToShort) = join(a, b, prunedServing(keptFrom = 20))
      b.sync.hold()

      val short = new CheckpointSync(b.chain, b.sync, checkpointHash, () => List(bToShort), (_, o) => charged ::= o).attempt()
      short.isLeft shouldBe true
      b.chain.atGenesis shouldBe true

      val (_, bToA) = join(a, b, serving)
      val head = new CheckpointSync(b.chain, b.sync, checkpointHash, () => List(bToShort, bToA), (_, o) => charged ::= o)
        .attempt().fold(why => fail(why), identity)
      head.hash shouldBe a.chain.headerHashOf(blocks(28))
      blocks.slice(4, 28).foreach(block => b.chain.hasBlock(a.chain.headerHashOf(block)) shouldBe true) // slots 5 to 28
      charged shouldBe Nil // a peer that pruned its history has done nothing wrong
    }
  }

  test("a peer answering a block request with more blocks than it was asked for is charged and not used") {
    withNodes(2, "checkpoint-overlong") { nodes =>
      val Seq(a, b) = nodes.toSeq
      author(Seq(a), 1 to 12)
      val genesisHash = a.chain.blockStore.getHead(io.forge.jam.db.BlockStore.GenesisHead).get
      val blocks = a.chain.blocksAscending(genesisHash, 100).map(bytes => a.chain.decodeBlock(bytes).toOption.get)
      val checkpointHash = a.chain.headerHashOf(blocks(9))
      val overlong: JamNode => Byte => Option[PeerStreamHandler] = node =>
        case StreamKind.BlockRequest =>
          Some((_, stream) =>
            stream.onMessage { msg =>
              val (from, _, max) = SyncCodec.decodeBlockRequest(msg).toOption.get
              stream.send(node.chain.blocksDescending(from, max.toInt + 10).toArray.flatten)
              stream.finish()
            }
          )
        case StreamKind.StateRequest => Some(node.sync.stateRequestHandler)
        case _                       => None
      val (_, bToOverlong) = join(a, b, overlong)
      b.sync.hold()
      new CheckpointSync(b.chain, b.sync, checkpointHash, () => List(bToOverlong)).attempt().isLeft shouldBe true
      b.scores.points(a.identity.publicKeyBytes) shouldBe (Offence.Malformed.points.toDouble +- 0.5)
      b.chain.atGenesis shouldBe true
    }
  }

  test("a state response of more than two messages is a failed, charged response") {
    withNodes(2, "checkpoint-state-extra") { nodes =>
      val Seq(a, b) = nodes.toSeq
      author(Seq(a), 1 to 6)
      val genesisHash = a.chain.blockStore.getHead(io.forge.jam.db.BlockStore.GenesisHead).get
      val blocks = a.chain.blocksAscending(genesisHash, 100).map(bytes => a.chain.decodeBlock(bytes).toOption.get)
      val checkpointHash = a.chain.headerHashOf(blocks(4))
      val chatty: JamNode => Byte => Option[PeerStreamHandler] = node =>
        case StreamKind.BlockRequest => Some(node.sync.blockRequestHandler)
        case StreamKind.StateRequest =>
          Some((_, stream) =>
            stream.onMessage { msg =>
              val (h, start, end, max) = SyncCodec.decodeStateRequest(msg).toOption.get
              val (boundary, pairs) = node.chain.stateAt(h).get.range(start, end, max.toInt)
              stream.send(SyncCodec.encodeBoundaryNodes(boundary))
              stream.send(SyncCodec.encodeStatePairs(pairs))
              stream.send(new Array[Byte](1000))
              stream.finish()
            }
          )
        case _ => None
      val (_, bToChatty) = join(a, b, chatty)
      var charged = List.empty[Offence]
      b.sync.hold()
      new CheckpointSync(b.chain, b.sync, checkpointHash, () => List(bToChatty), (_, o) => charged ::= o)
        .attempt().isLeft shouldBe true
      charged shouldBe List(Offence.Malformed)
      b.chain.atGenesis shouldBe true
    }
  }

  test("an attempt that throws is logged and retried, and the sync still ends") {
    var calls = 0
    var started = 0
    var running = true
    val sleeps = scala.collection.mutable.ListBuffer.empty[Long]
    CheckpointSync.untilStarted(
      () =>
        calls += 1
        if calls <= 2 then throw new IllegalStateException("store closed") else Right(ChainHead(Hash.zero, 0L, Hash.zero))
      ,
      running = () => running,
      started = () => { started += 1; running = false },
      retryMillis = 7L,
      sleep = ms => sleeps += ms
    )
    calls shouldBe 3
    started shouldBe 1
    sleeps.toList shouldBe List(7L, 7L)
  }

  test("a checkpoint start mid-epoch, holding fewer ancestors than back to the epoch's first block, follows the next set change") {
    val genesis = loadGenesis().getOrElse(cancel("dev genesis not available"))
    val rotated = genesis.state.keyvals.map { kv =>
      val k = kv.key.toArray
      if k.head == 7.toByte && k.drop(1).forall(_ == 0) then
        val v = kv.value.toArray.clone()
        (0 until 6).foreach(i => System.arraycopy(ValidatorKeySet.dev(6 + i).ed25519Public, 0, v, 1 + i * 336 + 32, 32))
        kv.copy(value = JamBytes(v))
      else kv
    }
    val spec = ChainSpec(
      id = "checkpoint-next-set",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = rotated,
      bootnodes = Nil
    )
    val dirs = (0 until 2).map(i => tempDir(s"jam-checkpoint-next-set-$i"))
    def node(i: Int) =
      new JamNode(spec, NodeConfig(dataDir = dirs(i), slotTicking = false, ed25519Seed = Some(devKeys(i).ed25519Secret)))
    val a = node(0)
    var b = node(1)
    try
      Seq(a, b).foreach(_.chain.initializeOrRestore(spec))
      author(Seq(a), 1 to 26)
      val genesisHash = a.chain.blockStore.getHead(io.forge.jam.db.BlockStore.GenesisHead).get
      val chainOfA = a.chain.blocksAscending(genesisHash, 100)
      val blocks = chainOfA.map(bytes => a.chain.decodeBlock(bytes).toOption.get)
      def keysAt(i: Int) = a.chain.stateViewAt(a.chain.headerHashOf(blocks(i))).get.validators.current.map(_.ed25519.bytes.toSeq)
      blocks(11).header.epochMark.isDefined shouldBe true // slot 12 begins epoch 1
      blocks(23).header.epochMark.isDefined shouldBe true // slot 24 begins epoch 2
      keysAt(23) should not be keysAt(17)

      val start = a.chain.headerHashOf(blocks(17))
      val root = a.chain.postStateRoot(start).get
      val (_, bToA) = join(a, b, serving)
      b.chain.downloadState(start, 18L, root, List(bToA), StateDownload.request(start)).isRight shouldBe true
      b.chain.startAt(chainOfA(17), root, headersOf(a.chain, chainOfA.slice(14, 17).reverse), grandpaSetId = Some(1L)).isRight shouldBe true
      b.chain.hasBlock(a.chain.headerHashOf(blocks(11))) shouldBe false
      val first = b.enableGrandpa(Some(devKeys(1)))
      first.voterSet.setId shouldBe 1L
      first.voterSet.keys.map(_.toSeq) shouldBe keysAt(17)

      b.shutdown()
      b = node(1)
      b.chain.initializeOrRestore(spec)
      val g = b.enableGrandpa(Some(devKeys(1)))
      g.voterSet.setId shouldBe 1L
      g.voterSet.base.hash shouldBe start

      chainOfA.slice(18, 26).foreach(bytes => b.chain.importBlock(bytes).isRight shouldBe true)
      b.chain.finalizeAdopting(a.chain.headerHashOf(blocks(24))).isRight shouldBe true
      eventually("the voter to adopt set 2")(g.voterSet.setId == 2L)
      g.voterSet.base.hash shouldBe a.chain.headerHashOf(blocks(23))
      g.voterSet.keys.map(_.toSeq) shouldBe keysAt(23)
    finally
      Seq(a, b).foreach(_.shutdown())
      dirs.foreach(cleanup)
  }

  private def guaranteeAnchoredAt(node: JamNode, slot: Long, lookup: io.forge.jam.core.types.block.Block): io.forge.jam.core.types.extrinsic.GuaranteeExtrinsic =
    import io.forge.jam.core.{Hashing, constants}
    import io.forge.jam.core.primitives.{CoreIndex, Ed25519Signature, Gas, ServiceId, Timeslot, ValidatorIndex}
    import io.forge.jam.core.types.context.Context
    import io.forge.jam.core.types.dispute.GuaranteeSignature
    import io.forge.jam.core.types.extrinsic.GuaranteeExtrinsic
    import io.forge.jam.core.types.work.{ExecutionResult, PackageSpec}
    import io.forge.jam.core.types.workpackage.WorkReport
    import io.forge.jam.core.types.workresult.{RefineLoad, WorkResult}
    import spire.math.{UInt, UShort}
    val view = node.chain.stateView()
    val history = view.beta.history
    val anchor = history(history.size - 2)
    val lookupHash = node.chain.headerHashOf(lookup)
    val codeHash = view.accumulation.serviceAccounts.find(_.id == 0).map(_.data.service.codeHash).get
    val report = WorkReport(
      packageSpec = PackageSpec(Hashing.blake2b256("anchored-package".getBytes), UInt(200), Hash.zero,
        UShort(view.config.validatorCount), Hash.zero, UShort(0)),
      context = Context(
        anchor = anchor.headerHash,
        anchorSlot = Timeslot(anchor.slot.toInt),
        stateRoot = anchor.stateRoot,
        beefyRoot = anchor.beefyRoot,
        lookupAnchor = lookupHash,
        lookupAnchorSlot = Timeslot(lookup.header.slot.toInt),
        lookupAnchorStateRoot = node.chain.postStateRoot(lookupHash).get,
        prerequisites = List.empty
      ),
      coreIndex = CoreIndex(0),
      authorizerHash = view.authPools.head.head,
      authGasUsed = Gas(3),
      authOutput = JamBytes.empty,
      segmentRootLookup = List.empty,
      results = List(WorkResult(
        serviceId = ServiceId(UInt(0)),
        codeHash = codeHash,
        payloadHash = Hashing.blake2b256("anchored-payload".getBytes),
        accumulateGas = Gas(node.spec.config.reportAccGas),
        result = ExecutionResult.Ok(JamBytes(Array[Byte](0x01, 0x0b) ++ new Array[Byte](17))),
        refineLoad = RefineLoad(Gas(1000L), UShort(0), UShort(1), UInt(100), UShort(0))
      ))
    )
    val message = constants.JAM_GUARANTEE_BYTES ++ Hashing.blake2b256(report.encode.toArray).bytes
    val signers = GuarantorService.assignments(view, slot, node.spec.config)._1.zipWithIndex.filter(_._1 == 0).map(_._2).take(3).toList
    val signatures = signers.map { vi =>
      GuaranteeSignature(ValidatorIndex(vi), Ed25519Signature(io.forge.jam.crypto.Ed25519ZebraWrapper.sign(devKeys(vi).ed25519Secret, message)))
    }.sortBy(_.validatorIndex.toInt)
    GuaranteeExtrinsic(report, Timeslot(UInt(slot.toInt)), signatures)

  test("a block anchored below the history a checkpoint start holds is not judged, and its sender is not charged") {
    withNodes(3, "checkpoint-floor") { nodes =>
      val Seq(a, b, c) = nodes.toSeq
      author(Seq(a), 1 to 20)
      val genesisHash = a.chain.blockStore.getHead(io.forge.jam.db.BlockStore.GenesisHead).get
      val chainOfA = a.chain.blocksAscending(genesisHash, 100)
      val blocks = chainOfA.map(bytes => a.chain.decodeBlock(bytes).toOption.get)
      val pools = new ExtrinsicPools
      pools.addGuarantee(guaranteeAnchoredAt(a, 21, blocks(4)))
      val anchored = new BlockAuthor(a.chain, devKeys, pools).tryAuthor(21).get
      anchored.extrinsic.guarantees.size shouldBe 1
      val anchoredBytes = a.chain.encodeBlock(anchored)
      val anchoredHash = a.chain.headerHashOf(anchored)
      a.chain.importBlock(anchoredBytes).isRight shouldBe true

      val (_, bToA) = join(a, b, serving)
      val start = a.chain.headerHashOf(blocks(18))
      val root = a.chain.postStateRoot(start).get
      b.chain.downloadState(start, 19L, root, List(bToA), StateDownload.request(start)).isRight shouldBe true
      b.chain.startAt(chainOfA(18), root, headersOf(a.chain, chainOfA.slice(9, 18).reverse)).isRight shouldBe true

      val (_, bToPruned) = join(a, b, prunedServing(keptFrom = 20))
      b.sync.fetchBlock(bToPruned, anchoredHash)
      eventually("the fetch to end")(!b.sync.fetchingBlock(anchoredHash))
      b.chain.best.hash shouldBe a.chain.headerHashOf(blocks(19))
      b.chain.meta.invalid(anchoredHash) shouldBe false
      b.scores.points(a.identity.publicKeyBytes) shouldBe 0.0
      b.chain.importBlock(anchoredBytes) match
        case Left(ChainError.CannotCheck(_)) => ()
        case other                           => fail(s"expected CannotCheck, got $other")
      b.chain.meta.invalid(anchoredHash) shouldBe false
      b.chain.meta.ancestryFloor shouldBe Some(10L)

      val (_, cToA) = join(a, c, serving)
      c.sync.hold()
      new CheckpointSync(c.chain, c.sync, a.chain.headerHashOf(blocks(19)), () => List(cToA)).attempt().isRight shouldBe true
      c.chain.meta.ancestryFloor shouldBe None
      c.chain.importBlock(chainOfA(19)).isRight shouldBe true
      c.chain.importBlock(anchoredBytes).isRight shouldBe true
    }
  }
