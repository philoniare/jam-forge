package io.forge.jam.node

import java.io.DataInputStream
import java.net.{InetAddress, InetSocketAddress, ServerSocket}

import io.forge.jam.core.LittleEndian
import io.forge.jam.core.primitives.Hash
import io.forge.jam.protocol.pvmhost.{AccumulateCost, ExecCost, IsAuthorizedCost, RefineCost}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TelemetryLayoutSpec extends AnyFunSuite with Matchers:

  private def u8(v: Int): Array[Byte] = Array(v.toByte)
  private def u16(v: Long): Array[Byte] = Array.tabulate(2)(i => ((v >>> (8 * i)) & 0xff).toByte)
  private def u32(v: Long): Array[Byte] = Array.tabulate(4)(i => ((v >>> (8 * i)) & 0xff).toByte)
  private def u64(v: Long): Array[Byte] = Array.tabulate(8)(i => ((v >>> (8 * i)) & 0xff).toByte)
  private def len(n: Int): Array[Byte] = { require(n < 128); Array(n.toByte) }
  private def reason(s: String): Array[Byte] = len(s.length) ++ s.getBytes("UTF-8")
  private def bytes(parts: Array[Byte]*): Array[Byte] = parts.foldLeft(Array.emptyByteArray)(_ ++ _)

  private val peer = Array.tabulate[Byte](32)(i => (i + 1).toByte)
  private val peer2 = Array.tabulate[Byte](32)(i => (100 + i).toByte)
  private def hash(b: Int): Hash = Hash(Array.fill[Byte](32)(b.toByte))
  private def h(b: Int): Array[Byte] = hash(b).bytes.toArray

  private def capture(count: Int, queueCapacity: Int = 4096)(send: Telemetry => Unit): List[(Long, Int, Array[Byte])] =
    val server = new ServerSocket(0, 1, InetAddress.getLoopbackAddress)
    server.setSoTimeout(10000)
    val t = new Telemetry(
      new InetSocketAddress(InetAddress.getLoopbackAddress, server.getLocalPort),
      () => Array[Byte](1),
      queueCapacity = queueCapacity
    )
    try
      t.start()
      val socket = server.accept()
      socket.setSoTimeout(10000)
      val in = new DataInputStream(socket.getInputStream)
      def message(): Array[Byte] =
        val size = new Array[Byte](4)
        in.readFully(size)
        val content = new Array[Byte](LittleEndian.get(size, 0, 4).toInt)
        in.readFully(content)
        content
      message() shouldBe Array[Byte](1)
      send(t)
      List.fill(count)(message()).map(m => (LittleEndian.get(m, 0, 8), m(8) & 0xff, m.drop(9)))
    finally
      t.shutdown()
      server.close()

  private def check(events: List[(Long, Int, Array[Byte])], expected: (Int, Array[Byte])*): Unit =
    events.map(_._2) shouldBe expected.map(_._1).toList
    events.zip(expected).zipWithIndex.foreach { case (((_, d, body), (_, want)), i) =>
      withClue(s"event $i (discriminator $d): ")(body.toSeq shouldBe want.toSeq)
    }

  test("47 block executed: each accumulated service with its accumulate cost, in the Accumulate Cost layout") {
    val cost = AccumulateCost(
      calls = 2,
      transfers = 3,
      items = 4,
      total = ExecCost(1000, 2000),
      compileNanos = 5,
      readWrite = ExecCost(6, 7),
      lookup = ExecCost(8, 9),
      preimages = ExecCost(10, 11),
      service = ExecCost(12, 13),
      transfer = ExecCost(14, 15),
      transferGas = 16,
      other = ExecCost(17, 18)
    )
    val wire = bytes(
      u32(2), u32(3), u32(4), u64(1000), u64(2000), u64(5), u64(6), u64(7), u64(8), u64(9), u64(10), u64(11),
      u64(12), u64(13), u64(14), u64(15), u64(16), u64(17), u64(18)
    )
    val events = capture(3) { t =>
      val authoring = t.authoring(5, hash(1)) // 0
      t.blockExecuted(authoring, Map(7L -> cost, 3L -> AccumulateCost.zero)) // 1
      t.blockExecuted(authoring, Map.empty) // 2
    }
    check(
      events,
      40 -> bytes(u32(5), h(1)),
      47 -> bytes(u64(0), len(2), u32(3), new Array[Byte](wire.length), u32(7), wire),
      47 -> bytes(u64(0), len(0))
    )
  }

  test("47 block executed: past 500 services the lowest gas users are combined under 0xffffffff, greater ids first") {
    def gas(g: Long) = AccumulateCost.zero.copy(calls = 1, total = ExecCost(g, 0))
    val lowest = Set(3L, 10L, 11L, 12L)
    val costs = (1L to 502L).map(id => id -> gas(if lowest(id) then 5 else 1000 + id)).toMap
    val events = capture(2) { t =>
      val importing = t.importing(1, Array.emptyByteArray) // 0
      t.blockExecuted(importing, costs) // 1
    }
    events.map(_._2) shouldBe List(43, 47)
    val body = events(1)._3
    LittleEndian.get(body, 0, 8) shouldBe 0L
    val (count, used) = io.forge.jam.core.codec.JamCodecs.decodeCompactInteger(body, 8)
    count shouldBe 500L
    val entrySize = 4 + 4 * 3 + 8 * 2 + 8 + 8 * 2 * 5 + 8 + 8 * 2
    body.length shouldBe 8 + used + 500 * entrySize
    val ids = (0 until 500).map(i => LittleEndian.get(body, 8 + used + i * entrySize, 4))
    ids.toSet should contain(3L)
    ids.toSet should contain noneOf (10L, 11L, 12L)
    ids.last shouldBe 0xffffffffL
    val combined = 8 + used + 499 * entrySize
    LittleEndian.get(body, combined + 4, 4) shouldBe 3L // calls
    LittleEndian.get(body, combined + 16, 8) shouldBe 15L // gas
  }

  test("0 dropped: a run of events the queue could not hold is replaced by one event counting them, and IDs go on") {
    val server = new ServerSocket(0, 1, InetAddress.getLoopbackAddress)
    server.setSoTimeout(20000)
    val t = new Telemetry(
      new InetSocketAddress(InetAddress.getLoopbackAddress, server.getLocalPort),
      () => Array[Byte](1),
      writeTimeoutMillis = 60000,
      queueCapacity = 8
    )
    try
      t.start()
      val socket = server.accept()
      socket.setSoTimeout(20000)
      socket.setReceiveBufferSize(4096)
      val in = new DataInputStream(socket.getInputStream)
      def message(): Array[Byte] =
        val size = new Array[Byte](4)
        in.readFully(size)
        val content = new Array[Byte](LittleEndian.get(size, 0, 4).toInt)
        in.readFully(content)
        content
      message() shouldBe Array[Byte](1)
      val total = 400
      (0 until total).foreach(seq => t.emit(99, u64(seq) ++ new Array[Byte](64 * 1024)))
      t.droppedEvents should be > 0L
      var nextId = 0L
      var droppedSeen = 0L
      while nextId < total do
        val m = message()
        m(8) & 0xff match
          case 0 =>
            m.length shouldBe 9 + 16
            val lastTimestamp = LittleEndian.get(m, 9, 8)
            lastTimestamp should be >= LittleEndian.get(m, 0, 8)
            val n = LittleEndian.get(m, 17, 8)
            n should be > 0L
            nextId += n
            droppedSeen += n
          case 99 =>
            LittleEndian.get(m, 9, 8) shouldBe nextId
            nextId += 1
          case other => fail(s"unexpected event $other")
      nextId shouldBe total.toLong
      droppedSeen shouldBe t.droppedEvents
      socket.close()
    finally
      t.shutdown()
      server.close()
  }

  test("60 to 68 block distribution: announcement streams, announcements and CE 128 requests on both sides") {
    val outline = Telemetry.blockOutline(100, hash(9), 1, 2, 3, 4, 5, 6)
    val events = capture(12) { t =>
      val stream = t.blockAnnouncementStreamOpened(peer, remote = true) // 0
      t.blockAnnouncementStreamClosed(stream, remote = false, "superseded") // 1
      t.blockAnnounced(peer, remote = true, 7, hash(2)) // 2
      val sending = t.sendingBlockRequest(peer, hash(3), ascending = false, 64) // 3
      t.blockRequestSent(sending) // 4
      t.blockTransferred(sending, 7, outline, last = true) // 5
      t.blockRequestFailed(sending, "no answer in time") // 6
      val receiving = t.receivingBlockRequest(peer2) // 7
      t.blockRequestReceived(receiving, hash(4), ascending = true, 9) // 8
      t.blockTransferred(receiving, 8, outline, last = false) // 9
      t.blockRequestFailed(receiving, "x") // 10
      t.blockAnnounced(peer2, remote = false, 8, hash(5)) // 11
    }
    check(
      events,
      60 -> bytes(peer, u8(1)),
      61 -> bytes(u64(0), u8(0), reason("superseded")),
      62 -> bytes(peer, u8(1), u32(7), h(2)),
      63 -> bytes(peer, h(3), u8(1), u32(64)),
      66 -> u64(3),
      68 -> bytes(u64(3), u32(7), outline, u8(1)),
      65 -> bytes(u64(3), reason("no answer in time")),
      64 -> peer2,
      67 -> bytes(u64(7), h(4), u8(0), u32(9)),
      68 -> bytes(u64(7), u32(8), outline, u8(0)),
      65 -> bytes(u64(7), reason("x")),
      62 -> bytes(peer2, u8(0), u32(8), h(5))
    )
  }

  test("80 to 84 Safrole tickets: generation and CE 131/132 transfers") {
    val events = capture(5) { t =>
      val generating = t.generatingTickets(12) // 0
      t.ticketsGenerated(generating, Seq(h(1), h(2))) // 1
      t.ticketGenerationFailed(generating, "no ring") // 2
      t.ticketTransferFailed(peer, remote = false, ce132 = true, "stream refused") // 3
      t.ticketTransferred(peer, remote = true, ce132 = false, 13, 1, hash(7)) // 4
    }
    check(
      events,
      80 -> u32(12),
      82 -> bytes(u64(0), len(2), h(1), h(2)),
      81 -> bytes(u64(0), reason("no ring")),
      83 -> bytes(peer, u8(0), u8(1), reason("stream refused")),
      84 -> bytes(peer, u8(1), u8(0), u32(13), u8(1), h(7))
    )
  }

  test("90 to 113 guaranteeing: the primary's and the secondary's pipelines, guarantee distribution and the pool") {
    val authCost = IsAuthorizedCost(ExecCost(1, 2), 3, ExecCost(4, 5))
    val refineCost = RefineCost(ExecCost(1, 2), 3, ExecCost(4, 5), ExecCost(6, 7), ExecCost(8, 9), ExecCost(10, 11), ExecCost(12, 13))
    val wpOutline = Array[Byte](42, 43)
    val reportOutline = Telemetry.workReportOutline(hash(1), 777, hash(2), hash(3))
    val guarantee = Telemetry.guaranteeOutline(hash(1), 99, Seq(4, 5))
    val events = capture(25) { t =>
      val submission = t.workPackageSubmission(peer, ce146 = true) // 0
      t.workPackageReceived(submission, 3, wpOutline) // 1
      t.authorized(submission, authCost) // 2
      t.extrinsicDataReceived(submission) // 3
      t.importsReceived(submission) // 4
      t.refined(submission, Seq(refineCost)) // 5
      t.workReportBuilt(submission, reportOutline) // 6
      t.sharingWorkPackage(submission, peer2) // 7
      t.bundleSent(submission, peer2) // 8
      t.workReportSignatureReceived(submission, peer2) // 9
      t.workPackageSharingFailed(submission, peer2, "reset") // 10
      val built = t.guaranteeBuilt(submission, guarantee) // 11
      val sending = t.sendingGuarantee(built, peer2) // 12
      t.guaranteeSent(sending) // 13
      t.guaranteeSendFailed(sending, "closed") // 14
      t.guaranteesDistributed(submission) // 15
      val shared = t.workPackageBeingShared(peer) // 16
      t.duplicateWorkPackage(shared, 2, hash(8)) // 17
      t.workReportSignatureSent(shared) // 18
      t.workPackageFailed(shared, "bad") // 19
      val receiving = t.receivingGuarantee(peer) // 20
      t.guaranteeReceived(receiving, guarantee) // 21
      t.guaranteeReceiveFailed(receiving, "undecodable") // 22
      t.guaranteeDiscarded(guarantee, 0) // 23
      t.workPackageSubmission(peer2, ce146 = false) // 24
    }
    val refineWire = (1 to 13).map(i => u64(i)).reduce(_ ++ _)
    check(
      events,
      90 -> bytes(peer, u8(1)),
      94 -> bytes(u64(0), u16(3), wpOutline),
      95 -> bytes(u64(0), u64(1), u64(2), u64(3), u64(4), u64(5)),
      96 -> u64(0),
      97 -> u64(0),
      101 -> bytes(u64(0), len(1), refineWire),
      102 -> bytes(u64(0), h(1), u32(777), h(2), h(3)),
      98 -> bytes(u64(0), peer2),
      100 -> bytes(u64(0), peer2),
      104 -> bytes(u64(0), peer2),
      99 -> bytes(u64(0), peer2, reason("reset")),
      105 -> bytes(u64(0), h(1), u32(99), len(2), u16(4), u16(5)),
      106 -> bytes(u64(11), peer2),
      108 -> u64(12),
      107 -> bytes(u64(12), reason("closed")),
      109 -> u64(0),
      91 -> peer,
      93 -> bytes(u64(16), u16(2), h(8)),
      103 -> u64(16),
      92 -> bytes(u64(16), reason("bad")),
      110 -> peer,
      112 -> bytes(u64(20), h(1), u32(99), len(2), u16(4), u16(5)),
      111 -> bytes(u64(20), reason("undecodable")),
      113 -> bytes(h(1), u32(99), len(2), u16(4), u16(5), u8(0)),
      90 -> bytes(peer2, u8(0))
    )
  }

  test("a work-package outline: size, hash, anchor, lookup anchor slot, prerequisites and work-item outlines") {
    import io.forge.jam.core.types.workpackage.WorkPackage
    import io.forge.jam.core.types.workitem.{WorkItem, WorkItemExtrinsic, WorkItemImportSegment}
    import io.forge.jam.core.types.context.Context
    import io.forge.jam.core.primitives.{Gas, ServiceId, Timeslot}
    import io.forge.jam.core.JamBytes
    import spire.math.{UInt, UShort}
    val item = WorkItem(
      ServiceId(UInt(5)),
      hash(1),
      JamBytes(Array[Byte](1, 2, 3)),
      Gas(100),
      Gas(200),
      List(WorkItemImportSegment(hash(2), UShort(3)), WorkItemImportSegment(hash(4), UShort(0x8001))),
      List(WorkItemExtrinsic(hash(5), UInt(10)), WorkItemExtrinsic(hash(6), UInt(20))),
      UShort(7)
    )
    val wp = WorkPackage(
      ServiceId(UInt(1)),
      hash(7),
      Context(hash(8), Timeslot(UInt(1)), hash(9), hash(10), hash(11), Timeslot(UInt(42)), hash(12), List(hash(13))),

      JamBytes.empty,
      JamBytes.empty,
      List(item)
    )
    Telemetry.workPackageOutline(wp, 321, hash(14)).toSeq shouldBe bytes(
      u32(321), h(14), h(8), u32(42), len(1), h(13),
      len(1), u32(5), u32(3), u64(100), u64(200), u32(30), len(2), h(2), u16(3), h(4), u16(0x8001), u16(7)
    ).toSeq
  }

  test("120 to 131 availability: CE 137 shard requests on both sides and CE 141 assurances") {
    val events = capture(14) { t =>
      val sending = t.sendingShardRequest(peer, hash(1), 6) // 0
      t.shardRequestSent(sending) // 1
      t.shardsTransferred(sending) // 2
      t.shardRequestFailed(sending, "no shard") // 3
      val receiving = t.receivingShardRequest(peer2) // 4
      t.shardRequestReceived(receiving, hash(2), 7) // 5
      t.shardsTransferred(receiving) // 6
      t.shardRequestFailed(receiving, "not held") // 7
      val distributing = t.distributingAssurance(hash(3), Array[Byte](5, 0)) // 8
      t.assuranceSent(distributing, peer) // 9
      t.assuranceSendFailed(distributing, peer2, "timeout") // 10
      t.assuranceDistributed(distributing) // 11
      t.assuranceReceiveFailed(peer, "malformed") // 12
      t.assuranceReceived(peer2, hash(4)) // 13
    }
    check(
      events,
      120 -> bytes(peer, h(1), u16(6)),
      123 -> u64(0),
      125 -> u64(0),
      122 -> bytes(u64(0), reason("no shard")),
      121 -> peer2,
      124 -> bytes(u64(4), h(2), u16(7)),
      125 -> u64(4),
      122 -> bytes(u64(4), reason("not held")),
      126 -> bytes(h(3), Array[Byte](5, 0)),
      128 -> bytes(u64(8), peer),
      127 -> bytes(u64(8), peer2, reason("timeout")),
      129 -> u64(8),
      130 -> bytes(peer, reason("malformed")),
      131 -> bytes(peer2, h(4))
    )
  }

  test("140 to 153 bundle recovery: CE 138 and CE 147 on both sides, and reconstruction") {
    val none = u64(-1L)
    val events = capture(18) { t =>
      val sending = t.sendingBundleShardRequest(peer, 3) // 0
      t.bundleShardRequestSent(sending) // 1
      t.bundleShardTransferred(sending) // 2
      t.bundleShardRequestFailed(sending, "reset") // 3
      val receiving = t.receivingBundleShardRequest(peer2) // 4
      t.bundleShardRequestReceived(receiving, hash(1), 4) // 5
      t.bundleShardTransferred(receiving) // 6
      t.bundleShardRequestFailed(receiving, "not held") // 7
      t.reconstructingBundle(trivial = true) // 8
      t.bundleReconstructed() // 9
      val bundle = t.sendingBundleRequest(peer) // 10
      t.bundleRequestSent(bundle) // 11
      t.bundleTransferred(bundle) // 12
      t.bundleRequestFailed(bundle, "timeout") // 13
      val serving = t.receivingBundleRequest(peer2) // 14
      t.bundleRequestReceived(serving, hash(2)) // 15
      t.bundleTransferred(serving) // 16
      t.bundleRequestFailed(serving, "unknown") // 17
    }
    check(
      events,
      140 -> bytes(none, peer, u16(3)),
      143 -> u64(0),
      145 -> u64(0),
      142 -> bytes(u64(0), reason("reset")),
      141 -> peer2,
      144 -> bytes(u64(4), h(1), u16(4)),
      145 -> u64(4),
      142 -> bytes(u64(4), reason("not held")),
      146 -> bytes(none, u8(1)),
      147 -> none,
      148 -> bytes(none, peer),
      151 -> u64(10),
      153 -> u64(10),
      150 -> bytes(u64(10), reason("timeout")),
      149 -> peer2,
      152 -> bytes(u64(14), h(2)),
      153 -> u64(14),
      150 -> bytes(u64(14), reason("unknown"))
    )
  }

  test("160 to 178 segment recovery: mappings, CE 139/140 and CE 148 on both sides, reconstruction and checks") {
    val events = capture(25) { t =>
      val submission = t.workPackageSubmission(peer, ce146 = false) // 0
      t.workPackageHashMapped(submission, hash(1), hash(2)) // 1
      t.segmentsRootMapped(submission, hash(2), hash(3)) // 2
      val shards = t.sendingSegmentShardRequest(submission, peer2, ce140 = true, Seq((1, 4), (0x8000, 5))) // 3
      t.segmentShardRequestSent(shards) // 4
      t.segmentShardsTransferred(shards) // 5
      t.segmentShardRequestFailed(shards, "reset") // 6
      val serving = t.receivingSegmentShardRequest(peer2, ce140 = false) // 7
      t.segmentShardRequestReceived(serving, 9) // 8
      t.segmentShardsTransferred(serving) // 9
      t.segmentShardRequestFailed(serving, "unknown root") // 10
      val rebuilding = t.reconstructingSegments(submission, Seq(1, 0x8000), trivial = false) // 11
      t.segmentsReconstructed(rebuilding) // 12
      t.segmentReconstructionFailed(rebuilding, "too few") // 13
      t.segmentVerificationFailed(submission, Seq(2), "bad proof") // 14
      t.segmentsVerified(submission, Seq(0, 1)) // 15
      val request = t.sendingSegmentRequest(submission, peer2, Seq(0, 3)) // 16
      t.segmentRequestSent(request) // 17
      t.segmentsTransferred(request) // 18
      t.segmentRequestFailed(request, "timeout") // 19
      val asked = t.receivingSegmentRequest(peer) // 20
      t.segmentRequestReceived(asked, 2) // 21
      t.segmentsTransferred(asked) // 22
      t.segmentRequestFailed(asked, "unknown") // 23
      t.workPackageBeingShared(peer) // 24
    }
    check(
      events,
      90 -> bytes(peer, u8(0)),
      160 -> bytes(u64(0), h(1), h(2)),
      161 -> bytes(u64(0), h(2), h(3)),
      162 -> bytes(u64(0), peer2, u8(1), len(2), u16(1), u16(4), u16(0x8000), u16(5)),
      165 -> u64(3),
      167 -> u64(3),
      164 -> bytes(u64(3), reason("reset")),
      163 -> bytes(peer2, u8(0)),
      166 -> bytes(u64(7), u16(9)),
      167 -> u64(7),
      164 -> bytes(u64(7), reason("unknown root")),
      168 -> bytes(u64(0), len(2), u16(1), u16(0x8000), u8(0)),
      170 -> u64(11),
      169 -> bytes(u64(11), reason("too few")),
      171 -> bytes(u64(0), len(1), u16(2), reason("bad proof")),
      172 -> bytes(u64(0), len(2), u16(0), u16(1)),
      173 -> bytes(u64(0), peer2, len(2), u16(0), u16(3)),
      176 -> u64(16),
      178 -> u64(16),
      175 -> bytes(u64(16), reason("timeout")),
      174 -> peer,
      177 -> bytes(u64(20), u16(2)),
      178 -> u64(20),
      175 -> bytes(u64(20), reason("unknown")),
      91 -> peer
    )
  }

  test("190 to 199 preimages: CE 142 announcements, CE 143 requests on both sides and the pool") {
    val events = capture(13) { t =>
      t.preimageAnnouncementFailed(peer, remote = true, "malformed") // 0
      t.preimageAnnounced(peer, remote = false, 9, hash(1), 300) // 1
      t.announcedPreimageForgotten(9, hash(1), 300, 2) // 2
      val sending = t.sendingPreimageRequest(peer2, hash(2)) // 3
      t.preimageRequestSent(sending) // 4
      t.preimageTransferred(sending, 400) // 5
      t.preimageRequestFailed(sending, "timeout") // 6
      val receiving = t.receivingPreimageRequest(peer) // 7
      t.preimageRequestReceived(receiving, hash(3)) // 8
      t.preimageTransferred(receiving, 500) // 9
      t.preimageRequestFailed(receiving, "not held") // 10
      t.preimageDiscarded(hash(4), 600, 0) // 11
      t.preimageAnnounced(peer2, remote = true, 10, hash(5), 1) // 12
    }
    check(
      events,
      190 -> bytes(peer, u8(1), reason("malformed")),
      191 -> bytes(peer, u8(0), u32(9), h(1), u32(300)),
      192 -> bytes(u32(9), h(1), u32(300), u8(2)),
      193 -> bytes(peer2, h(2)),
      196 -> u64(3),
      198 -> bytes(u64(3), u32(400)),
      195 -> bytes(u64(3), reason("timeout")),
      194 -> peer,
      197 -> bytes(u64(7), h(3)),
      198 -> bytes(u64(7), u32(500)),
      195 -> bytes(u64(7), reason("not held")),
      199 -> bytes(h(4), u32(600), u8(0)),
      191 -> bytes(peer2, u8(1), u32(10), h(5), u32(1))
    )
  }
