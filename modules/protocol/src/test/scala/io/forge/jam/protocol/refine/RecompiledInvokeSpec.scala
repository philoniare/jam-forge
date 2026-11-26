package io.forge.jam.protocol.refine

import java.nio.file.{Files, Path, Paths}

import scala.jdk.CollectionConverters.*

import io.circe.{ACursor, Json}
import io.circe.parser.parse
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.primitives.{Gas, Hash, ServiceId, Timeslot}
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.workitem.WorkItem
import io.forge.jam.core.types.workpackage.WorkPackage
import io.forge.jam.protocol.accumulation.MockPvmInstance
import io.forge.jam.protocol.pvmhost.{HostCall, HostCallResult, NativeRunner}
import io.forge.jam.pvm.ExecutionMode
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import spire.math.{ULong, UShort}

class RecompiledInvokeSpec extends AnyFunSuite with Matchers:

  private val config = ChainConfig.TINY

  private def canRunNative: Boolean =
    Option(System.getProperty("jam.pvm.recompiler.lib")).map(Path.of(_)).exists(Files.exists(_))

  private def vectorDir: Option[Path] =
    val user = Paths.get(System.getProperty("user.dir"))
    List(
      user.resolve("modules/pvm/src/test/resources/pvm"),
      user.resolve("../pvm/src/test/resources/pvm"),
      user.resolve("../../modules/pvm/src/test/resources/pvm")
    ).find(Files.isDirectory(_))

  private final case class Vector(
      name: String,
      program: Array[Byte],
      pc: Long,
      gas: Long,
      regs: Array[Long],
      maps: List[(Long, Long, Boolean)],
      writes: List[(Long, Array[Byte])]
  )

  private def u64(c: ACursor): Long =
    val n = c.focus.flatMap(_.asNumber).getOrElse(fail(s"not a number at ${c.history}"))
    n.toLong.getOrElse(n.toBigInt.map(b => (if b > Long.MaxValue then b - BigInt(2).pow(64) else b).toLong).get)

  private def bytes(c: ACursor): Array[Byte] = c.as[List[Int]].toOption.get.map(_.toByte).toArray

  private def load(path: Path): Vector =
    val json = parse(Files.readString(path)).fold(e => fail(s"$path: $e"), identity)
    val c = json.hcursor
    val steps = c.downField("steps").values.getOrElse(Nil).toList
    val prologue = steps.takeWhile(s => !s.hcursor.downField("run").succeeded)
    val regs = new Array[Long](13)
    val maps = List.newBuilder[(Long, Long, Boolean)]
    val writes = List.newBuilder[(Long, Array[Byte])]
    prologue.foreach { (s: Json) =>
      val h = s.hcursor
      if h.downField("set-reg").succeeded then
        val r = h.downField("set-reg").downField("reg").as[Int].toOption.get
        if r < 13 then regs(r) = u64(h.downField("set-reg").downField("value"))
      else if h.downField("map").succeeded then
        val m = h.downField("map")
        maps += ((u64(m.downField("address")), u64(m.downField("length")), m.downField("is-writable").as[Boolean].toOption.get))
      else if h.downField("write").succeeded then
        val w = h.downField("write")
        writes += ((u64(w.downField("address")), bytes(w.downField("contents"))))
    }
    Vector(
      c.downField("name").as[String].toOption.get,
      bytes(c.downField("program")),
      u64(c.downField("initial-pc")),
      u64(c.downField("initial-gas")),
      regs,
      maps.result(),
      writes.result()
    )

  private def context(deadlineMillis: Option[Long] = None): RefineContext =
    val item = WorkItem(
      service = ServiceId(42),
      codeHash = Hash(Array.fill[Byte](32)(7)),
      payload = JamBytes(Array[Byte](1)),
      refineGasLimit = Gas(1_000_000L),
      accumulateGasLimit = Gas(10_000L),
      importSegments = Nil,
      extrinsic = Nil,
      exportCount = UShort(0)
    )
    val wp = WorkPackage(
      authCodeHost = ServiceId(1),
      authCodeHash = Hash(Array.fill[Byte](32)(9)),
      context = Context(
        anchor = Hash.zero,
        anchorSlot = Timeslot(0),
        stateRoot = Hash.zero,
        beefyRoot = Hash.zero,
        lookupAnchor = Hash.zero,
        lookupAnchorSlot = Timeslot(0),
        lookupAnchorStateRoot = Hash.zero,
        prerequisites = Nil
      ),
      authorization = JamBytes(Array.emptyByteArray),
      authorizerConfig = JamBytes(Array.emptyByteArray),
      items = List(item)
    )
    new RefineContext(
      config = config,
      workPackage = wp,
      workItemIndex = 0,
      coreIndex = 0,
      authorizerTrace = Array.emptyByteArray,
      importSegments = IndexedSeq(IndexedSeq.empty),
      extrinsicData = IndexedSeq(IndexedSeq.empty),
      exportSegmentOffset = 0L,
      accounts = new HistoricalLookupService:
        def serviceExists(serviceId: Long): Boolean = false
        def historicalLookup(serviceId: Long, anchor: Long, hash: Hash): Option[Array[Byte]] = None
      ,
      deadlineMillis = deadlineMillis
    )

  private val CodeAt = 0x1000
  private val BlockAt = 0x80000
  private val BufferAt = 0x90000
  private val BufferLen = 0x60000

  private def call(hc: RefineHostCalls, outer: MockPvmInstance, id: Int, args: Long*): Long =
    args.zipWithIndex.foreach((v, i) => outer.setReg(7 + i, v))
    outer.setGas(outer.gas - hc.getGasCost(id, outer))
    hc.dispatch(id, outer)
    outer.reg(7)

  private def putBlock(outer: MockPvmInstance, gas: Long, regs: Array[Long]): Unit =
    val block = new Array[Byte](112)
    (gas +: regs.toSeq).zipWithIndex.foreach { (v, i) =>
      (0 until 8).foreach(b => block(8 * i + b) = (v >>> (8 * b)).toByte)
    }
    outer.writeBytes(BlockAt, block) shouldBe true

  private def block(outer: MockPvmInstance): Seq[Long] =
    val b = outer.readBytes(BlockAt, 112).get
    (0 until 14).map(i => (0 until 8).foldLeft(0L)((v, k) => v | ((b(8 * i + k) & 0xffL) << (8 * k))))

  private def pages(address: Long, length: Long): (Long, Long) =
    val first = address >>> 12
    (first, ((address + length + 4095) >>> 12) - first)

  private def observe(v: Vector, mode: ExecutionMode, deadlineMillis: Option[Long] = None): List[Any] =
    val hc = new RefineHostCalls(context(deadlineMillis), guestMode = mode)
    val outer = new MockPvmInstance(memorySize = 0x100000, initialGas = Long.MaxValue / 2)
    val seen = List.newBuilder[Any]
    outer.writeBytes(CodeAt, v.program) shouldBe true
    val n = call(hc, outer, HostCall.MACHINE, CodeAt.toLong, v.program.length.toLong, v.pc)
    seen += s"machine ${ULong(n)}"
    if ULong(n) == HostCallResult.HUH then return seen.result()

    v.maps.foreach { (a, l, _) =>
      val (p, c) = pages(a, l)
      seen += s"pages rw ${ULong(call(hc, outer, HostCall.PAGES, n, p, c, 2L))}"
    }
    v.writes.foreach { (a, data) =>
      data.grouped(BufferLen).zipWithIndex.foreach { (chunk, i) =>
        outer.writeBytes(BufferAt, chunk) shouldBe true
        seen += s"poke ${ULong(call(hc, outer, HostCall.POKE, n, BufferAt.toLong, a + i.toLong * BufferLen, chunk.length.toLong))}"
      }
    }
    v.maps.filterNot(_._3).foreach { (a, l, _) =>
      val (p, c) = pages(a, l)
      seen += s"pages ro ${ULong(call(hc, outer, HostCall.PAGES, n, p, c, 3L))}"
    }

    def invoke(gas: Long, regs: Array[Long]): Unit =
      putBlock(outer, gas, regs)
      val r7 = call(hc, outer, HostCall.INVOKE, n, BlockAt.toLong)
      seen += s"invoke r7=${ULong(r7)} r8=${ULong(outer.reg(8))} block=${block(outer)} outer gas=${outer.gas}"
      v.maps.foreach { (a, l, _) =>
        val (p, c) = pages(a, l)
        (0L until c * 4096 by BufferLen.toLong).foreach { off =>
          val len = math.min(BufferLen.toLong, c * 4096 - off)
          val r = call(hc, outer, HostCall.PEEK, n, BufferAt.toLong, (p << 12) + off, len)
          seen += s"peek ${ULong(r)} ${java.util.Arrays.hashCode(outer.readBytes(BufferAt, len.toInt).get)}"
        }
      }

    invoke(v.gas, v.regs)
    val after = block(outer)
    invoke(10_000L, after.drop(1).toArray) // resume where the first run stopped, with gas to spare
    invoke(3L, block(outer).drop(1).toArray) // and once with too little gas for most blocks
    seen += s"expunge pc=${ULong(call(hc, outer, HostCall.EXPUNGE, n))}"
    seen.result()

  private lazy val vectors: List[Vector] =
    vectorDir.fold(List.empty[Vector])(d => Files.list(d).iterator().asScala.filter(_.toString.endsWith(".json")).toList.sorted.map(load))

  test("every PVM test vector, run as an inner machine, gives the same results on the recompiler as interpreted") {
    assume(canRunNative, "recompiler library not available")
    assume(vectors.nonEmpty, "PVM test vectors not found")
    val before = NativeRunner.guestNativeCount
    val outcomes = scala.collection.mutable.Set.empty[String]
    val mismatches = vectors.flatMap { v =>
      val interpreted = observe(v, ExecutionMode.Interpreted)
      val recompiled = observe(v, ExecutionMode.Recompiled)
      interpreted.collect { case s: String if s.startsWith("invoke r7=") => s.drop("invoke r7=".length).takeWhile(_ != ' ') }
        .foreach(outcomes += _)
      Option.when(interpreted != recompiled)(
        s"${v.name}:\n  interpreted ${interpreted.mkString("\n              ")}\n  recompiled  ${recompiled.mkString("\n              ")}"
      )
    }
    withClue(mismatches.take(5).mkString("\n")) { mismatches shouldBe Nil }
    outcomes.toSet should contain allElementsOf
      List(HostCallResult.HALT, HostCallResult.PANIC, HostCallResult.FAULT, HostCallResult.OOG, HostCallResult.HOST).map(_.toString)
    val invoked = vectors.count(v => observe(v, ExecutionMode.Interpreted).head != s"machine ${HostCallResult.HUH}")
    NativeRunner.guestNativeCount - before should be >= (3L * invoked)
  }

  test("a recompiled inner machine that loops without host calls is abandoned once the refine deadline has passed") {
    assume(canRunNative, "recompiler library not available")
    val loop = Vector("loop", Array[Byte](0, 0, 2, 40, 0, 1), 0L, Long.MaxValue / 4, new Array[Long](13), Nil, Nil)
    intercept[ComputeReport.DeadlinePassed](observe(loop, ExecutionMode.Recompiled, Some(0L))).deadlineMillis shouldBe 0L
  }
