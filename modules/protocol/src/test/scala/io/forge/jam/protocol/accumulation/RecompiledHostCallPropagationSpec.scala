package io.forge.jam.protocol.accumulation

import io.forge.jam.protocol.HostCallPanic
import io.forge.jam.pvm.native_.PvmRecompiler
import io.forge.jam.pvm.program.JumpTable
import io.forge.jam.pvm.recompiler.RecompilerAbi

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}

class RecompiledHostCallPropagationSpec extends AnyFlatSpec with Matchers:

  private def libPath: Option[Path] =
    Option(System.getProperty("jam.pvm.recompiler.lib")).map(Path.of(_)).filter(Files.exists(_))

  private def canRunNative: Boolean =
    val arch = System.getProperty("os.arch", "").toLowerCase
    (arch == "aarch64" || arch == "arm64") && libPath.isDefined

  private val PAGE_SHIFT = 12

  private def intLE(v: Int): Array[Byte] = Array.tabulate(4)(i => ((v >>> (i * 8)) & 0xff).toByte)

  /** `Ecalli(hostId); Trap` — opcode 10 + 4-byte hostId LE, then opcode 0.
    * Instruction boundaries at byte 0 and byte 5.
    */
  private def ecalliThenTrap(hostId: Int): (Array[Byte], Array[Byte]) =
    val code = Array[Byte](10.toByte) ++ intLE(hostId) ++ Array[Byte](0)
    val bitmask = new Array[Byte]((code.length + 7) / 8)
    Seq(0, 5).foreach(o => bitmask(o >> 3) = (bitmask(o >> 3) | (1 << (o & 7))).toByte)
    (code, bitmask)

  /** Compile `Ecalli(0); Trap`, run it live with `handler`, hand the result to
    * `check`. All native resources are closed on the way out.
    */
  private def withEcalliRun(handler: PvmRecompiler.HostCallHandler)(
      check: (=> PvmRecompiler.ExecResult) => Unit
  ): Unit =
    libPath match
      case None => cancel("recompiler dylib not found (set -Djam.pvm.recompiler.lib); skipping")
      case Some(lib) =>
        val rc = new PvmRecompiler(lib)
        try
          val (code, bitmask) = ecalliThenTrap(0)
          val pp = RecompilerAbi.prepareProgram(code, bitmask, JumpTable.Empty)
          val blk = rc.compile(pp.opcodes, pp.a, pp.b, pp.c, pp.pc, pp.imm, pp.imm2, pp.jumpTable, pp.codeLen)
          try
            blk.isValid shouldBe true
            val live = rc.executeLive(
              blk, Array.fill(13)(0L), 10000L,
              new Array[PvmRecompiler.Region](0), new Array[Byte](0), PAGE_SHIFT, 0,
              handler
            )
            try check(live.run())
            finally live.close()
          finally blk.close()
        finally rc.close()

  // A dispatcher shaped exactly like the one NativeRunner wraps: the host call
  // body throws, and only HostCallPanic is caught at the handler boundary.
  private def narrowlyCatchingHandler(body: => Unit): PvmRecompiler.HostCallHandler =
    (_: Long, _: Int) =>
      try
        body
        PvmRecompiler.HOST_CONTINUE
      catch case _: HostCallPanic => PvmRecompiler.HOST_PANIC

  "the recompiled host-call path" should "map a HostCallPanic to EXIT_PANIC" in {
    if !canRunNative then cancel("recompiler unavailable on this host (AArch64 dylib required); skipping")
    else
      var wasDispatched = false
      val handler = narrowlyCatchingHandler {
        wasDispatched = true
        throw new HostCallPanic("Test PANIC")
      }
      withEcalliRun(handler) { out =>
        val r = out
        // Guards against a PANIC produced by anything other than the handler.
        wasDispatched shouldBe true
        r.exit shouldBe PvmRecompiler.EXIT_PANIC
      }
  }

  it should "propagate an implementation bug (NPE) out of LiveExecution.run()" in {
    if !canRunNative then cancel("recompiler unavailable on this host (AArch64 dylib required); skipping")
    else
      var wasDispatched = false
      val handler = narrowlyCatchingHandler {
        wasDispatched = true
        throw new NullPointerException("simulated bug")
      }
      withEcalliRun(handler) { out =>
        // The NPE is NOT caught by the narrow handler catch; upcallTarget
        // stashes it and rethrowIfPending() surfaces it from run().
        intercept[NullPointerException](out)
        wasDispatched shouldBe true
      }
  }
