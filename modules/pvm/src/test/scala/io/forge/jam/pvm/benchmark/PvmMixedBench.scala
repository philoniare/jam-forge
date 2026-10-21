package io.forge.jam.pvm.benchmark

import io.forge.jam.pvm.InterruptKind
import io.forge.jam.pvm.engine.*
import io.forge.jam.pvm.program.ProgramBlob
import io.forge.jam.pvm.types.ProgramCounter

/** Interpreter benchmark with a storage-like instruction mix, unlike the ALU-only [[PvmThroughputBench]].
  *
  * The loop body loads from roData, rwData, the stack and the argument region, stores to rwData and the stack, and
  * takes a data-dependent conditional branch, so it exercises the memory windows, the branch path and block-entry gas
  * charging. No host calls. Three workloads:
  *   - warm: one instance runs the loop until out of gas (steady-state interpreter speed);
  *   - fresh-instance: one module, a new instance per short call (per-invocation allocation and setup);
  *   - cold: a new module per short call (module creation, block compilation and gas-model cost).
  *
  * Run: `sbt "pvm/Test/runMain io.forge.jam.pvm.benchmark.PvmMixedBench"`.
  */
object PvmMixedBench:

  /** Little assembler: emits instructions and remembers their start offsets for the bitmask. */
  private final class Asm:
    private val out = Array.newBuilder[Byte]
    private val starts = scala.collection.mutable.ArrayBuffer.empty[Int]
    private val fixups = scala.collection.mutable.ArrayBuffer.empty[(Int, Int, String)] // (at, instrPc, label)
    private val labels = scala.collection.mutable.Map.empty[String, Int]
    private var pc = 0

    private def emit(bytes: Int*): Unit =
      starts += pc
      bytes.foreach(b => out += b.toByte)
      pc += bytes.length

    private def le4(v: Int): Seq[Int] = Seq(v & 0xff, (v >>> 8) & 0xff, (v >>> 16) & 0xff, (v >>> 24) & 0xff)
    private def regs(lo: Int, hi: Int): Int = (lo & 0xf) | ((hi & 0xf) << 4)

    def label(name: String): Unit = labels(name) = pc
    def fallthrough(): Unit = emit(1)
    def loadImm(r: Int, imm: Int): Unit = emit(51 +: r +: le4(imm)*)
    def addImm64(d: Int, s: Int, imm: Int): Unit = emit(149 +: regs(d, s) +: le4(imm)*)
    def andImm(d: Int, s: Int, imm: Int): Unit = emit(132 +: regs(d, s) +: le4(imm)*)
    def add64(d: Int, a: Int, b: Int): Unit = emit(200, regs(a, b), d)
    def loadIndU64(d: Int, base: Int, off: Int): Unit = emit(130 +: regs(d, base) +: le4(off)*)
    def storeIndU64(src: Int, base: Int, off: Int): Unit = emit(123 +: regs(src, base) +: le4(off)*)
    def jumpInd(r: Int): Unit = emit(50, r)

    /** branch_*_imm with a 2-byte immediate and a 4-byte offset (ℓ = 7). */
    def branchImm(opcode: Int, r: Int, imm: Int, target: String): Unit =
      fixups += ((pc + 4, pc, target))
      emit(opcode, r | (2 << 4), imm & 0xff, (imm >>> 8) & 0xff, 0, 0, 0, 0)

    def result(): (Array[Byte], Array[Byte]) =
      val code = out.result()
      for (at, instrPc, name) <- fixups do
        val rel = labels(name) - instrPc
        for i <- 0 until 4 do code(at + i) = ((rel >>> (8 * i)) & 0xff).toByte
      val bitmask = new Array[Byte]((code.length + 7) / 8)
      starts.foreach(s => bitmask(s >> 3) = (bitmask(s >> 3) | (1 << (s & 7))).toByte)
      (code, bitmask)

  private val OpBranchNeImm = 82
  private val OpBranchLtUImm = 83
  private val ItersPerPass = 256

  // r0 halt address, r1 ro base, r2 rw base, r3 stack low, r10 input base, r4 iterations left in the pass,
  // r5 byte index, r11 total iterations, r12 passes left.
  private def buildProgram(): (Array[Byte], Array[Byte]) =
    val a = Asm()
    a.label("top")
    a.loadImm(4, ItersPerPass)
    a.loadImm(5, 0)
    a.fallthrough()
    a.label("loop")
    a.add64(6, 1, 5); a.loadIndU64(7, 6, 0) // ro
    a.add64(8, 2, 5); a.loadIndU64(9, 8, 0) // rw
    a.add64(9, 9, 7); a.storeIndU64(9, 8, 8) // rw
    a.add64(6, 3, 5); a.storeIndU64(9, 6, 0); a.loadIndU64(7, 6, 0) // stack
    a.add64(6, 10, 5); a.loadIndU64(7, 6, 0) // input
    a.andImm(7, 7, 0xff)
    a.branchImm(OpBranchLtUImm, 7, 0x80, "skip")
    a.addImm64(9, 9, 1)
    a.fallthrough()
    a.label("skip")
    a.addImm64(5, 5, 8)
    a.addImm64(11, 11, 1)
    a.addImm64(4, 4, -1)
    a.branchImm(OpBranchNeImm, 4, 0, "loop")
    a.addImm64(12, 12, -1)
    a.branchImm(OpBranchNeImm, 12, 0, "top")
    a.jumpInd(0)
    a.result()

  private val (code, bitmask) = buildProgram()
  private val rnd = new scala.util.Random(42)
  private val roData = Array.fill[Byte](8192)(rnd.nextInt().toByte)
  private val rwData = Array.fill[Byte](8192)(rnd.nextInt().toByte)
  private val input = Array.fill[Byte](4096)(rnd.nextInt().toByte)

  /** The deblob container: no jump table, then code and bitmask. */
  private def deblob(): Array[Byte] =
    val n = code.length
    require(n < (1 << 14))
    Array[Byte](0, 0, (0x80 | (n >>> 8)).toByte, (n & 0xff).toByte) ++ code ++ bitmask

  /** Mirrors ServiceCode: rw data padded to 256 KiB, 16 heap pages, 64 KiB stack. */
  private def newModule(): InterpretedModule =
    val padded = new Array[Byte](262144)
    System.arraycopy(rwData, 0, padded, 0, rwData.length)
    val blob = ProgramBlob
      .fromCodeAndJumpTable(deblob(), roData, padded, 65536, true, 16, rwData.length)
      .getOrElse(sys.error("bad program"))
    InterpretedModule.create(blob).fold(e => sys.error(e), identity)

  private def newInstance(module: InterpretedModule, gas: Long, passes: Long): InterpretedInstance =
    val inst = InterpretedInstance.fromModule(module, input)
    val mm = module.memoryMap
    inst.setReg(0, 0xffff0000L)
    inst.setReg(1, mm.roDataAddress.toLong)
    inst.setReg(2, mm.rwDataAddress.toLong)
    inst.setReg(3, mm.stackAddressLow.toLong)
    inst.setReg(10, mm.auxDataAddress.toLong)
    inst.setReg(12, passes)
    inst.setGas(gas)
    inst.setNextProgramCounter(ProgramCounter(0))
    inst

  private def runToEnd(inst: InterpretedInstance, expected: InterruptKind): Unit =
    inst.run() match
      case Right(k) if k == expected => ()
      case other => sys.error(s"unexpected result $other, expected $expected")

  /** Steady state: returns (Mgas/s, M loop iterations/s). */
  private def warm(module: InterpretedModule, gas: Long): (Double, Double) =
    val inst = newInstance(module, gas, Long.MaxValue)
    val t0 = System.nanoTime()
    runToEnd(inst, InterruptKind.OutOfGas)
    val s = (System.nanoTime() - t0) / 1e9
    ((gas - inst.gas) / s / 1e6, inst.reg(11) / s / 1e6)

  /** Short calls (4 passes = 1024 iterations each): returns calls per second. */
  private def calls(n: Int, fresh: () => InterpretedModule): Double =
    val t0 = System.nanoTime()
    var i = 0
    while i < n do
      runToEnd(newInstance(fresh(), 100_000_000L, 4), InterruptKind.Finished)
      i += 1
    n / ((System.nanoTime() - t0) / 1e9)

  private def report(name: String, unit: String, xs: Seq[Double]): Unit =
    println(f"RESULT $name%-15s mean ${xs.sum / xs.size}%10.1f  best ${xs.max}%10.1f  $unit")

  def main(args: Array[String]): Unit =
    val gas = args.headOption.map(_.toLong).getOrElse(500_000_000L)
    val runs = 5
    println(s"PVM mixed bench: code ${code.length} bytes, gas budget $gas, $runs runs after warmup")
    val shared = newModule()

    for _ <- 1 to 3 do warm(shared, gas / 5)
    val w = (1 to runs).map(_ => warm(shared, gas))
    report("warm", "Mgas/s", w.map(_._1))
    report("warm", "M iterations/s", w.map(_._2))

    calls(2000, () => shared)
    report("fresh-instance", "calls/s", (1 to runs).map(_ => calls(2000, () => shared)))

    calls(200, () => newModule())
    report("cold", "calls/s", (1 to runs).map(_ => calls(200, () => newModule())))
