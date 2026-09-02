package io.forge.jam.pvm.benchmark

import io.forge.jam.pvm.InterruptKind
import io.forge.jam.pvm.engine.*
import io.forge.jam.pvm.program.ProgramBlob
import io.forge.jam.pvm.types.ProgramCounter

object PvmThroughputBench:

  private val OpAddImm64 = 149
  private val OpJump = 40

  /** Build the loop program: `adds` x (r7 += 1) then jump back to offset 0. */
  private def buildLoopProgram(adds: Int): Array[Byte] =
    val code = Array.newBuilder[Byte]
    // AddImm64: [opcode, regByte = (b<<4)|a with a=b=r7, imm(1 byte) = 1]
    for _ <- 0 until adds do code ++= Array[Byte](OpAddImm64.toByte, 0x77, 0x01)
    // Jump: target = ownOffset + signExtend(imm); back to 0 => imm = -ownOffset
    val jumpOffset = adds * 3
    require(jumpOffset <= 127, "jump offset must fit signed byte")
    code ++= Array[Byte](OpJump.toByte, (-jumpOffset).toByte)
    val c = code.result()

    // Bitmask: one bit per code byte, set at every instruction start
    val bitmask = new Array[Byte]((c.length + 7) / 8)
    var off = 0
    while off < c.length do
      bitmask(off >> 3) = (bitmask(off >> 3) | (1 << (off & 7))).toByte
      off += (if (c(off) & 0xff) == OpJump then 2 else 3)

    // Container: [varint jumpTableEntryCount=0][entrySize=0][varint codeLen][code][bitmask]
    // (single-byte varints suffice for values < 128; codeLen here is < 128)
    require(c.length < 128)
    val blob = Array.newBuilder[Byte]
    blob += 0.toByte // jump table entry count
    blob += 0.toByte // jump table entry size
    blob += c.length.toByte // code length
    blob ++= c
    blob ++= bitmask
    blob.result()

  def main(args: Array[String]): Unit =
    val adds = 24
    val gasBudget = args.headOption.map(_.toLong).getOrElse(100_000_000L)
    val data = buildLoopProgram(adds)

    val blob = ProgramBlob
      .fromCodeAndJumpTable(data = data, stackSize = 4096, is64Bit = true)
      .getOrElse(sys.error("failed to parse hand-assembled program blob"))

    val module = InterpretedModule.create(blob) match
      case Right(m) => m
      case Left(e) => sys.error(s"module create failed: $e")

    def runOnce(gas: Long): Double =
      val instance = InterpretedInstance.fromModule(module, forceStepTracing = false)
      instance.setGas(gas)
      instance.setNextProgramCounter(ProgramCounter(0))
      val t0 = System.nanoTime()
      var running = true
      while running do
        instance.run() match
          case Right(InterruptKind.OutOfGas) => running = false
          case Right(InterruptKind.Step) => () // continue
          case Right(other) => sys.error(s"unexpected interrupt: $other (expected OutOfGas)")
          case Left(err) => sys.error(s"execution error: $err")
      val t1 = System.nanoTime()
      (t1 - t0) / 1e9

    // Warmup: let C2 compile the interpreter loop + handlers
    println(s"PVM throughput bench: ${adds}x AddImm64 + Jump, gas budget $gasBudget (1 gas/instr)")
    print("warmup: ")
    for i <- 1 to 3 do
      val s = runOnce(20_000_000L)
      print(f"${20.0 / s}%.0f MIPS  ")
    println()

    val runs = 5
    val results = (1 to runs).map { i =>
      val s = runOnce(gasBudget)
      val mips = gasBudget / s / 1e6
      println(f"run $i: $s%.3f s  ->  $mips%.1f Minstr/s")
      mips
    }
    val best = results.max
    val mean = results.sum / runs
    println(f"\nRESULT: mean $mean%.1f Minstr/s, best $best%.1f Minstr/s (ALU upper bound)")
    println(f"refine budget check: 5e9 gas @ mean rate = ${5000.0 / mean}%.1f s wall " +
      f"(slot = 6 s; full-speed target needs ~0.8-1.7 Ginstr/s)")
