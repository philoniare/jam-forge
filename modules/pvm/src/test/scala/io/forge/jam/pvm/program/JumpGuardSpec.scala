package io.forge.jam.pvm.program

import org.scalatest.funsuite.AnyFunSuite
import spire.math.UInt
import io.forge.jam.pvm.InterruptKind
import io.forge.jam.pvm.engine.{InterpretedInstance, InterpretedModule}
import io.forge.jam.pvm.types.ProgramCounter

/** djump targets outside the jump table panic without reaching an exception */
class JumpGuardSpec extends AnyFunSuite:

  // jump_ind r0, 0 at offset 0, ecalli 7 at offset 3 (a block start after the terminator), trap at 5.
  private val code = Array[Byte](50, 0x00, 0x00, 10, 7, 0)
  private val bitmask = Array[Byte](0x29)

  private def run(jumpTable: JumpTable, r0: Long): Either[?, InterruptKind] =
    val blob = ProgramBlob(code, bitmask, jumpTable, true, Array.empty, new Array[Byte](4096), 4096)
    val module = InterpretedModule.create(blob).fold(e => fail(s"module: $e"), identity)
    val instance = InterpretedInstance.fromModule(module)
    instance.setReg(0, r0)
    instance.setGas(1_000_000L)
    instance.setNextProgramCounter(ProgramCounter(0))
    instance.run()

  private val oneEntryTable = JumpTable(Array[Byte](3, 0, 0, 0), 4)

  test("GP: jump table lookups with a negative index return None"):
    assert(oneEntryTable.getByAddress(0x80000002) == None)
    assert(oneEntryTable.getByAddress(0xFFFFFFFC) == None)
    assert(oneEntryTable.getByIndex(-1) == None)
    assert(oneEntryTable.getByAddress(2) == Some(3))

  test("GP: a negative target offset is never a valid jump target"):
    assert(!Program.isJumpTargetValid(code, bitmask, Int.MinValue))
    assert(!Program.getBitForOffset(bitmask, code.length, -8))

  test("GP: jump_ind to an address of 2^31 + 2 or more panics"):
    for address <- Seq(0x80000002L, 0xFFFFFFF0L, 0xFFFFFFFCL) do
      assert(run(oneEntryTable, address) == Right(InterruptKind.Panic), f"address 0x$address%x")

  test("GP: jump_ind through a jump-table entry of 2^31 or more panics"):
    val table = JumpTable(Array[Byte](0, 0, 0, 0x80.toByte, 0xFE.toByte, 0xFF.toByte, 0xFF.toByte, 0xFF.toByte), 4)
    assert(run(table, 2L) == Right(InterruptKind.Panic))
    assert(run(table, 4L) == Right(InterruptKind.Panic))

  test("jump_ind through a valid entry still reaches its target"):
    assert(run(oneEntryTable, 2L) == Right(InterruptKind.Ecalli(UInt(7))))
