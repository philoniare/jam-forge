package io.forge.jam.pvm

import io.forge.jam.pvm.engine.*
import io.forge.jam.pvm.program.Compiler.Companion.TARGET_OUT_OF_RANGE
import io.forge.jam.pvm.program.Compiler.Companion.notEnoughGasImpl
import io.forge.jam.pvm.program.Compiler.Companion.trapImpl
import io.forge.jam.pvm.program.ProgramCounter
import io.forge.jam.pvm.program.Reg
import io.forge.jam.pvm.program.toRegImm

typealias Target = UInt
typealias Handler = (Visitor) -> Target?

fun transmuteReg(value: UInt): Reg {
    require(Reg.fromRaw(value.toInt()) != null) {
        "assertion failed: Reg.fromRaw(value) must not be null"
    }

    return Reg.fromRaw(value.toInt())
        ?: throw IllegalStateException("Failed to transmute value to Reg")
}

fun wrappingAdd(a: UInt, b: UInt): UInt = a.plus(b).toUInt()

fun getArgs(visitor: Visitor): Args =
    visitor.inner.compiledArgs[visitor.inner.compiledOffset.toInt()]

object RawHandlers {
    val logger = PvmLogger(RawHandlers::class.java)

    val trap: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        logger.debug("Trap at ${programCounter.value}: explicit trap")
        trapImpl(visitor, programCounter)
    }

    val chargeGas: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val gasCost = args.a1
        val newGas = visitor.inner.gas - gasCost.toLong()

        if (newGas < 0) {
            notEnoughGasImpl(visitor, programCounter, newGas)
        } else {
            visitor.inner.gas = newGas
            visitor.goToNextInstruction()
        }
    }

    val step: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        visitor.inner.programCounter = programCounter
        visitor.inner.programCounterValid = true
        visitor.inner.nextProgramCounter = programCounter
        visitor.inner.nextProgramCounterChanged = false
        visitor.inner.interrupt = InterruptKind.Step
        visitor.inner.compiledOffset++
        null
    }


    val stepOutOfRange: Handler = { visitor ->
        with(visitor.inner) {
            programCounterValid = true
            nextProgramCounter = programCounter
            nextProgramCounterChanged = false
            interrupt = InterruptKind.Step
            compiledOffset++
        }
        null
    }

    val outOfRange: Handler = { visitor ->
        val args = getArgs(visitor)
        val gasCost = args.a0
        val programCounter = visitor.inner.programCounter
        val newGas = visitor.inner.gas - gasCost.toLong()
        if (newGas < 0) {
            notEnoughGasImpl(visitor, programCounter, newGas)
        } else {
            visitor.inner.gas = newGas.toLong()
            trapImpl(visitor, programCounter)
        }
    }

    val moveReg: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s = transmuteReg(args.a1)
        val imm = visitor.get64(s.toRegImm())
        visitor.set64(d, imm)
        visitor.goToNextInstruction()
    }

    val add32: Handler = { visitor ->
        val args = getArgs(visitor)
        logger.debug("Args: $args")
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = transmuteReg(args.a2)
        visitor.set3_32(d, s1.toRegImm(), s2.toRegImm(), ::wrappingAdd)
    }

    val addImm32: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = args.a2
        visitor.set3_32(d, s1.toRegImm(), s2.intoRegImm(), ::wrappingAdd)
    }

    val and: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = transmuteReg(args.a2)
        visitor.set3_64(d, s1.toRegImm(), s2.toRegImm()) { a, b -> a and b }
    }

    val andImm: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = args.a2
        visitor.set3_64(d, s1.toRegImm(), s2.intoRegImm()) { a, b -> a and b }
    }

    val loadImm: Handler = { visitor ->
        val args = getArgs(visitor)
        val dst = transmuteReg(args.a0)
        val imm = args.a1
        visitor.set32(dst, imm)
        visitor.goToNextInstruction()
    }

    val branchEq: Handler = { visitor ->
        val args = getArgs(visitor)
        val s1 = transmuteReg(args.a0)
        val s2 = transmuteReg(args.a1)
        val tt = args.a2
        val tf = args.a3
        visitor.branch(s1.toRegImm(), s2.toRegImm(), tt, tf, { a, b -> a == b })
    }

    val unresolvedBranchEq: Handler = { visitor ->
        val args = getArgs(visitor)
        val s1 = transmuteReg(args.a0)
        val s2 = transmuteReg(args.a1)
        val targetTrue = ProgramCounter(args.a2)
        val targetFalse = ProgramCounter(args.a3)

        logger.debug("[${visitor.inner.compiledOffset}]: jump $targetTrue if $s1 == $s2")

        val targetFalseResolved = visitor.inner.resolveJump(targetFalse) ?: TARGET_OUT_OF_RANGE
        visitor.inner.resolveJump(targetTrue)?.let { targetTrueResolved ->
            val offset = visitor.inner.compiledOffset
            visitor.inner.compiledHandlers[offset.toInt()] = branchEq
            visitor.inner.compiledArgs[offset.toInt()] = Args.branchEq(
                s1.toRawReg(),
                s2.toRawReg(),
                targetTrueResolved,
                targetFalseResolved
            )
            offset
        }
    }

    val unresolvedBranchEqImm: Handler = { visitor ->
        val args = getArgs(visitor)
        val s1 = transmuteReg(args.a0)
        val s2 = args.a1
        val targetTrue = ProgramCounter(args.a2)
        val targetFalse = ProgramCounter(args.a3)

        logger.debug("[${visitor.inner.compiledOffset}]: jump $targetTrue if $s1 == $s2")

        val targetFalseResolved = visitor.inner.resolveJump(targetFalse) ?: TARGET_OUT_OF_RANGE
        visitor.inner.resolveJump(targetTrue)?.let { targetTrueResolved ->
            val offset = visitor.inner.compiledOffset
            visitor.inner.compiledHandlers[offset.toInt()] = branchEqImm
            visitor.inner.compiledArgs[offset.toInt()] = Args.branchEqImm(
                s1.toRawReg(),
                s2,
                targetTrueResolved,
                targetFalseResolved
            )
            offset
        }
    }

    val branchEqImm: Handler = { visitor ->
        val args = getArgs(visitor)
        val s1 = transmuteReg(args.a0)
        val s2 = args.a1
        val tt = args.a2
        val tf = args.a3
        visitor.branch(s1.toRegImm(), s2.intoRegImm(), tt, tf, { a, b -> a == b })
    }

    val invalidBranch: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        trapImpl(visitor, programCounter)
    }

    val xorImm: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = args.a2
        visitor.set3_64(d, s1.toRegImm(), s2.intoRegImm()) { a, b -> a xor b }
    }

    val xor: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = transmuteReg(args.a2)
        visitor.set3_64(d, s1.toRegImm(), s2.toRegImm()) { a, b -> a xor b }
    }

    val sub32: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = transmuteReg(args.a2)
        visitor.set3_32(d, s1.toRegImm(), s2.toRegImm()) { a, b -> a - b }
    }

    val branchGreaterOrEqualSignedImm: Handler = { visitor ->
        val args = getArgs(visitor)
        val s1 = transmuteReg(args.a0)
        val s2 = args.a1
        val tt = args.a2
        val tf = args.a3
        logger.debug("[${visitor.inner.compiledOffset}]: jump ~$tt if $s1 >=s $s2")
        visitor.branch(s1.toRegImm(), s2.intoRegImm(), tt, tf) { a, b ->
            Cast(a).ulongToSigned() >= Cast(b).ulongToSigned()
        }
    }

    val unresolvedBranchGreaterOrEqualSignedImm: Handler = { visitor ->
        val args = getArgs(visitor)
        val s1 = transmuteReg(args.a0)
        val s2 = args.a1
        val targetTrue = ProgramCounter(args.a2)
        val targetFalse = ProgramCounter(args.a3)

        logger.debug("[${visitor.inner.compiledOffset}]: jump $targetTrue if $s1 >=s $s2")

        val targetFalseResolved = visitor.inner.resolveJump(targetFalse) ?: TARGET_OUT_OF_RANGE
        visitor.inner.resolveJump(targetTrue)?.let { targetTrueResolved ->
            val offset = visitor.inner.compiledOffset
            visitor.inner.compiledHandlers[offset.toInt()] = branchGreaterOrEqualSignedImm
            visitor.inner.compiledArgs[offset.toInt()] = Args.branchGreaterOrEqualSignedImm(
                s1.toRawReg(),
                s2,
                targetTrueResolved,
                targetFalseResolved
            )
            offset
        }
    }

    val branchGreaterOrEqualSigned: Handler = { visitor ->
        val args = getArgs(visitor)
        val s1 = transmuteReg(args.a0)
        val s2 = transmuteReg(args.a1)  // Note: transmuteReg here instead of immediate
        val tt = args.a2
        val tf = args.a3
        logger.debug("[${visitor.inner.compiledOffset}]: jump ~$tt if $s1 >=s $s2")
        visitor.branch(s1.toRegImm(), s2.toRegImm(), tt, tf) { a, b ->
            Cast(a).ulongToSigned() >= Cast(b).ulongToSigned()
        }
    }

    val unresolvedBranchGreaterOrEqualSigned: Handler = { visitor ->
        val args = getArgs(visitor)
        val s1 = transmuteReg(args.a0)
        val s2 = transmuteReg(args.a1)
        val targetTrue = ProgramCounter(args.a2)
        val targetFalse = ProgramCounter(args.a3)

        logger.debug("[${visitor.inner.compiledOffset}]: jump $targetTrue if $s1 >=s $s2")

        val targetFalseResolved = visitor.inner.resolveJump(targetFalse) ?: TARGET_OUT_OF_RANGE
        visitor.inner.resolveJump(targetTrue)?.let { targetTrueResolved ->
            val offset = visitor.inner.compiledOffset
            visitor.inner.compiledHandlers[offset.toInt()] = branchGreaterOrEqualSigned
            visitor.inner.compiledArgs[offset.toInt()] = Args.branchGreaterOrEqualSigned(
                s1.toRawReg(),
                s2.toRawReg(),
                targetTrueResolved,
                targetFalseResolved
            )
            offset
        }
    }

    val branchGreaterOrEqualUnsignedImm: Handler = { visitor ->
        val args = getArgs(visitor)
        val s1 = transmuteReg(args.a0)
        val s2 = args.a1
        val tt = args.a2
        val tf = args.a3
        logger.debug("[${visitor.inner.compiledOffset}]: jump ~$tt if $s1 >=u $s2")
        visitor.branch(s1.toRegImm(), s2.intoRegImm(), tt, tf) { a, b ->
            a >= b  // Direct unsigned comparison
        }
    }

    val unresolvedBranchGreaterOrEqualUnsignedImm: Handler = { visitor ->
        val args = getArgs(visitor)
        val s1 = transmuteReg(args.a0)
        val s2 = args.a1
        val targetTrue = ProgramCounter(args.a2)
        val targetFalse = ProgramCounter(args.a3)

        logger.debug("[${visitor.inner.compiledOffset}]: jump $targetTrue if $s1 >=u $s2")

        val targetFalseResolved = visitor.inner.resolveJump(targetFalse) ?: TARGET_OUT_OF_RANGE
        visitor.inner.resolveJump(targetTrue)?.let { targetTrueResolved ->
            val offset = visitor.inner.compiledOffset
            visitor.inner.compiledHandlers[offset.toInt()] = branchGreaterOrEqualUnsignedImm
            visitor.inner.compiledArgs[offset.toInt()] = Args.branchGreaterOrEqualUnsignedImm(
                s1.toRawReg(),
                s2,
                targetTrueResolved,
                targetFalseResolved
            )
            offset
        }
    }
}
