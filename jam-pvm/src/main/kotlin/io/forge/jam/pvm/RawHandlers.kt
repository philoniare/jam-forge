package io.forge.jam.pvm

import io.forge.jam.pvm.engine.*
import io.forge.jam.pvm.program.Compiler.Companion.TARGET_OUT_OF_RANGE
import io.forge.jam.pvm.program.Compiler.Companion.notEnoughGasImpl
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

    val panic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        logger.debug("Panic at ${programCounter.value}: explicit panic")
        panicImpl(visitor, programCounter)
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
            panicImpl(visitor, programCounter)
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
        panicImpl(visitor, programCounter)
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

    val branchGreaterOrEqualUnsigned: Handler = { visitor ->
        val args = getArgs(visitor)
        val s1 = transmuteReg(args.a0)
        val s2 = transmuteReg(args.a1)
        val tt = args.a2
        val tf = args.a3
        logger.debug("[${visitor.inner.compiledOffset}]: jump ~$tt if $s1 >=u $s2")
        visitor.branch(s1.toRegImm(), s2.toRegImm(), tt, tf) { a, b ->
            a >= b
        }
    }

    val unresolvedBranchGreaterOrEqualUnsigned: Handler = { visitor ->
        val args = getArgs(visitor)
        val s1 = transmuteReg(args.a0)
        val s2 = transmuteReg(args.a1)
        val targetTrue = ProgramCounter(args.a2)
        val targetFalse = ProgramCounter(args.a3)

        logger.debug("[${visitor.inner.compiledOffset}]: jump $targetTrue if $s1 >=u $s2")

        val targetFalseResolved = visitor.inner.resolveJump(targetFalse) ?: TARGET_OUT_OF_RANGE
        visitor.inner.resolveJump(targetTrue)?.let { targetTrueResolved ->
            val offset = visitor.inner.compiledOffset
            visitor.inner.compiledHandlers[offset.toInt()] = branchGreaterOrEqualUnsigned
            visitor.inner.compiledArgs[offset.toInt()] = Args.branchGreaterOrEqualUnsigned(
                s1.toRawReg(),
                s2.toRawReg(),
                targetTrueResolved,
                targetFalseResolved
            )
            offset
        }
    }

    val branchGreaterSignedImm: Handler = { visitor ->
        val args = getArgs(visitor)
        val s1 = transmuteReg(args.a0)
        val s2 = args.a1
        val tt = args.a2
        val tf = args.a3
        logger.debug("[${visitor.inner.compiledOffset}]: jump ~$tt if $s1 >s $s2")
        visitor.branch(s1.toRegImm(), s2.intoRegImm(), tt, tf) { a, b ->
            Cast(a).ulongToSigned() > Cast(b).ulongToSigned()
        }
    }

    val unresolvedBranchGreaterSignedImm: Handler = { visitor ->
        val args = getArgs(visitor)
        val s1 = transmuteReg(args.a0)
        val s2 = args.a1
        val targetTrue = ProgramCounter(args.a2)
        val targetFalse = ProgramCounter(args.a3)

        logger.debug("[${visitor.inner.compiledOffset}]: jump $targetTrue if $s1 >s $s2")

        val targetFalseResolved = visitor.inner.resolveJump(targetFalse) ?: TARGET_OUT_OF_RANGE
        visitor.inner.resolveJump(targetTrue)?.let { targetTrueResolved ->
            val offset = visitor.inner.compiledOffset
            visitor.inner.compiledHandlers[offset.toInt()] = branchGreaterSignedImm
            visitor.inner.compiledArgs[offset.toInt()] = Args.branchGreaterSignedImm(
                s1.toRawReg(),
                s2,
                targetTrueResolved,
                targetFalseResolved
            )
            offset
        }
    }

    val branchGreaterUnsignedImm: Handler = { visitor ->
        val args = getArgs(visitor)
        val s1 = transmuteReg(args.a0)
        val s2 = args.a1
        val tt = args.a2
        val tf = args.a3
        logger.debug("[${visitor.inner.compiledOffset}]: jump ~$tt if $s1 >u $s2")
        visitor.branch(s1.toRegImm(), s2.intoRegImm(), tt, tf) { a, b ->
            a > b
        }
    }

    val unresolvedBranchGreaterUnsignedImm: Handler = { visitor ->
        val args = getArgs(visitor)
        val s1 = transmuteReg(args.a0)
        val s2 = args.a1
        val targetTrue = ProgramCounter(args.a2)
        val targetFalse = ProgramCounter(args.a3)

        logger.debug("[${visitor.inner.compiledOffset}]: jump $targetTrue if $s1 >u $s2")

        val targetFalseResolved = visitor.inner.resolveJump(targetFalse) ?: TARGET_OUT_OF_RANGE
        visitor.inner.resolveJump(targetTrue)?.let { targetTrueResolved ->
            val offset = visitor.inner.compiledOffset
            visitor.inner.compiledHandlers[offset.toInt()] = branchGreaterUnsignedImm
            visitor.inner.compiledArgs[offset.toInt()] = Args.branchGreaterUnsignedImm(
                s1.toRawReg(),
                s2,
                targetTrueResolved,
                targetFalseResolved
            )
            offset
        }
    }

    val branchLessOrEqualUnsignedImm: Handler = { visitor ->
        val args = getArgs(visitor)
        val s1 = transmuteReg(args.a0)
        val s2 = args.a1
        val tt = args.a2
        val tf = args.a3
        logger.debug("[${visitor.inner.compiledOffset}]: jump ~$tt if $s1 <=u $s2")
        visitor.branch(s1.toRegImm(), s2.intoRegImm(), tt, tf) { a, b ->
            a <= b
        }
    }

    val unresolvedBranchLessOrEqualUnsignedImm: Handler = { visitor ->
        val args = getArgs(visitor)
        val s1 = transmuteReg(args.a0)
        val s2 = args.a1
        val targetTrue = ProgramCounter(args.a2)
        val targetFalse = ProgramCounter(args.a3)

        logger.debug("[${visitor.inner.compiledOffset}]: jump $targetTrue if $s1 <=u $s2")

        val targetFalseResolved = visitor.inner.resolveJump(targetFalse) ?: TARGET_OUT_OF_RANGE
        visitor.inner.resolveJump(targetTrue)?.let { targetTrueResolved ->
            val offset = visitor.inner.compiledOffset
            visitor.inner.compiledHandlers[offset.toInt()] = branchLessOrEqualUnsignedImm
            visitor.inner.compiledArgs[offset.toInt()] = Args.branchLessOrEqualUnsignedImm(
                s1.toRawReg(),
                s2,
                targetTrueResolved,
                targetFalseResolved
            )
            offset
        }
    }

    val branchLessOrEqualSignedImm: Handler = { visitor ->
        val args = getArgs(visitor)
        val s1 = transmuteReg(args.a0)
        val s2 = args.a1
        val tt = args.a2
        val tf = args.a3
        logger.debug("[${visitor.inner.compiledOffset}]: jump ~$tt if $s1 <=s $s2")
        visitor.branch(s1.toRegImm(), s2.intoRegImm(), tt, tf) { a, b ->
            Cast(a).ulongToSigned() <= Cast(b).ulongToSigned()
        }
    }

    val unresolvedBranchLessOrEqualSignedImm: Handler = { visitor ->
        val args = getArgs(visitor)
        val s1 = transmuteReg(args.a0)
        val s2 = args.a1
        val targetTrue = ProgramCounter(args.a2)
        val targetFalse = ProgramCounter(args.a3)

        logger.debug("[${visitor.inner.compiledOffset}]: jump $targetTrue if $s1 <=s $s2")

        val targetFalseResolved = visitor.inner.resolveJump(targetFalse) ?: TARGET_OUT_OF_RANGE
        visitor.inner.resolveJump(targetTrue)?.let { targetTrueResolved ->
            val offset = visitor.inner.compiledOffset
            visitor.inner.compiledHandlers[offset.toInt()] = branchLessOrEqualSignedImm
            visitor.inner.compiledArgs[offset.toInt()] = Args.branchLessOrEqualSignedImm(
                s1.toRawReg(),
                s2,
                targetTrueResolved,
                targetFalseResolved
            )
            offset
        }
    }

    val branchLessSigned: Handler = { visitor ->
        val args = getArgs(visitor)
        val s1 = transmuteReg(args.a0)
        val s2 = transmuteReg(args.a1)
        val tt = args.a2
        val tf = args.a3
        logger.debug("[${visitor.inner.compiledOffset}]: jump ~$tt if $s1 <s $s2")
        visitor.branch(s1.toRegImm(), s2.toRegImm(), tt, tf) { a, b ->
            Cast(a).ulongToSigned() < Cast(b).ulongToSigned()
        }
    }

    val unresolvedBranchLessSigned: Handler = { visitor ->
        val args = getArgs(visitor)
        val s1 = transmuteReg(args.a0)
        val s2 = transmuteReg(args.a1)
        val targetTrue = ProgramCounter(args.a2)
        val targetFalse = ProgramCounter(args.a3)

        logger.debug("[${visitor.inner.compiledOffset}]: jump $targetTrue if $s1 <s $s2")

        val targetFalseResolved = visitor.inner.resolveJump(targetFalse) ?: TARGET_OUT_OF_RANGE
        visitor.inner.resolveJump(targetTrue)?.let { targetTrueResolved ->
            val offset = visitor.inner.compiledOffset
            visitor.inner.compiledHandlers[offset.toInt()] = branchLessSigned
            visitor.inner.compiledArgs[offset.toInt()] = Args.branchLessSigned(
                s1.toRawReg(),
                s2.toRawReg(),
                targetTrueResolved,
                targetFalseResolved
            )
            offset
        }
    }

    val branchLessSignedImm: Handler = { visitor ->
        val args = getArgs(visitor)
        val s1 = transmuteReg(args.a0)
        val s2 = args.a1
        val tt = args.a2
        val tf = args.a3
        logger.debug("[${visitor.inner.compiledOffset}]: jump ~$tt if $s1 <s $s2")
        visitor.branch(s1.toRegImm(), s2.intoRegImm(), tt, tf) { a, b ->
            Cast(a).ulongToSigned() < Cast(b).ulongToSigned()
        }
    }

    val unresolvedBranchLessSignedImm: Handler = { visitor ->
        val args = getArgs(visitor)
        val s1 = transmuteReg(args.a0)
        val s2 = args.a1
        val targetTrue = ProgramCounter(args.a2)
        val targetFalse = ProgramCounter(args.a3)

        logger.debug("[${visitor.inner.compiledOffset}]: jump $targetTrue if $s1 <s $s2")

        val targetFalseResolved = visitor.inner.resolveJump(targetFalse) ?: TARGET_OUT_OF_RANGE
        visitor.inner.resolveJump(targetTrue)?.let { targetTrueResolved ->
            val offset = visitor.inner.compiledOffset
            visitor.inner.compiledHandlers[offset.toInt()] = branchLessSignedImm
            visitor.inner.compiledArgs[offset.toInt()] = Args.branchLessSignedImm(
                s1.toRawReg(),
                s2,
                targetTrueResolved,
                targetFalseResolved
            )
            offset
        }
    }

    val branchLessUnsigned: Handler = { visitor ->
        val args = getArgs(visitor)
        val s1 = transmuteReg(args.a0)
        val s2 = transmuteReg(args.a1)
        val tt = args.a2
        val tf = args.a3
        logger.debug("[${visitor.inner.compiledOffset}]: jump ~$tt if $s1 <u $s2")
        visitor.branch(s1.toRegImm(), s2.toRegImm(), tt, tf) { a, b ->
            a < b
        }
    }

    val unresolvedBranchLessUnsigned: Handler = { visitor ->
        val args = getArgs(visitor)
        val s1 = transmuteReg(args.a0)
        val s2 = transmuteReg(args.a1)
        val targetTrue = ProgramCounter(args.a2)
        val targetFalse = ProgramCounter(args.a3)

        logger.debug("[${visitor.inner.compiledOffset}]: jump $targetTrue if $s1 <u $s2")

        val targetFalseResolved = visitor.inner.resolveJump(targetFalse) ?: TARGET_OUT_OF_RANGE
        visitor.inner.resolveJump(targetTrue)?.let { targetTrueResolved ->
            val offset = visitor.inner.compiledOffset
            visitor.inner.compiledHandlers[offset.toInt()] = branchLessUnsigned
            visitor.inner.compiledArgs[offset.toInt()] = Args.branchLessUnsigned(
                s1.toRawReg(),
                s2.toRawReg(),
                targetTrueResolved,
                targetFalseResolved
            )
            offset
        }
    }

    val branchLessUnsignedImm: Handler = { visitor ->
        val args = getArgs(visitor)
        val s1 = transmuteReg(args.a0)
        val s2 = args.a1
        val tt = args.a2
        val tf = args.a3
        logger.debug("[${visitor.inner.compiledOffset}]: jump ~$tt if $s1 <u $s2")
        visitor.branch(s1.toRegImm(), s2.intoRegImm(), tt, tf) { a, b ->
            a < b
        }
    }

    val unresolvedBranchLessUnsignedImm: Handler = { visitor ->
        val args = getArgs(visitor)
        val s1 = transmuteReg(args.a0)
        val s2 = args.a1
        val targetTrue = ProgramCounter(args.a2)
        val targetFalse = ProgramCounter(args.a3)

        logger.debug("[${visitor.inner.compiledOffset}]: jump $targetTrue if $s1 <u $s2")

        val targetFalseResolved = visitor.inner.resolveJump(targetFalse) ?: TARGET_OUT_OF_RANGE
        visitor.inner.resolveJump(targetTrue)?.let { targetTrueResolved ->
            val offset = visitor.inner.compiledOffset
            visitor.inner.compiledHandlers[offset.toInt()] = branchLessUnsignedImm
            visitor.inner.compiledArgs[offset.toInt()] = Args.branchLessUnsignedImm(
                s1.toRawReg(),
                s2,
                targetTrueResolved,
                targetFalseResolved
            )
            offset
        }
    }

    val branchNotEq: Handler = { visitor ->
        val args = getArgs(visitor)
        val s1 = transmuteReg(args.a0)
        val s2 = transmuteReg(args.a1)
        val tt = args.a2
        val tf = args.a3
        logger.debug("[${visitor.inner.compiledOffset}]: jump ~$tt if $s1 != $s2")
        visitor.branch(s1.toRegImm(), s2.toRegImm(), tt, tf) { a, b ->
            a != b
        }
    }

    val unresolvedBranchNotEq: Handler = { visitor ->
        val args = getArgs(visitor)
        val s1 = transmuteReg(args.a0)
        val s2 = transmuteReg(args.a1)
        val targetTrue = ProgramCounter(args.a2)
        val targetFalse = ProgramCounter(args.a3)

        logger.debug("[${visitor.inner.compiledOffset}]: jump $targetTrue if $s1 != $s2")

        val targetFalseResolved = visitor.inner.resolveJump(targetFalse) ?: TARGET_OUT_OF_RANGE
        visitor.inner.resolveJump(targetTrue)?.let { targetTrueResolved ->
            val offset = visitor.inner.compiledOffset
            visitor.inner.compiledHandlers[offset.toInt()] = branchNotEq
            visitor.inner.compiledArgs[offset.toInt()] = Args.branchNotEq(
                s1.toRawReg(),
                s2.toRawReg(),
                targetTrueResolved,
                targetFalseResolved
            )
            offset
        }
    }

    val branchNotEqImm: Handler = { visitor ->
        val args = getArgs(visitor)
        val s1 = transmuteReg(args.a0)
        val s2 = args.a1
        val tt = args.a2
        val tf = args.a3
        logger.debug("[${visitor.inner.compiledOffset}]: jump ~$tt if $s1 != $s2")
        visitor.branch(s1.toRegImm(), s2.intoRegImm(), tt, tf) { a, b ->
            a != b
        }
    }

    val unresolvedBranchNotEqImm: Handler = { visitor ->
        val args = getArgs(visitor)
        val s1 = transmuteReg(args.a0)
        val s2 = args.a1
        val targetTrue = ProgramCounter(args.a2)
        val targetFalse = ProgramCounter(args.a3)

        logger.debug("[${visitor.inner.compiledOffset}]: jump $targetTrue if $s1 != $s2")

        val targetFalseResolved = visitor.inner.resolveJump(targetFalse) ?: TARGET_OUT_OF_RANGE
        visitor.inner.resolveJump(targetTrue)?.let { targetTrueResolved ->
            val offset = visitor.inner.compiledOffset
            visitor.inner.compiledHandlers[offset.toInt()] = branchNotEqImm
            visitor.inner.compiledArgs[offset.toInt()] = Args.branchNotEqImm(
                s1.toRawReg(),
                s2,
                targetTrueResolved,
                targetFalseResolved
            )
            offset
        }
    }

    val cmovIfZeroImm: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val c = transmuteReg(args.a1)
        val s = args.a2
        if (visitor.get64(c.toRegImm()) == 0uL) {
            visitor.set32(d, s)
        }
        visitor.goToNextInstruction()
    }

    val cmovIfZero: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s = transmuteReg(args.a1)
        val c = transmuteReg(args.a2)
        if (visitor.get64(c.toRegImm()) == 0uL) {
            visitor.set64(d, visitor.get64(s.toRegImm()))
        }
        visitor.goToNextInstruction()
    }

    val divSigned32: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = transmuteReg(args.a2)
        logger.debug("[${visitor.inner.compiledOffset}]: div_signed_32 $d = $s1 / $s2")
        visitor.set3_32(d, s1.toRegImm(), s2.toRegImm()) { a, b ->
            Cast(
                ArithmeticOps.div(
                    Cast(a).uintToSigned(),
                    Cast(b).uintToSigned()
                )
            ).intToUnsigned()
        }
    }

    val divSigned64: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = transmuteReg(args.a2)
        logger.debug("[${visitor.inner.compiledOffset}]: div_signed_64 $d = $s1 / $s2")
        visitor.set3_64(d, s1.toRegImm(), s2.toRegImm()) { a, b ->
            Cast(
                ArithmeticOps.div64(
                    Cast(a).ulongToSigned(),
                    Cast(b).ulongToSigned()
                )
            ).longToUnsigned()
        }
    }

    val divUnsigned32: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = transmuteReg(args.a2)
        logger.debug("[${visitor.inner.compiledOffset}]: div_unsigned_32 $d = $s1 / $s2")
        visitor.set3_32(d, s1.toRegImm(), s2.toRegImm()) { a, b ->
            ArithmeticOps.divu(a, b)
        }
    }

    val divUnsigned64: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = transmuteReg(args.a2)
        logger.debug("[${visitor.inner.compiledOffset}]: div_unsigned_64 $d = $s1 / $s2")
        visitor.set3_64(d, s1.toRegImm(), s2.toRegImm()) { a, b ->
            ArithmeticOps.divu64(a, b)
        }
    }

    val loadI8Basic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val dst = transmuteReg(args.a1)
        val offset = args.a2
        visitor.load<I8LoadTy>(programCounter, dst, null, offset, 1u, false)
    }

    val loadI8Dynamic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val dst = transmuteReg(args.a1)
        val offset = args.a2
        visitor.load<I8LoadTy>(programCounter, dst, null, offset, 1u, true)
    }

    val mul32: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = transmuteReg(args.a2)
        logger.debug("[${visitor.inner.compiledOffset}]: mul_32 $d = $s1 * $s2")
        visitor.set3_32(d, s1.toRegImm(), s2.toRegImm()) { a, b ->
            a.times(b)
        }
    }

    val mul64: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = transmuteReg(args.a2)
        logger.debug("[${visitor.inner.compiledOffset}]: mul_64 $d = $s1 * $s2")
        visitor.set3_64(d, s1.toRegImm(), s2.toRegImm()) { a, b ->
            a.times(b)
        }
    }

    val mulImm32: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = args.a2
        logger.debug("[${visitor.inner.compiledOffset}]: mul_imm_32 $d = $s1 * $s2")
        visitor.set3_32(d, s1.toRegImm(), s2.intoRegImm()) { a, b ->
            a.times(b)
        }
    }

    val mulImm64: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = args.a2
        logger.debug("[${visitor.inner.compiledOffset}]: mul_imm_64 $d = $s1 * $s2")
        visitor.set3_64(d, s1.toRegImm(), s2.intoRegImm()) { a, b ->
            a.times(b)
        }
    }

    val mulUpperSignedSigned32: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = transmuteReg(args.a2)
        logger.debug("[${visitor.inner.compiledOffset}]: mul_upper_signed_signed_32 $d = mulh($s1, $s2)")
        visitor.set3_32(d, s1.toRegImm(), s2.toRegImm()) { a, b ->
            Cast(
                ArithmeticOps.mulh(
                    Cast(a).uintToSigned(),
                    Cast(b).uintToSigned()
                )
            ).intToUnsigned()
        }
    }

    val mulUpperSignedSigned64: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = transmuteReg(args.a2)
        logger.debug("[${visitor.inner.compiledOffset}]: mul_upper_signed_signed_64 $d = mulh64($s1, $s2)")
        visitor.set3_64(d, s1.toRegImm(), s2.toRegImm()) { a, b ->
            Cast(
                ArithmeticOps.mulh64(
                    Cast(a).ulongToSigned(),
                    Cast(b).ulongToSigned()
                )
            ).longToUnsigned()
        }
    }

    val mulUpperUnsignedUnsigned32: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = transmuteReg(args.a2)
        logger.debug("[${visitor.inner.compiledOffset}]: mul_upper_unsigned_unsigned_32 $d = mulhu($s1, $s2)")
        visitor.set3_32(d, s1.toRegImm(), s2.toRegImm()) { a, b ->
            ArithmeticOps.mulhu(a, b)
        }
    }

    val mulUpperUnsignedUnsigned64: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = transmuteReg(args.a2)
        logger.debug("[${visitor.inner.compiledOffset}]: mul_upper_unsigned_unsigned_64 $d = mulhu64($s1, $s2)")
        visitor.set3_64(d, s1.toRegImm(), s2.toRegImm()) { a, b ->
            ArithmeticOps.mulhu64(a, b)
        }
    }

    val mulUpperSignedUnsigned32: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = transmuteReg(args.a2)
        logger.debug("[${visitor.inner.compiledOffset}]: mul_upper_signed_unsigned_32 $d = mulhsu($s1, $s2)")
        visitor.set3_32(d, s1.toRegImm(), s2.toRegImm()) { a, b ->
            Cast(
                ArithmeticOps.mulhsu(
                    Cast(a).uintToSigned(),
                    b
                )
            ).intToUnsigned()
        }
    }

    val mulUpperSignedUnsigned64: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = transmuteReg(args.a2)
        logger.debug("[${visitor.inner.compiledOffset}]: mul_upper_signed_unsigned_64 $d = mulhsu64($s1, $s2)")
        visitor.set3_64(d, s1.toRegImm(), s2.toRegImm()) { a, b ->
            Cast(
                ArithmeticOps.mulhsu64(
                    Cast(a).ulongToSigned(),
                    b
                )
            ).longToUnsigned()
        }
    }

    val orImm: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = args.a2
        visitor.set3_64(d, s1.toRegImm(), s2.intoRegImm()) { a, b -> a or b }
    }

    val or: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = transmuteReg(args.a2)
        visitor.set3_64(d, s1.toRegImm(), s2.toRegImm()) { a, b -> a or b }
    }

    val negateAndAddImm32: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = args.a2
        logger.debug("[${visitor.inner.compiledOffset}]: negate_and_add_imm_32 $d = $s2 - $s1")
        visitor.set3_32(d, s1.toRegImm(), s2.intoRegImm()) { s1, s2 ->
            s2.minus(s1)
        }
    }

    val negateAndAddImm64: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = args.a2
        logger.debug("[${visitor.inner.compiledOffset}]: negate_and_add_imm_64 $d = $s2 - $s1")
        visitor.set3_64(d, s1.toRegImm(), s2.intoRegImm()) { s1, s2 ->
            s2.minus(s1)
        }
    }

    val remUnsigned32: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = transmuteReg(args.a2)
        logger.debug("[${visitor.inner.compiledOffset}]: rem_unsigned_32 $d = $s1 % $s2")
        visitor.set3_32(d, s1.toRegImm(), s2.toRegImm()) { a, b ->
            ArithmeticOps.remu(a, b)
        }
    }

    val remUnsigned64: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = transmuteReg(args.a2)
        logger.debug("[${visitor.inner.compiledOffset}]: rem_unsigned_64 $d = $s1 % $s2")
        visitor.set3_64(d, s1.toRegImm(), s2.toRegImm()) { a, b ->
            ArithmeticOps.remu64(a, b)
        }
    }

    val remSigned32: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = transmuteReg(args.a2)
        logger.debug("[${visitor.inner.compiledOffset}]: rem_signed_32 $d = $s1 % $s2")
        visitor.set3_32(d, s1.toRegImm(), s2.toRegImm()) { a, b ->
            Cast(
                ArithmeticOps.rem(
                    Cast(a).uintToSigned(),
                    Cast(b).uintToSigned()
                )
            ).intToUnsigned()
        }
    }

    val remSigned64: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = transmuteReg(args.a2)
        logger.debug("[${visitor.inner.compiledOffset}]: rem_signed_64 $d = $s1 % $s2")
        visitor.set3_64(d, s1.toRegImm(), s2.toRegImm()) { a, b ->
            Cast(
                ArithmeticOps.rem64(
                    Cast(a).ulongToSigned(),
                    Cast(b).ulongToSigned()
                )
            ).longToUnsigned()
        }
    }

    val jumpIndirect: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val base = transmuteReg(args.a1)
        val offset = args.a2

        logger.debug("[${visitor.inner.compiledOffset}]: jump_indirect ${base}, $offset")

        val dynamicAddress = visitor.get32(base.toRegImm()).plus(offset)
        visitor.jumpIndirectImpl(programCounter, dynamicAddress)
    }

    val loadImmAndJumpIndirect: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val ra = transmuteReg(args.a1)
        val base = transmuteReg(args.a2)
        val value = args.a3
        val offset = args.a4

        logger.debug("[${visitor.inner.compiledOffset}]: load_imm_and_jump_indirect $ra, $base, $value, $offset")

        val dynamicAddress = visitor.get32(base.toRegImm()).plus(offset)
        visitor.set32(ra, value)
        visitor.jumpIndirectImpl(programCounter, dynamicAddress)
    }

    val setLessThanUnsignedImm: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = args.a2
        logger.debug("[${visitor.inner.compiledOffset}]: set_less_than_unsigned_imm $d = $s1 <u $s2")
        visitor.set3_64(d, s1.toRegImm(), s2.intoRegImm()) { s1, s2 ->
            if (s1 < s2) 1uL else 0uL
        }
    }

    val setGreaterThanUnsignedImm: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = args.a2
        logger.debug("[${visitor.inner.compiledOffset}]: set_greater_than_unsigned_imm $d = $s1 >u $s2")
        visitor.set3_64(d, s1.toRegImm(), s2.intoRegImm()) { s1, s2 ->
            if (s1 > s2) 1uL else 0uL
        }
    }

    val setLessThanSignedImm: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = args.a2
        logger.debug("[${visitor.inner.compiledOffset}]: set_less_than_signed_imm $d = $s1 <s $s2")
        visitor.set3_64(d, s1.toRegImm(), s2.intoRegImm()) { s1, s2 ->
            if (Cast(s1).ulongToSigned() < Cast(s2).ulongToSigned()) 1uL else 0uL
        }
    }

    val setGreaterThanSignedImm: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = args.a2
        logger.debug("[${visitor.inner.compiledOffset}]: set_greater_than_signed_imm $d = $s1 >s $s2")
        visitor.set3_64(d, s1.toRegImm(), s2.intoRegImm()) { s1, s2 ->
            if (Cast(s1).ulongToSigned() > Cast(s2).ulongToSigned()) 1uL else 0uL
        }
    }

    val setLessThanUnsigned: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = transmuteReg(args.a2)
        logger.debug("[${visitor.inner.compiledOffset}]: set_less_than_unsigned $d = $s1 <u $s2")
        visitor.set3_64(d, s1.toRegImm(), s2.toRegImm()) { s1, s2 ->
            if (s1 < s2) 1uL else 0uL
        }
    }

    val setLessThanSigned: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = transmuteReg(args.a2)
        logger.debug("[${visitor.inner.compiledOffset}]: set_less_than_signed $d = $s1 <s $s2")
        visitor.set3_64(d, s1.toRegImm(), s2.toRegImm()) { s1, s2 ->
            if (Cast(s1).ulongToSigned() < Cast(s2).ulongToSigned()) 1uL else 0uL
        }
    }

    val shiftLogicalRight32: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = transmuteReg(args.a2)
        logger.debug("[${visitor.inner.compiledOffset}]: shift_logical_right_32 $d = $s1 >>> $s2")
        visitor.set3_32(d, s1.toRegImm(), s2.toRegImm()) { s1, s2 ->
            s1.shr(s2.toInt() and 0x1F)
        }
    }

    val shiftLogicalRight64: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = transmuteReg(args.a2)
        logger.debug("[${visitor.inner.compiledOffset}]: shift_logical_right_64 $d = $s1 >>> $s2")
        visitor.set3_64(d, s1.toRegImm(), s2.toRegImm()) { s1, s2 ->
            s1.shr(Cast(s2).ulongTruncateToU32().toInt() and 0x3F)
        }
    }

    val shiftArithmeticRight32: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = transmuteReg(args.a2)
        logger.debug("[${visitor.inner.compiledOffset}]: shift_arithmetic_right_32 $d = $s1 >> $s2")
        visitor.set3_32(d, s1.toRegImm(), s2.toRegImm()) { s1, s2 ->
            Cast(Cast(s1).uintToSigned().shr(s2.toInt() and 0x1F)).intToUnsigned()
        }
    }

    val shiftArithmeticRight64: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = transmuteReg(args.a2)
        logger.debug("[${visitor.inner.compiledOffset}]: shift_arithmetic_right_64 $d = $s1 >> $s2")
        visitor.set3_64(d, s1.toRegImm(), s2.toRegImm()) { s1, s2 ->
            Cast(Cast(s1).ulongToSigned().shr(Cast(s2).ulongTruncateToU32().toInt() and 0x3F)).longToUnsigned()
        }
    }

    val shiftLogicalLeft32: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = transmuteReg(args.a2)
        logger.debug("[${visitor.inner.compiledOffset}]: shift_logical_left_32 $d = $s1 << $s2")
        visitor.set3_32(d, s1.toRegImm(), s2.toRegImm()) { s1, s2 ->
            s1.shl(s2.toInt() and 0x1F)
        }
    }

    val shiftLogicalLeft64: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = transmuteReg(args.a2)
        logger.debug("[${visitor.inner.compiledOffset}]: shift_logical_left_64 $d = $s1 << $s2")
        visitor.set3_64(d, s1.toRegImm(), s2.toRegImm()) { s1, s2 ->
            s1.shl(Cast(s2).ulongTruncateToU32().toInt() and 0x3F)
        }
    }

    val shiftLogicalRightImm32: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = args.a2
        logger.debug("[${visitor.inner.compiledOffset}]: shift_logical_right_imm_32 $d = $s1 >>> $s2")
        visitor.set3_32(d, s1.toRegImm(), s2.intoRegImm()) { s1, s2 ->
            s1.shr(s2.toInt() and 0x1F)
        }
    }

    val shiftLogicalRightImm64: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = args.a2
        logger.debug("[${visitor.inner.compiledOffset}]: shift_logical_right_imm_64 $d = $s1 >>> $s2")
        visitor.set3_64(d, s1.toRegImm(), s2.intoRegImm()) { s1, s2 ->
            s1.shr(Cast(s2).ulongTruncateToU32().toInt() and 0x3F)
        }
    }

    val shiftLogicalRightImmAlt32: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s2 = transmuteReg(args.a1)
        val s1 = args.a2
        logger.debug("[${visitor.inner.compiledOffset}]: shift_logical_right_imm_alt_32 $d = $s1 >>> $s2")
        visitor.set3_32(d, s1.intoRegImm(), s2.toRegImm()) { s1, s2 ->
            s1.shr(s2.toInt() and 0x1F)
        }
    }

    val shiftLogicalRightImmAlt64: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s2 = transmuteReg(args.a1)
        val s1 = args.a2
        logger.debug("[${visitor.inner.compiledOffset}]: shift_logical_right_imm_alt_64 $d = $s1 >>> $s2")
        visitor.set3_64(d, s1.intoRegImm(), s2.toRegImm()) { s1, s2 ->
            s1.shr(Cast(s2).ulongTruncateToU32().toInt() and 0x3F)
        }
    }

    val shiftArithmeticRightImm32: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = args.a2
        logger.debug("[${visitor.inner.compiledOffset}]: shift_arithmetic_right_imm_32 $d = $s1 >> $s2")
        visitor.set3_32(d, s1.toRegImm(), s2.intoRegImm()) { s1, s2 ->
            Cast(Cast(s1).uintToSigned().shr(s2.toInt() and 0x1F)).intToUnsigned()
        }
    }

    val shiftArithmeticRightImm64: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = args.a2
        logger.debug("[${visitor.inner.compiledOffset}]: shift_arithmetic_right_imm_64 $d = $s1 >> $s2")
        visitor.set3_64(d, s1.toRegImm(), s2.intoRegImm()) { s1, s2 ->
            Cast(Cast(s1).ulongToSigned().shr(Cast(s2).ulongTruncateToU32().toInt() and 0x3F)).longToUnsigned()
        }
    }

    val shiftArithmeticRightImmAlt32: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s2 = transmuteReg(args.a1)
        val s1 = args.a2
        logger.debug("[${visitor.inner.compiledOffset}]: shift_arithmetic_right_imm_alt_32 $d = $s1 >> $s2")
        visitor.set3_32(d, s1.intoRegImm(), s2.toRegImm()) { s1, s2 ->
            Cast(Cast(s1).uintToSigned().shr(s2.toInt() and 0x1F)).intToUnsigned()
        }
    }

    val shiftArithmeticRightImmAlt64: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s2 = transmuteReg(args.a1)
        val s1 = args.a2
        logger.debug("[${visitor.inner.compiledOffset}]: shift_arithmetic_right_imm_alt_64 $d = $s1 >> $s2")
        visitor.set3_64(d, s1.intoRegImm(), s2.toRegImm()) { s1, s2 ->
            Cast(Cast(s1).ulongToSigned().shr(Cast(s2).ulongTruncateToU32().toInt() and 0x3F)).longToUnsigned()
        }
    }

    val shiftLogicalLeftImm32: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = args.a2
        logger.debug("[${visitor.inner.compiledOffset}]: shift_logical_left_imm_32 $d = $s1 << $s2")
        visitor.set3_32(d, s1.toRegImm(), s2.intoRegImm()) { s1, s2 ->
            s1.shl(s2.toInt() and 0x1F)
        }
    }

    val shiftLogicalLeftImm64: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = args.a2
        logger.debug("[${visitor.inner.compiledOffset}]: shift_logical_left_imm_64 $d = $s1 << $s2")
        visitor.set3_64(d, s1.toRegImm(), s2.intoRegImm()) { s1, s2 ->
            s1.shl(Cast(s2).ulongTruncateToU32().toInt() and 0x3F)
        }
    }

    val shiftLogicalLeftImmAlt32: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s2 = transmuteReg(args.a1)
        val s1 = args.a2
        logger.debug("[${visitor.inner.compiledOffset}]: shift_logical_left_imm_alt_32 $d = $s1 << $s2")
        visitor.set3_32(d, s1.intoRegImm(), s2.toRegImm()) { s1, s2 ->
            s1.shl(s2.toInt() and 0x1F)
        }
    }

    val shiftLogicalLeftImmAlt64: Handler = { visitor ->
        val args = getArgs(visitor)
        val d = transmuteReg(args.a0)
        val s2 = transmuteReg(args.a1)
        val s1 = args.a2
        logger.debug("[${visitor.inner.compiledOffset}]: shift_logical_left_imm_alt_64 $d = $s1 << $s2")
        visitor.set3_64(d, s1.intoRegImm(), s2.toRegImm()) { s1, s2 ->
            s1.shl(Cast(s2).ulongTruncateToU32().toInt() and 0x3F)
        }
    }

    val jump: Handler = { visitor ->
        val args = getArgs(visitor)
        val target = args.a0
        logger.debug("[${visitor.inner.compiledOffset}]: jump ~$target")
        target
    }

    val fallthrough: Handler = { visitor ->
        logger.debug("[${visitor.inner.compiledOffset}]: fallthrough")
        visitor.goToNextInstruction()
    }

    val unresolvedJump: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val jumpTo = ProgramCounter(args.a1)
        logger.debug("[${visitor.inner.compiledOffset}]: unresolved jump $jumpTo")

        visitor.inner.resolveJump(jumpTo)?.let { target ->
            val offset = visitor.inner.compiledOffset

            if (offset + 1u == target) {
                logger.debug("  -> resolved to fallthrough")
                visitor.inner.compiledHandlers[offset.toInt()] = fallthrough
                visitor.inner.compiledArgs[offset.toInt()] = Args.fallthrough()
            } else {
                logger.debug("  -> resolved to jump")
                visitor.inner.compiledHandlers[offset.toInt()] = jump
                visitor.inner.compiledArgs[offset.toInt()] = Args.jump(target)
            }
            target
        } ?: run {
            logger.debug("  -> resolved to panic")
            panicImpl(visitor, programCounter)
        }
    }

    val unresolvedFallthrough: Handler = { visitor ->
        val args = getArgs(visitor)
        val jumpTo = ProgramCounter(args.a0)
        logger.debug("[${visitor.inner.compiledOffset}]: unresolved fallthrough $jumpTo")

        val offset = visitor.inner.compiledOffset

        visitor.inner.resolveFallthrough(jumpTo)?.let { target ->
            logger.debug("Resolved fallthrough: ${target}")
            panicImpl(visitor, jumpTo)
            if (offset + 1u == target) {
                logger.debug("  -> resolved to fallthrough")
                visitor.inner.compiledHandlers[offset.toInt()] = fallthrough
                visitor.inner.compiledArgs[offset.toInt()] = Args.fallthrough()
            } else {
                logger.debug("  -> resolved to jump")
                visitor.inner.compiledHandlers[offset.toInt()] = jump
                visitor.inner.compiledArgs[offset.toInt()] = Args.jump(target)
            }
            target
        } ?: run {
            logger.debug("UNIResolved fallthrough: ${jumpTo}")
            panicImpl(visitor, jumpTo)
            visitor.inner.compiledHandlers[offset.toInt()] = jump
            visitor.inner.compiledArgs[offset.toInt()] = Args.jump(TARGET_OUT_OF_RANGE)
            TARGET_OUT_OF_RANGE
        }
    }

    val loadI16Basic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val dst = transmuteReg(args.a1)
        val offset = args.a2

        visitor.load<I16LoadTy>(
            programCounter,
            dst,
            null,
            offset,
            2u,
            false
        )
    }

    val loadI16Dynamic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val dst = transmuteReg(args.a1)
        val offset = args.a2

        visitor.load<I16LoadTy>(
            programCounter,
            dst,
            null,
            offset,
            2u,
            true
        )
    }

    val loadIndirectI8Basic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val dst = transmuteReg(args.a1)
        val base = transmuteReg(args.a2)
        val offset = args.a3

        visitor.load<I8LoadTy>(
            programCounter,
            dst,
            base,
            offset,
            1u,
            false
        )
    }

    val loadIndirectI8Dynamic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val dst = transmuteReg(args.a1)
        val base = transmuteReg(args.a2)
        val offset = args.a3

        visitor.load<I8LoadTy>(
            programCounter,
            dst,
            base,
            offset,
            1u,
            true
        )
    }

    val loadIndirectU16Basic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val dst = transmuteReg(args.a1)
        val base = transmuteReg(args.a2)
        val offset = args.a3

        visitor.load<U16LoadTy>(
            programCounter,
            dst,
            base,
            offset,
            2u,
            false
        )
    }

    val loadIndirectU16Dynamic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val dst = transmuteReg(args.a1)
        val base = transmuteReg(args.a2)
        val offset = args.a3

        visitor.load<U16LoadTy>(
            programCounter,
            dst,
            base,
            offset,
            2u,
            true
        )
    }

    val loadIndirectI16Basic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val dst = transmuteReg(args.a1)
        val base = transmuteReg(args.a2)
        val offset = args.a3

        visitor.load<I16LoadTy>(
            programCounter,
            dst,
            base,
            offset,
            2u,
            false
        )
    }

    val loadIndirectI16Dynamic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val dst = transmuteReg(args.a1)
        val base = transmuteReg(args.a2)
        val offset = args.a3

        visitor.load<I16LoadTy>(
            programCounter,
            dst,
            base,
            offset,
            2u,
            true
        )
    }

    val loadIndirectU8Basic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val dst = transmuteReg(args.a1)
        val base = transmuteReg(args.a2)
        val offset = args.a3

        visitor.load<U8LoadTy>(
            programCounter,
            dst,
            base,
            offset,
            1u,
            false
        )
    }

    val loadIndirectU8Dynamic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val dst = transmuteReg(args.a1)
        val base = transmuteReg(args.a2)
        val offset = args.a3

        visitor.load<U8LoadTy>(
            programCounter,
            dst,
            base,
            offset,
            1u,
            true
        )
    }

    val loadIndirectU32Basic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val dst = transmuteReg(args.a1)
        val base = transmuteReg(args.a2)
        val offset = args.a3

        visitor.load<U32LoadTy>(
            programCounter,
            dst,
            base,
            offset,
            4u,
            false
        )
    }

    val loadIndirectU32Dynamic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val dst = transmuteReg(args.a1)
        val base = transmuteReg(args.a2)
        val offset = args.a3

        visitor.load<U32LoadTy>(
            programCounter,
            dst,
            base,
            offset,
            4u,
            true
        )
    }

    val loadIndirectU64Basic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val dst = transmuteReg(args.a1)
        val base = transmuteReg(args.a2)
        val offset = args.a3

        visitor.load<U64LoadTy>(
            programCounter,
            dst,
            base,
            offset,
            8u,
            false
        )
    }

    val loadIndirectU64Dynamic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val dst = transmuteReg(args.a1)
        val base = transmuteReg(args.a2)
        val offset = args.a3

        visitor.load<U64LoadTy>(
            programCounter,
            dst,
            base,
            offset,
            8u,
            true
        )
    }

    val loadU8Basic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val dst = transmuteReg(args.a1)
        val offset = args.a2

        visitor.load<U8LoadTy>(
            programCounter,
            dst,
            null,
            offset,
            1u,
            false
        )
    }

    val loadU8Dynamic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val dst = transmuteReg(args.a1)
        val offset = args.a2

        visitor.load<U8LoadTy>(
            programCounter,
            dst,
            null,
            offset,
            1u,
            true
        )
    }

    val loadU16Basic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val dst = transmuteReg(args.a1)
        val offset = args.a2

        visitor.load<U16LoadTy>(
            programCounter,
            dst,
            null,
            offset,
            2u,
            false
        )
    }

    val loadU16Dynamic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val dst = transmuteReg(args.a1)
        val offset = args.a2

        visitor.load<U16LoadTy>(
            programCounter,
            dst,
            null,
            offset,
            2u,
            true
        )
    }

    val loadU32Basic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val dst = transmuteReg(args.a1)
        val offset = args.a2

        visitor.load<U32LoadTy>(
            programCounter,
            dst,
            null,
            offset,
            4u,
            false
        )
    }

    val loadU32Dynamic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val dst = transmuteReg(args.a1)
        val offset = args.a2

        visitor.load<U32LoadTy>(
            programCounter,
            dst,
            null,
            offset,
            4u,
            true
        )
    }

    val loadI32Basic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val dst = transmuteReg(args.a1)
        val offset = args.a2

        visitor.load<I32LoadTy>(
            programCounter,
            dst,
            null,
            offset,
            4u,
            false
        )
    }

    val loadI32Dynamic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val dst = transmuteReg(args.a1)
        val offset = args.a2

        visitor.load<I32LoadTy>(
            programCounter,
            dst,
            null,
            offset,
            4u,
            true
        )
    }

    val loadU64Basic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val dst = transmuteReg(args.a1)
        val offset = args.a2

        visitor.load<U64LoadTy>(
            programCounter,
            dst,
            null,
            offset,
            8u,
            false
        )
    }

    val loadU64Dynamic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val dst = transmuteReg(args.a1)
        val offset = args.a2

        visitor.load<U64LoadTy>(
            programCounter,
            dst,
            null,
            offset,
            8u,
            true
        )
    }

    val loadIndirectI32Basic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val dst = transmuteReg(args.a1)
        val base = transmuteReg(args.a2)
        val offset = args.a3

        visitor.load<I32LoadTy>(
            programCounter,
            dst,
            base,
            offset,
            4u,
            false
        )
    }

    val loadIndirectI32Dynamic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val dst = transmuteReg(args.a1)
        val base = transmuteReg(args.a2)
        val offset = args.a3

        visitor.load<I32LoadTy>(
            programCounter,
            dst,
            base,
            offset,
            4u,
            true
        )
    }

    val loadImm64: Handler = { visitor ->
        val args = getArgs(visitor)
        val dst = transmuteReg(args.a0)
        val immLo = args.a1
        val immHi = args.a2
        val imm = Cast(immLo).uintToU64() or (Cast(immHi).uintToU64() shl 32)
        visitor.set64(dst, imm)
        visitor.goToNextInstruction()
    }

    val storeImmU8Basic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val offset = args.a1
        val value = args.a2

        logger.debug("[${visitor.inner.compiledOffset}]: store_imm_u8_basic 0x${offset.toString(16)} = ${value}")
        visitor.store<U8StoreTy>(programCounter, value.intoRegImm(), null, offset, false)
    }

    val storeImmU8Dynamic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val offset = args.a1
        val value = args.a2

        logger.debug("[${visitor.inner.compiledOffset}]: store_imm_u8_dynamic 0x${offset.toString(16)} = ${value}")
        visitor.store<U8StoreTy>(programCounter, value.intoRegImm(), null, offset, true)
    }

    val storeU16Basic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val src = transmuteReg(args.a1)
        val offset = args.a2

        logger.debug("[${visitor.inner.compiledOffset}]: store_u16_basic [0x${offset.toString(16)}] = $src")
        visitor.store<U16StoreTy>(programCounter, src.toRegImm(), null, offset, false)
    }

    val storeU16Dynamic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val src = transmuteReg(args.a1)
        val offset = args.a2

        logger.debug("[${visitor.inner.compiledOffset}]: store_u16_dynamic [0x${offset.toString(16)}] = $src")
        visitor.store<U16StoreTy>(programCounter, src.toRegImm(), null, offset, true)
    }

    val storeU32Basic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val src = transmuteReg(args.a1)
        val offset = args.a2

        logger.debug("[${visitor.inner.compiledOffset}]: store_u32_basic [0x${offset.toString(16)}] = $src")
        visitor.store<U32StoreTy>(programCounter, src.toRegImm(), null, offset, false)
    }

    val storeU32Dynamic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val src = transmuteReg(args.a1)
        val offset = args.a2

        logger.debug("[${visitor.inner.compiledOffset}]: store_u32_dynamic [0x${offset.toString(16)}] = $src")
        visitor.store<U32StoreTy>(programCounter, src.toRegImm(), null, offset, true)
    }

    val storeU64Basic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val src = transmuteReg(args.a1)
        val offset = args.a2

        logger.debug("[${visitor.inner.compiledOffset}]: store_u64_basic [0x${offset.toString(16)}] = $src")
        visitor.store<U64StoreTy>(programCounter, src.toRegImm(), null, offset, false)
    }

    val storeU64Dynamic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val src = transmuteReg(args.a1)
        val offset = args.a2

        logger.debug("[${visitor.inner.compiledOffset}]: store_u64_dynamic [0x${offset.toString(16)}] = $src")
        visitor.store<U64StoreTy>(programCounter, src.toRegImm(), null, offset, true)
    }

    val storeImmU16Basic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val offset = args.a1
        val value = args.a2

        logger.debug("[${visitor.inner.compiledOffset}]: store_imm_u16_basic [0x${offset.toString(16)}] = ${value}")
        visitor.store<U16StoreTy>(programCounter, value.intoRegImm(), null, offset, false)
    }

    val storeImmU16Dynamic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val offset = args.a1
        val value = args.a2

        logger.debug("[${visitor.inner.compiledOffset}]: store_imm_u16_dynamic [0x${offset.toString(16)}] = ${value}")
        visitor.store<U16StoreTy>(programCounter, value.intoRegImm(), null, offset, true)
    }

    val storeImmU32Basic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val offset = args.a1
        val value = args.a2

        logger.debug("[${visitor.inner.compiledOffset}]: store_imm_u32_basic [0x${offset.toString(16)}] = ${value}")
        visitor.store<U32StoreTy>(programCounter, value.intoRegImm(), null, offset, false)
    }

    val storeImmU32Dynamic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val offset = args.a1
        val value = args.a2

        logger.debug("[${visitor.inner.compiledOffset}]: store_imm_u32_dynamic [0x${offset.toString(16)}] = ${value}")
        visitor.store<U32StoreTy>(programCounter, value.intoRegImm(), null, offset, true)
    }

    val storeImmU64Basic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val offset = args.a1
        val value = args.a2

        logger.debug("[${visitor.inner.compiledOffset}]: store_imm_u64_basic [0x${offset.toString(16)}] = ${value}")
        visitor.store<U64StoreTy>(programCounter, value.intoRegImm(), null, offset, false)
    }

    val storeImmU64Dynamic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val offset = args.a1
        val value = args.a2

        logger.debug("[${visitor.inner.compiledOffset}]: store_imm_u64_dynamic [0x${offset.toString(16)}] = ${value}")
        visitor.store<U64StoreTy>(programCounter, value.intoRegImm(), null, offset, true)
    }

    val storeU8Basic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val src = transmuteReg(args.a1)
        val offset = args.a2

        logger.debug("[${visitor.inner.compiledOffset}]: store_u8_basic [0x${offset.toString(16)}] = $src")
        visitor.store<U8StoreTy>(programCounter, src.toRegImm(), null, offset, false)
    }

    val storeU8Dynamic: Handler = { visitor ->
        val args = getArgs(visitor)
        val programCounter = ProgramCounter(args.a0)
        val src = transmuteReg(args.a1)
        val offset = args.a2

        logger.debug("[${visitor.inner.compiledOffset}]: store_u8_dynamic [0x${offset.toString(16)}] = $src")
        visitor.store<U8StoreTy>(programCounter, src.toRegImm(), null, offset, true)
    }

}
