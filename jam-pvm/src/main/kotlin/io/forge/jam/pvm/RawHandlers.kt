package io.forge.jam.pvm

import io.forge.jam.pvm.engine.InterruptKind
import io.forge.jam.pvm.engine.Visitor
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

object RawHandlers {
    val logger = PvmLogger(RawHandlers::class.java)

    val trap: Handler = { visitor ->
        val args = visitor.inner.compiledArgs[visitor.inner.compiledOffset.toInt()]
        val programCounter = ProgramCounter(args.a0)
        logger.debug("Trap at ${programCounter.value}: explicit trap")
        trapImpl(visitor, programCounter)
    }

    val chargeGas: Handler = { visitor ->
        val args = visitor.inner.compiledArgs[visitor.inner.compiledOffset.toInt()]
        val programCounter = ProgramCounter(args.a0)
        val gasCost = args.a1
        logger.debug("Charging gas: $gasCost")
        val newGas = visitor.inner.gas - gasCost.toLong()

        if (newGas < 0) {
            notEnoughGasImpl(visitor, programCounter, newGas)
        } else {
            visitor.inner.gas = newGas
            visitor.goToNextInstruction()
        }
    }

    val step: Handler = { visitor ->
        val args = visitor.inner.compiledArgs[visitor.inner.compiledOffset.toInt()]
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
        val args = visitor.inner.compiledArgs[visitor.inner.compiledOffset.toInt()]
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
        logger.debug("[${visitor.inner.compiledOffset}]: ")
        val args = visitor.inner.compiledArgs[visitor.inner.compiledOffset.toInt()]
        val d = transmuteReg(args.a0)
        val s = transmuteReg(args.a1)
        val imm = visitor.get64(s.toRegImm())
        visitor.set64(d, imm)
        visitor.goToNextInstruction()
    }

    val add32: Handler = { visitor ->
        val args = visitor.inner.compiledArgs[visitor.inner.compiledOffset.toInt()]
        logger.debug("Args: $args")
        val d = transmuteReg(args.a0)
        val s1 = transmuteReg(args.a1)
        val s2 = transmuteReg(args.a2)
        visitor.set3_32(d, s1.toRegImm(), s2.toRegImm(), { a, b -> a + b })
    }
}
