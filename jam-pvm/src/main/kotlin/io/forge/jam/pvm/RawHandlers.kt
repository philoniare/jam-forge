package io.forge.jam.pvm

import io.forge.jam.pvm.engine.InterruptKind
import io.forge.jam.pvm.engine.Visitor
import io.forge.jam.pvm.program.Compiler.Companion.notEnoughGasImpl
import io.forge.jam.pvm.program.Compiler.Companion.trapImpl
import io.forge.jam.pvm.program.ProgramCounter

typealias Target = UInt
typealias Handler = (Visitor) -> Target?

object RawHandlers {
    val logger = PvmLogger(RawHandlers::class.java)

    val trap: Handler = { visitor ->
        logger.debug("[${visitor.inner.compiledOffset}]: trap")
        val args = visitor.inner.compiledArgs[visitor.inner.compiledOffset.toInt()]
        val programCounter = ProgramCounter(args.a0)
        trapImpl(visitor, programCounter)
    }

    val chargeGas: Handler = { visitor ->
        val args = visitor.inner.compiledArgs[visitor.inner.compiledOffset.toInt()]
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
        val args = visitor.inner.compiledArgs[visitor.inner.compiledOffset.toInt()]
        val programCounter = ProgramCounter(args.a0)
        visitor.inner.programCounter = programCounter
        visitor.inner.programCounterValid = true
        visitor.inner.nextProgramCounter = programCounter
        visitor.inner.nextProgramCounterChanged = false
        visitor.inner.interrupt = InterruptKind.Step
        visitor.inner.compiledOffset++
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
}
