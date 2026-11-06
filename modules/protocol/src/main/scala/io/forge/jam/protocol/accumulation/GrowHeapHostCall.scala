package io.forge.jam.protocol.accumulation

import spire.math.ULong

object GrowHeapHostCall:

  val CgasGeminiConst: Long = 275L
  val CgasGeminiLinear: Long = 121L

  def handle(instance: PvmInstance): Unit =
    val requested = ULong(instance.reg(7))

    instance.growHeapPageBounds match
      case None =>
        throw new io.forge.jam.protocol.HostCallPanic(
          "GrowHeap PANIC: dispatched on a PvmInstance without heap-page-growth support " +
            "(growHeapPageBounds = None) — the native path must deopt to the interpreter " +
            "before dispatching grow_heap, and test doubles must configureGrowHeap"
        )
      case Some((h, b)) =>
        val hU = ULong(h)
        val bU = ULong(b)
        val noChange = requested <= hU || requested > bU
        val deltaPages = if noChange then 0L else (requested - hU).toLong
        val g = CgasGeminiConst + deltaPages * CgasGeminiLinear
        if instance.gas < g then
          instance.setGas(0L)
          instance.forceOutOfGas()
        else if noChange then
          instance.setGas(instance.gas - g)
          instance.setReg(7, hU.signed)
        else
          instance.setGas(instance.gas - g)
          instance.growHeapPages(deltaPages)
          instance.setReg(7, requested.signed)
