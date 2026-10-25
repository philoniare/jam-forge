package io.forge.jam.pvm.benchmark

import io.forge.jam.pvm.MemoryMap
import io.forge.jam.pvm.memory.BasicMemory
import spire.math.UInt

/** Heap growth cost (`grow_heap`, Ω_Gemini), timed at `BasicMemory.sbrk`, the call the host
  * call makes for each page delta. Each run starts from a fresh memory and releases it at the end, as each PVM
  * invocation does.
  *
  * Scenarios follow what the jamtestvectors `fuzzy` trace does (797 growing calls over 200 invocations, deltas of 1 to
  * 3 pages mostly, plus doubling ramps to 128 MiB):
  *   - fuzzy-ramp: 20 grows of 1-3 pages, then doubling deltas 32 .. 16384 pages (heap ends at ~128 MiB, fuzzy's max);
  *   - small-steps: 4096 grows of 2 pages (32 MiB);
  *   - small-on-large: one 32 MiB grow, then 200 grows of 2 pages.
  *
  * Each scenario runs twice: `touch` writes the last byte of every new range (a guest using all it asked for, the
  * worst case), `untouched` only grows.
  *
  * Args: optional subset of scenario names. Prints `RESULT name value unit`; figures are the median of 7 runs.
  */
object GrowHeapBench:
  private val PageSize = UInt(4096)
  private val Runs = 7

  private def memory(): BasicMemory =
    val map = MemoryMap.builder(PageSize).withRoDataSize(PageSize).withRwDataSize(PageSize)
      .withStackSize(UInt(65536)).withAuxDataSize(PageSize).build().fold(e => sys.error(e), identity)
    BasicMemory.create(map)

  private val Scenarios: Vector[(String, Vector[Int])] = Vector(
    "fuzzy-ramp" -> (Vector.tabulate(20)(i => 1 + i % 3) ++ Vector.iterate(32, 10)(_ * 2)),
    "small-steps" -> Vector.fill(4096)(2),
    "small-on-large" -> (8192 +: Vector.fill(200)(2))
  )

  private def run(deltas: Vector[Int], touch: Boolean): Double =
    val mem = memory()
    var sink = 0L
    val t0 = System.nanoTime()
    var i = 0
    while i < deltas.length do
      val prev = mem.sbrk(UInt(deltas(i) * PageSize.signed)).getOrElse(sys.error("sbrk failed"))
      if touch then mem.storeU8(mem.heapEnd - UInt(1), spire.math.UByte(1))
      sink += prev.signed
      i += 1
    // As PvmRunner does when an invocation ends.
    mem.release()
    val ms = (System.nanoTime() - t0) / 1e6
    if sink == 42 then println("")
    ms

  def main(args: Array[String]): Unit =
    val only = args.toSet
    println(s"GrowHeapBench maxMemory=${Runtime.getRuntime.maxMemory >> 20} MiB")
    for (scenario, deltas) <- Scenarios if only.isEmpty || only.contains(scenario); touch <- Seq(true, false) do
      val name = s"${scenario}_${if touch then "touch" else "untouched"}"
      for _ <- 1 to 3 do run(deltas, touch)
      val ms = Vector.fill(Runs)(run(deltas, touch)).sorted
      val med = ms(Runs / 2)
      val pages = deltas.map(_.toLong).sum
      println(f"RESULT grow_heap_${name}_total $med%.3f ms (${deltas.length} calls, ${pages * 4096 >> 20} MiB)")
      println(f"RESULT grow_heap_${name}_per_call ${med * 1000 / deltas.length}%.2f us")
