package io.forge.jam.core.benchmark

import io.forge.jam.core.Hashing

object Blake2bBench:
  def main(args: Array[String]): Unit =
    // sizes: 64B trie-node preimage, 200B typical report chunk, 4KiB segment-ish,
    // 128KiB preimage-ish
    val sizes = List(64, 200, 4096, 131072)
    val data = sizes.map(sz => sz -> Array.tabulate[Byte](sz)(i => (i * 31).toByte)).toMap

    def bench(size: Int, seconds: Double): (Double, Double) =
      val arr = data(size)
      var sink = 0L
      // warmup
      var i = 0
      while i < 20000 do { sink ^= Hashing.blake2b256(arr).bytes(0).toLong; i += 1 }
      val deadline = System.nanoTime() + (seconds * 1e9).toLong
      var ops = 0L
      while System.nanoTime() < deadline do
        sink ^= Hashing.blake2b256(arr).bytes(0).toLong
        ops += 1
      val elapsed = seconds
      if sink == 42 then println("") // keep sink live
      val opsPerSec = ops / elapsed
      val mbPerSec = opsPerSec * size / 1e6
      (opsPerSec, mbPerSec)

    println("Bouncycastle Blake2b-256 throughput (single thread):")
    println(f"${"size"}%8s ${"ops/s"}%12s ${"MB/s"}%10s ${"ns/op"}%8s")
    for sz <- sizes do
      val (ops, mb) = bench(sz, 2.0)
      println(f"$sz%8d ${ops}%12.0f ${mb}%10.1f ${1e9 / ops}%8.0f")
