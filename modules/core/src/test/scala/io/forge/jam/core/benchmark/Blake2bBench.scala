package io.forge.jam.core.benchmark

import io.forge.jam.core.Blake2b256
import org.bouncycastle.crypto.digests.Blake2bDigest

/** Single-thread Blake2b-256 latency of `Blake2b256` (behind `Hashing.blake2b256`) vs Bouncy Castle, interleaved. */
object Blake2bBench:
  def main(args: Array[String]): Unit =
    // 32B key, 64B trie-node preimage, 128B one full block, 200B typical report chunk, 1KiB, 4KiB segment-ish,
    // 128KiB preimage-ish
    val sizes = List(32, 64, 128, 200, 1024, 4096, 131072)
    val data = sizes.map(sz => sz -> Array.tabulate[Byte](sz)(i => (i * 31).toByte)).toMap
    val bc = new Blake2bDigest(256)

    def bouncyCastle(arr: Array[Byte]): Array[Byte] =
      bc.reset()
      bc.update(arr, 0, arr.length)
      val out = new Array[Byte](32)
      bc.doFinal(out, 0)
      out

    def nsPerOp(arr: Array[Byte], seconds: Double, f: Array[Byte] => Byte): Double =
      var sink = 0L
      var i = 0
      while i < 20000 do { sink ^= f(arr); i += 1 }
      val start = System.nanoTime()
      val deadline = start + (seconds * 1e9).toLong
      var ops = 0L
      while System.nanoTime() < deadline do
        sink ^= f(arr)
        ops += 1
      if sink == 42 then println("") // keep sink live
      (System.nanoTime() - start).toDouble / ops

    println("Blake2b-256 ns/op (single thread, median of 5 interleaved 1 s runs):")
    println(f"${"size"}%8s ${"ours"}%10s ${"BC"}%10s ${"speedup"}%8s")
    for sz <- sizes do
      val arr = data(sz)
      val runs = (1 to 5).map { _ =>
        (nsPerOp(arr, 1.0, a => Blake2b256.hash(a, 0, a.length)(0)), nsPerOp(arr, 1.0, a => bouncyCastle(a)(0)))
      }
      val ours = runs.map(_._1).sorted.apply(2)
      val theirs = runs.map(_._2).sorted.apply(2)
      println(f"$sz%8d $ours%10.0f $theirs%10.0f ${theirs / ours}%7.2fx")
