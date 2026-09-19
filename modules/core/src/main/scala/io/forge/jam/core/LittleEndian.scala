package io.forge.jam.core

object LittleEndian:

  /** Write the low `size` bytes of `value` at `buf(offset)` in LE order. */
  def put(buf: Array[Byte], offset: Int, value: Long, size: Int): Unit =
    var i = 0
    while i < size do
      buf(offset + i) = ((value >>> (8 * i)) & 0xff).toByte
      i += 1

  /** Read `size` LE bytes at `buf(offset)` as an unsigned integer. */
  def get(buf: Array[Byte], offset: Int, size: Int): Long =
    var acc = 0L
    var i = 0
    while i < size do
      acc |= (buf(offset + i).toLong & 0xff) << (8 * i)
      i += 1
    acc

  /** Allocate a fresh `size`-byte array holding `value` in LE order. */
  def encode(value: Long, size: Int): Array[Byte] =
    val out = new Array[Byte](size)
    put(out, 0, value, size)
    out
