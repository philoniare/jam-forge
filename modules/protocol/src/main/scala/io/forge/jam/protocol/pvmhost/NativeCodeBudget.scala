package io.forge.jam.protocol.pvmhost

import java.util.concurrent.atomic.AtomicLong

final class NativeCodeBudget(val limitBytes: Long):
  private val live = new AtomicLong()

  def liveBytes: Long = live.get()

  def hasRoomFor(bytes: Long): Boolean = live.get() + bytes <= limitBytes

  def add(bytes: Long): Unit = { live.addAndGet(bytes); () }

  def remove(bytes: Long): Unit = { live.addAndGet(-bytes); () }

object NativeCodeBudget:
  val CacheBytes: Long = 768L << 20

  val ProcessBytes: Long = CacheBytes + (256L << 20)
