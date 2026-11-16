package io.forge.jam.node

private[node] final class AnnouncementMailbox(capacity: Int):
  private val waiting = new java.util.ArrayDeque[(Boolean, Array[Byte])]()
  private var scheduled = false

  def offer(first: Boolean, msg: Array[Byte]): Boolean = synchronized {
    if waiting.size >= capacity then
      val it = waiting.iterator()
      var dropped = false
      while !dropped && it.hasNext do
        if !it.next()._1 then
          it.remove()
          dropped = true
    waiting.addLast((first, msg))
    if scheduled then false
    else
      scheduled = true
      true
  }

  def idle(): Unit = synchronized { scheduled = false }

  def drain(handle: (Boolean, Array[Byte]) => Unit): Unit =
    var next = take()
    while next != null do
      handle(next._1, next._2)
      next = take()

  private def take(): (Boolean, Array[Byte]) = synchronized {
    val m = waiting.pollFirst()
    if m == null then scheduled = false
    m
  }
