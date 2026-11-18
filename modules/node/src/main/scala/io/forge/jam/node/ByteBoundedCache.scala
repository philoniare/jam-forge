package io.forge.jam.node

final class ByteBoundedCache[K, V](maxEntries: Int, maxBytes: Long, sizeOf: V => Long):
  private val entries = new java.util.LinkedHashMap[K, V](16, 0.75f, false)
  private var held = 0L

  def put(key: K, value: V): Unit = synchronized {
    Option(entries.remove(key)).foreach(old => held -= sizeOf(old))
    entries.put(key, value)
    held += sizeOf(value)
    val it = entries.entrySet().iterator()
    while entries.size > 1 && (entries.size > maxEntries || held > maxBytes) do
      val eldest = it.next()
      held -= sizeOf(eldest.getValue)
      it.remove()
  }

  def get(key: K): Option[V] = synchronized(Option(entries.get(key)))

  def bytes: Long = synchronized(held)

  def size: Int = synchronized(entries.size)
