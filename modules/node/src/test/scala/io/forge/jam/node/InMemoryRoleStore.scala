package io.forge.jam.node

/** What a node's role state would find on disk after a restart, kept in memory. */
final class InMemoryRoleStore extends RoleStore:
  private val records = new java.util.TreeMap[String, Array[Byte]]()

  def write(puts: Iterable[(String, Array[Byte])], deletes: Iterable[String]): Unit = synchronized {
    puts.foreach((k, v) => records.put(k, v.clone()))
    deletes.foreach(records.remove)
  }

  def scan(prefix: String, before: String): List[(String, Array[Byte])] = synchronized {
    val out = List.newBuilder[(String, Array[Byte])]
    records.tailMap(prefix, true).forEach { (k, v) =>
      if k.startsWith(prefix) && (before.isEmpty || k.compareTo(prefix + before) < 0) then out += (k -> v.clone())
    }
    out.result()
  }

  def size: Int = synchronized(records.size)
