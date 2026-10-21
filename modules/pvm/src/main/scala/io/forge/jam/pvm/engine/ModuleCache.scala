package io.forge.jam.pvm.engine

/** Bounded, thread-safe LRU cache of compiled modules keyed by code hash, meant to be shared by every executor in the
  * process so a module is compiled once rather than once per importer.
  *
  * The key is only the hash the caller looked the code up by, so each entry also keeps the code it was built from and
  * a lookup with different code builds a new module instead of returning one for other code. The cache keeps a
  * reference to `code`: callers must not modify it afterwards.
  */
final class ModuleCache[K](maxEntries: Int):
  private final class Entry(val code: Array[Byte], val module: InterpretedModule)

  private val entries = new java.util.LinkedHashMap[K, Entry](16, 0.75f, true):
    override def removeEldestEntry(eldest: java.util.Map.Entry[K, Entry]): Boolean = this.size() > maxEntries

  private def lookup(key: K, code: Array[Byte]): Option[InterpretedModule] =
    val entry = entries.synchronized(entries.get(key))
    if entry != null && java.util.Arrays.equals(entry.code, code) then Some(entry.module) else None

  /** The cached module for `key` and `code`, or the result of `create`, cached when defined. `create` runs outside
    * the lock; if two threads miss together, the first module stored wins and both return it.
    */
  def getOrCreate(key: K, code: Array[Byte])(create: => Option[InterpretedModule]): Option[InterpretedModule] =
    lookup(key, code).orElse {
      create.map { module =>
        entries.synchronized {
          val raced = entries.get(key)
          if raced != null && java.util.Arrays.equals(raced.code, code) then raced.module
          else
            entries.put(key, Entry(code, module))
            module
        }
      }
    }

  def size: Int = entries.synchronized(entries.size())
