package io.forge.jam.pvm.engine

/** Bounded, thread-safe LRU cache of compiled modules keyed by code hash, meant to be shared by every executor in the
  * process so a module is compiled once rather than once per importer.
  *
  * The key is only the hash the caller looked the code up by, so each entry also keeps the code it was built from and
  * a lookup with different code builds a new module instead of returning one for other code. The cache keeps a
  * reference to `code`: callers must not modify it afterwards.
  *
  * The cache holds at most `maxEntries` modules and, as far as it can, at most `maxBytes` of retained memory: an
  * entry weighs its code plus [[InterpretedModule.retainedBytes]]. A module's weight grows while it runs (blocks are
  * compiled on first use), so the weights are re-read on every insertion and every [[ModuleCache.ReweighEvery]]
  * lookups, and least recently used entries are dropped until both bounds hold. The most recently used entry always
  * stays, even when it alone is over `maxBytes`: dropping it would only rebuild it on the next call.
  */
final class ModuleCache[K](maxEntries: Int, maxBytes: Long = Long.MaxValue):
  private final class Entry(val code: Array[Byte], val module: InterpretedModule):
    def bytes: Long = code.length.toLong + module.retainedBytes

  // Access-ordered: iteration starts at the least recently used entry. Guarded by its own monitor.
  private val entries = new java.util.LinkedHashMap[K, Entry](16, 0.75f, true)
  private var lookups = 0

  private def totalBytes(): Long =
    var total = 0L
    val it = entries.values().iterator()
    while it.hasNext do total += it.next().bytes
    total

  /** Drops least recently used entries until both bounds hold. Caller holds the lock. */
  private def trim(): Unit =
    var total = totalBytes()
    val it = entries.values().iterator()
    while entries.size() > 1 && (entries.size() > maxEntries || total > maxBytes) do
      total -= it.next().bytes
      it.remove()

  private def lookup(key: K, code: Array[Byte]): Option[InterpretedModule] =
    val entry = entries.synchronized {
      val e = entries.get(key)
      lookups += 1
      if (lookups & (ModuleCache.ReweighEvery - 1)) == 0 then trim()
      e
    }
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
            trim()
            module
        }
      }
    }

  def size: Int = entries.synchronized(entries.size())

  /** Current estimate of the bytes the cached entries retain. */
  def retainedBytes: Long = entries.synchronized(totalBytes())

object ModuleCache:
  /** A power of two: every this many lookups the entries are weighed again, as running modules grow. */
  private final val ReweighEvery = 256
