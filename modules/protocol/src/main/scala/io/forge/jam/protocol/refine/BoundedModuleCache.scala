package io.forge.jam.protocol.refine

import io.forge.jam.core.JamBytes
import io.forge.jam.pvm.engine.InterpretedModule

/** Fixed-capacity LRU cache of compiled [[InterpretedModule]]s keyed by code
  * hash
  */
final class BoundedModuleCache(maxSize: Int):
  private val underlying: java.util.LinkedHashMap[JamBytes, InterpretedModule] =
    new java.util.LinkedHashMap[JamBytes, InterpretedModule](
      maxSize,
      0.75f,
      true
    ) {
      override def removeEldestEntry(
          eldest: java.util.Map.Entry[JamBytes, InterpretedModule]
      ): Boolean =
        size() > maxSize
    }

  /** Returns the cached module for `key`, compiling and caching it via
    * `compile` on a miss. `compile` may return `None` (e.g. malformed code);
    * failures are never cached, so a later call with the same key retries.
    */
  def getOrCompile(
      key: JamBytes
  )(compile: => Option[InterpretedModule]): Option[InterpretedModule] =
    val cached = underlying.get(key)
    if cached != null then Some(cached)
    else
      compile.map { module =>
        underlying.put(key, module)
        module
      }
