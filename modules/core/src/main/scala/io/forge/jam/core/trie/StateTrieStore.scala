package io.forge.jam.core.trie

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference

/**
 * The pinned state root, plus caches of service-account records and of the set of service ids, keyed by service id
 * only. The caches describe the pinned root: an import updates them as it commits the child of the pinned root
 * ([[commitChild]]); moving the pin anywhere else ([[markCommitted]], e.g. a reorg back to a common ancestor) drops
 * them, since they would otherwise answer with the abandoned branch's accounts.
 */
final class StateTrieStore(val backend: StateTrieBackend):

  @volatile private var pinned: Hash = Hash.zero
  private val serviceInfoCache = new ConcurrentHashMap[java.lang.Long, JamBytes]()
  private val knownServiceIdsCache = new AtomicReference[Option[Set[Long]]](None)

  def currentRoot: Hash = pinned

  def at(root: Hash): StateTrie =
    if root != pinned && root != Hash.zero then
      throw new IllegalStateException(
        s"StateTrieStore.at: root $root is not the pinned root $pinned"
      )
    StateTrie.at(backend, root)

  def bootstrap(keyvals: Seq[(JamBytes, JamBytes)]): Hash =
    backend.clear()
    val trie = StateTrie.at(backend, Hash.zero)
    trie.update(keyvals.map { case (k, v) => (k, Some(v)) })
    trie.save()
    synchronized {
      pinned = trie.rootHash
      clearCaches()
    }
    pinned

  /**
   * Pins `root`, the state an import just saved on top of `parent`, having updated the caches as it committed. They
   * stay only if `parent` is the root they described, the pinned one.
   */
  def commitChild(parent: Hash, root: Hash): Unit = synchronized {
    if parent != pinned then clearCaches()
    pinned = root
  }

  /** Pins an arbitrary `root` (a restart, or a reorg's rewind); the caches go unless it is already the pinned root. */
  def markCommitted(root: Hash): Unit = synchronized {
    if root != pinned then clearCaches()
    pinned = root
  }

  private def clearCaches(): Unit =
    serviceInfoCache.clear()
    knownServiceIdsCache.set(None)

  def gc(): Unit = backend.gc()

  def cachedServiceInfo(id: Long): Option[JamBytes] =
    Option(serviceInfoCache.get(id))

  def putCachedServiceInfo(id: Long, encoded: JamBytes): Unit =
    serviceInfoCache.put(id, encoded)
    ()

  def evictCachedServiceInfo(id: Long): Unit =
    serviceInfoCache.remove(id)
    ()

  def cachedServiceIds: Option[Set[Long]] = knownServiceIdsCache.get

  /**
   * Seeds the id set when there is none yet. Once set it changes only through add/remove, so a reader that computed
   * `ids` from an older state cannot overwrite a set an import has since extended.
   */
  def primeKnownServiceIds(ids: Set[Long]): Unit =
    knownServiceIdsCache.compareAndSet(None, Some(ids))
    ()

  def addKnownServiceId(id: Long): Unit =
    knownServiceIdsCache.updateAndGet(_.map(_ + id))
    ()

  def removeKnownServiceId(id: Long): Unit =
    knownServiceIdsCache.updateAndGet(_.map(_ - id))
    ()
