package io.forge.jam.core.trie

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import java.util.concurrent.atomic.AtomicReference
import scala.collection.immutable.HashMap

/**
 * The pinned state root, plus caches of service-account records and of the set of service ids of the state at that
 * root, for the block importer, which works on the pinned root.
 *
 * Every cache call names the root of the state its caller reads, and is a miss or a no-op unless that is the root
 * the caches describe. The caches are one immutable [[StateTrieStore.CacheGeneration]] that carries its root, held
 * in one atomic reference; the pin lives in it too. Why this is race-free:
 *   - A read takes the current generation once and answers only if its root is the caller's, so it never answers
 *     from another state's caches, whatever moves the pin meanwhile.
 *   - A write builds a new generation from the one it read and installs it with compare-and-set, only if that one
 *     carried the caller's root. If the pin moved in between, the reference no longer holds the generation it read,
 *     the set fails, and the retry sees the new root and does nothing. So no entry read from one state can land in a
 *     generation of another root.
 *   - Moving the pin ([[bootstrap]], [[commitChild]], [[markCommitted]], one at a time under this store's lock)
 *     installs a new generation with the new root, also by compare-and-set so that a write landing first is not
 *     lost: an empty one, or for [[commitChild]] on the pinned parent, the parent's entries with the block's account
 *     changes applied in the same step (on top of any entry a reader of the parent added, so the block's records
 *     win). Any generation with root `r` describes the state at `r` (a root names one state), so a generation that is
 *     replaced and later re-created for the same root is as good.
 * A cache hit costs one volatile read and a hash-map lookup; a write, one uncontended compare-and-set while only the
 * importer uses the store.
 */
final class StateTrieStore(val backend: StateTrieBackend):
  import StateTrieStore.CacheGeneration

  private val generation = new AtomicReference[CacheGeneration](CacheGeneration.empty(Hash.zero))

  def currentRoot: Hash = generation.get.root

  def at(root: Hash): StateTrie =
    val pinned = currentRoot
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
      generation.set(CacheGeneration.empty(trie.rootHash))
    }
    trie.rootHash

  /**
   * Pins `root`, the state an import just saved on top of `parent`. When `parent` is the pinned root the caches carry
   * over to `root` with the block's `accountChanges` applied (service id to its new encoded account record, or None
   * for a removed account); otherwise they start empty for `root`.
   */
  def commitChild(parent: Hash, root: Hash, accountChanges: Iterable[(Long, Option[JamBytes])] = Nil): Unit =
    synchronized {
      generation.updateAndGet { g =>
        if g.root != parent then CacheGeneration.empty(root)
        else
          var info = g.serviceInfo
          var ids = g.serviceIds
          accountChanges.foreach {
            case (id, Some(encoded)) =>
              info = info.updated(id, encoded)
              ids = ids.map(_ + id)
            case (id, None) =>
              info = info.removed(id)
              ids = ids.map(_ - id)
          }
          CacheGeneration(root, info, ids)
      }
      ()
    }

  /** Pins an arbitrary `root` (a restart, or a reorg's rewind); the caches go unless it is already the pinned root. */
  def markCommitted(root: Hash): Unit = synchronized {
    generation.updateAndGet(g => if g.root == root then g else CacheGeneration.empty(root))
    ()
  }

  def gc(): Unit = backend.gc()

  def cachedServiceInfo(root: Hash, id: Long): Option[JamBytes] =
    val g = generation.get
    if g.root == root then g.serviceInfo.get(id) else None

  def putCachedServiceInfo(root: Hash, id: Long, encoded: JamBytes): Unit =
    update(root)(g => g.copy(serviceInfo = g.serviceInfo.updated(id, encoded)))

  def putCachedServiceInfos(root: Hash, records: Iterable[(Long, JamBytes)]): Unit =
    if records.nonEmpty then update(root)(g => g.copy(serviceInfo = g.serviceInfo ++ records))

  def evictCachedServiceInfo(root: Hash, id: Long): Unit =
    update(root)(g => g.copy(serviceInfo = g.serviceInfo.removed(id)))

  def cachedServiceIds(root: Hash): Option[Set[Long]] =
    val g = generation.get
    if g.root == root then g.serviceIds else None

  /**
   * Seeds the id set of the state at `root` when there is none yet and `root` is the pinned root. Once set it changes
   * only through add/remove and the pin moving.
   */
  def primeKnownServiceIds(root: Hash, ids: Set[Long]): Unit =
    update(root)(g => if g.serviceIds.isEmpty then g.copy(serviceIds = Some(ids)) else g)

  def addKnownServiceId(root: Hash, id: Long): Unit =
    update(root)(g => g.copy(serviceIds = g.serviceIds.map(_ + id)))

  def removeKnownServiceId(root: Hash, id: Long): Unit =
    update(root)(g => g.copy(serviceIds = g.serviceIds.map(_ - id)))

  private def update(root: Hash)(f: CacheGeneration => CacheGeneration): Unit =
    var done = false
    while !done do
      val g = generation.get
      if g.root != root then done = true
      else
        val next = f(g)
        done = (next eq g) || generation.compareAndSet(g, next)

object StateTrieStore:
  private final case class CacheGeneration(
      root: Hash,
      serviceInfo: HashMap[Long, JamBytes],
      serviceIds: Option[Set[Long]]
  )

  private object CacheGeneration:
    def empty(root: Hash): CacheGeneration = CacheGeneration(root, HashMap.empty, None)
