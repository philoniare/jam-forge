package io.forge.jam.core.trie

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import scala.collection.mutable

final class StateTrieStore(val backend: StateTrieBackend):

  private var pinned: Hash = Hash.zero
  private val serviceInfoCache = mutable.HashMap.empty[Long, JamBytes]
  private var knownServiceIdsCache: Option[Set[Long]] = None

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
    pinned = trie.rootHash
    serviceInfoCache.clear()
    knownServiceIdsCache = None
    pinned

  def markCommitted(root: Hash): Unit =
    pinned = root

  def gc(): Unit = backend.gc()

  def cachedServiceInfo(id: Long): Option[JamBytes] = serviceInfoCache.get(id)

  def putCachedServiceInfo(id: Long, encoded: JamBytes): Unit =
    serviceInfoCache.update(id, encoded)

  def evictCachedServiceInfo(id: Long): Unit =
    serviceInfoCache.remove(id)

  def cachedServiceIds: Option[Set[Long]] = knownServiceIdsCache

  def primeKnownServiceIds(ids: Set[Long]): Unit =
    knownServiceIdsCache = Some(ids)

  def addKnownServiceId(id: Long): Unit =
    knownServiceIdsCache = knownServiceIdsCache.map(_ + id)

  def removeKnownServiceId(id: Long): Unit =
    knownServiceIdsCache = knownServiceIdsCache.map(_ - id)
