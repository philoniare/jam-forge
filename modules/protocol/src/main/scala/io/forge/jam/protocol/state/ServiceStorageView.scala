package io.forge.jam.protocol.state

import io.forge.jam.core.JamBytes
import io.forge.jam.core.trie.StateTrie
import io.forge.jam.protocol.accumulation.StateKey

import scala.collection.mutable

final class ServiceStorageView private (
    trie: StateTrie,
    readCache: mutable.HashMap[JamBytes, Option[JamBytes]]
):

  def this(trie: StateTrie) = this(trie, mutable.HashMap.empty)

  private var pending = Map.empty[JamBytes, Option[JamBytes]]

  /** The staged writes this view was forked from: the state its accpar batch started from (None for a root view). */
  private var batchStart: Option[Map[JamBytes, Option[JamBytes]]] = None

  def get(serviceId: Long, storageKey: JamBytes): Option[JamBytes] =
    getByStateKey(StateKey.computeStorageStateKey(serviceId, storageKey))

  def put(serviceId: Long, storageKey: JamBytes, value: JamBytes): Unit =
    val stateKey = StateKey.computeStorageStateKey(serviceId, storageKey)
    pending = pending.updated(stateKey, Some(value))

  def delete(serviceId: Long, storageKey: JamBytes): Unit =
    val stateKey = StateKey.computeStorageStateKey(serviceId, storageKey)
    pending = pending.updated(stateKey, None)


  def readTrie(stateKey: JamBytes): Option[JamBytes] =
    trie.read(stateKey)

  def getByStateKey(stateKey: JamBytes): Option[JamBytes] =
    readStaged(pending, stateKey)

  /**
   * Reads `stateKey` as it was when this view was forked, ignoring this view's own writes since. Every service of an
   * accpar batch starts from the same state, the posterior of the previous batch, so this is what a service sees
   * of another service's data. A root view reads its current staged state.
   */
  def getAtBatchStart(stateKey: JamBytes): Option[JamBytes] =
    readStaged(batchStart.getOrElse(pending), stateKey)

  private def readStaged(staged: Map[JamBytes, Option[JamBytes]], stateKey: JamBytes): Option[JamBytes] =
    staged.get(stateKey) match
      case Some(value) => value
      case None        =>
        readCache.get(stateKey) match
          case Some(cached) => cached
          case None         =>
            val v = trie.read(stateKey)
            readCache.update(stateKey, v)
            v

  def putByStateKey(stateKey: JamBytes, value: JamBytes): Unit =
    pending = pending.updated(stateKey, Some(value))

  def deleteByStateKey(stateKey: JamBytes): Unit =
    pending = pending.updated(stateKey, None)

  def enumerate(
      prefix: JamBytes,
      bitsCount: Int
  ): Iterator[(JamBytes, JamBytes)] =
    val trieEntries = trie.getKeyValues(prefix, bitsCount).iterator
    val merged = mutable.LinkedHashMap.empty[JamBytes, JamBytes]
    trieEntries.foreach { case (k, v) => merged.update(k, v) }
    pending.foreach {
      case (k, Some(v)) if trie.prefixMatches(k, prefix, bitsCount) => merged.update(k, v)
      case (k, None) if trie.prefixMatches(k, prefix, bitsCount)    => merged.remove(k)
      case _                                                        => ()
    }
    merged.iterator

  def hasPendingWrites: Boolean = pending.nonEmpty


  private val savepoints =
    mutable.ArrayBuffer.empty[Map[JamBytes, Option[JamBytes]]]

  def savepoint(): Unit =
    savepoints += pending

  def restore(): Unit =
    if savepoints.isEmpty then return
    pending = savepoints.remove(savepoints.length - 1)

  def discardCheckpoint(): Unit =
    if savepoints.nonEmpty then savepoints.remove(savepoints.length - 1)

  def savepointDepth: Int = savepoints.length

  def commit(target: StateTrie): Unit =
    val updates = commitUpdates()
    if updates.nonEmpty then target.update(updates)

  /** The staged writes [[commit]] would apply, for a caller merging them into one [[StateTrie.update]]. */
  def commitUpdates(): Seq[(JamBytes, Option[JamBytes])] =
    readCache.clear()
    pending.iterator.toSeq

  def pendingStateKeys: collection.Set[JamBytes] = pending.keySet

  /**
   * A view for one service's accumulation run in an accpar batch. It starts from this view's
   * staged writes and keeps its own writes and savepoints until [[absorb]]. Forks run concurrently must not share the
   * read cache; forks run one after another may. This view must not change while forks are in use, except through
   * [[absorb]].
   */
  def fork(shareReadCache: Boolean = false): ServiceStorageView =
    val child = new ServiceStorageView(trie, if shareReadCache then readCache else mutable.HashMap.empty)
    child.pending = pending
    child.batchStart = Some(pending)
    child

  /**
   * Applies the writes `child` (from [[fork]]) made since it was forked. Two services of one batch may not both alter
   * the same service, so a key that another absorbed fork already set to a
   * different value makes the block invalid, as conflicting account updates do.
   */
  def absorb(child: ServiceStorageView): Unit =
    val base = child.batchStart.getOrElse(
      throw new IllegalArgumentException("absorb expects a view made by fork")
    )
    child.pending.foreach { (k, v) =>
      val inherited = base.get(k)
      // The fork shares its base's entries, so an entry it did not write is the very same Option instance.
      if !inherited.exists(_ eq v) then
        pending.get(k) match
          case Some(current) if !inherited.exists(_ eq current) && current != v =>
            throw new RuntimeException(
              s"Conflicting parallel storage updates for key ${k.toHex}: block invalid"
            )
          case _ => pending = pending.updated(k, v)
    }
