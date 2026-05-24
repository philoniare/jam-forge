package io.forge.jam.protocol.state

import io.forge.jam.core.JamBytes
import io.forge.jam.core.trie.StateTrie
import io.forge.jam.protocol.accumulation.StateKey

import scala.collection.mutable

final class ServiceStorageView(trie: StateTrie):

  private val pending = mutable.HashMap.empty[JamBytes, Option[JamBytes]]

  private val readCache = mutable.HashMap.empty[JamBytes, Option[JamBytes]]

  private var reads: Int = 0
  private var writes: Int = 0

  def readCount: Int = reads

  def writeCount: Int = pending.size

  def get(serviceId: Long, storageKey: JamBytes): Option[JamBytes] =
    val stateKey = StateKey.computeStorageStateKey(serviceId, storageKey)
    pending.get(stateKey) match
      case Some(staged) => staged
      case None         =>
        readCache.get(stateKey) match
          case Some(cached) => cached
          case None         =>
            reads += 1
            val v = trie.read(stateKey)
            readCache.update(stateKey, v)
            v

  def put(serviceId: Long, storageKey: JamBytes, value: JamBytes): Unit =
    val stateKey = StateKey.computeStorageStateKey(serviceId, storageKey)
    pending.update(stateKey, Some(value))
    writes += 1

  def delete(serviceId: Long, storageKey: JamBytes): Unit =
    val stateKey = StateKey.computeStorageStateKey(serviceId, storageKey)
    pending.update(stateKey, None)
    writes += 1


  def readTrie(stateKey: JamBytes): Option[JamBytes] =
    reads += 1
    trie.read(stateKey)

  def getByStateKey(stateKey: JamBytes): Option[JamBytes] =
    pending.get(stateKey) match
      case Some(staged) => staged
      case None         =>
        readCache.get(stateKey) match
          case Some(cached) => cached
          case None         =>
            reads += 1
            val v = trie.read(stateKey)
            readCache.update(stateKey, v)
            v

  def putByStateKey(stateKey: JamBytes, value: JamBytes): Unit =
    pending.update(stateKey, Some(value))
    writes += 1

  def deleteByStateKey(stateKey: JamBytes): Unit =
    pending.update(stateKey, None)
    writes += 1

  def enumerate(
      prefix: JamBytes,
      bitsCount: Int
  ): Iterator[(JamBytes, JamBytes)] =
    val trieEntries = trie.getKeyValues(prefix, bitsCount).iterator
    val merged = mutable.LinkedHashMap.empty[JamBytes, JamBytes]
    trieEntries.foreach { case (k, v) => merged.update(k, v) }
    pending.foreachEntry {
      case (k, Some(v)) => merged.update(k, v)
      case (k, None)    => merged.remove(k)
    }
    merged.iterator

  def hasPendingWrites: Boolean = pending.nonEmpty


  private val savepoints =
    mutable.ArrayBuffer.empty[Map[JamBytes, Option[JamBytes]]]

  def savepoint(): Unit =
    savepoints += pending.toMap

  def restore(): Unit =
    if savepoints.isEmpty then return
    val snap = savepoints.remove(savepoints.length - 1)
    pending.clear()
    pending ++= snap

  def discardCheckpoint(): Unit =
    if savepoints.nonEmpty then savepoints.remove(savepoints.length - 1)

  def savepointDepth: Int = savepoints.length

  def commit(target: StateTrie): Unit =
    if pending.isEmpty then return
    val updates = pending.iterator.toSeq
    target.update(updates)

  def pendingStateKeys: collection.Set[JamBytes] = pending.keySet
