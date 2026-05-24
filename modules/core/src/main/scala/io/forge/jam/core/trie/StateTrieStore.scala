package io.forge.jam.core.trie

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash

final class StateTrieStore(val backend: InMemoryTrieBackend):

  private var pinned: Hash = Hash.zero

  def currentRoot: Hash = pinned

  def at(root: Hash): StateTrie =
    if root != pinned && root != Hash.zero then
      throw new IllegalStateException(
        s"StateTrieStore.at: root $root is not the pinned root $pinned"
      )
    StateTrie.at(backend, root)

  def bootstrap(keyvals: Seq[(JamBytes, JamBytes)]): Hash =
    val trie = StateTrie.at(backend, Hash.zero)
    trie.update(keyvals.map { case (k, v) => (k, Some(v)) })
    trie.save()
    pinned = trie.rootHash
    pinned

  def markCommitted(root: Hash): Unit =
    pinned = root

  def gc(): Unit = backend.gc()
