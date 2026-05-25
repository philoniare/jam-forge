package io.forge.jam.core.trie

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash

enum BackendOp:
  case WriteNode(node: TrieNode)
  case WriteRawValue(value: JamBytes)
  case NodeRefDelta(hash: Hash, delta: Long)
  case RawValueRefDelta(hash: Hash, delta: Long)

trait StateTrieBackend:
  def readNode(hash: Hash): Option[TrieNode]
  def readRawValue(hash: Hash): Option[JamBytes]
  def batchUpdate(ops: Seq[BackendOp]): Unit
  def gc(): Unit
  def clear(): Unit
