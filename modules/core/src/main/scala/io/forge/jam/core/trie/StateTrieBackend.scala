package io.forge.jam.core.trie

import io.forge.jam.core.{Hashing, JamBytes}
import io.forge.jam.core.primitives.Hash

enum BackendOp:
  case WriteNode(node: TrieNode)
  /** A raw value stored under its Blake2b-256 `hash`, which the trie has already computed for the leaf node. */
  case WriteRawValue(hash: Hash, value: JamBytes)
  case NodeRefDelta(hash: Hash, delta: Long)
  case RawValueRefDelta(hash: Hash, delta: Long)

object BackendOp:
  /** Builds a [[BackendOp.WriteRawValue]], hashing `value`; for callers that do not already hold the hash. */
  def writeRawValue(value: JamBytes): BackendOp = BackendOp.WriteRawValue(Hashing.blake2b256(value), value)

trait StateTrieBackend:
  def readNode(hash: Hash): Option[TrieNode]
  def readRawValue(hash: Hash): Option[JamBytes]
  def batchUpdate(ops: Seq[BackendOp]): Unit
  def gc(): Unit
  def clear(): Unit
