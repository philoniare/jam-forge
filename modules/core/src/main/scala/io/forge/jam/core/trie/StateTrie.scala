package io.forge.jam.core.trie

import io.forge.jam.core.{JamBytes, Hashing}
import io.forge.jam.core.primitives.Hash
import scala.collection.mutable

final class StateTrie private (backend: StateTrieBackend, initialRoot: Hash):

  private var currentRoot: Hash = initialRoot

  private val pendingNodes = mutable.Map[Hash, TrieNode]()
  private val pendingValues = mutable.Map[Hash, JamBytes]()
  private val nodeRefDeltas = mutable.Map[Hash, Long]().withDefaultValue(0L)
  private val valueRefDeltas = mutable.Map[Hash, Long]().withDefaultValue(0L)

  def rootHash: Hash = currentRoot

  def read(key: JamBytes): Option[JamBytes] =
    require(key.length == 31, s"key must be 31 bytes, got ${key.length}")
    findLeaf(currentRoot, key, depth = 0).flatMap { node =>
      node.nodeType match
        case TrieNodeType.EmbeddedLeaf =>
          TrieNode.embeddedValue(node)
        case TrieNodeType.RegularLeaf =>
          val vh = Hash.fromByteVectorUnsafe(node.right.toByteVector)
          pendingValues.get(vh).orElse(backend.readRawValue(vh))
        case TrieNodeType.Branch =>
          None
    }

  def update(updates: Seq[(JamBytes, Option[JamBytes])]): Unit =
    updates.foreach {
      case (k, Some(v)) => currentRoot = insert(currentRoot, k, v, 0)
      case (k, None)    => currentRoot = delete(currentRoot, k, 0)
    }

  def save(): Unit =
    val ops = mutable.ArrayBuffer[BackendOp]()
    pendingNodes.values.foreach(n => ops += BackendOp.WriteNode(n))
    pendingValues.values.foreach(v => ops += BackendOp.WriteRawValue(v))
    nodeRefDeltas.foreach {
      case (h, d) if d != 0 => ops += BackendOp.NodeRefDelta(h, d)
      case _ => ()
    }
    valueRefDeltas.foreach {
      case (h, d) if d != 0 => ops += BackendOp.RawValueRefDelta(h, d)
      case _ => ()
    }
    backend.batchUpdate(ops.toSeq)
    pendingNodes.clear()
    pendingValues.clear()
    nodeRefDeltas.clear()
    valueRefDeltas.clear()

  private def insert(rootHash: Hash, key: JamBytes, value: JamBytes, depth: Int): Hash =
    require(key.length == 31, s"key must be 31 bytes, got ${key.length}")
    getNode(rootHash) match
      case None =>
        emitLeaf(key, value)
      case Some(node) if node.nodeType == TrieNodeType.Branch =>
        val bit = bitAt(key, depth)
        val leftHash = Hash.fromByteVectorUnsafe(node.left.toByteVector)
        val rightHash = Hash.fromByteVectorUnsafe(node.right.toByteVector)
        val newChildHash =
          if bit then insert(rightHash, key, value, depth + 1)
          else insert(leftHash, key, value, depth + 1)
        unrefNode(rootHash)
        val newBranch =
          if bit then TrieNode.branch(leftHash, newChildHash)
          else TrieNode.branch(newChildHash, rightHash)
        pendingNodes.update(newBranch.hash, newBranch)
        refNode(newBranch.hash)
        newBranch.hash
      case Some(leaf) =>
        val existingKey = TrieNode.leafKey(leaf)
        if existingKey.toByteVector == key.toByteVector then
          unrefNode(rootHash)
          emitLeaf(key, value)
        else
          unrefNode(rootHash)
          splitLeaves(leaf, existingKey, key, value, depth)

  private def splitLeaves(
      existing: TrieNode,
      existingKey: JamBytes,
      newKey: JamBytes,
      newValue: JamBytes,
      depth: Int,
  ): Hash =
    val existingBit = bitAt(existingKey, depth)
    val newBit = bitAt(newKey, depth)
    if existingBit != newBit then
      val newLeafHash = emitLeaf(newKey, newValue)
      pendingNodes.update(existing.hash, existing)
      refNode(existing.hash)
      if existing.nodeType == TrieNodeType.RegularLeaf then
        refValue(Hash.fromByteVectorUnsafe(existing.right.toByteVector))
      val branch =
        if newBit then TrieNode.branch(existing.hash, newLeafHash)
        else TrieNode.branch(newLeafHash, existing.hash)
      pendingNodes.update(branch.hash, branch)
      refNode(branch.hash)
      branch.hash
    else
      val childHash = splitLeaves(existing, existingKey, newKey, newValue, depth + 1)
      val branch =
        if existingBit then TrieNode.branch(Hash.zero, childHash)
        else TrieNode.branch(childHash, Hash.zero)
      pendingNodes.update(branch.hash, branch)
      refNode(branch.hash)
      branch.hash

  private def delete(rootHash: Hash, key: JamBytes, depth: Int): Hash =
    require(key.length == 31, s"key must be 31 bytes, got ${key.length}")
    getNode(rootHash) match
      case None => rootHash
      case Some(node) if node.nodeType == TrieNodeType.Branch =>
        val bit = bitAt(key, depth)
        val leftHash = Hash.fromByteVectorUnsafe(node.left.toByteVector)
        val rightHash = Hash.fromByteVectorUnsafe(node.right.toByteVector)
        val newLeftHash = if bit then leftHash else delete(leftHash, key, depth + 1)
        val newRightHash = if bit then delete(rightHash, key, depth + 1) else rightHash
        if newLeftHash == leftHash && newRightHash == rightHash then
          rootHash
        else
          unrefNode(rootHash)
          collapseBranch(newLeftHash, newRightHash)
      case Some(leaf) =>
        if TrieNode.leafKey(leaf).toByteVector == key.toByteVector then
          unrefNode(rootHash)
          Hash.zero
        else
          rootHash

  private def collapseBranch(leftHash: Hash, rightHash: Hash): Hash =
    val leftEmpty = leftHash == Hash.zero
    val rightEmpty = rightHash == Hash.zero
    if leftEmpty && rightEmpty then Hash.zero
    else if leftEmpty then
      getNode(rightHash) match
        case Some(n) if n.nodeType != TrieNodeType.Branch => rightHash
        case _ => writeBranch(leftHash, rightHash)
    else if rightEmpty then
      getNode(leftHash) match
        case Some(n) if n.nodeType != TrieNodeType.Branch => leftHash
        case _ => writeBranch(leftHash, rightHash)
    else
      writeBranch(leftHash, rightHash)

  private def writeBranch(leftHash: Hash, rightHash: Hash): Hash =
    val branch = TrieNode.branch(leftHash, rightHash)
    pendingNodes.update(branch.hash, branch)
    refNode(branch.hash)
    branch.hash

  private def emitLeaf(key: JamBytes, value: JamBytes): Hash =
    val leaf = TrieNode.leaf(key, value)
    pendingNodes.update(leaf.hash, leaf)
    refNode(leaf.hash)
    if leaf.nodeType == TrieNodeType.RegularLeaf then
      val vh = Hashing.blake2b256(value)
      pendingValues.update(vh, value)
      refValue(vh)
    leaf.hash

  private def refNode(h: Hash): Unit   = nodeRefDeltas.update(h, nodeRefDeltas(h) + 1)
  private def unrefNode(h: Hash): Unit = nodeRefDeltas.update(h, nodeRefDeltas(h) - 1)
  private def refValue(h: Hash): Unit  = valueRefDeltas.update(h, valueRefDeltas(h) + 1)

  def getKeyValues(prefix: JamBytes, bitsCount: Int): List[(JamBytes, JamBytes)] =
    require(bitsCount >= 0 && bitsCount <= 248, "bitsCount must be in [0, 248]")
    findSubtreeRoot(currentRoot, prefix, bitsCount, depth = 0) match
      case None => Nil
      case Some(node) =>
        val buf = mutable.ArrayBuffer[(JamBytes, JamBytes)]()
        collectLeaves(node, buf)
        buf.toList

  private def findSubtreeRoot(hash: Hash, prefix: JamBytes, bitsCount: Int, depth: Int): Option[TrieNode] =
    getNode(hash).flatMap { node =>
      if depth >= bitsCount then Some(node)
      else
        node.nodeType match
          case TrieNodeType.Branch =>
            val childHash =
              if bitAt(prefix, depth) then Hash.fromByteVectorUnsafe(node.right.toByteVector)
              else Hash.fromByteVectorUnsafe(node.left.toByteVector)
            findSubtreeRoot(childHash, prefix, bitsCount, depth + 1)
          case _ =>
            val leafKey = TrieNode.leafKey(node)
            if prefixMatches(leafKey, prefix, bitsCount) then Some(node) else None
    }

  def prefixMatches(key: JamBytes, prefix: JamBytes, bitsCount: Int): Boolean =
    var i = 0
    while i < bitsCount do
      if bitAt(key, i) != bitAt(prefix, i) then return false
      i += 1
    true

  private def collectLeaves(node: TrieNode, buf: mutable.ArrayBuffer[(JamBytes, JamBytes)]): Unit =
    node.nodeType match
      case TrieNodeType.Branch =>
        getNode(Hash.fromByteVectorUnsafe(node.left.toByteVector)).foreach(collectLeaves(_, buf))
        getNode(Hash.fromByteVectorUnsafe(node.right.toByteVector)).foreach(collectLeaves(_, buf))
      case TrieNodeType.EmbeddedLeaf =>
        TrieNode.embeddedValue(node).foreach { v =>
          buf += ((TrieNode.leafKey(node), v))
        }
      case TrieNodeType.RegularLeaf =>
        val vh = Hash.fromByteVectorUnsafe(node.right.toByteVector)
        pendingValues.get(vh).orElse(backend.readRawValue(vh)).foreach { v =>
          buf += ((TrieNode.leafKey(node), v))
        }

  private[trie] def getNode(hash: Hash): Option[TrieNode] =
    if hash == Hash.zero then None
    else pendingNodes.get(hash).orElse(backend.readNode(hash))

  private[trie] def findLeaf(hash: Hash, key: JamBytes, depth: Int): Option[TrieNode] =
    getNode(hash).flatMap { node =>
      node.nodeType match
        case TrieNodeType.Branch =>
          val childHash =
            if bitAt(key, depth) then Hash.fromByteVectorUnsafe(node.right.toByteVector)
            else Hash.fromByteVectorUnsafe(node.left.toByteVector)
          findLeaf(childHash, key, depth + 1)
        case _ =>
          if TrieNode.leafKey(node).toByteVector == key.toByteVector then Some(node)
          else None
    }

  private[trie] def bitAt(data: JamBytes, position: Int): Boolean =
    val byteIndex = position / 8
    val bitIndex = 7 - (position % 8)
    if byteIndex >= data.length then false
    else (data(byteIndex) & (1 << bitIndex)) != 0

object StateTrie:
  def empty(backend: StateTrieBackend): StateTrie = new StateTrie(backend, Hash.zero)
  def at(backend: StateTrieBackend, root: Hash): StateTrie = new StateTrie(backend, root)
