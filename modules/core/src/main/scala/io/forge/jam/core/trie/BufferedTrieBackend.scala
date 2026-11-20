package io.forge.jam.core.trie

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash

import java.util.concurrent.ConcurrentHashMap

final class BufferedTrieBackend(val underlying: StateTrieBackend) extends StateTrieBackend:
  import BufferedTrieBackend.Buffer

  @volatile private var buffer: Buffer | Null = null

  def begin(): Unit =
    if buffer != null then throw new IllegalStateException("BufferedTrieBackend.begin: already buffering")
    buffer = new Buffer

  def buffering: Boolean = buffer != null

  def flush(): Unit =
    val b = buffer
    if b != null then
      underlying.batchUpdate(b.net())
      buffer = null

  def discard(): Unit = buffer = null

  def readNode(hash: Hash): Option[TrieNode] =
    val b = buffer
    val held = if b == null then null else b.node(hash)
    if held != null then Some(held) else underlying.readNode(hash)

  override def readsInBulk: Boolean = underlying.readsInBulk

  override def prefetchNodes(hashes: IndexedSeq[Hash]): Unit =
    val b = buffer
    underlying.prefetchNodes(if b == null then hashes else hashes.filter(h => b.node(h) == null))

  def readRawValue(hash: Hash): Option[JamBytes] =
    val b = buffer
    val held = if b == null then null else b.value(hash)
    if held != null then Some(held) else underlying.readRawValue(hash)

  def batchUpdate(ops: Seq[BackendOp]): Unit =
    val b = buffer
    if b == null then underlying.batchUpdate(ops) else b.take(ops)

  def gc(): Unit = underlying.gc()

  def clear(): Unit =
    if buffer != null then buffer = new Buffer
    underlying.clear()

object BufferedTrieBackend:

  private final class NodeEntry:
    @volatile var payload: TrieNode | Null = null
    var delta = 0L
    var counted = false
    var replacedValue: Hash | Null = null

  private final class ValueEntry:
    @volatile var payload: JamBytes | Null = null
    var delta = 0L
    var counted = false

  private def valueOf(node: TrieNode | Null): Hash | Null =
    if node != null && node.nodeType == TrieNodeType.RegularLeaf then Hash.wrap(node.right.toArrayUnsafe) else null

  private final class Buffer:
    private val nodes = new ConcurrentHashMap[Hash, NodeEntry]()
    private val values = new ConcurrentHashMap[Hash, ValueEntry]()

    def node(h: Hash): TrieNode | Null =
      val e = nodes.get(h)
      if e == null then null else e.payload

    def value(h: Hash): JamBytes | Null =
      val e = values.get(h)
      if e == null then null else e.payload

    private def nodeEntry(h: Hash): NodeEntry = nodes.computeIfAbsent(h, _ => new NodeEntry)
    private def valueEntry(h: Hash): ValueEntry = values.computeIfAbsent(h, _ => new ValueEntry)

    private def release(value: Hash | Null): Unit =
      if value != null then
        val e = valueEntry(value)
        e.delta -= 1
        e.counted = true

    def take(ops: Seq[BackendOp]): Unit =
      ops.foreach {
        case BackendOp.WriteNode(node) => nodeEntry(node.hash).payload = node
        case BackendOp.NodeRefDelta(h, d, replaced) =>
          val e = nodeEntry(h)
          e.counted = true
          var left = d
          while left > 0 do
            if e.delta < 0 then release(valueOf(e.payload))
            e.delta += 1
            left -= 1
          while left < 0 do
            if e.delta > 0 then release(valueOf(e.payload))
            else if replaced != null then e.replacedValue = replaced
            e.delta -= 1
            left += 1
        case BackendOp.WriteRawValue(h, v) => valueEntry(h).payload = v
        case BackendOp.RawValueRefDelta(h, d) =>
          val e = valueEntry(h)
          e.delta += d
          e.counted = true
      }

    def net(): Seq[BackendOp] =
      val out = Vector.newBuilder[BackendOp]
      nodes.forEach { (h, e) =>
        val payload = e.payload
        if payload != null && (e.delta > 0 || !e.counted) then out += BackendOp.WriteNode(payload)
        if e.delta > 0 then out += BackendOp.NodeRefDelta(h, e.delta)
        else if e.delta < 0 then out += BackendOp.NodeRefDelta(h, e.delta, e.replacedValue)
      }
      values.forEach { (h, e) =>
        val payload = e.payload
        if payload != null && (e.delta > 0 || !e.counted) then out += BackendOp.WriteRawValue(h, payload)
        if e.delta != 0 then out += BackendOp.RawValueRefDelta(h, e.delta)
      }
      out.result()
