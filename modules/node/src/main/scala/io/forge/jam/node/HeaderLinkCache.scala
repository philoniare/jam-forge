package io.forge.jam.node

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong

import io.forge.jam.core.LittleEndian
import io.forge.jam.core.primitives.Hash

/** The fields of a stored header that the ancestor walk reads: its slot, its parent's hash and its prior state root
  * (the parent's posterior state root).
  */
private[node] final case class HeaderLink(slot: Long, parent: Hash, priorStateRoot: Hash)

private[node] object HeaderLink:
  /** Parse stored header bytes (parent ++ prior state root ++ extrinsic hash ++ E_4(slot) ++ ...); None when too short
    * to carry a slot, which ends an ancestor walk.
    */
  def parse(header: Array[Byte]): Option[HeaderLink] =
    if header.length < 100 then None
    else
      Some(
        HeaderLink(
          LittleEndian.get(header, 96, 4),
          Hash(java.util.Arrays.copyOfRange(header, 0, 32)),
          Hash(java.util.Arrays.copyOfRange(header, 32, 64))
        )
      )

/** In-memory index of stored headers by hash, so the per-import ancestor walk does map lookups instead of RocksDB
  * reads.
  *
  * Exactness: a header hash is the hash of the header bytes, so the bytes stored under a hash never change and an
  * entry is a pure function of the store. Only headers present in the store are cached (filled after `putBlock` or
  * on a read that found the header), and a miss falls back to `load`, so results equal reading the store directly.
  * Headers are never deleted from the node's block store; a caller deleting one must call [[invalidate]].
  *
  * Memory: entries more than `window` slots behind the newest validated slot are swept once the map passes a high-water
  * mark that doubles with the live size, so the sweep is amortised O(1) per insert and the map holds about the
  * headers of the last `window` slots (on every fork).
  *
  * Thread safety: a ConcurrentHashMap of immutable entries plus an atomic newest slot; any interleaving of lookups,
  * fills and sweeps only changes whether a lookup hits, never what it returns.
  */
private[node] final class HeaderLinkCache(window: Long, load: Hash => Option[Array[Byte]]):
  private val links = new ConcurrentHashMap[Hash, HeaderLink]()
  private val newestSlot = new AtomicLong(Long.MinValue)
  private val minHighWater = math.max(1024L, 2L * (window + 1L))
  @volatile private var highWater: Long = minHighWater

  /** The header link of `h`, from memory or else from the store (and then cached). */
  def get(h: Hash): Option[HeaderLink] =
    val cached = links.get(h)
    if cached != null then Some(cached)
    else
      val loaded = load(h).flatMap(HeaderLink.parse)
      loaded.foreach(put(h, _))
      loaded

  /** Record a header just written to the store. */
  def record(h: Hash, header: Array[Byte]): Unit =
    HeaderLink.parse(header).foreach(put(h, _))

  /** Forget `h` (its header left the store). */
  def invalidate(h: Hash): Unit =
    links.remove(h)
    ()

  /** Advance the eviction horizon to `slot`. Only trusted slots (a validated best head) move it, so an unvalidated
    * header claiming a far-future slot cannot flush the cache.
    */
  def advance(slot: Long): Unit =
    newestSlot.accumulateAndGet(slot, math.max)
    ()

  def size: Int = links.size

  private[node] def clear(): Unit = links.clear()

  private def put(h: Hash, link: HeaderLink): Unit =
    links.put(h, link)
    if links.size > highWater then sweep()

  /** Drop entries more than `window` slots behind the eviction horizon. */
  private[node] def sweep(): Unit =
    val newest = newestSlot.get
    if newest != Long.MinValue then
      val cutoff = newest - window
      links.values().removeIf(_.slot < cutoff)
    highWater = math.max(minHighWater, 2L * links.size)
