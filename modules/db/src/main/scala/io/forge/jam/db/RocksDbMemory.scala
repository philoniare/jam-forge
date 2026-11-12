package io.forge.jam.db

import org.rocksdb.{BlockBasedTableConfig, Cache, DBOptions, LRUCache, RocksDB, WriteBufferManager}

/** One memory budget for every RocksDB store a process opens (trie, blocks, shards).
  *
  * Left to its defaults each column family may hold two 64 MiB write buffers and each store its own block cache,
  * with index and filter blocks outside any cache and growing with the data: about a dozen column families make that
  * well over a gigabyte of native memory that no JVM limit covers. Here every store shares one block cache, write
  * buffers are flushed once their total passes a limit and are charged to the cache, and index and filter blocks
  * live in the cache too, so RocksDB's memory stays close to `cacheBytes` however much is stored.
  *
  * @param cacheBytes        capacity of the shared block cache, which also accounts for the write buffers
  * @param writeBufferBytes  total size of all write buffers at which the largest is flushed
  */
final class RocksDbMemory(val cacheBytes: Long, val writeBufferBytes: Long):
  RocksDB.loadLibrary()

  /** Shared by every table of every store opened with this budget. Lives as long as the process. */
  val blockCache: Cache = new LRUCache(cacheBytes)

  private val writeBuffers = new WriteBufferManager(writeBufferBytes, blockCache)

  /** Table settings for a column family: blocks, indexes and filters all in the shared cache. */
  def tableConfig(): BlockBasedTableConfig =
    new BlockBasedTableConfig()
      .setBlockCache(blockCache)
      .setCacheIndexAndFilterBlocks(true)
      // The newest files are read most: keeping their index and filter pinned saves a lookup, and level 0 is small.
      .setPinL0FilterAndIndexBlocksInCache(true)

  /** Put a store's write buffers under the shared limit. */
  def apply(options: DBOptions): DBOptions = options.setWriteBufferManager(writeBuffers)

  /** Bytes the cache holds now: data, index and filter blocks and the write buffers charged to it. */
  def usedBytes: Long = blockCache.getUsage

object RocksDbMemory:

  private def bytes(property: String, default: Long): Long =
    sys.props.get(property).flatMap(_.toLongOption).filter(_ > 0).getOrElse(default)

  /** Default budget: 256 MiB in all, of which write buffers may take 96 MiB before a flush.
    * `-Djam.db.cacheBytes` and `-Djam.db.writeBufferBytes` change it.
    */
  lazy val shared: RocksDbMemory =
    new RocksDbMemory(
      cacheBytes = bytes("jam.db.cacheBytes", 256L * 1024 * 1024),
      writeBufferBytes = bytes("jam.db.writeBufferBytes", 96L * 1024 * 1024)
    )
