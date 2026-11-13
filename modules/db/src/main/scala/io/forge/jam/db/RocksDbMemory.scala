package io.forge.jam.db

import java.util.concurrent.{ConcurrentHashMap, Executors, ScheduledFuture, TimeUnit}

import org.rocksdb.{BlockBasedTableConfig, Cache, ColumnFamilyHandle, DBOptions, FlushOptions, LRUCache, RocksDB, WriteBufferManager}

/** One memory budget for every RocksDB store a process opens (trie, blocks, shards).
  *
  * Left to its defaults each column family may hold two 64 MiB write buffers and each store its own block cache,
  * with index and filter blocks outside any cache and growing with the data: about a dozen column families make that
  * well over a gigabyte of native memory that no JVM limit covers. Here every store shares one block cache, write
  * buffers are flushed once their total passes a limit and are charged to the cache, and index and filter blocks
  * live in the cache too, so RocksDB's memory stays close to `cacheBytes` however much is stored.
  *
  * RocksDB applies the write-buffer limit only to the store being written, and to one column family of it that
  * need not be the full one: a store that filled its buffers and then went quiet keeps them, and every write to
  * another store is then over the limit and flushes a nearly empty buffer. So the budget also looks at the stores
  * it is given ([[watch]]) a few times a second and, while their buffers together are over the limit, flushes
  * the store holding the most.
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

  /** One watched store. Its monitor is held whenever the database is used here and when it is taken off the
    * watch, so a store that closes is never touched afterwards.
    */
  private final class Watched(db: RocksDB, handles: Seq[ColumnFamilyHandle]) extends AutoCloseable:
    private var open = true

    /** Bytes in the store's active write buffers; 0 once it is off the watch. */
    def activeBytes: Long = synchronized {
      if !open then 0L
      else handles.map(h => db.getLongProperty(h, "rocksdb.cur-size-active-mem-table")).sum
    }

    /** Start flushing every column family's write buffer, without waiting for it. */
    def flush(): Unit = synchronized {
      if open then
        val options = new FlushOptions().setWaitForFlush(false)
        try db.flush(options, java.util.List.copyOf(java.util.Arrays.asList(handles*)))
        finally options.close()
    }

    override def close(): Unit =
      synchronized { open = false }
      unwatch(this)

  private val watching = ConcurrentHashMap.newKeySet[Watched]()
  private var check: ScheduledFuture[?] = null

  /** Keep `db`'s write buffers within the shared limit even while it is not being written. Close the result
    * before closing the database.
    */
  def watch(db: RocksDB, handles: Seq[ColumnFamilyHandle]): AutoCloseable = synchronized {
    val w = new Watched(db, handles)
    watching.add(w)
    if check == null then
      check = RocksDbMemory.timer.scheduleWithFixedDelay(
        () => relieve(), RocksDbMemory.CheckMillis, RocksDbMemory.CheckMillis, TimeUnit.MILLISECONDS
      )
    w
  }

  private def unwatch(w: Watched): Unit = synchronized {
    watching.remove(w)
    if watching.isEmpty && check != null then
      check.cancel(false)
      check = null
  }

  /** While the watched stores' active write buffers are together over the limit RocksDB itself flushes at, flush
    * the store holding the most.
    */
  private def relieve(): Unit =
    try
      val sizes = watching.toArray(new Array[Watched](0)).map(w => w -> w.activeBytes)
      if sizes.nonEmpty && sizes.map(_._2).sum > writeBufferBytes / 8 * 7 then sizes.maxBy(_._2)._1.flush()
    catch case scala.util.control.NonFatal(_) => () // a store closing under us, or a flush already running

object RocksDbMemory:

  /** How often a budget looks at its watched stores. */
  private val CheckMillis = 100L

  /** One daemon thread for every budget's checks. */
  private lazy val timer = Executors.newSingleThreadScheduledExecutor { r =>
    val t = new Thread(r, "rocksdb-memory")
    t.setDaemon(true)
    t
  }

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
