package io.forge.jam.db

class RocksDbTrieBackendSwitchedSpec extends RocksDbTrieBackendSpec:
  private val defaults = RocksDbTrieBackend.Settings()

  override protected val settings: RocksDbTrieBackend.Settings = RocksDbTrieBackend.Settings(
    journalValueHashes = !defaults.journalValueHashes,
    manualWalFlush = !defaults.manualWalFlush,
    manualWalFlushBytes = 64L << 10,
    partitionedIndexFilters = !defaults.partitionedIndexFilters,
    nodeFiltersOutsideCache = !defaults.nodeFiltersOutsideCache,
    hashIndex = !defaults.hashIndex,
    blobValues = !defaults.blobValues,
    backgroundJobs = if defaults.backgroundJobs == 0 then 4 else 0,
    subcompactions = if defaults.subcompactions == 0 then 2 else 0,
    storageThreads = if defaults.storageThreads == 0 then 3 else 0
  )

  override protected val memory: RocksDbMemory =
    val d = RocksDbMemory.Settings()
    new RocksDbMemory(d.cacheBytes, d.writeBufferBytes, if d.highPriorityPoolRatio == 0.0 then 0.5 else 0.0)
