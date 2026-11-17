package io.forge.jam.node

import java.util.concurrent.{Executors, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger

import io.forge.jam.core.ChainConfig
import io.forge.jam.core.codec.JamCodecs.encode
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PeriodicTasksSpec extends AnyFunSuite with Matchers with NodeFixture:

  private def eventually(what: String, seconds: Int = 10)(cond: => Boolean): Unit =
    val deadline = System.currentTimeMillis() + seconds * 1000L
    while !cond && System.currentTimeMillis() < deadline do Thread.sleep(10)
    if !cond then fail(s"timed out waiting for: $what")

  test("a guarded periodic task runs again after it throws, and a failure repeated is counted once") {
    val exec = Executors.newSingleThreadScheduledExecutor()
    try
      val runs = new AtomicInteger()
      val guard = new Periodic("test task")
      exec.scheduleWithFixedDelay(
        guard {
          val n = runs.incrementAndGet()
          if n <= 3 then throw new IllegalStateException("store closed")
          if n == 4 then throw new AssertionError("not an Exception")
        },
        1,
        1,
        TimeUnit.MILLISECONDS
      )
      eventually("the task to run after it threw")(runs.get > 6)
      guard.distinctFailures shouldBe 2
    finally exec.shutdownNow(): Unit
  }

  test("connectivity upkeep keeps running after a step throws") {
    val genesis = loadGenesis().getOrElse(cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available"))
    val spec = ChainSpec(
      id = "periodic-tasks",
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )
    val dir = tempDir("jam-periodic")
    var node: JamNode = null
    val threads = new NodeThreads
    try
      node = new JamNode(spec, NodeConfig(dataDir = dir, slotTicking = false))
      node.chain.initializeOrRestore(spec)
      val reads = new AtomicInteger()
      val slot: () => Long = () =>
        if reads.incrementAndGet() == 1 then throw new AssertionError("slot clock failed") else 0L
      val peers = new PeerConnectivity(
        node.identity,
        node.network,
        node.chain,
        node.sync,
        node.distribution,
        spec,
        NodeConfig(dataDir = dir, slotTicking = false),
        currentSlot = slot,
        upkeep = () => (),
        threads = threads
      )
      peers.start()
      try eventually("upkeep to run again after it threw")(reads.get >= 3)
      finally peers.shutdown()
    finally
      threads.close()
      if node != null then node.shutdownStorageOnly()
      cleanup(dir)
  }
