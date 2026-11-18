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

  test("a failure that keeps repeating is logged at warn again once a period, with the count since the last line") {
    val logger = org.slf4j.LoggerFactory.getLogger(classOf[Periodic]).asInstanceOf[ch.qos.logback.classic.Logger]
    val seen = new ch.qos.logback.core.read.ListAppender[ch.qos.logback.classic.spi.ILoggingEvent]()
    seen.start()
    logger.addAppender(seen)
    try
      var now = 1_000_000L
      val guard = new Periodic("state store sync", repeatLogMillis = 60_000L, clock = () => now)
      def failing(): Unit = guard.run(throw new java.io.IOException("No space left on device"))
      def lines(level: ch.qos.logback.classic.Level) =
        seen.list.toArray.toList.collect {
          case e: ch.qos.logback.classic.spi.ILoggingEvent if e.getLevel == level => e.getFormattedMessage
        }
      import ch.qos.logback.classic.Level.{ERROR, WARN}

      failing()
      lines(ERROR).size shouldBe 1
      (1 to 11).foreach { _ => now += 5_000L; failing() }
      lines(WARN) shouldBe Nil
      now += 5_000L
      failing()
      lines(WARN).size shouldBe 1
      lines(WARN).head should include("state store sync")
      lines(WARN).head should include("12 more failure(s)")
      lines(WARN).head should include("No space left on device")
      now += 1_000L
      failing()
      lines(WARN).size shouldBe 1
      now += 1_000L
      guard.run(throw new IllegalStateException("store closed"))
      lines(ERROR).size shouldBe 2
      now += 60_000L
      failing()
      lines(WARN).size shouldBe 2
      lines(WARN)(1) should include("1 more failure(s)")
      lines(ERROR).size shouldBe 2
      guard.run(())
      lines(WARN).size shouldBe 2
    finally logger.detachAppender(seen): Unit
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
