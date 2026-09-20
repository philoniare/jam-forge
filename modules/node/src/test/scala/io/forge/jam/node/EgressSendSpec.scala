package io.forge.jam.node

import java.util.concurrent.{CompletableFuture, CountDownLatch, ExecutorService, Executors, TimeUnit}
import java.util.concurrent.atomic.AtomicReference

import io.forge.jam.network.{JamnpStream, StreamKind}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class EgressSendSpec extends AnyFunSuite with Matchers:

  private def egressExecutor(): ExecutorService =
    Executors.newSingleThreadExecutor(r =>
      val t = new Thread(r, "jam-egress"); t.setDaemon(true); t
    )

  test("submitSend returns immediately and runs the blocking wait on the egress thread") {
    val egress = egressExecutor()
    try
      val dist = new DistributionService(new ExtrinsicPools, coresCount = 1, egress)

      val never = new CompletableFuture[JamnpStream]()
      val runThread = new AtomicReference[String]()
      val opened = new CountDownLatch(1)

      val t0 = System.nanoTime()
      dist.submitSend(
        () =>
          runThread.set(Thread.currentThread().getName)
          opened.countDown()
          never,
        StreamKind.WorkReportDistribution,
        Array[Byte](1, 2, 3)
      )
      val submitNanos = System.nanoTime() - t0
      submitNanos should be < 1_000_000_000L
      opened.await(5, TimeUnit.SECONDS) shouldBe true
      runThread.get() shouldBe "jam-egress"
    finally
      egress.shutdownNow()
  }
