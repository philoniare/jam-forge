package io.forge.jam.node

import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, TimeUnit}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import RefinePool.Priority

class RefinePoolSpec extends AnyFunSuite with Matchers:

  test("waiting work runs audits first, then co-signing, then submissions, in arrival order within each") {
    val pool = new RefinePool("refine-spec", threads = 1)
    try
      val busy = new CountDownLatch(1)
      val started = new CountDownLatch(1)
      pool.submit(Priority.Submission) { started.countDown(); busy.await() }
      started.await(5, TimeUnit.SECONDS) shouldBe true

      val ran = new ConcurrentLinkedQueue[String]()
      val done = new CountDownLatch(6)
      def task(p: Priority, name: String): Unit = pool.submit(p) { ran.add(name); done.countDown() }
      task(Priority.Submission, "s1")
      task(Priority.CoSign, "c1")
      task(Priority.Audit, "a1")
      task(Priority.Submission, "s2")
      pool.at(Priority.Audit).execute(() => { ran.add("a2"); done.countDown() })
      task(Priority.CoSign, "c2")
      pool.queued shouldBe 6

      busy.countDown()
      done.await(5, TimeUnit.SECONDS) shouldBe true
      ran.toArray.toList shouldBe List("a1", "a2", "c1", "c2", "s1", "s2")
    finally pool.shutdownNow()
  }

  test("a task that throws does not take its thread down") {
    val pool = new RefinePool("refine-spec", threads = 1)
    try
      val ok = new CountDownLatch(1)
      pool.submit(Priority.Audit)(throw new IllegalStateException("boom"))
      pool.submit(Priority.Audit)(ok.countDown())
      ok.await(5, TimeUnit.SECONDS) shouldBe true
    finally pool.shutdownNow()
  }
