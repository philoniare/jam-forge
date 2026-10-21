package io.forge.jam.core

import java.util.concurrent.{CompletableFuture, CompletionException, ForkJoinPool}

/**
 * A shared pool for independent work within one block import:
 */
object Parallel:

  private val pool = new ForkJoinPool(math.max(2, Runtime.getRuntime.availableProcessors()))

  /** Starts `f` on the pool. */
  def async[A](f: => A): CompletableFuture[A] = CompletableFuture.supplyAsync(() => f, pool)

  /** Waits for `future`, rethrowing what the task threw rather than a wrapping CompletionException. */
  def await[A](future: CompletableFuture[A]): A =
    try future.join()
    catch case e: CompletionException if e.getCause != null => throw e.getCause

  /** `xs.map(f)` with every element but the last on the pool, and the last on the calling thread. */
  def map[A, B](xs: List[A])(f: A => B): List[B] =
    if xs.lengthCompare(1) <= 0 then xs.map(f)
    else
      val started = xs.init.map(x => async(f(x)))
      val last = f(xs.last)
      started.map(await) :+ last
