package io.forge.jam.protocol.accumulation

import io.forge.jam.core.primitives.Hash
import io.forge.jam.protocol.accumulation.AccumulationTransition.PrivilegeSnapshot
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable
import scala.util.Random

/** The per-core auth-queue merge after a batch: core c takes its queue from the post-state of the service that was
  * c's assigner before the batch, else keeps the prior queue. No trace vector in the corpus changes the auth queues,
  * so this checks the merge against the plain per-core definition on generated inputs.
  */
class AuthQueueReconcileSpec extends AnyFunSuite with Matchers:

  private def queue(rnd: Random): List[Hash] =
    List.fill(rnd.nextInt(3))(Hash(Array.fill[Byte](32)(rnd.nextInt(4).toByte)))

  private def state(authQueue: Vector[List[Hash]], assigners: List[Long]): PartialState =
    PartialState(
      accounts = Map.empty,
      stagingSet = mutable.ListBuffer.empty,
      authQueue = authQueue,
      manager = 1L,
      assigners = mutable.ListBuffer.from(assigners),
      delegator = 2L,
      registrar = 3L,
      alwaysAccers = mutable.Map.empty
    )

  private def snapshot(authQueues: Vector[List[Hash]], assigners: List[Long]): PrivilegeSnapshot =
    PrivilegeSnapshot(1L, 2L, 3L, assigners, Map.empty, authQueues = authQueues)

  /** The definition, core by core. */
  private def expected(prior: PartialState, snapshots: Map[Long, PrivilegeSnapshot]): List[List[Hash]] =
    prior.assigners.toList.zipWithIndex.map { case (assigner, core) =>
      snapshots.get(assigner).flatMap(_.authQueues.lift(core)).getOrElse(prior.authQueue.lift(core).getOrElse(Nil))
    }

  test("merged auth queues equal the per-core definition, for any shapes of prior queues and snapshots") {
    val rnd = new Random(20261112L)
    for _ <- 0 until 3000 do
      val cores = rnd.nextInt(6)
      val assigners = List.fill(cores)(10L + rnd.nextInt(4))
      val prior = state(Vector.fill(rnd.nextInt(8))(queue(rnd)), assigners)
      val snapshots = (1L :: (10L to 13L).toList).filter(_ => rnd.nextBoolean()).map { service =>
        // A service's post-state: the prior queues with some cores rewritten, sometimes shorter or longer.
        val post = Vector.tabulate(rnd.nextInt(8))(c =>
          if rnd.nextInt(3) == 0 then queue(rnd) else prior.authQueue.lift(c).getOrElse(queue(rnd))
        )
        service -> snapshot(post, assigners)
      }.toMap

      val merged = AccumulationTransition.reconcilePrivileges(prior, snapshots).authQueues
      merged.toList shouldBe expected(prior, snapshots)
  }

  test("a batch that writes no queue hands back the prior vector itself") {
    val rnd = new Random(7L)
    val assigners = List(10L, 11L, 10L)
    val prior = state(Vector.fill(3)(queue(rnd)), assigners)
    // Service 10 ran and changed nothing: its post-state shares the prior queues.
    val snapshots = Map(10L -> snapshot(prior.authQueue, assigners))
    assert(AccumulationTransition.reconcilePrivileges(prior, snapshots).authQueues eq prior.authQueue)
    assert(AccumulationTransition.reconcilePrivileges(prior, Map.empty).authQueues eq prior.authQueue)
  }

  test("only the cores the prior assigner rewrote are replaced; a non-assigner's writes are ignored") {
    val rnd = new Random(8L)
    val assigners = List(10L, 11L, 10L)
    val prior = state(Vector.fill(3)(List(Hash(Array.fill[Byte](32)(1.toByte)))), assigners)
    val written = List(Hash(Array.fill[Byte](32)(9.toByte)))
    val snapshots = Map(
      10L -> snapshot(prior.authQueue.updated(2, written), assigners),
      // 12 is nobody's assigner; 11 is core 1's assigner but rewrote core 0, which it does not own.
      12L -> snapshot(Vector.fill(3)(queue(rnd)), assigners),
      11L -> snapshot(prior.authQueue.updated(0, written), assigners)
    )
    val merged = AccumulationTransition.reconcilePrivileges(prior, snapshots).authQueues
    merged shouldBe prior.authQueue.updated(2, written)
    assert(merged(0) eq prior.authQueue(0))
    assert(merged(1) eq prior.authQueue(1))
  }
