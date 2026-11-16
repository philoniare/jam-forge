package io.forge.jam.protocol.authorization

import io.forge.jam.core.constants
import io.forge.jam.core.primitives.Hash
import io.forge.jam.protocol.authorization.AuthorizationTypes.*
import io.forge.jam.protocol.state.TrieBackedJamState
import io.forge.jam.protocol.state.TrieBackedJamStateBridges.AuthorizationBridge

/**
 * Authorization State Transition Function.
 *
 * Manages authorization pools per core, handling authorization consumption
 * from guarantee extrinsics and queue rotation based on timeslot.
 */
object AuthorizationTransition:

  /**
   * Execute the Authorization STF using unified JamState.
   *
   * Reads: authPools, authQueues
   * Writes: authPools, authQueues
   *
   * @param input The authorization input containing slot and consumed authorizations.
   * @param state The unified JamState.
   * @param config The chain configuration.
   * @return The updated JamState.
   */
  def stfView(
      input: AuthInput,
      view: TrieBackedJamState
  ): Unit =
    val preState = AuthorizationBridge.extract(view)
    val postState = stfInternal(input, preState)
    AuthorizationBridge.apply(view, postState)

  /**
   * Internal Authorization STF implementation using AuthState.
   *
   * @param input The authorization input containing slot and consumed authorizations.
   * @param preState The pre-transition state.
   * @param config The chain configuration.
   * @return The post-transition state.
   */
  def stfInternal(input: AuthInput, preState: AuthState): AuthState =
    // Group authorizations by core index
    val authsByCoreIndex =
      if input.auths.isEmpty then Map.empty[Int, List[Auth]] else input.auths.groupBy(_.core.toInt)

    val queues = indexedQueues(preState.authQueues)
    val queueIndex = (input.slot % constants.Q).toInt

    // Update pools for each core according to the Gray Paper formula
    val updatedPools = preState.authPools.iterator.zipWithIndex.map {
      case (pool, coreIndex) =>
        // Step 1: F(c) - Remove consumed authorizers from the pool
        // For each consumed auth, remove first matching hash from pool
        val poolAfterRemoval = authsByCoreIndex.get(coreIndex) match
          case None => pool
          case Some(auths) =>
            auths.foldLeft(pool) { (currentPool, auth) =>
              val idx = currentPool.indexWhere(_ == auth.authHash)
              if idx >= 0 then currentPool.take(idx) ++ currentPool.drop(idx + 1)
              else currentPool
            }

        appendKeepingNewest(poolAfterRemoval, queues(coreIndex)(queueIndex))
    }.toList

    AuthState(
      authPools = updatedPools,
      authQueues = preState.authQueues
    )

  private def appendKeepingNewest(pool: List[Hash], item: Hash): List[Hash] =
    var size = 0
    var allItem = true
    var rest = pool
    while rest.nonEmpty do
      if allItem && rest.head != item then allItem = false
      size += 1
      rest = rest.tail
    if allItem && size == constants.O then pool
    else (pool :+ item).takeRight(constants.O)

  private final class IndexedQueues(val source: List[List[Hash]], val queues: Vector[Vector[Hash]])

  @volatile private var lastIndexed: IndexedQueues = null

  private def indexedQueues(authQueues: List[List[Hash]]): Vector[Vector[Hash]] =
    val seen = lastIndexed
    if seen != null && (seen.source eq authQueues) then seen.queues
    else
      val queues = authQueues.iterator.map(_.toVector).toVector
      queues.indices.foreach { coreIndex =>
        require(
          queues(coreIndex).size == constants.Q,
          s"authQueue for core $coreIndex has size ${queues(coreIndex).size}, expected ${constants.Q}"
        )
      }
      lastIndexed = new IndexedQueues(authQueues, queues)
      queues
