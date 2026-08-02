package io.forge.jam.protocol.authorization

import io.forge.jam.core.constants
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
    val authsByCoreIndex = input.auths.groupBy(_.core.toInt)

    // Update pools for each core according to the Gray Paper formula
    val updatedPools = preState.authPools.zipWithIndex.map {
      case (pool, coreIndex) =>
        val coreQueue = preState.authQueues(coreIndex)

        // Step 1: F(c) - Remove consumed authorizers from the pool
        // For each consumed auth, remove first matching hash from pool
        val consumedHashes = authsByCoreIndex.getOrElse(coreIndex, List.empty).map(_.authHash)
        val poolAfterRemoval = consumedHashes.foldLeft(pool) { (currentPool, hashToRemove) =>
          val idx = currentPool.indexWhere(_ == hashToRemove)
          if idx >= 0 then
            currentPool.take(idx) ++ currentPool.drop(idx + 1)
          else
            currentPool
        }

        // Step 2: Append new item from queue at cyclic position slot % Q.
        require(
          coreQueue.size == constants.Q,
          s"authQueue for core $coreIndex has size ${coreQueue.size}, expected ${constants.Q}"
        )
        val queueIndex = (input.slot % constants.Q).toInt
        val newItem = coreQueue(queueIndex)
        val poolWithNew = poolAfterRemoval :+ newItem

        // Step 3: Take rightmost O items (i.e., drop from front if size > O)
        poolWithNew.takeRight(constants.O)
    }

    AuthState(
      authPools = updatedPools,
      authQueues = preState.authQueues
    )
