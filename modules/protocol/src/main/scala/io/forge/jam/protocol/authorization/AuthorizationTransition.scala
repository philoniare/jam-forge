package io.forge.jam.protocol.authorization

import io.forge.jam.core.{ChainConfig, constants}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.protocol.authorization.AuthorizationTypes.*

/**
 * Authorization State Transition Function.
 *
 * Manages authorization pools per core, handling authorization consumption
 * from guarantee extrinsics and queue rotation based on timeslot.
 */
object AuthorizationTransition:

  /**
   * Execute the Authorization STF.
   *
   * @param input The authorization input containing slot and consumed authorizations.
   * @param preState The pre-transition state.
   * @param config The chain configuration.
   * @return The post-transition state.
   */
  def stf(input: AuthInput, preState: AuthState, config: ChainConfig): AuthState =
    // Group authorizations by core index
    val authsByCoreIndex = input.auths.groupBy(_.core.toInt)

    // Update pools for each core
    val updatedPools = preState.authPools.zipWithIndex.map { case (pool, coreIndex) =>
      val coreQueue = preState.authQueues(coreIndex)
      if coreQueue.isEmpty then
        pool
      else
        // Create mutable pool for modifications
        val mutablePool = scala.collection.mutable.ListBuffer.from(pool)

        // Handle authorizations for this core - remove consumed ones
        authsByCoreIndex.get(coreIndex).foreach { auths =>
          for auth <- auths do
            val idx = mutablePool.indexWhere(h => h == auth.authHash)
            if idx >= 0 then
              mutablePool.remove(idx)
        }

        if mutablePool.isEmpty then
          // If pool is empty after consumption, add a zero hash
          mutablePool += Hash.zero
        else
          // Check if all remaining items are zero hashes
          val allAreZero = mutablePool.forall(_ == Hash.zero)
          if allAreZero then
            // Add another zero hash if all are zeros
            mutablePool += Hash.zero
          else
            // Get new item from queue using rotation
            val queueIndex = input.slot.toInt % coreQueue.size
            val newItem = coreQueue(queueIndex)
            mutablePool += newItem

            // Ensure size constraints - remove oldest items from front
            while mutablePool.size > constants.O do
              mutablePool.remove(0)

        mutablePool.toList
    }

    AuthState(
      authPools = updatedPools,
      authQueues = preState.authQueues
    )
