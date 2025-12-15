package io.forge.jam.protocol.authorization

import io.forge.jam.core.{ChainConfig, constants}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.protocol.authorization.AuthorizationTypes.*
import io.forge.jam.protocol.state.JamState
import monocle.syntax.all.*
import org.slf4j.LoggerFactory

/**
 * Authorization State Transition Function.
 *
 * Manages authorization pools per core, handling authorization consumption
 * from guarantee extrinsics and queue rotation based on timeslot.
 */
object AuthorizationTransition:
  private val logger = LoggerFactory.getLogger(getClass)

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
  def stf(input: AuthInput, state: JamState, config: ChainConfig): JamState =
    // Extract AuthState using lens bundle
    val preState = JamState.AuthorizationLenses.extract(state)

    val postState = stfInternal(input, preState, config)

    // Apply results back using lens bundle
    JamState.AuthorizationLenses.apply(state, postState)

  /**
   * Internal Authorization STF implementation using AuthState.
   *
   * @param input The authorization input containing slot and consumed authorizations.
   * @param preState The pre-transition state.
   * @param config The chain configuration.
   * @return The post-transition state.
   */
  def stfInternal(input: AuthInput, preState: AuthState, config: ChainConfig): AuthState =
    // Group authorizations by core index
    val authsByCoreIndex = input.auths.groupBy(_.core.toInt)

    // Update pools for each core according to the Gray Paper formula
    val updatedPools = preState.authPools.zipWithIndex.map {
      case (pool, coreIndex) =>
        val coreQueue = preState.authQueues(coreIndex)

        // Step 1: F(c) - Remove consumed authorizers from the pool
        val mutablePool = scala.collection.mutable.ListBuffer.from(pool)
        val prePoolSize = mutablePool.size
        authsByCoreIndex.get(coreIndex).foreach { auths =>
          for auth <- auths do
            val idx = mutablePool.indexWhere(h => h == auth.authHash)
            if idx >= 0 then
              mutablePool.remove(idx)
        }
        val afterRemoveSize = mutablePool.size

        // Step 2: Append new item from queue at cyclic position slot % queue_size
        if coreQueue.nonEmpty then
          val queueIndex = (input.slot % coreQueue.size).toInt
          val newItem = coreQueue(queueIndex)
          if coreIndex < 2 then
            logger.debug(s"Core $coreIndex: adding queue[$queueIndex] = ${newItem.toHex.take(16)}...")
          mutablePool += newItem

        // Step 3: Take rightmost O items (i.e., drop from front if size > O)
        while mutablePool.size > constants.O do
          mutablePool.remove(0)

        mutablePool.toList
    }

    AuthState(
      authPools = updatedPools,
      authQueues = preState.authQueues
    )
