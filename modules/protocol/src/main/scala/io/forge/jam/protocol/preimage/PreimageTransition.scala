package io.forge.jam.protocol.preimage

import cats.syntax.all.*
import io.forge.jam.core.JamBytes.compareUnsigned
import io.forge.jam.core.{JamBytes, Hashing, StfResult}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.extrinsic.Preimage
import io.forge.jam.core.types.preimage.PreimageHash
import io.forge.jam.protocol.preimage.PreimageTypes.*
import io.forge.jam.protocol.accumulation.StateKey
import io.forge.jam.protocol.state.JamState

/**
 * Preimages State Transition Function.
 *
 * Manages preimage storage and retrieval by service account:
 * - Validates preimage was solicited (lookup entry exists with empty timestamp list)
 * - Checks sorted/unique ordering by (requester, blake2b hash)
 * - Stores preimage data by service account
 * - Updates lookup metadata with submission timestamp
 * - Updates service statistics with provided count and size
 */
object PreimageTransition:

  /**
   * Compare two preimages by (requester, blob).
   * Returns negative if first < second, 0 if equal, positive if first > second.
   */
  private def comparePreimages(
    requester1: Long,
    blob1: Array[Byte],
    requester2: Long,
    blob2: Array[Byte]
  ): Int =
    // First compare by requester
    val requesterComparison = requester1.compareTo(requester2)
    if requesterComparison != 0 then
      requesterComparison
    else
      // Then compare by blob (lexicographically)
      compareUnsigned(blob1, blob2)

  /**
   * Check that preimages are sorted by (requester, blob) and unique.
   */
  private def arePreimagesSortedAndUnique(preimages: List[Preimage]): Boolean =
    if preimages.size <= 1 then true
    else
      var prevRequester: Option[Long] = None
      var prevBlob: Option[Array[Byte]] = None
      var result = true

      val iter = preimages.iterator
      while iter.hasNext && result do
        val submission = iter.next()
        val currentBlob = submission.blob.toArray

        (prevRequester, prevBlob) match
          case (Some(pr), Some(pb)) =>
            val comparison = comparePreimages(pr, pb, submission.requester.value.toLong, currentBlob)
            // Must be strictly less than (sorted and unique means no duplicates)
            if comparison >= 0 then
              result = false
          case _ => // First element, no comparison needed
        prevRequester = Some(submission.requester.value.toLong)
        prevBlob = Some(currentBlob)

      result

  /**
   * Check if a preimage was solicited (lookup entry exists with empty timestamp list).
   * This version checks from the lookupMeta structure.
   */
  private def isPreimageSolicited(
    account: PreimageAccount,
    hash: Array[Byte],
    length: Long
  ): Boolean =
    account.data.lookupMeta.exists { historyItem =>
      java.util.Arrays.equals(historyItem.key.hash.bytes, hash) &&
      historyItem.key.length == length &&
      historyItem.value.isEmpty
    }

  /**
   * Check if a preimage was solicited by looking up directly from rawServiceDataByStateKey.
   * This is the primary check method since preimagesStatus may not be populated from raw state.
   */
  private def isPreimageSolicitedFromRaw(
    serviceId: Long,
    hash: Array[Byte],
    length: Int,
    rawServiceDataByStateKey: Map[JamBytes, JamBytes]
  ): Boolean =
    // Compute the state key for this preimage request
    val stateKey = StateKey.computePreimageInfoStateKey(serviceId, length, JamBytes(hash))

    // Look up the key in rawServiceDataByStateKey
    rawServiceDataByStateKey.get(stateKey) match
      case Some(value) =>
        // Decode the timeslots list
        val timeslots = StateKey.decodePreimageInfoValue(value)
        // Solicited means the list is empty (requested but not yet provided)
        timeslots.isEmpty
      case None =>
        // Key not found, preimage was not requested
        false

  /**
   * Execute the Preimages STF using unified JamState.
   *
   * This implementation uses rawServiceDataByStateKey directly for lookups,
   * since preimagesStatus may not be populated from the raw state encoding.
   *
   * @param input The preimage input containing preimages and slot.
   * @param state The unified JamState.
   * @return Tuple of (updated JamState, PreimageOutput).
   */
  def stf(input: PreimageInput, state: JamState): (JamState, PreimageOutput) =
    stfWithPreAccumState(input, state, state.rawServiceDataByStateKey)

  /**
   * Execute the Preimages STF using unified JamState with explicit pre-accumulation state.
   *
   * Per GP §12.1, preimage validation uses the pre-accumulation state (accountspre),
   * not the post-accumulation state.
   *
   * @param input The preimage input containing preimages and slot.
   * @param state The unified JamState (post-accumulation).
   * @param preAccumRawServiceData The rawServiceDataByStateKey from BEFORE accumulation.
   * @return Tuple of (updated JamState, PreimageOutput).
   */
  def stfWithPreAccumState(
    input: PreimageInput,
    state: JamState,
    preAccumRawServiceData: Map[JamBytes, JamBytes]
  ): (JamState, PreimageOutput) =
    // First validate that preimages are sorted and unique by (requester, hash)
    if !arePreimagesSortedAndUnique(input.preimages) then
      return (state, StfResult.error(PreimageErrorCode.PreimagesNotSortedUnique))

    // Check if all preimages are solicited using PRE-ACCUMULATION rawServiceDataByStateKey
    // Per GP §12.1: "The data must have been solicited by a service but not yet provided in the *prior* state"
    val validationResult = input.preimages.traverse { submission =>
      val serviceId = submission.requester.value.toLong
      val hash = Hashing.blake2b256(submission.blob).bytes
      val length = submission.blob.length

      // Check if the service account exists (can use current state for this)
      val accountExists = state.accumulation.serviceAccounts.exists(_.id == serviceId)
      if !accountExists then
        Left(PreimageErrorCode.PreimageUnneeded)
      // Check if preimage is solicited using PRE-ACCUMULATION raw state
      else if !isPreimageSolicitedFromRaw(serviceId, hash, length, preAccumRawServiceData) then
        Left(PreimageErrorCode.PreimageUnneeded)
      else
        Right(submission)
    }

    if validationResult.isLeft then
      return (state, StfResult.error(validationResult.left.toOption.get))

    // Process preimages and update rawServiceDataByStateKey
    // IMPORTANT: Only add preimage info if the request still exists in POST-accumulation state.
    // If accumulation deleted/reverted the request (e.g., service PANIC), don't add the preimage.
    var updatedRawServiceData = state.rawServiceDataByStateKey
    val statsUpdates = scala.collection.mutable.Map[Long, (Int, Long)]() // serviceId -> (count, totalSize)

    for submission <- input.preimages do
      val serviceId = submission.requester.value.toLong
      val hash = Hashing.blake2b256(submission.blob).bytes
      val length = submission.blob.length

      // Compute the preimage info state key
      val infoStateKey = StateKey.computePreimageInfoStateKey(serviceId, length, JamBytes(hash))

      // Check if the preimage request still exists in POST-accumulation state
      // If accumulation deleted/reverted the request, don't add the preimage info
      val requestStillExists = state.rawServiceDataByStateKey.contains(infoStateKey)

      if requestStillExists then
        val newTimeslots = List(input.slot)
        val newInfoValue = StateKey.encodePreimageInfoValue(newTimeslots)

        updatedRawServiceData = updatedRawServiceData.updated(infoStateKey, newInfoValue)

        // Add preimage blob to state using discriminator 0xFFFFFFFE
        val blobStateKey = StateKey.computeServiceDataStateKey(serviceId, 0xfffffffeL, JamBytes(hash))
        updatedRawServiceData = updatedRawServiceData.updated(blobStateKey, submission.blob)

        // Track statistics update
        val (currentCount, currentSize) = statsUpdates.getOrElse(serviceId, (0, 0L))
        statsUpdates(serviceId) = (currentCount + 1, currentSize + length.toLong)

    // Update JamState with new rawServiceDataByStateKey
    val updatedState = state.copy(
      rawServiceDataByStateKey = updatedRawServiceData
    )

    (updatedState, StfResult.success(()))

  /**
   * Internal Preimages STF implementation using PreimageState.
   *
   * @param input The preimage input containing preimages and slot.
   * @param preState The pre-transition state.
   * @return Tuple of (post-transition state, output).
   */
  def stfInternal(
    input: PreimageInput,
    preState: PreimageState
  ): (PreimageState, PreimageOutput) =
    // First check if all preimages are needed (account exists, lookup entry exists with empty value)
    val validationResult = input.preimages.traverse { submission =>
      preState.accounts.find(_.id == submission.requester.value.toLong)
        .toRight(PreimageErrorCode.PreimageUnneeded)
        .flatMap { account =>
          val hash = Hashing.blake2b256(submission.blob).bytes
          val length = submission.blob.length.toLong
          Either.cond(isPreimageSolicited(account, hash, length), submission, PreimageErrorCode.PreimageUnneeded)
        }
    }

    if validationResult.isLeft then
      return (preState, StfResult.error(validationResult.left.toOption.get))

    // Then validate that preimages are sorted and unique by (requester, hash)
    if !arePreimagesSortedAndUnique(input.preimages) then
      return (preState, StfResult.error(PreimageErrorCode.PreimagesNotSortedUnique))

    // Track statistics updates by service ID
    val statsUpdates = scala.collection.mutable.Map[Long, (Int, Long)]() // serviceId -> (count, totalSize)

    // Process preimages and update state
    val updatedAccounts = preState.accounts.map { account =>
      val submissionsForAccount = input.preimages.filter(_.requester.value.toLong == account.id)
      if submissionsForAccount.isEmpty then
        account
      else
        var currentPreimages = account.data.preimages
        var currentLookupMeta = account.data.lookupMeta

        for submission <- submissionsForAccount do
          val hash = Hashing.blake2b256(submission.blob).bytes
          val hashObj = Hash(hash)
          val length = submission.blob.length.toLong

          // Update preimages list - add new preimage
          val newPreimage = PreimageHash(hashObj, submission.blob)
          currentPreimages = (currentPreimages :+ newPreimage).sortWith { (a, b) =>
            compareUnsigned(a.hash.bytes, b.hash.bytes) < 0
          }

          // Update lookup metadata - set timestamp to current slot
          currentLookupMeta = currentLookupMeta.map { historyItem =>
            if java.util.Arrays.equals(historyItem.key.hash.bytes, hash) && historyItem.key.length == length then
              historyItem.copy(value = List(input.slot))
            else
              historyItem
          }.sortWith((a, b) => compareUnsigned(a.key.hash.bytes, b.key.hash.bytes) < 0)

          // Track statistics update
          val (currentCount, currentSize) = statsUpdates.getOrElse(account.id, (0, 0L))
          statsUpdates(account.id) = (currentCount + 1, currentSize + submission.blob.length.toLong)

        account.copy(data = AccountInfo(currentPreimages, currentLookupMeta))
    }

    // Build updated statistics list
    val updatedStatistics = statsUpdates.toList.sortBy(_._1).map {
      case (serviceId, (count, size)) =>
        // Find existing stat entry or create new one
        val existingEntry = preState.statistics.find(_.id == serviceId)
        existingEntry match
          case Some(entry) =>
            entry.copy(
              record = entry.record.copy(
                providedCount = entry.record.providedCount + count,
                providedSize = entry.record.providedSize + size
              )
            )
          case None =>
            ServiceStatisticsEntry(
              id = serviceId,
              record = ServiceActivityRecord(
                providedCount = count,
                providedSize = size
              )
            )
    }

    // Merge existing stats not updated with new stats
    val existingNotUpdated = preState.statistics.filterNot(s => statsUpdates.contains(s.id))
    val mergedStatistics = (existingNotUpdated ++ updatedStatistics).sortBy(_.id)

    val postState = preState.copy(accounts = updatedAccounts, statistics = mergedStatistics)
    (postState, StfResult.success(()))
