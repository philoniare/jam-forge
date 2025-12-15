package io.forge.jam.protocol.preimage

import io.forge.jam.core.JamBytes.compareUnsigned
import io.forge.jam.core.{Hashing, StfResult}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.extrinsic.Preimage
import io.forge.jam.core.types.preimage.PreimageHash
import io.forge.jam.protocol.preimage.PreimageTypes.*

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
   * Compare two preimages by (requester, hash).
   * Returns negative if first < second, 0 if equal, positive if first > second.
   */
  private def comparePreimages(
    requester1: Long,
    hash1: Array[Byte],
    requester2: Long,
    hash2: Array[Byte]
  ): Int =
    // First compare by requester
    val requesterComparison = requester1.compareTo(requester2)
    if requesterComparison != 0 then
      requesterComparison
    else
      // Then compare by hash (lexicographically)
      compareUnsigned(hash1, hash2)

  /**
   * Check that preimages are sorted by (requester, hash) and unique.
   * Sorting is ascending by requester ID, then by blake2b hash of the blob.
   */
  private def arePreimagesSortedAndUnique(preimages: List[Preimage]): Boolean =
    if preimages.size <= 1 then true
    else
      var prevRequester: Option[Long] = None
      var prevHash: Option[Array[Byte]] = None
      var result = true

      val iter = preimages.iterator
      while iter.hasNext && result do
        val submission = iter.next()
        val currentHash = Hashing.blake2b256(submission.blob).bytes

        (prevRequester, prevHash) match
          case (Some(pr), Some(ph)) =>
            val comparison = comparePreimages(pr, ph, submission.requester.value.toLong, currentHash)
            // Must be strictly less than (sorted and unique means no duplicates)
            if comparison >= 0 then
              result = false
          case _ => // First element, no comparison needed

        prevRequester = Some(submission.requester.value.toLong)
        prevHash = Some(currentHash)

      result

  /**
   * Check if a preimage was solicited (lookup entry exists with empty timestamp list).
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
   * Execute the Preimages STF.
   *
   * @param input The preimage input containing preimages and slot.
   * @param preState The pre-transition state.
   * @return Tuple of (post-transition state, output).
   */
  def stf(
    input: PreimageInput,
    preState: PreimageState
  ): (PreimageState, PreimageOutput) =
    // First check if all preimages are needed (account exists, lookup entry exists with empty value)
    var validationError: Option[PreimageErrorCode] = None

    val iter = input.preimages.iterator
    while iter.hasNext && validationError.isEmpty do
      val submission = iter.next()
      val accountOpt = preState.accounts.find(_.id == submission.requester.value.toLong)
      accountOpt match
        case None =>
          validationError = Some(PreimageErrorCode.PreimageUnneeded)
        case Some(account) =>
          val hash = Hashing.blake2b256(submission.blob).bytes
          val length = submission.blob.length.toLong

          // Verify this preimage was solicited and not already available
          if !isPreimageSolicited(account, hash, length) then
            validationError = Some(PreimageErrorCode.PreimageUnneeded)

    if validationError.isDefined then
      return (preState, StfResult.error(validationError.get))

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
          }.sortWith { (a, b) =>
              compareUnsigned(a.key.hash.bytes, b.key.hash.bytes) < 0
          }

          // Track statistics update
          val (currentCount, currentSize) = statsUpdates.getOrElse(account.id, (0, 0L))
          statsUpdates(account.id) = (currentCount + 1, currentSize + submission.blob.length.toLong)

        account.copy(data = AccountInfo(currentPreimages, currentLookupMeta))
    }

    // Build updated statistics list
    val updatedStatistics = statsUpdates.toList.sortBy(_._1).map { case (serviceId, (count, size)) =>
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
