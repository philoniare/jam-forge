package io.forge.jam.protocol.preimage

import cats.syntax.all.*
import io.forge.jam.core.JamBytes.compareUnsigned
import io.forge.jam.core.{JamBytes, Hashing, StfResult}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.extrinsic.Preimage
import io.forge.jam.core.types.preimage.PreimageHash
import io.forge.jam.protocol.preimage.PreimageTypes.*
import io.forge.jam.protocol.accumulation.StateKey
import io.forge.jam.protocol.state.TrieBackedJamState

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

  private def validatePreimages(
    preimages: List[Preimage],
    isProvidable: (Long, Array[Byte], Int) => Boolean
  ): Either[PreimageErrorCode, Unit] =
    if !arePreimagesSortedAndUnique(preimages) then
      Left(PreimageErrorCode.PreimagesNotSortedUnique)
    else
      preimages
        .traverse { submission =>
          val serviceId = submission.requester.value.toLong
          val hash = Hashing.blake2b256(submission.blob).bytes
          val length = submission.blob.length
          Either.cond(
            isProvidable(serviceId, hash, length),
            (),
            PreimageErrorCode.PreimageUnneeded
          )
        }
        .map(_ => ())

  def stfView(
    input: PreimageInput,
    view: TrieBackedJamState
  ): PreimageOutput =
    val validationResult = validatePreimages(
      input.preimages,
      (serviceId, hash, length) =>
        val accountExists =
          view.storage.readTrie(StateKey.computeServiceAccountKey(serviceId)).isDefined
        if !accountExists then false
        else
          val infoStateKey =
            StateKey.computePreimageInfoStateKey(serviceId, length, JamBytes(hash))
          view.storage.readTrie(infoStateKey) match
            case Some(value) => StateKey.isUnprovidedRequest(value)
            case None        => false
    )

    if validationResult.isLeft then
      return StfResult.error(validationResult.left.toOption.get)

    for submission <- input.preimages do
      val serviceId = submission.requester.value.toLong
      val hash = Hashing.blake2b256(submission.blob).bytes
      val length = submission.blob.length

      val infoStateKey =
        StateKey.computePreimageInfoStateKey(serviceId, length, JamBytes(hash))
      val stillRequested = view.storage.getByStateKey(infoStateKey) match
        case Some(value) => StateKey.isUnprovidedRequest(value)
        case None        => false

      if stillRequested then
        val newTimeslots = List(input.slot)
        view.storage.putByStateKey(
          infoStateKey,
          StateKey.encodePreimageInfoValue(newTimeslots)
        )
        val blobStateKey = StateKey.computeServiceDataStateKey(
          serviceId,
          0xfffffffeL,
          JamBytes(hash)
        )
        view.storage.putByStateKey(blobStateKey, submission.blob)

    StfResult.success(())

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
    val accountsById = preState.accounts.view.map(a => a.id -> a).toMap

    val validationResult = validatePreimages(
      input.preimages,
      (serviceId, hash, length) =>
        accountsById.get(serviceId).exists(account =>
          isPreimageSolicited(account, hash, length.toLong)
        )
    )

    if validationResult.isLeft then
      return (preState, StfResult.error(validationResult.left.toOption.get))

    // Track statistics updates by service ID
    val statsUpdates = scala.collection.mutable.Map[Long, (Int, Long)]() // serviceId -> (count, totalSize)

    val submissionsByAccount: Map[Long, List[Preimage]] =
      input.preimages.groupBy(_.requester.value.toLong)

    // Process preimages and update state
    val updatedAccounts = preState.accounts.map { account =>
      val submissionsForAccount = submissionsByAccount.getOrElse(account.id, Nil)
      if submissionsForAccount.isEmpty then
        account
      else
        val hashesAndLengths = submissionsForAccount.map { submission =>
          val hash = Hashing.blake2b256(submission.blob).bytes
          (hash, submission.blob.length.toLong, submission)
        }

        val newPreimages = hashesAndLengths.map { case (hash, _, submission) =>
          PreimageHash(Hash(hash), submission.blob)
        }
        val currentPreimages = (account.data.preimages ++ newPreimages).sortWith { (a, b) =>
          compareUnsigned(a.hash.bytes, b.hash.bytes) < 0
        }

        val submittedKeys = hashesAndLengths.map { case (hash, length, _) => (hash, length) }
        val currentLookupMeta = account.data.lookupMeta.map { historyItem =>
          val matched = submittedKeys.exists { case (hash, length) =>
            java.util.Arrays.equals(historyItem.key.hash.bytes, hash) && historyItem.key.length == length
          }
          if matched then historyItem.copy(value = List(input.slot)) else historyItem
        }.sortWith((a, b) => compareUnsigned(a.key.hash.bytes, b.key.hash.bytes) < 0)

        // Track statistics update
        val (currentCount, currentSize) = statsUpdates.getOrElse(account.id, (0, 0L))
        val addedCount = submissionsForAccount.size
        val addedSize = submissionsForAccount.map(_.blob.length.toLong).sum
        statsUpdates(account.id) = (currentCount + addedCount, currentSize + addedSize)

        account.copy(data = AccountInfo(currentPreimages, currentLookupMeta))
    }

    val statsById = preState.statistics.view.map(s => s.id -> s).toMap

    // Build updated statistics list
    val updatedStatistics = statsUpdates.toList.sortBy(_._1).map {
      case (serviceId, (count, size)) =>
        statsById.get(serviceId) match
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
