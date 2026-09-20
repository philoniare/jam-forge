package io.forge.jam.protocol.accumulation

import io.forge.jam.core.JamBytes

import scala.collection.mutable
import java.nio.{ByteBuffer, ByteOrder}

/** Account changes tracker for merging parallel service executions.
  */
class AccountChanges:
  val accountUpdates: mutable.Map[Long, ServiceAccount] = mutable.Map.empty
  val removedAccounts: mutable.Set[Long] = mutable.Set.empty
  // Storage data changes (for WRITE host call updates)
  val rawServiceDataUpdates: mutable.Map[JamBytes, JamBytes] = mutable.Map.empty
  val rawServiceDataRemovals: mutable.Set[JamBytes] = mutable.Set.empty

  def checkAndMerge(other: AccountChanges): Unit =
    // Merge account updates
    for (id, account) <- other.accountUpdates do
      accountUpdates.get(id) match
        case Some(existing) if existing != account =>
          throw new RuntimeException(
            s"Conflicting parallel account updates for service $id: block invalid"
          )
        case _ => accountUpdates(id) = account

    // Merge removed accounts
    removedAccounts ++= other.removedAccounts

    for (key, value) <- other.rawServiceDataUpdates do
      rawServiceDataUpdates.get(key) match
        case Some(existing) if existing != value =>
          throw new RuntimeException(
            s"Conflicting parallel storage updates for key $key: block invalid"
          )
        case _ => rawServiceDataUpdates(key) = value
    rawServiceDataRemovals ++= other.rawServiceDataRemovals

  def applyTo(state: PartialState): Unit =
    // Apply account updates FIRST
    for (id, account) <- accountUpdates do
      state.accounts = state.accounts.updated(id, account)

    // THEN apply removals — removals take precedence
    for id <- removedAccounts do
      state.accounts = state.accounts.removed(id)

      val serviceIdBytes = ByteBuffer
        .allocate(4)
        .order(ByteOrder.LITTLE_ENDIAN)
        .putInt(id.toInt)
        .array()
      val keysToRemove = state.rawServiceDataByStateKey.keys.filter { key =>
        val arr = key.toArray
        arr.length >= 8 &&
        arr(0) == serviceIdBytes(0) &&
        arr(2) == serviceIdBytes(1) &&
        arr(4) == serviceIdBytes(2) &&
        arr(6) == serviceIdBytes(3) &&
        !StateKey.isChapterKey(arr) &&
        !StateKey.isAccountRecordKey(arr)
      }.toList
      state.rawServiceDataByStateKey =
        state.rawServiceDataByStateKey.removedAll(keysToRemove)

      // Also remove the service account key from rawServiceAccountsByStateKey
      val serviceAccountKey = StateKey.computeServiceAccountKey(id)
      state.rawServiceAccountsByStateKey =
        state.rawServiceAccountsByStateKey.removed(serviceAccountKey)

    // Apply rawServiceData changes
    state.rawServiceDataByStateKey =
      state.rawServiceDataByStateKey.removedAll(rawServiceDataRemovals)
    state.rawServiceDataByStateKey =
      state.rawServiceDataByStateKey ++ rawServiceDataUpdates
