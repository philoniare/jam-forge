package io.forge.jam.node

import io.forge.jam.core.JamBytes
import io.forge.jam.network.PreferredInitiator

import java.net.{InetAddress, InetSocketAddress}

/**
 * Validator connectivity rules from JAMNP-S "Required connectivity", "Epoch
 * transitions" and "Grid structure".
 */
object Connectivity:

  /**
   * How long the non-preferred side waits for the preferred initiator to dial
   * before dialing itself (JAMNP-S: "a reasonable timeout (e.g. 5 seconds)").
   */
  val PreferredInitiatorGraceMillis: Long = 5000L

  /**
   * True when `self` should open the connection to `other`: it is the
   * JAMNP-S preferred initiator P(self, other).
   */
  def isPreferredInitiator(self: Array[Byte], other: Array[Byte]): Boolean =
    PreferredInitiator.of(self, other) eq self

  /**
   * Validator endpoint from metadata: 16-byte IPv6 address then a 2-byte
   * little-endian port (JAMNP-S "Required connectivity"). None for short
   * metadata, the unspecified address or port 0.
   */
  def endpoint(metadata: Array[Byte]): Option[InetSocketAddress] =
    if metadata.length < 18 then None
    else
      val ip = metadata.take(16)
      val port = (metadata(16) & 0xff) | ((metadata(17) & 0xff) << 8)
      if port == 0 || ip.forall(_ == 0) then None
      else
        // getByAddress unwraps IPv4-mapped addresses (::ffff:a.b.c.d) to IPv4.
        Some(new InetSocketAddress(InetAddress.getByAddress(ip), port))

  /**
   * Epoch whose validator connectivity is in force at `slot`: the slot's own
   * epoch once its first block is finalized and max(⌊E/30⌋, 1) slots of it
   * have passed, otherwise the epoch before (JAMNP-S "Epoch transitions").
   */
  def appliedEpoch(slot: Long, epochLength: Int, finalizedSlot: Long, finalityRunning: Boolean): Long =
    val epoch = slot / epochLength
    val elapsed = slot - epoch * epochLength
    // Without a finality gadget nothing past genesis is finalized, so the
    // first condition is waived.
    val firstFinalized = !finalityRunning || finalizedSlot >= epoch * epochLength
    if epoch == 0 || (firstFinalized && elapsed >= transitionDelay(epochLength)) then epoch
    else epoch - 1

  /** max(⌊E/30⌋, 1): slots into an epoch before connectivity changes apply. */
  def transitionDelay(epochLength: Int): Int = math.max(epochLength / 30, 1)

  /**
   * Grid neighbours of `self` among the previous, current and next validator
   * sets: same row (index / W) or column (index % W) within one set, with
   * W = ⌊√V⌋, or the same index in different sets (JAMNP-S "Grid
   * structure"). Returns the neighbours' Ed25519 keys.
   */
  def gridNeighbours(sets: Seq[Seq[Array[Byte]]], self: Array[Byte]): Set[JamBytes] =
    val selfKey = JamBytes(self)
    val positions = sets.zipWithIndex.flatMap {
      case (set, s) =>
        set.zipWithIndex.collect { case (k, i) if JamBytes(k) == selfKey => (s, i) }
    }
    val found =
      for
        (s, i) <- positions
        (set, t) <- sets.zipWithIndex
        w = math.max(math.sqrt(set.size.toDouble).toInt, 1)
        (k, j) <- set.zipWithIndex
        if (s == t && (j / w == i / w || j % w == i % w)) || (s != t && j == i)
      yield JamBytes(k)
    found.toSet - selfKey

  /**
   * UP 0 is opened between two connected nodes when both are validators and
   * grid neighbours, or when at least one of them is not a validator
   * (JAMNP-S UP 0).
   */
  def wantsBlockAnnouncements(
    validators: Set[JamBytes],
    neighbours: Set[JamBytes],
    self: Array[Byte],
    peer: Array[Byte]
  ): Boolean =
    val selfIsValidator = validators.contains(JamBytes(self))
    val peerKey = JamBytes(peer)
    !selfIsValidator || !validators.contains(peerKey) || neighbours.contains(peerKey)
