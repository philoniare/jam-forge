package io.forge.jam.node

import io.forge.jam.core.primitives.{Ed25519PublicKey, Hash}
import io.forge.jam.core.types.extrinsic.GuaranteeExtrinsic
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.network.PeerConnection

final class GuarantorIndex(maxEntries: Int = GuarantorIndex.Capacity):

  private val byErasureRoot =
    new java.util.LinkedHashMap[Hash, Set[Ed25519PublicKey]](64, 0.75f, false):
      override def removeEldestEntry(e: java.util.Map.Entry[Hash, Set[Ed25519PublicKey]]): Boolean =
        size > maxEntries

  def record(sets: List[IndexedSeq[ValidatorKey]], guarantees: Iterable[GuaranteeExtrinsic]): Unit =
    val entries = guarantees.toList.map { g =>
      val keys = for
        s <- g.signatures
        set <- sets
        vk <- set.lift(s.validatorIndex.toInt)
      yield vk.ed25519
      g.report.packageSpec.erasureRoot -> keys.toSet
    }
    synchronized(entries.foreach((root, keys) => byErasureRoot.put(root, keys)))

  def guarantorsOf(erasureRoot: Hash): Set[Ed25519PublicKey] =
    synchronized(Option(byErasureRoot.get(erasureRoot))).getOrElse(Set.empty)

  def first(erasureRoot: Hash, peers: List[PeerConnection]): List[PeerConnection] =
    val keys = guarantorsOf(erasureRoot)
    if keys.isEmpty then peers
    else
      val (guarantors, others) = peers.partition(c => keys.contains(c.peerPublicKey))
      guarantors ++ others

object GuarantorIndex:
  val Capacity: Int = 16384

  val unordered: (Hash, List[PeerConnection]) => List[PeerConnection] = (_, peers) => peers
