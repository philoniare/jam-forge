package io.forge.jam.node

import io.forge.jam.core.primitives.Hash
import io.forge.jam.db.BlockStore

final class ChainMeta(store: BlockStore):

  private def long(key: String): Option[Long] =
    store.getMeta(key).map(b => java.nio.ByteBuffer.wrap(b).getLong)

  private def putLong(key: String, v: Long): Unit =
    store.setMeta(key, java.nio.ByteBuffer.allocate(8).putLong(v).array())

  private def flag(key: String): Boolean = long(key).contains(1L)

  private def putFlag(key: String, on: Boolean): Unit = putLong(key, if on then 1L else 0L)

  private def hash(key: String): Option[Hash] =
    store.getMeta(key).filter(_.length == 32).map(Hash(_))

  private def putHash(key: String, h: Hash): Unit = store.setMeta(key, h.bytes.toArray)

  private def holds(key: String, h: Hash): Boolean =
    store.getMeta(key).exists(java.util.Arrays.equals(_, h.bytes.toArray))

  def stateRoot: Option[Hash] = store.getMeta("state_root").map(Hash(_))
  def stateRoot_=(root: Hash): Unit = putHash("state_root", root)

  def bestSlot: Option[Long] = long("best_slot")
  def bestSlot_=(slot: Long): Unit = putLong("best_slot", slot)

  def finalizedSlot: Option[Long] = long("finalized_slot")
  def finalizedSlot_=(slot: Long): Unit = putLong("finalized_slot", slot)

  def finalizedRoot: Option[Hash] = hash("finalized_root")
  def finalizedRoot_=(root: Hash): Unit = putHash("finalized_root", root)

  def hasFinalizedRoot: Boolean = store.getMeta("finalized_root").isDefined

  def height(h: Hash): Option[Long] = long(s"height:${h.toHex}")
  def putHeight(h: Hash, height: Long): Unit = putLong(s"height:${h.toHex}", height)

  def blockRoot(h: Hash): Option[Hash] = hash(s"blockroot:${h.toHex}")
  def putBlockRoot(h: Hash, root: Hash): Unit = putHash(s"blockroot:${h.toHex}", root)
  def dropBlockRoot(h: Hash): Unit = store.deleteMeta(s"blockroot:${h.toHex}")

  def ticketed(h: Hash): Boolean = flag(s"ticketed:${h.toHex}")

  def marked(h: Hash): Boolean = flag(s"marked:${h.toHex}")

  def weight(h: Hash): Long = long(s"weight:${h.toHex}").getOrElse(0L)

  def putForkChoice(h: Hash, ticketed: Boolean, marked: Boolean, weight: Long): Unit =
    putFlag(s"ticketed:${h.toHex}", ticketed)
    putFlag(s"marked:${h.toHex}", marked)
    putLong(s"weight:${h.toHex}", weight)

  def invalid(h: Hash): Boolean = long(s"invalid:${h.toHex}").exists(_ != 0L)
  def markInvalid(h: Hash): Unit = putLong(s"invalid:${h.toHex}", ChainMeta.Invalid)

  def disregarded(h: Hash): Boolean = long(s"invalid:${h.toHex}").contains(ChainMeta.Disregarded)

  def markDisregarded(h: Hash): Unit =
    if !invalid(h) then putLong(s"invalid:${h.toHex}", ChainMeta.Disregarded)

  def unverifiedSize(h: Hash): Option[Long] = long(s"unverified:${h.toHex}")
  def putUnverified(h: Hash, size: Long): Unit = putLong(s"unverified:${h.toHex}", size)
  def dropUnverified(h: Hash): Unit = store.deleteMeta(s"unverified:${h.toHex}")

  def dropSideBranchMarks(h: Hash): Unit =
    ChainMeta.SideBranchKeys.foreach(k => store.deleteMeta(s"$k:${h.toHex}"))

  def dropBlock(h: Hash): Unit =
    ChainMeta.BlockKeys.foreach(k => store.deleteMeta(s"$k:${h.toHex}"))

  def slotAuthor(slot: Long, author: Int): Option[Hash] =
    store.getMeta(s"slotauthor:$slot:$author").map(Hash(_))

  def putSlotAuthor(slot: Long, author: Int, h: Hash): Unit = putHash(s"slotauthor:$slot:$author", h)

  def dropSlotAuthorIf(slot: Long, author: Int, h: Hash): Unit =
    val key = s"slotauthor:$slot:$author"
    if holds(key, h) then store.deleteMeta(key)

  def equivocated(slot: Long): Boolean = flag(s"equivocated:$slot")
  def markEquivocated(slot: Long): Unit = putLong(s"equivocated:$slot", 1L)
  def dropEquivocated(slot: Long): Unit = store.deleteMeta(s"equivocated:$slot")

  private def finalizedIndexKey(height: Long): String = f"fin:$height%016x"

  def finalizedAt(height: Long): Option[Hash] = hash(finalizedIndexKey(height))
  def hasFinalizedAt(height: Long): Boolean = store.getMeta(finalizedIndexKey(height)).isDefined
  def isFinalizedAt(height: Long, h: Hash): Boolean = holds(finalizedIndexKey(height), h)
  def putFinalizedAt(height: Long, h: Hash): Unit = putHash(finalizedIndexKey(height), h)
  def dropFinalizedAt(height: Long): Unit = store.deleteMeta(finalizedIndexKey(height))

  def pruneFloor: Option[Hash] = hash("prune_floor")
  def pruneFloor_=(h: Hash): Unit = putHash("prune_floor", h)

  def ancestryFloor: Option[Long] = long("ancestry_floor")
  def ancestryFloor_=(slot: Option[Long]): Unit = slot match
    case Some(s) => putLong("ancestry_floor", s)
    case None    => store.deleteMeta("ancestry_floor")

  def grandpaSet: Option[Array[Byte]] = store.getMeta("grandpa:set")
  def grandpaSet_=(encoded: Array[Byte]): Unit = store.setMeta("grandpa:set", encoded)

  def putGrandpaSet(encoded: Array[Byte], settlesCheckpoint: Boolean): Unit =
    store.writeMeta(List("grandpa:set" -> encoded), if settlesCheckpoint then List("grandpa:checkpoint") else Nil)

  def grandpaRound: Option[Array[Byte]] = store.getMeta("grandpa:round")

  def putGrandpaRound(encoded: Array[Byte], durable: Boolean): Unit =
    if durable then store.setMetaSynced("grandpa:round", encoded) else store.setMeta("grandpa:round", encoded)

  def grandpaCheckpoint: Option[Long] = long("grandpa:checkpoint")

  def startGrandpaAtCheckpoint(setId: Long): Unit =
    putLong("grandpa:checkpoint", setId)
    store.deleteMeta("grandpa:set")
    store.deleteMeta("grandpa:round")

  def grandpaJustification(h: Hash): Option[Array[Byte]] = store.getMeta(s"grandpa:just:${h.toHex}")
  def putGrandpaJustification(h: Hash, encoded: Array[Byte]): Unit =
    store.setMeta(s"grandpa:just:${h.toHex}", encoded)

  def grandpaWarp(setId: Long): Option[Array[Byte]] = store.getMeta(s"grandpa:warp:$setId")
  def putGrandpaWarp(setId: Long, fragment: Array[Byte]): Unit = store.setMeta(s"grandpa:warp:$setId", fragment)

  def beefyProof(h: Hash): Option[Array[Byte]] = store.getMeta(s"beefy:proof:${h.toHex}")
  def putBeefyProof(h: Hash, encoded: Array[Byte]): Unit = store.setMeta(s"beefy:proof:${h.toHex}", encoded)

  def beefyLatest: Option[Array[Byte]] = store.getMeta("beefy:latest")
  def beefyLatest_=(encoded: Array[Byte]): Unit = store.setMeta("beefy:latest", encoded)

object ChainMeta:

  private val Invalid = 1L
  private val Disregarded = 2L

  private val SideBranchKeys = List("height", "ticketed", "marked", "weight")

  private val BlockKeys =
    List("height", "ticketed", "marked", "weight", "blockroot", "unverified", "invalid", "grandpa:just")
