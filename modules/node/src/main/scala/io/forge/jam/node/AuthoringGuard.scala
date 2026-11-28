package io.forge.jam.node

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.block.Block
import io.forge.jam.vrfs.BandersnatchWrapper

import scala.collection.mutable
import scala.util.control.NonFatal

trait AuthoringGuard:
  def mayAuthor(slot: Long): Boolean

  def sealing(slot: Long, hash: Hash, block: Block): Boolean

  def durable(slot: Long): Boolean = true

object AuthoringGuard:
  val none: AuthoringGuard = new AuthoringGuard:
    def mayAuthor(slot: Long): Boolean = true
    def sealing(slot: Long, hash: Hash, block: Block): Boolean = true

final class EquivocationGuard(
    meta: ChainMeta,
    keys: Seq[ValidatorKeySet],
    reported: String => Unit,
    write: Long => Unit = null
) extends AuthoringGuard
    with LazyLogging
    with AutoCloseable:

  private val writeSlot: Long => Unit = if write != null then write else meta.putLastAuthoredSlot

  private val writer = java.util.concurrent.Executors.newSingleThreadExecutor { r =>
    val t = new Thread(r, "jam-author-guard")
    t.setDaemon(true)
    t
  }

  private val writes = mutable.HashMap.empty[Long, java.util.concurrent.Future[?]]

  private val ourKeys: Set[JamBytes] = keys.map(k => JamBytes(k.bandersnatchPublic)).toSet

  private var last: Long = meta.lastAuthoredSlot.getOrElse(-1L)

  private var beforeLast: Long = last

  private var armedFrom: Long = Long.MaxValue

  private val sealedHere = mutable.LinkedHashMap.empty[Long, List[(Hash, JamBytes)]]

  @volatile private var stoppedFor: Option[String] = None

  def stopped: Option[String] = stoppedFor

  def mayAuthor(slot: Long): Boolean = synchronized(stoppedFor.isEmpty && slot > last)

  def sealing(slot: Long, hash: Hash, block: Block): Boolean = synchronized {
    val write =
      if !mayAuthor(slot) then None
      else
        try Some(writer.submit((() => writeSlot(slot)): Runnable))
        catch case _: java.util.concurrent.RejectedExecutionException => None
    if write.isEmpty then false
    else
      writes.filterInPlace((_, w) => !(w.isDone && scala.util.Try(w.get()).isSuccess))
      writes.update(slot, write.get)
      beforeLast = last
      last = slot
      if armedFrom == Long.MaxValue then armedFrom = slot
      sealedHere.update(slot, (hash, sealOutput(block)) :: sealedHere.getOrElse(slot, Nil))
      while sealedHere.size > EquivocationGuard.Kept do sealedHere.remove(sealedHere.head._1)
      true
  }

  def withdraw(slot: Long): Unit = synchronized {
    if last == slot && beforeLast < slot then
      last = beforeLast
      val back = last
      try writer.submit((() => if back >= 0 then meta.putLastAuthoredSlot(back) else meta.dropLastAuthoredSlot()): Runnable).get()
      catch case _: java.util.concurrent.RejectedExecutionException => ()
  }

  override def durable(slot: Long): Boolean =
    val pending = synchronized(writes.remove(slot))
    pending.forall { w =>
      try
        w.get(EquivocationGuard.ReleaseWaitMillis, java.util.concurrent.TimeUnit.MILLISECONDS)
        true
      catch
        case _: java.util.concurrent.TimeoutException =>
          logger.warn(
            s"the record that slot $slot was sealed is not on disk ${EquivocationGuard.ReleaseWaitMillis} ms after " +
              "sealing (a slow disk): its block is not released"
          )
          false
        case NonFatal(e) =>
          val cause = Option(e.getCause).getOrElse(e)
          stop(
            s"the record that slot $slot was sealed could not be written (${cause.getMessage}), so its block is not released",
            duplicate = false
          )
          false
    }

  def close(): Unit =
    writer.shutdown()
    writer.awaitTermination(EquivocationGuard.WriteTimeoutSeconds, java.util.concurrent.TimeUnit.SECONDS): Unit

  def observe(hash: Hash, block: Block, sealer: () => Option[Array[Byte]]): Unit =
    val slot = block.header.slot.toLong
    val ours = synchronized {
      if stoppedFor.isDefined || slot < armedFrom then None else Some(sealedHere.getOrElse(slot, Nil))
    }
    ours.filterNot(_.exists(_._1 == hash)).foreach { sealedForSlot =>
      val output = sealOutput(block)
      val sameSeal = sealedForSlot.exists(_._2 == output)
      val ourKey = sameSeal || (try sealer().exists(k => ourKeys(JamBytes(k))) catch case NonFatal(_) => false)
      if ourKey then
        stop(
          s"block ${hash.toHex.take(18)} at slot $slot is sealed with this node's validator key but was not authored " +
            s"here${if sameSeal then " (a second block for a slot this node sealed)" else ""}: another process is " +
            "authoring with the same keys"
        )
    }

  private def stop(why: String, duplicate: Boolean = true): Unit =
    val first = synchronized {
      val was = stoppedFor.isEmpty
      if was then stoppedFor = Some(why)
      was
    }
    if first then
      if duplicate then
        logger.error(
          s"DUPLICATE AUTHORING: $why. Authoring stops so that this validator does not equivocate; stop the other " +
            "process, then restart this node"
        )
        reported(why)
      else logger.error(s"$why. Authoring stops; restart the node once the disk is writable")

  private def sealOutput(block: Block): JamBytes =
    try JamBytes(BandersnatchWrapper.getIetfVrfOutput(block.header.seal.toArray))
    catch case NonFatal(_) => JamBytes.empty

object EquivocationGuard:
  val Kept: Int = 4096

  val WriteTimeoutSeconds: Long = 10L

  val ReleaseWaitMillis: Long = 2000L
