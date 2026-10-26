package io.forge.jam.node

import java.util.concurrent.{Executors, ScheduledExecutorService, TimeUnit}

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.{Ed25519PublicKey, Hash}
import io.forge.jam.crypto.Bls
import io.forge.jam.network.{JamnpConnection, JamnpNode, StreamHandler}
import io.forge.jam.node.BeefyMessages.*
import io.forge.jam.protocol.state.{ServiceStorageView, TrieBackedJamState}

import scala.collection.mutable

/** BEEFY prototype: signs every block that becomes finalized, sends the signed commitment to every
  * connected peer that takes it, and, once a super-majority of κ' signed one block, stores the aggregated [[BeefyProof]] under
  * `beefy:proof:<hash>` (and `beefy:latest`). Works with any finality source (GRANDPA or the devnet rule). All state
  * lives on one thread.
  */
final class BeefyService(
    chain: ChainManager,
    distribution: DistributionService,
    keys: Option[ValidatorKeySet],
    pollMillis: Long = 500L,
    outbound: Option[SignedCommitment => Unit] = None
) extends LazyLogging:

  private val exec: ScheduledExecutorService = Executors.newSingleThreadScheduledExecutor { r =>
    val t = new Thread(r, "jam-beefy"); t.setDaemon(true); t
  }

  /** At most this many newly finalized blocks are signed per step (finality can jump after a stall). */
  private val MaxSignPerStep = 16
  private val MaxPending = 1024
  private val MaxTracked = 64

  /** What a block's posterior state says about BEEFY: κ' BLS keys and the last super-peak of β_H. */
  private final case class Ctx(keys: IndexedSeq[Array[Byte]], ed25519: IndexedSeq[Ed25519PublicKey], superPeak: Hash)

  private var lastSigned: Hash = chain.finalized.hash
  private val ctxCache = mutable.LinkedHashMap.empty[Hash, Option[Ctx]]
  private val received = mutable.LinkedHashMap.empty[Hash, mutable.Map[Int, SignedCommitment]]
  private val proven = mutable.LinkedHashSet.empty[Hash]
  private val pending = mutable.ArrayBuffer.empty[SignedCommitment]
  private val consistentKeys = mutable.Set.empty[JamBytes]
  private var warnedUnregistered = false

  /** Whether a peer (by Ed25519 key) takes our BEEFY stream. JAMNP-S has none, and a peer that does not know it may
    * drop the whole connection, so the first commitment to an unknown peer is a probe: a peer that
    * keeps the connection for [[ProbeMillis]] takes it, one that drops it is never sent BEEFY again.
    */
  private val speaks = mutable.Map.empty[Ed25519PublicKey, Boolean]
  private val probes = mutable.Map.empty[Ed25519PublicKey, (JamnpConnection, Long)]
  private val ProbeMillis = 3000L

  @volatile private var latest: Option[BeefyProof] = None

  /** The newest block with an aggregated proof. */
  def latestProof: Option[BeefyProof] = latest

  def proof(block: Hash): Option[BeefyProof] =
    chain.meta(s"beefy:proof:${block.toHex}").flatMap(decodeProof(_).toOption)

  def start(): Unit =
    exec.scheduleAtFixedRate(() => guarded(step()), pollMillis, pollMillis, TimeUnit.MILLISECONDS)
    logger.info(s"BEEFY started, signing=${keys.isDefined}")

  def shutdown(): Unit = exec.shutdownNow()

  def register(network: JamnpNode): Unit =
    network.registerHandler(StreamKind, handler)
    ()

  def handler: StreamHandler = (conn, stream) =>
    stream.onMessage { msg =>
      stream.finish()
      decode(msg) match
        case Right(c) =>
          exec.execute(() =>
            guarded {
              speaks(conn.peerPublicKey) = true
              onCommitment(c)
            }
          )
        case Left(err) => logger.warn(s"bad BEEFY payload: $err")
    }

  private def guarded(f: => Unit): Unit =
    try f
    catch case e: Exception => logger.error("BEEFY step failed", e)

  private[node] def step(): Unit =
    resolveProbes(System.currentTimeMillis())
    val fin = chain.finalized
    if fin.hash != lastSigned && fin.slot > 0 then
      newlyFinalized(fin.hash).foreach(signBlock)
      lastSigned = fin.hash
    if pending.nonEmpty then
      val retry = pending.toList
      pending.clear()
      retry.foreach(onCommitment)

  /** Finalized blocks after the last one signed, oldest first. */
  private def newlyFinalized(head: Hash): List[Hash] =
    Iterator
      .iterate(Option(head))(_.flatMap(chain.parentHash))
      .takeWhile(h => h.exists(_ != lastSigned))
      .flatten
      .take(MaxSignPerStep)
      .toList
      .reverse

  private def signBlock(block: Hash): Unit =
    for
      k <- keys
      ctx <- context(block)
      slot <- chain.slotOf(block)
      index = ctx.ed25519.indexOf(k.ed25519Key)
      if index >= 0
    do
      val c = BeefyMessages.sign(block, slot, index, ctx.superPeak, k)
      accept(c, ctx)
      outbound.getOrElse(broadcast)(c)

  private def broadcast(c: SignedCommitment): Unit =
    val bytes = encode(c)
    val now = System.currentTimeMillis()
    resolveProbes(now)
    distribution.peers
      .distinctBy(_.peerPublicKey)
      .foreach { conn =>
        val key = conn.peerPublicKey
        val send = speaks.get(key) match
          case Some(known) => known
          case None =>
            val probe = !probes.contains(key)
            if probe then probes(key) = (conn, now)
            probe
        if send then distribution.submitSend(() => conn.openStream(StreamKind), StreamKind, bytes)
      }

  private def resolveProbes(now: Long): Unit =
    probes.toList.foreach { case (key, (conn, at)) =>
      if !conn.isOpen then
        probes.remove(key)
        if !speaks.contains(key) then
          speaks(key) = false
          logger.info(s"BEEFY: peer ${JamNode.hex(key.toArrayUnsafe).take(16)} dropped the connection; not sending it BEEFY")
      else if now - at >= ProbeMillis then
        probes.remove(key)
        speaks.getOrElseUpdate(key, true)
    }

  private def context(block: Hash): Option[Ctx] =
    ctxCache.getOrElseUpdate(block, readContext(block)) match
      case None if !chain.hasBlock(block) => ctxCache.remove(block); None
      case ctx =>
        while ctxCache.size > MaxTracked do ctxCache.remove(ctxCache.head._1)
        ctx

  private def readContext(block: Hash): Option[Ctx] =
    chain.stateAt(block).flatMap { trie =>
      val state = new TrieBackedJamState(trie, chain.config, new ServiceStorageView(trie))
      val vs = state.validators.current.toIndexedSeq
      state.beta.history.lastOption.map(h =>
        Ctx(vs.map(_.bls.bytes), vs.map(_.ed25519), h.beefyRoot)
      )
    }

  private[node] def onCommitment(c: SignedCommitment): Unit =
    if !proven.contains(c.block) then
      context(c.block) match
        case None =>
          if chain.hasBlock(c.block) then logger.debug(s"BEEFY: no super-peak at ${c.block.toHex.take(18)}")
          else if pending.size < MaxPending then pending += c
        case Some(ctx) =>
          check(c, ctx) match
            case Right(()) => accept(c, ctx)
            case Left(err) => logger.debug(s"BEEFY commitment from ${c.validatorIndex} rejected: $err")

  private def check(c: SignedCommitment, ctx: Ctx): Either[String, Unit] =
    val registered = ctx.keys.lift(c.validatorIndex)
    if registered.isEmpty then Left(s"validator index ${c.validatorIndex} out of range")
    else if c.superPeak != ctx.superPeak then Left("super-peak differs from ours")
    else if registered.exists(k => k.exists(_ != 0) && !java.util.Arrays.equals(k, c.blsKey)) then
      Left("BLS key differs from κ'")
    else if !keyConsistent(c.blsKey) then Left("BLS key halves inconsistent")
    else if !verify(c) then Left("bad signature")
    else
      if registered.exists(_.forall(_ == 0)) && !warnedUnregistered then
        warnedUnregistered = true
        logger.warn("BEEFY: κ' has no BLS keys (dev chain spec); trusting the keys sent with commitments")
      Right(())

  private def keyConsistent(k: Array[Byte]): Boolean =
    val key = JamBytes(k)
    consistentKeys.contains(key) || Bls.keyConsistent(k) && { consistentKeys += key; true }

  private def accept(c: SignedCommitment, ctx: Ctx): Unit =
    val sigs = received.getOrElseUpdate(c.block, mutable.Map.empty)
    sigs.getOrElseUpdate(c.validatorIndex, c)
    while received.size > MaxTracked do received.remove(received.head._1)
    if sigs.size >= threshold(ctx.keys.size) && !proven.contains(c.block) then
      BeefyMessages.aggregate(ctx.keys.size, sigs.values.toSeq.sortBy(_.validatorIndex)) match
        case Some(p) if Bls.verifyAggregate(p.signerIndices.map(sigs(_).blsKey), payload(p.superPeak), p.aggregate) =>
          proven += c.block
          while proven.size > MaxTracked do proven -= proven.head
          val bytes = encodeProof(p)
          chain.putMeta(s"beefy:proof:${c.block.toHex}", bytes)
          if latest.forall(_.slot < p.slot) then
            latest = Some(p)
            chain.putMeta("beefy:latest", bytes)
          logger.info(
            s"BEEFY proof for ${c.block.toHex.take(18)}@${p.slot}: ${sigs.size}/${ctx.keys.size} signers, " +
              s"super-peak ${p.superPeak.toHex.take(18)}"
          )
        case _ => logger.warn(s"BEEFY aggregate for ${c.block.toHex.take(18)} failed to verify")
