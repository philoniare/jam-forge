package io.forge.jam.node

import java.util.concurrent.ExecutorService

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.{JamBytes, LittleEndian}
import io.forge.jam.core.primitives.{BandersnatchPublicKey, Ed25519PublicKey, Hash}
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.tickets.{RingVrfSignatureSize, TicketEnvelope}
import io.forge.jam.crypto.BandersnatchVrf
import io.forge.jam.network.{PeerConnection, MessageStream, PeerStreamHandler, StreamKind}
import io.forge.jam.protocol.safrole.SafroleTransition
import spire.math.UByte

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.*

/** Safrole ticket generation and CE 131/132 distribution: once per epoch, for
  * every held validator key present in the pending set γ_k, ring-VRF proofs
  * over `jam_ticket_seal ++ η'_2 ++ attempt` are generated for each permitted
  * entry index. Tickets go to the local extrinsic pool for inclusion while
  * the submission window is open, and the (id → key, attempt) mapping is
  * retained for ticketed sealing in the next epoch.
  *
  * Tickets also cross the network: `distributeTickets` sends each locally
  * generated ticket to its proxy validator on CE 131; `ticketHandler`
  * verifies inbound tickets (CE 131 or CE 132), pools them, and — when acting
  * as proxy (CE 131) — re-distributes them to every validator on CE 132.
  */
final class TicketService(
    chain: ChainReader,
    pools: ExtrinsicPools,
    distribution: DistributionService,
    egress: ExecutorService,
    /** CE 132 recipients: the current validators (JAMNP-S CE 131/132). */
    forwardTargets: () => Iterable[PeerConnection] = () => Nil,
    /** Where received tickets are verified (a ring proof each): off the network threads. */
    inbound: InboundWork = InboundWork.direct,
    /** Owner of this service's threads: the node's, or its own when made alone. */
    threads: NodeThreads = new NodeThreads,
    misbehaved: (PeerConnection, Offence) => Unit = (_, _) => (),
    verifyTickets: TicketService.Verifier = TicketService.ringVerifier,
    ringRootOf: List[BandersnatchPublicKey] => Option[JamBytes] = null,
    telemetry: Option[Telemetry] = None
) extends LazyLogging:

  private val ringRoot: List[BandersnatchPublicKey] => Option[JamBytes] =
    if ringRootOf != null then ringRootOf else keys => BandersnatchVrf.generateRingRoot(keys, chain.config.ringSize)

  /** Tickets accepted as proxy (or generated for ourselves as proxy) and
    * waiting for their CE 132 send, with their contest epoch.
    */
  private val pendingForwards = new java.util.concurrent.ConcurrentLinkedQueue[(Long, TicketEnvelope)]()

  /** Ids of the tickets we took as proxy (CE 131), with their contest epoch: each is queued for CE 132 once, however
    * often it is sent to us. A ticket id is the VRF output for a ring member and attempt, so an epoch has at most
    * validators × attempts of them.
    */
  private val proxied = new ConcurrentHashMap[Hash, java.lang.Long]()

  /** Our tickets: id bytes → (keys, attempt), consulted when sealing a
    * ticketed slot.
    */
  val ownTickets = new ConcurrentHashMap[Hash, (ValidatorKeySet, UByte)]()

  /** Our tickets' full envelopes (attempt + ring proof), consulted by
    * `distributeTickets` for CE 131 submission.
    */
  private val ownTicketEnvelopes = new ConcurrentHashMap[Hash, TicketEnvelope]()

  /** Epoch each ticket id was generated in, so stale entries can be pruned
    * once their ticketed-sealing window has passed.
    */
  private val ticketEpoch = new ConcurrentHashMap[Hash, Long]()

  /** Locally held validator keys, registered via `setValidatorKeys` (called
    * by `JamNode.enableAuthoring`). Empty on a node that only relays/proxies
    * tickets without generating its own.
    */
  @volatile private var validatorKeys: Seq[ValidatorKeySet] = Nil

  private def bsByPublic: Map[BandersnatchPublicKey, ValidatorKeySet] =
    validatorKeys.map(k => k.bandersnatchKey -> k).toMap

  @volatile private var generatedForEpoch: Long = -1L

  /** Bumped at each (re)generation, so a sender can tell new tickets from ones it already sent. */
  @volatile var generation: Int = 0

  /** Epoch whose tickets are completely generated: senders on other threads must not see a partial set. */
  @volatile private var readyEpoch: Long = -1L
  @volatile private var generatedForEntropy: Hash = Hash.zero

  /** Where [[generateInBackground]] makes the ring proofs: one thread, so at most one generation at a time. */
  private lazy val generator = threads.single("jam-tickets")
  private val generating = new java.util.concurrent.atomic.AtomicBoolean(false)

  /** [[maybeGenerate]] on a thread of its own, unless one is already running: ring-VRF proofs for an epoch take far
    * longer than authoring a block, and the slot thread must not wait for them. Senders see the tickets once all of
    * them are made ([[hasTicketsFor]]).
    */
  def generateInBackground(wallSlot: Long): Unit =
    if validatorKeys.nonEmpty && generating.compareAndSet(false, true) then
      try
        generator.execute { () =>
          try maybeGenerate(wallSlot): Unit
          catch case e: Exception => logger.error("ticket generation failed", e)
          finally generating.set(false)
        }
      catch
        case _: java.util.concurrent.RejectedExecutionException => generating.set(false) // shutting down

  def shutdown(): Unit =
    if validatorKeys.nonEmpty then generator.shutdownNow(): Unit
    deferredRound.incrementAndGet(): Unit // a round still running stops at its next batch

  /** Register the validator keys this node holds, enabling ticket generation
    * for them.
    */
  def setValidatorKeys(keys: Seq[ValidatorKeySet]): Unit = validatorKeys = keys

  /** Generate this epoch's tickets if not already done for the head's
    * epoch and η_2; true when tickets were (re)generated. A no-op — including
    * skipping the `chain.stateView()` snapshot — when this node holds no
    * validator keys, so calling this every slot costs nothing on
    * sync/guarantor/auditor-only nodes.
    */
  def maybeGenerate(wallSlot: Long): Boolean =
    if validatorKeys.isEmpty then return false
    val view = chain.stateView()
    val epoch = view.timeslot / chain.config.epochLength
    // Only for the epoch we are in, while its lottery is open (slot index < Y, GP 0.8.0 §safrole): a head still
    // syncing (at genesis, say) would make tickets for an epoch that is long over.
    val e = chain.config.epochLength
    if epoch != wallSlot / e || wallSlot % e >= chain.config.ticketCutoff then return false
    val entropy = TicketService.contestEntropy(view)
    def done = epoch == generatedForEpoch && entropy == generatedForEntropy
    if done then return false
    synchronized {
      if done then false
      else
        if epoch == generatedForEpoch then
          // A reorg moved us to a fork with different epoch entropy: the
          // tickets made on the old fork can no longer be verified here.
          logger.info(s"epoch $epoch entropy changed after a reorg; regenerating tickets")
          dropEpoch(epoch)
        generatedForEpoch = epoch
        generatedForEntropy = entropy
        pruneStale(epoch)
        readyEpoch = -1L
        generate(view)
        readyEpoch = epoch
        generation += 1
        true
    }

  private def dropEpoch(epoch: Long): Unit =
    val it = ticketEpoch.entrySet().iterator()
    while it.hasNext do
      val entry = it.next()
      if entry.getValue == epoch then
        ownTickets.remove(entry.getKey)
        ownTicketEnvelopes.remove(entry.getKey)
        it.remove()

  /** Drop tickets generated more than one epoch ago: a ticket from epoch E is
    * consulted for ticketed sealing in epoch E+1, so it is safe to forget by
    * the time epoch E+2 starts
    */
  private def pruneStale(currentEpoch: Long): Unit =
    val it = ticketEpoch.entrySet().iterator()
    while it.hasNext do
      val entry = it.next()
      if entry.getValue < currentEpoch - 1 then
        ownTickets.remove(entry.getKey)
        ownTicketEnvelopes.remove(entry.getKey)
        it.remove()

  private def generate(view: io.forge.jam.protocol.state.TrieBackedJamState): Unit =
    val pending = view.validators.nextEpoch // γ_k: the ring for next epoch's contest
    val commitment = view.gamma.z // γ_z: ring root the STF verifies against
    val entropy = TicketService.contestEntropy(view)
    val ringSize = chain.config.ringSize
    val ringKeys = pending.map(_.bandersnatch).toList.padTo(ringSize, BandersnatchPublicKey(new Array[Byte](32)))

    val epoch = view.timeslot / chain.config.epochLength
    var produced = 0
    pending.zipWithIndex.foreach { case (vk, index) =>
      bsByPublic.get(vk.bandersnatch).foreach { keys =>
        val generating = telemetry.flatMap(_.generatingTickets(epoch + 1))
        val outputs = scala.collection.mutable.ListBuffer.empty[Array[Byte]]
        var failure: Option[String] = None
        for attemptInt <- 0 until SafroleTransition.maxTicketEntries(pending.size, chain.config) do
          val attempt = UByte(attemptInt)
          BandersnatchVrf.createRingProof(
            keys.bandersnatchSecret,
            ringKeys,
            index,
            entropy,
            attempt
          ) match
            case None =>
              logger.warn(s"ring proof failed for validator index $index attempt $attemptInt")
              failure = failure.orElse(Some(s"ring proof failed for attempt $attemptInt"))
            case Some(proof) =>
              // Verify our own proof to learn the ticket id (the VRF output).
              BandersnatchVrf.verifyRingProof(
                proof,
                commitment,
                entropy,
                attempt,
                ringSize
              ) match
                case None =>
                  logger.warn(s"self-verification failed for index $index attempt $attemptInt")
                  failure = failure.orElse(Some(s"ring proof of attempt $attemptInt does not verify"))
                case Some(result) =>
                  val id = Hash(result.ticketId.toArray)
                  outputs += id.bytes.toArray
                  val envelope = TicketEnvelope(attempt, proof)
                  pools.addTicket(id, envelope, epoch, entropy)
                  ownTickets.put(id, (keys, attempt))
                  ownTicketEnvelopes.put(id, envelope)
                  ticketEpoch.put(id, epoch)
                  produced += 1
        telemetry.foreach { t =>
          failure match
            case Some(why) => t.ticketGenerationFailed(generating, why)
            case None      => t.ticketsGenerated(generating, outputs.toList)
        }
      }
    }
    if produced > 0 then
      logger.info(s"generated $produced Safrole ticket(s) for epoch $epoch")

  /** Inbound CE 131/132: verify the ring proof, pool the ticket, and — when we
    * are the ticket's proxy (CE 131) — forward it to all validators on CE 132.
    */
  def ticketHandler: PeerStreamHandler = new PeerStreamHandler:
    def onStream(connection: PeerConnection, stream: MessageStream): Unit =
      stream.onMessage { msg =>
        val asProxy = stream.kind == StreamKind.TicketDistributionStep1
        val decoded = TicketCodec.decode(msg)
        telemetry.foreach { t =>
          decoded.flatMap(d => TicketService.vrfOutput(d._2).toRight("proof output does not decode").map(d -> _)) match
            case Right(((epoch, env), output)) =>
              t.ticketTransferred(connection.peerKey, remote = true, ce132 = !asProxy, epoch + 1, env.attempt.toInt, output)
            case Left(why) => t.ticketTransferFailed(connection.peerKey, remote = true, ce132 = !asProxy, why)
        }
        if decoded.isLeft then misbehaved(connection, Offence.Malformed)
        val sender = senderKey(connection)
        val admitted = inbound.submit(connection, msg.length.toLong) {
          val taken =
            receive(msg, asProxy, peerLabel(connection), sender, () => misbehaved(connection, Offence.BadProof))
          if taken then stream.finish() else stream.reset()
        }
        if !admitted then stream.reset()
      }

  private[node] def receive(
      msg: Array[Byte],
      asProxy: Boolean,
      from: String,
      sender: Option[Ed25519PublicKey] = None,
      badProof: () => Unit = () => ()
  ): Boolean =
    try
      TicketCodec.decode(msg) match
        case Left(err) =>
          logger.warn(s"CE131/132: $err")
          false
        case Right((epoch, env)) =>
          val view = chain.stateView()
          val currentEpoch = view.timeslot / chain.config.epochLength
          if epoch == currentEpoch then accept(view, epoch, env, asProxy, from, sender, badProof)
          else if epoch == currentEpoch + 1 then
            defer(view, epoch, env, asProxy, from, sender)
          else
            // JAMNP-S CE 131/132: reset when the state needed to verify
            // the ticket is not known.
            logger.debug(s"ticket from $from for epoch $epoch, current $currentEpoch — rejected")
            false
    catch
      case e: Exception =>
        logger.warn(s"ticket handler failed: ${e.getMessage}")
        false

  private final class Deferred(
      val epoch: Long,
      val id: Hash,
      val env: TicketEnvelope,
      @volatile var asProxy: Boolean,
      val peer: String,
      val root: JamBytes,
      val entropy: Hash
  )

  private val deferredLock = new Object
  private val deferred = scala.collection.mutable.LinkedHashMap.empty[Hash, Deferred]
  private val deferredByPeer = scala.collection.mutable.Map.empty[String, Int]

  private val maxDeferred: Int = 2 * chain.config.epochLength + chain.config.ringSize

  private def maxDeferredPerPeer(pendingSize: Int): Int =
    2 * SafroleTransition.maxTicketEntries(pendingSize, chain.config)

  private def predictedPending(view: io.forge.jam.protocol.state.TrieBackedJamState): IndexedSeq[Option[ValidatorKey]] =
    val offenders = view.psi.offenders.toSet
    view.validators.queue.map(k => Option.when(!offenders.contains(k.ed25519))(k))

  @volatile private var predictedRoot: (List[BandersnatchPublicKey], JamBytes) = null

  private def ringRootFor(keys: List[BandersnatchPublicKey]): Option[JamBytes] =
    val cached = predictedRoot
    if cached != null && cached._1 == keys then Some(cached._2)
    else
      val root = ringRoot(keys)
      root.foreach(r => predictedRoot = (keys, r))
      root

  private def defer(
      view: io.forge.jam.protocol.state.TrieBackedJamState,
      epoch: Long,
      env: TicketEnvelope,
      asProxy: Boolean,
      from: String,
      sender: Option[Ed25519PublicKey]
  ): Boolean =
    val pending = predictedPending(view)
    def refuse(why: String): Boolean =
      logger.debug(s"ticket from $from for epoch $epoch not kept: $why")
      false
    if pending.isEmpty then refuse("no pending set")
    else if env.attempt.toInt >= SafroleTransition.maxTicketEntries(pending.size, chain.config) then
      refuse(s"entry index ${env.attempt.toInt} out of range")
    else if env.signature.length != RingVrfSignatureSize then refuse("proof length")
    else
      BandersnatchVrf.ringVrfOutput(env.signature.toArray).map(Hash(_)) match
        case None => refuse("proof output does not decode")
        case Some(id) =>
          val known = deferredLock.synchronized {
            deferred.get(id).map { d =>
              if asProxy then d.asProxy = true
              true
            }
          }
          if known.isDefined then true
          else if sender.exists(k => !pending.exists(_.exists(_.ed25519 == k))) then
            refuse("sender is not a validator of the pending set")
          else if asProxy && !pending.lift(TicketService.proxyIndexFor(id, pending.size)).flatten.exists(vk =>
              validatorKeys.exists(_.ed25519Key == vk.ed25519)
            )
          then refuse("CE 131 to the wrong proxy")
          else
            val peer = sender.map(k => JamBytes(k.bytes).toHex).getOrElse(from)
            val perPeer = maxDeferredPerPeer(pending.size)
            val reserved = deferredLock.synchronized {
              val held = deferredByPeer.getOrElse(peer, 0)
              if held >= perPeer || deferred.size + reservedTotal >= maxDeferred then false
              else
                deferredByPeer.update(peer, held + 1)
                reservedTotal += 1
                true
            }
            if !reserved then refuse(s"its sender has $perPeer kept or all $maxDeferred are taken")
            else
              var kept = false
              try
                val zero = BandersnatchPublicKey(new Array[Byte](32))
                val ring = pending.map(_.map(_.bandersnatch).getOrElse(zero)).toList.padTo(chain.config.ringSize, zero)
                val entropy = view.entropy.pool.lift(1).getOrElse(Hash.zero) // η_1 now, η'_2 in the next epoch
                ringRootFor(ring) match
                  case None => refuse("ring root of the pending set")
                  case Some(root) =>
                    val verified = verifyTickets(Seq(env), root, entropy, chain.config.ringSize).headOption.flatten
                    if !verified.contains(id) then refuse("ring proof does not verify against the pending set")
                    else
                      deferredLock.synchronized {
                        deferred.get(id) match
                          case Some(d) => if asProxy then d.asProxy = true
                          case None =>
                            deferred.put(id, Deferred(epoch, id, env, asProxy, peer, root, entropy))
                            kept = true
                      }
                      true
              finally
                deferredLock.synchronized {
                  reservedTotal -= 1
                  if !kept then release(peer)
                }

  private var reservedTotal = 0

  private def release(peer: String): Unit =
    deferredByPeer.get(peer).foreach(n => if n <= 1 then deferredByPeer.remove(peer) else deferredByPeer.update(peer, n - 1))

  private def take(d: Deferred): Boolean =
    if deferred.get(d.id).exists(_ eq d) then
      deferred.remove(d.id)
      release(d.peer)
      true
    else false

  private[node] def deferredCount: Int = deferredLock.synchronized(deferred.size)
  private[node] def pendingForwardCount: Int = pendingForwards.size

  private lazy val deferredWorker = threads.single("jam-tickets-deferred")
  private val deferredRound = new java.util.concurrent.atomic.AtomicLong()

  def processDeferred(): Unit =
    val due = deferredLock.synchronized(deferred.nonEmpty) && {
      val currentEpoch = chain.stateView().timeslot / chain.config.epochLength
      deferredLock.synchronized(deferred.valuesIterator.exists(_.epoch <= currentEpoch))
    }
    if due then
      val round = deferredRound.incrementAndGet()
      try
        deferredWorker.execute { () =>
          try takeInDeferred(round)
          catch case e: Exception => logger.error(s"deferred tickets: ${e.getMessage}", e)
        }
      catch case _: java.util.concurrent.RejectedExecutionException => () // shutting down

  private def takeInDeferred(round: Long): Unit =
    val view = chain.stateView()
    val epoch = view.timeslot / chain.config.epochLength
    val entropy = TicketService.contestEntropy(view)
    val root = view.gamma.z
    val due = deferredLock.synchronized {
      deferred.values.filter(_.epoch < epoch).toList.foreach(take)
      deferred.values.filter(_.epoch == epoch).toList
    }
    val (checked, unchecked) = due.partition(d => d.root == root && d.entropy == entropy)
    checked.foreach(d => if deferredLock.synchronized(take(d)) then admit(view, epoch, d.id, d.env, d.asProxy, d.peer))
    val batches = unchecked.grouped(TicketService.DeferredBatch)
    while batches.hasNext && deferredRound.get == round do
      val batch = batches.next()
      val ids = verifyTickets(batch.map(_.env), root, entropy, chain.config.ringSize)
      batch.zip(ids).foreach { (d, id) =>
        if deferredLock.synchronized(take(d)) then
          if id.contains(d.id) then admit(view, epoch, d.id, d.env, d.asProxy, d.peer)
          else logger.debug(s"deferred ticket ${d.id.toHex.take(18)} for epoch $epoch does not verify; dropped")
      }
    if checked.nonEmpty || unchecked.nonEmpty then
      logger.debug(s"deferred tickets for epoch $epoch: ${checked.size} pooled, ${unchecked.size} checked again")

  private def peerLabel(conn: PeerConnection): String =
    try JamBytes(conn.peerKey).toHex.take(16)
    catch case _: Exception => "?"

  private def senderKey(conn: PeerConnection): Option[Ed25519PublicKey] =
    try Option(conn.peerKey).filter(_.length == 32).map(Ed25519PublicKey(_))
    catch case _: Exception => None

  /** Verify a ticket for the current epoch against γ_z and η_2 and pool it.
    * A CE 131 ticket is queued for CE 132 forwarding only when we hold the
    * key of its proxy validator (JAMNP-S CE 131: "verify that they are the
    * correct proxy"). False when a check fails.
    */
  private def accept(
      view: io.forge.jam.protocol.state.TrieBackedJamState,
      epoch: Long,
      env: TicketEnvelope,
      asProxy: Boolean,
      from: String,
      sender: Option[Ed25519PublicKey],
      badProof: () => Unit
  ): Boolean =
    val next = view.validators.nextEpoch
    def refuse(why: String): Boolean =
      logger.debug(s"ticket from $from for epoch $epoch not checked: $why")
      false
    if sender.exists(k => !next.exists(_.ed25519 == k) && !view.validators.current.exists(_.ed25519 == k)) then
      refuse("the sender is not a validator of the current or the next set")
    else if env.attempt.toInt >= SafroleTransition.maxTicketEntries(next.size, chain.config) then
      refuse(s"entry index ${env.attempt.toInt} out of range")
    else if env.signature.length != RingVrfSignatureSize then refuse("proof length")
    else if view.timeslot % chain.config.epochLength >= chain.config.ticketCutoff then
      refuse("the lottery of this epoch is over")
    else
      val entropy = TicketService.contestEntropy(view)
      verifyTickets(Seq(env), view.gamma.z, entropy, chain.config.ringSize).headOption.flatten match
        case None =>
          logger.warn(
            s"CE131/132 ticket from $from for epoch $epoch failed ring verification against η_2 ${entropy.toHex.take(18)}; dropped"
          )
          badProof()
          false
        case Some(id) => admit(view, epoch, id, env, asProxy, from)

  private def admit(
      view: io.forge.jam.protocol.state.TrieBackedJamState,
      epoch: Long,
      id: Hash,
      env: TicketEnvelope,
      asProxy: Boolean,
      from: String
  ): Boolean =
    if asProxy && !holdsProxyKey(view, id) then
      logger.debug(s"CE131 ticket ${id.toHex.take(18)} from $from sent to the wrong proxy — rejected")
      false
    else
      pools.addTicket(id, env, epoch, TicketService.contestEntropy(view))
      if asProxy && proxied.putIfAbsent(id, epoch) == null then
        proxied.values.removeIf(_ < epoch)
        pendingForwards.add((epoch, env))
      true

  /** True when we hold the Ed25519 key of `id`'s proxy in γ_k. */
  private def holdsProxyKey(view: io.forge.jam.protocol.state.TrieBackedJamState, id: Hash): Boolean =
    val next = view.validators.nextEpoch
    next.nonEmpty && next.lift(TicketService.proxyIndexFor(id, next.size)).exists(vk =>
      validatorKeys.exists(_.ed25519Key == vk.ed25519)
    )

  /** Send queued proxy tickets on CE 132, spread evenly from
    * max(⌊E/20⌋, 1) slots after connectivity changes were applied
    * (`appliedSlot`) until half-way through the lottery period Y (JAMNP-S
    * CE 131). Tickets of an earlier contest epoch are dropped. Called once
    * per slot.
    */
  def releaseForwards(slot: Long, appliedSlot: Long): Unit =
    val e = chain.config.epochLength
    val epoch = slot / e
    pendingForwards.removeIf(_._1 < epoch)
    // A ticket reaching an author at slot index `ticketCutoff - 1` or later misses the lottery: one still queued
    // then, as on a node that came up late in the epoch, is dropped, not sent.
    if slot % e >= chain.config.ticketCutoff - 1 then
      if !pendingForwards.isEmpty then
        logger.debug(s"dropping ${pendingForwards.size} proxy ticket(s): the lottery of epoch $epoch is over")
        pendingForwards.clear()
      return
    val start = appliedSlot + math.max(e / 20, 1)
    if pendingForwards.isEmpty || slot < start then return
    val end = math.max(epoch * e + chain.config.ticketCutoff / 2, start + 1)
    val slotsLeft = math.max(end - slot, 1L)
    val n = math.ceil(pendingForwards.size.toDouble / slotsLeft).toInt
    forwardQueued(n)

  /** Send every queued proxy ticket now. */
  def flushForwards(): Unit = forwardQueued(Int.MaxValue)

  private def forwardQueued(n: Int): Unit =
    var left = n
    while left > 0 do
      Option(pendingForwards.poll()) match
        case Some((epoch, env)) =>
          forwardAsProxy(epoch, env)
          left -= 1
        case None => left = 0

  /** Forward a ticket we accepted as proxy on CE 132 to the current
    * validators, or to every peer when none of them is connected under its
    * validator key.
    */
  private def forwardAsProxy(epoch: Long, env: TicketEnvelope): Unit =
    val payload = TicketCodec.encode(epoch, env)
    val targets = forwardTargets()
    (if targets.nonEmpty then targets else distribution.peers).foreach { conn =>
      submitSend(
        () => conn.openStream(StreamKind.TicketDistributionStep2),
        StreamKind.TicketDistributionStep2,
        payload,
        conn
      )
    }

  /** Send each ticket we generated this epoch on CE 131 to its proxy
    * validator (`proxyIndexFor`, resolved to a connection via
    * `byValidatorIndex`). When we hold the proxy's key ourselves we act as
    * proxy and send on CE 132 to every peer; when the proxy has no open
    * connection (a peer not using its validator key as network identity, or
    * a dropped link) we fall back to the same CE 132 broadcast.
    */
  /** Whether this epoch's tickets are fully generated. */
  def hasTicketsFor(epoch: Long): Boolean = readyEpoch == epoch

  def distributeTickets(
      peers: Iterable[PeerConnection],
      byValidatorIndex: Int => Option[PeerConnection]
  ): Unit =
    if ownTicketEnvelopes.isEmpty then return
    val view = chain.stateView()
    val epoch = view.timeslot / chain.config.epochLength
    val validatorCount = view.validators.nextEpoch.size
    if validatorCount == 0 then return
    val current =
      TicketService.ticketsForEpoch(epoch, ownTicketEnvelopes.asScala.toMap, ticketEpoch.asScala.toMap)
    current.foreach { case (id, env) =>
      val proxyIndex = TicketService.proxyIndexFor(id, validatorCount)
      val payload = TicketCodec.encode(epoch, env)
      val ownProxy = view.validators.nextEpoch.lift(proxyIndex).exists(vk =>
        validatorKeys.exists(_.ed25519Key == vk.ed25519)
      )
      // As our own proxy we send on CE 132 ourselves, on the proxy schedule.
      if ownProxy then pendingForwards.add((epoch, env))
      else byValidatorIndex(proxyIndex) match
        case Some(proxyConn) =>
          submitSend(
            () => proxyConn.openStream(StreamKind.TicketDistributionStep1),
            StreamKind.TicketDistributionStep1,
            payload,
        proxyConn
          )
        case None =>
          logger.debug(
            s"no open connection to proxy validator $proxyIndex " +
              s"(ticket ${id.toHex}); broadcasting on CE132 directly"
          )
          peers.foreach { conn =>
            submitSend(
              () => conn.openStream(StreamKind.TicketDistributionStep2),
              StreamKind.TicketDistributionStep2,
              payload,
        conn
            )
          }
    }

  private[node] def submitSend(
      open: () => java.util.concurrent.CompletableFuture[? <: MessageStream],
      kind: Byte,
      payload: Array[Byte],
      peer: AnyRef = null
  ): Unit =
    OneShotSend.submit(
      egress,
      peer,
      open,
      List(payload),
      why => {
        logger.warn(s"ticket distribution on ${StreamKind.name(kind)} failed: $why")
        reportSend(peer, kind, payload, Some(why))
      },
      () => logger.warn(s"egress rejected ${StreamKind.name(kind)} send (shutting down)"),
      sent = () => reportSend(peer, kind, payload, None)
    )

  private def reportSend(peer: AnyRef, kind: Byte, payload: Array[Byte], failed: Option[String]): Unit =
    (telemetry, peer) match
      case (Some(t), conn: PeerConnection) =>
        val ce132 = kind == StreamKind.TicketDistributionStep2
        failed match
          case Some(why) => t.ticketTransferFailed(conn.peerKey, remote = false, ce132, why)
          case None =>
            TicketCodec.decode(payload).foreach { (epoch, env) =>
              TicketService.vrfOutput(env).foreach(out =>
                t.ticketTransferred(conn.peerKey, remote = false, ce132, epoch + 1, env.attempt.toInt, out)
              )
            }
      case _ => ()

object TicketService:
  type Verifier = (Seq[TicketEnvelope], JamBytes, Hash, Int) => Seq[Option[Hash]]

  val DeferredBatch: Int = 64

  val ringVerifier: Verifier = (envs, root, entropy, ringSize) =>
    def single(e: TicketEnvelope): Option[Hash] =
      BandersnatchVrf.verifyRingProof(e.signature, root, entropy, e.attempt, ringSize).map(r => Hash(r.ticketId.toArray))
    if envs.sizeIs < 2 then envs.map(single)
    else
      BandersnatchVrf.verifyRingProofs(envs.map(e => (e.signature, e.attempt)), root, entropy, ringSize) match
        case Some(ids) => ids.map(id => Some(Hash(id.toArray)))
        case None      => envs.map(single)
  def vrfOutput(env: TicketEnvelope): Option[Hash] =
    try BandersnatchVrf.ringVrfOutput(env.signature.toArray).map(Hash(_))
    catch case _: Exception => None

  /** η_2 of `view`: the entropy the current epoch's ticket contest uses. */
  def contestEntropy(view: io.forge.jam.protocol.state.TrieBackedJamState): Hash =
    view.entropy.pool.lift(2).getOrElse(Hash.zero)

  /** Proxy validator for a ticket: last 4 bytes of the VRF output id,
    * big-endian, modulo validator count (jamnp-s CE 131).
    */
  def proxyIndexFor(ticketId: Hash, validatorCount: Int): Int =
    val b = ticketId.bytes
    val v = ((b(28) & 0xffL) << 24) | ((b(29) & 0xffL) << 16) | ((b(30) & 0xffL) << 8) | (b(31) & 0xffL)
    (v % validatorCount).toInt

  private[node] def ticketsForEpoch(
      epoch: Long,
      envelopes: Map[Hash, TicketEnvelope],
      epochOf: Map[Hash, Long]
  ): Map[Hash, TicketEnvelope] =
    envelopes.filter { case (id, _) => epochOf.getOrElse(id, -1L) == epoch }

/** CE 131/132 wire format (jamnp-s): Epoch Index (u32 LE) ++ Attempt (1) ++
  * RingVRF proof (784). Both kinds carry the identical message.
  *
  * The wire epoch is the epoch the ticket will be *used* in (JAMNP-S CE 131:
  * "Epoch index should identify the epoch that the ticket will be used in"),
  * one after the contest epoch whose γ_z and η_2 the proof is made against.
  * Callers work in contest epochs; the codec adds and removes the one.
  */
object TicketCodec:
  val MessageLength: Int = 4 + 1 + RingVrfSignatureSize

  def encode(contestEpoch: Long, env: TicketEnvelope): Array[Byte] =
    LittleEndian.encode(contestEpoch + 1, 4) ++ Array(env.attempt.toByte) ++ env.signature.toArray

  /** Decodes to `(contest epoch, ticket)`. */
  def decode(bytes: Array[Byte]): Either[String, (Long, TicketEnvelope)] =
    if bytes.length != MessageLength then
      Left(s"CE131/132: expected $MessageLength bytes, got ${bytes.length}")
    else
      val useEpoch = LittleEndian.get(bytes, 0, 4)
      if useEpoch == 0 then Left("CE131/132: epoch 0 cannot be a ticket's use epoch")
      else
        val attempt = UByte(bytes(4))
        val proof = JamBytes(java.util.Arrays.copyOfRange(bytes, 5, MessageLength))
        Right((useEpoch - 1, TicketEnvelope(attempt, proof)))
