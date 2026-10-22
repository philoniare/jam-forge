package io.forge.jam.node

import java.util.concurrent.{CompletableFuture, ExecutorService, TimeUnit}

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.{JamBytes, LittleEndian}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.tickets.{RingVrfSignatureSize, TicketEnvelope}
import io.forge.jam.crypto.BandersnatchVrf
import io.forge.jam.network.{JamnpConnection, JamnpStream, StreamHandler, StreamKind}
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
    chain: ChainManager,
    pools: ExtrinsicPools,
    distribution: DistributionService,
    egress: ExecutorService,
    /** CE 132 recipients: the current validators (JAMNP-S CE 131/132). */
    forwardTargets: () => Iterable[JamnpConnection] = () => Nil
) extends LazyLogging:

  /** Tickets accepted as proxy (or generated for ourselves as proxy) and
    * waiting for their CE 132 send, with their contest epoch.
    */
  private val pendingForwards = new java.util.concurrent.ConcurrentLinkedQueue[(Long, TicketEnvelope)]()

  /** Our tickets: id bytes → (keys, attempt), consulted when sealing a
    * ticketed slot.
    */
  val ownTickets = new ConcurrentHashMap[Seq[Byte], (ValidatorKeySet, UByte)]()

  /** Our tickets' full envelopes (attempt + ring proof), consulted by
    * `distributeTickets` for CE 131 submission.
    */
  private val ownTicketEnvelopes = new ConcurrentHashMap[Seq[Byte], TicketEnvelope]()

  /** Epoch each ticket id was generated in, so stale entries can be pruned
    * once their ticketed-sealing window has passed.
    */
  private val ticketEpoch = new ConcurrentHashMap[Seq[Byte], Long]()

  /** Locally held validator keys, registered via `setValidatorKeys` (called
    * by `JamNode.enableAuthoring`). Empty on a node that only relays/proxies
    * tickets without generating its own.
    */
  @volatile private var validatorKeys: Seq[ValidatorKeySet] = Nil

  private def bsByPublic: Map[Seq[Byte], ValidatorKeySet] =
    validatorKeys.map(k => k.bandersnatchPublic.toSeq -> k).toMap

  @volatile private var generatedForEpoch: Long = -1L
  @volatile private var generatedForEntropy: Hash = Hash.zero

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
  def maybeGenerate(): Boolean =
    if validatorKeys.isEmpty then return false
    val view = chain.stateView()
    val epoch = view.timeslot / chain.config.epochLength
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
        generate(view)
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
    val ringKeys = pending.map(_.bandersnatch)
    val ringSize = ringKeys.size

    val epoch = view.timeslot / chain.config.epochLength
    var produced = 0
    pending.zipWithIndex.foreach { case (vk, index) =>
      bsByPublic.get(vk.bandersnatch.bytes.toArray.toSeq).foreach { keys =>
        for attemptInt <- 0 until chain.config.ticketsPerValidator do
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
                case Some(result) =>
                  val id = Hash(result.ticketId.toArray)
                  val envelope = TicketEnvelope(attempt, proof)
                  pools.addTicket(id, envelope, epoch, entropy)
                  val idSeq = result.ticketId.toArray.toSeq
                  ownTickets.put(idSeq, (keys, attempt))
                  ownTicketEnvelopes.put(idSeq, envelope)
                  ticketEpoch.put(idSeq, epoch)
                  produced += 1
      }
    }
    if produced > 0 then
      logger.info(s"generated $produced Safrole ticket(s) for epoch $epoch")

  /** Inbound CE 131/132: verify the ring proof, pool the ticket, and — when we
    * are the ticket's proxy (CE 131) — forward it to all validators on CE 132.
    */
  def ticketHandler: StreamHandler = new StreamHandler:
    def onStream(connection: JamnpConnection, stream: JamnpStream): Unit =
      stream.onMessage { msg =>
        val accepted =
          try
            TicketCodec.decode(msg) match
              case Left(err) =>
                logger.warn(s"${StreamKind.name(stream.kind)}: $err")
                false
              case Right((epoch, env)) =>
                val currentEpoch = chain.stateView().timeslot / chain.config.epochLength
                val asProxy = stream.kind == StreamKind.TicketDistributionStep1
                if epoch == currentEpoch then accept(epoch, env, asProxy)
                else if epoch == currentEpoch + 1 then
                  // The sender's head reached the new epoch before ours: its
                  // proof is against the next epoch's η_2/γ_z, which our state
                  // will hold once we import that epoch's first block.
                  deferred.add((epoch, env, asProxy))
                  true
                else
                  // JAMNP-S CE 131/132: reset when the state needed to verify
                  // the ticket is not known.
                  logger.debug(s"ticket for epoch $epoch, current $currentEpoch — rejected")
                  false
          catch
            case e: Exception =>
              logger.warn(s"ticket handler failed: ${e.getMessage}")
              false
        if accepted then stream.finish() else stream.reset()
      }

  /** Tickets received for the epoch after our head's, checked once our head
    * enters that epoch.
    */
  private val deferred = new java.util.concurrent.ConcurrentLinkedQueue[(Long, TicketEnvelope, Boolean)]()

  /** Verify and pool deferred tickets whose epoch our head has reached;
    * drop those that are now stale. Called after every import.
    */
  def processDeferred(): Unit =
    if deferred.isEmpty then return
    val currentEpoch = chain.stateView().timeslot / chain.config.epochLength
    val it = deferred.iterator()
    while it.hasNext do
      val (epoch, env, asProxy) = it.next()
      if epoch <= currentEpoch then
        it.remove()
        if epoch == currentEpoch && !accept(epoch, env, asProxy) then
          logger.debug(s"deferred ticket for epoch $epoch rejected")

  /** Verify a ticket for the current epoch against γ_z and η_2 and pool it.
    * A CE 131 ticket is queued for CE 132 forwarding only when we hold the
    * key of its proxy validator (JAMNP-S CE 131: "verify that they are the
    * correct proxy"). False when a check fails.
    */
  private def accept(epoch: Long, env: TicketEnvelope, asProxy: Boolean): Boolean =
    val view = chain.stateView()
    val entropy = TicketService.contestEntropy(view)
    BandersnatchVrf.verifyRingProof(
      env.signature,
      view.gamma.z,
      entropy,
      env.attempt,
      view.validators.nextEpoch.size
    ) match
      case None =>
        logger.warn("CE131/132 ticket failed ring verification — dropped")
        false
      case Some(result) =>
        val id = Hash(result.ticketId.toArray)
        if asProxy && !holdsProxyKey(view, id) then
          logger.debug(s"CE131 ticket ${id.toHex.take(18)} sent to the wrong proxy — rejected")
          false
        else
          pools.addTicket(id, env, epoch, entropy)
          if asProxy then pendingForwards.add((epoch, env))
          true

  /** True when we hold the Ed25519 key of `id`'s proxy in γ_k. */
  private def holdsProxyKey(view: io.forge.jam.protocol.state.TrieBackedJamState, id: Hash): Boolean =
    val next = view.validators.nextEpoch
    next.nonEmpty && next.lift(TicketService.proxyIndexFor(id, next.size)).exists(vk =>
      validatorKeys.exists(k => java.util.Arrays.equals(k.ed25519Public, vk.ed25519.bytes.toArray))
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
        payload
      )
    }

  /** Send each ticket we generated this epoch on CE 131 to its proxy
    * validator (`proxyIndexFor`, resolved to a connection via
    * `byValidatorIndex`). When we hold the proxy's key ourselves we act as
    * proxy and send on CE 132 to every peer; when the proxy has no open
    * connection (a peer not using its validator key as network identity, or
    * a dropped link) we fall back to the same CE 132 broadcast.
    */
  def distributeTickets(
      peers: Iterable[JamnpConnection],
      byValidatorIndex: Int => Option[JamnpConnection]
  ): Unit =
    if ownTicketEnvelopes.isEmpty then return
    val view = chain.stateView()
    val epoch = view.timeslot / chain.config.epochLength
    val validatorCount = view.validators.nextEpoch.size
    if validatorCount == 0 then return
    val current =
      TicketService.ticketsForEpoch(epoch, ownTicketEnvelopes.asScala.toMap, ticketEpoch.asScala.toMap)
    current.foreach { case (idSeq, env) =>
      val id = Hash(idSeq.toArray)
      val proxyIndex = TicketService.proxyIndexFor(id, validatorCount)
      val payload = TicketCodec.encode(epoch, env)
      val ownProxy = view.validators.nextEpoch.lift(proxyIndex).exists(vk =>
        validatorKeys.exists(k => java.util.Arrays.equals(k.ed25519Public, vk.ed25519.bytes.toArray))
      )
      // As our own proxy we send on CE 132 ourselves, on the proxy schedule.
      if ownProxy then pendingForwards.add((epoch, env))
      else byValidatorIndex(proxyIndex) match
        case Some(proxyConn) =>
          submitSend(
            () => proxyConn.openStream(StreamKind.TicketDistributionStep1),
            StreamKind.TicketDistributionStep1,
            payload
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
              payload
            )
          }
    }

  private[node] def submitSend(
      open: () => CompletableFuture[JamnpStream],
      kind: Byte,
      payload: Array[Byte]
  ): Unit =
    try
      egress.submit(new Runnable {
        def run(): Unit =
          try
            val stream = open().get(10, TimeUnit.SECONDS)
            stream.send(payload)
            stream.finish()
          catch
            case e: Exception =>
              logger.warn(s"ticket distribution on ${StreamKind.name(kind)} failed: ${e.getMessage}")
      })
    catch
      case _: java.util.concurrent.RejectedExecutionException =>
        logger.warn(s"egress rejected ${StreamKind.name(kind)} send (shutting down)")

object TicketService:
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
      envelopes: Map[Seq[Byte], TicketEnvelope],
      epochOf: Map[Seq[Byte], Long]
  ): Map[Seq[Byte], TicketEnvelope] =
    envelopes.filter { case (idSeq, _) => epochOf.getOrElse(idSeq, -1L) == epoch }

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
