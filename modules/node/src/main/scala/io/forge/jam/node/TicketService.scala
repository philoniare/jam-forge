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
    egress: ExecutorService
) extends LazyLogging:

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

  /** Register the validator keys this node holds, enabling ticket generation
    * for them.
    */
  def setValidatorKeys(keys: Seq[ValidatorKeySet]): Unit = validatorKeys = keys

  /** Generate this epoch's tickets if not already done. A no-op — including
    * skipping the `chain.stateView()` snapshot — when this node holds no
    * validator keys, so calling this every slot costs nothing on
    * sync/guarantor/auditor-only nodes.
    */
  def maybeGenerate(): Unit =
    if validatorKeys.isEmpty then return
    val view = chain.stateView()
    val epoch = view.timeslot / chain.config.epochLength
    if epoch == generatedForEpoch then return
    synchronized {
      if epoch == generatedForEpoch then return
      generatedForEpoch = epoch
      pruneStale(epoch)
      generate(view)
    }

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
    val entropy = // η_2: ticket-contest entropy
      if view.entropy.pool.length > 2 then view.entropy.pool(2)
      else Hash(new Array[Byte](32))
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
                  pools.addTicket(id, envelope)
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
        try
          TicketCodec.decode(msg) match
            case Left(err) => logger.warn(s"${StreamKind.name(stream.kind)}: $err")
            case Right((epoch, env)) =>
              val view = chain.stateView()
              val currentEpoch = view.timeslot / chain.config.epochLength
              if epoch != currentEpoch then
                logger.warn(s"ticket for epoch $epoch, current $currentEpoch — dropped")
              else
                val commitment = view.gamma.z
                val entropy =
                  if view.entropy.pool.length > 2 then view.entropy.pool(2)
                  else Hash(new Array[Byte](32))
                BandersnatchVrf.verifyRingProof(
                  env.signature,
                  commitment,
                  entropy,
                  env.attempt,
                  view.validators.nextEpoch.size
                ) match
                  case None => logger.warn("CE131/132 ticket failed ring verification — dropped")
                  case Some(result) =>
                    val id = Hash(result.ticketId.toArray)
                    pools.addTicket(id, env)
                    if stream.kind == StreamKind.TicketDistributionStep1 then
                      forwardAsProxy(epoch, env)
        catch case e: Exception => logger.warn(s"ticket handler failed: ${e.getMessage}")
        stream.finish()
      }

  /** Forward a ticket we accepted as proxy (CE 131) to every connected
    * validator on CE 132.
    */
  private def forwardAsProxy(epoch: Long, env: TicketEnvelope): Unit =
    val payload = TicketCodec.encode(epoch, env)
    distribution.peers.foreach { conn =>
      submitSend(
        () => conn.openStream(StreamKind.TicketDistributionStep2),
        StreamKind.TicketDistributionStep2,
        payload
      )
    }

  /** Send each ticket we generated this epoch on CE 131 to its proxy
    * validator (`proxyIndexFor`, resolved to a connection via
    * `byValidatorIndex`). When no tracked connection exists for the proxy —
    * a devnet-scale simplification, since validator-index → connection
    * mapping isn't tracked yet — fall back to broadcasting directly on
    * CE 132 to every peer.
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
      byValidatorIndex(proxyIndex) match
        case Some(proxyConn) =>
          submitSend(
            () => proxyConn.openStream(StreamKind.TicketDistributionStep1),
            StreamKind.TicketDistributionStep1,
            payload
          )
        case None =>
          logger.warn(
            s"no tracked connection for proxy validator $proxyIndex " +
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
  */
object TicketCodec:
  val MessageLength: Int = 4 + 1 + RingVrfSignatureSize

  def encode(epoch: Long, env: TicketEnvelope): Array[Byte] =
    LittleEndian.encode(epoch, 4) ++ Array(env.attempt.toByte) ++ env.signature.toArray

  def decode(bytes: Array[Byte]): Either[String, (Long, TicketEnvelope)] =
    if bytes.length != MessageLength then
      Left(s"CE131/132: expected $MessageLength bytes, got ${bytes.length}")
    else
      val epoch = LittleEndian.get(bytes, 0, 4)
      val attempt = UByte(bytes(4))
      val proof = JamBytes(java.util.Arrays.copyOfRange(bytes, 5, MessageLength))
      Right((epoch, TicketEnvelope(attempt, proof)))
