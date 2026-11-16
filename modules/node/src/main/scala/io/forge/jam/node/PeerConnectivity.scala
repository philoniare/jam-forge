package io.forge.jam.node

import java.net.InetSocketAddress

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.JamBytes
import io.forge.jam.network.{PeerConnection, JamnpNode, NodeIdentity}

private[node] final class PeerConnectivity(
    identity: NodeIdentity,
    network: JamnpNode,
    chain: ChainReader,
    sync: SyncService,
    distribution: DistributionService,
    spec: ChainSpec,
    nodeConfig: NodeConfig,
    currentSlot: () => Long,
    upkeep: () => Unit,
    /** Owner of this service's threads: the node's, or its own when made alone. */
    threads: NodeThreads = new NodeThreads
) extends LazyLogging:

  /** Peers this node keeps a connection to (bootnodes and `--peer`s): a lost
    * connection is redialed with backoff, so a node cut off for longer than
    * the QUIC idle timeout rejoins once the network heals.
    */
  private final class DialTarget(val address: InetSocketAddress, val key: Option[Array[Byte]]):
    @volatile var connection: PeerConnection = null
    @volatile var nextAttemptMillis: Long = 0L
    @volatile var backoffMillis: Long = nodeConfig.redialMillis
    @volatile var lastFailure: String = ""
    @volatile var inFlight: Boolean = false

  private val dialTargets = new java.util.concurrent.CopyOnWriteArrayList[DialTarget]()

  /** Validator connectivity in force: the previous, current and next sets of
    * the epoch whose connectivity changes have been applied (JAMNP-S
    * "Required connectivity", "Epoch transitions").
    */
  private final case class ValidatorView(
      epoch: Long,
      appliedSlot: Long,
      validators: Set[JamBytes],
      current: Set[JamBytes],
      neighbours: Set[JamBytes],
      endpoints: Map[JamBytes, InetSocketAddress]
  )
  @volatile private var connectivity: Option[ValidatorView] = None

  /** A validator this node must stay connected to, dialed from its metadata
    * endpoint.
    */
  private final class ValidatorDial(val key: Array[Byte], val address: InetSocketAddress):
    /** Since when no connection to the validator has been seen. */
    @volatile var disconnectedSince: Long = System.currentTimeMillis()
    @volatile var nextAttemptMillis: Long = 0L
    @volatile var backoffMillis: Long = nodeConfig.redialMillis
    @volatile var inFlight: Boolean = false

  private val validatorDials = new java.util.concurrent.ConcurrentHashMap[JamBytes, ValidatorDial]()
  private val dialer = threads.cached("jam-dial")
  @volatile private var redialer: java.util.concurrent.ScheduledExecutorService = null
  @volatile private var stopping: Boolean = false

  private val connectedAt = new java.util.concurrent.ConcurrentHashMap[PeerConnection, java.lang.Long]()

  def start(): Unit =
    spec.bootnodes.foreach(bn => dialTargets.add(new DialTarget(bn.address, Some(bn.ed25519Key))))
    dialTargets.forEach(t => dial(t, initial = true))
    redialer = threads.scheduled("jam-redial")
    redialer.scheduleWithFixedDelay(() => redialLost(), 500, 500, java.util.concurrent.TimeUnit.MILLISECONDS)
    redialer.scheduleWithFixedDelay(
      () =>
        try maintain()
        catch case e: Exception => logger.warn(s"connectivity upkeep failed: ${e.getMessage}"),
      500,
      500,
      java.util.concurrent.TimeUnit.MILLISECONDS
    )

  def beginShutdown(): Unit = stopping = true

  def shutdown(): Unit =
    stopping = true
    if redialer != null then redialer.shutdownNow()
    dialer.shutdownNow()

  /** Keep a connection to `address` (redialed when lost), as for a bootnode. */
  def addPeer(address: InetSocketAddress, expectedEd25519: Option[Array[Byte]] = None): Unit =
    val target = new DialTarget(address, expectedEd25519)
    dialTargets.add(target)
    dial(target, initial = true)

  def appliedSlotOf(epoch: Long): Option[Long] = connectivity.filter(_.epoch == epoch).map(_.appliedSlot)

  def isValidator(key: Array[Byte]): Boolean = connectivity.exists(_.validators.contains(JamBytes(key)))

  def currentValidatorConnections: List[PeerConnection] =
    connectivity.map(v => connectionsTo(v.current)).getOrElse(Nil)

  /** Dial targets whose connection is missing or closed, once their backoff
    * has passed. Runs on the redial thread.
    */
  private def redialLost(): Unit =
    if stopping then return
    val now = System.currentTimeMillis()
    dialTargets.forEach { t =>
      // A connection the peer dialed to us (or one kept over ours as the
      // preferred initiator's) counts as live too.
      val live = (t.connection != null && t.connection.isOpen) || t.key.exists(k => liveConnectionTo(k).nonEmpty)
      // Dialed off this thread: a dial waits up to 10 s on an unreachable peer, and the upkeep runs here too.
      if !live && !t.inFlight && now >= t.nextAttemptMillis then
        t.inFlight = true
        try
          dialer.execute { () =>
            try dial(t, initial = false)
            finally t.inFlight = false
          }
        catch case _: java.util.concurrent.RejectedExecutionException => t.inFlight = false
    }

  /** An open connection to the peer with Ed25519 key `key`. */
  private def liveConnectionTo(key: Array[Byte]): Option[PeerConnection] =
    distribution.peers.find(c => java.util.Arrays.equals(c.peerKey, key))

  private def connectionsTo(keys: Set[JamBytes]): List[PeerConnection] =
    distribution.peers.filter(c => keys.contains(JamBytes(c.peerKey)))

  /** Devnet quorum finality counts the best heads peers announce on UP 0,
    * so UP 0 stays open to every peer while it runs; the grid restriction
    * applies otherwise, and always under GRANDPA, which needs no peer bests.
    */
  private def gridRestrictsAnnouncements: Boolean =
    nodeConfig.grandpa || !(spec.devnetFinalityDepth > 0 && spec.finalityQuorum > 1)

  /** Whether UP 0 should be open to `peer` (JAMNP-S UP 0). */
  def announcementWanted(peer: Array[Byte]): Boolean =
    !gridRestrictsAnnouncements || connectivity.forall(v =>
      Connectivity.wantsBlockAnnouncements(v.validators, v.neighbours, identity.publicKeyBytes, peer)
    )

  /** Apply the validator connectivity of the epoch `Connectivity.appliedEpoch`
    * names once our head has reached it.
    */
  def update(slot: Long): Unit = synchronized {
    val e = spec.config.epochLength
    val headEpoch = chain.best.slot / e
    val applied = Connectivity.appliedEpoch(slot, e, chain.finalized.slot, spec.devnetFinalityDepth > 0)
    val stale = connectivity.forall(_.epoch < applied)
    if stale && (connectivity.isEmpty || applied == headEpoch) then
      val view = chain.stateView()
      val sets = Seq(view.validators.previous, view.validators.current, view.validators.nextEpoch)
      val keySets = sets.map(_.map(_.ed25519.bytes.toArray))
      val endpoints = sets.flatten.flatMap { vk =>
        Connectivity.endpoint(vk.metadata.toArray).map(JamBytes(vk.ed25519.bytes.toArray) -> _)
      }.toMap
      val appliedSlot = math.max(slot, headEpoch * e + Connectivity.transitionDelay(e))
      val v = ValidatorView(
        headEpoch,
        appliedSlot,
        keySets.flatten.map(JamBytes(_)).toSet,
        keySets(1).map(JamBytes(_)).toSet,
        Connectivity.gridNeighbours(keySets, identity.publicKeyBytes),
        endpoints
      )
      connectivity = Some(v)
      val self = JamBytes(identity.publicKeyBytes)
      // Only validators keep connections to all other validators.
      val wanted = if v.validators.contains(self) then v.endpoints - self else Map.empty
      validatorDials.keySet.removeIf(k => !wanted.contains(k))
      wanted.foreach { case (k, address) =>
        val existing = validatorDials.get(k)
        if existing == null || existing.address != address then
          validatorDials.put(k, new ValidatorDial(k.toArray, address))
      }
      logger.info(
        s"validator connectivity for epoch $headEpoch applied at slot $slot: " +
          s"${v.validators.size} validators, ${wanted.size} endpoints, ${v.neighbours.size} grid neighbours"
      )
  }

  /** Dial validators we lack a connection to, as preferred initiator at once
    * and otherwise after the grace period (JAMNP-S "Required connectivity"),
    * and open or close UP 0 streams on our dialed connections to follow the
    * grid. Runs on the redial thread.
    */
  private def maintain(): Unit =
    if stopping then return
    update(currentSlot())
    upkeep()
    val now = System.currentTimeMillis()
    val self = identity.publicKeyBytes
    validatorDials.values.forEach { d =>
      if liveConnectionTo(d.key).nonEmpty then d.disconnectedSince = now
      else if !d.inFlight && now >= d.nextAttemptMillis &&
        (Connectivity.isPreferredInitiator(self, d.key) ||
          now - d.disconnectedSince >= Connectivity.PreferredInitiatorGraceMillis)
      then
        d.inFlight = true
        dialer.execute { () =>
          try
            connectPeer(d.address, Some(d.key))
            d.backoffMillis = nodeConfig.redialMillis
          catch
            case e: Exception =>
              logger.debug(s"validator ${JamNode.hex(d.key).take(16)} at ${d.address} unreachable: ${e.getMessage}")
              d.nextAttemptMillis = System.currentTimeMillis() + d.backoffMillis
              d.backoffMillis = math.min(d.backoffMillis * 2, JamNode.MaxRedialMillis)
          finally d.inFlight = false
        }
    }
    // UP 0 may be opened by either side. A connection kept after a duplicate was closed, or an accepted one whose
    // peer never opened UP 0, gets one from us after a grace period.
    distribution.peers.foreach { c =>
      val wanted = announcementWanted(c.peerKey)
      val open = sync.hasAnnouncementStream(c)
      val since = connectedAt.computeIfAbsent(c, _ => now)
      if wanted && !open && (c.initiator || now - since >= JamNode.UpZeroGraceMillis) then
        connectedAt.put(c, now)
        dialer.execute(() =>
          try sync.openAnnouncementStream(c): Unit
          catch case e: Exception => logger.debug(s"UP0 open failed: ${e.getMessage}")
        )
      else if gridRestrictsAnnouncements && !wanted && open && c.initiator then sync.closeAnnouncementStream(c)
    }
    connectedAt.keySet.removeIf(c => !c.isOpen)

  /** When a peer is reached over two connections, one dialed by each side,
    * keep the one opened by the preferred initiator P(a, b) and close the
    * other. Both sides apply the same rule, so they close the same one.
    */
  def dropDuplicateConnections(conn: PeerConnection): Unit =
    val key = conn.peerKey
    val same = (conn :: distribution.peers).distinct.filter(c =>
      c.isOpen && java.util.Arrays.equals(c.peerKey, key)
    )
    if same.exists(_.initiator) && same.exists(!_.initiator) then
      val weInitiate = Connectivity.isPreferredInitiator(identity.publicKeyBytes, key)
      val (preferred, others) = same.partition(_.initiator == weInitiate)
      val latest = others.maxBy(_.openedAtMillis)
      val closing =
        if Connectivity.laterDialReplaces(preferred.map(_.openedAtMillis).max, latest.openedAtMillis) then
          logger.info(s"connection to ${JamNode.hex(key).take(16)} replaced by the one its peer dialed since")
          preferred ++ others.filter(_ ne latest)
        else others
      closing.foreach { c =>
        logger.info(s"closing duplicate connection to ${JamNode.hex(key).take(16)}")
        c.close()
      }

  private def dial(t: DialTarget, initial: Boolean): Unit =
    val where = s"${t.address.getHostString}:${t.address.getPort}"
    try
      t.connection = connectPeer(t.address, t.key)
      t.backoffMillis = nodeConfig.redialMillis
      t.lastFailure = ""
      logger.info(s"${if initial then "connected" else "reconnected"} peer $where")
    catch
      case e: Exception =>
        val why = Option(e.getMessage).getOrElse(e.getClass.getSimpleName)
        // Log the first failure of a streak and changes, not every retry.
        if initial || why != t.lastFailure then logger.warn(s"peer $where unreachable: $why; retrying")
        t.lastFailure = why
        t.nextAttemptMillis = System.currentTimeMillis() + t.backoffMillis
        t.backoffMillis = math.min(t.backoffMillis * 2, JamNode.MaxRedialMillis)

  def connectPeer(
      address: InetSocketAddress,
      expectedEd25519: Option[Array[Byte]] = None
  ): PeerConnection =
    val conn = network.connect(address).get(10, java.util.concurrent.TimeUnit.SECONDS)
    expectedEd25519.foreach { expected =>
      JamNode.verifyPeerKey(expected, conn.peerKey) match
        case Left(msg) =>
          conn.close()
          throw new SecurityException(msg)
        case Right(()) => ()
    }
    val expectedAlpn = network.config.alpn
    if conn.negotiatedAlpn != expectedAlpn then
      conn.close()
      throw new SecurityException(
        s"ALPN mismatch: expected $expectedAlpn, got ${conn.negotiatedAlpn}"
      )
    if announcementWanted(conn.peerKey) then sync.openAnnouncementStream(conn): Unit
    distribution.trackConnection(conn)
    conn
