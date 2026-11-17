package io.forge.jam.node

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.primitives.Hash
import io.forge.jam.network.PeerConnection

import scala.util.control.NonFatal

final class CheckpointSync(
    chain: ChainManager,
    sync: SyncService,
    checkpoint: Hash,
    peers: () => List[PeerConnection],
    misbehaved: (PeerConnection, Offence) => Unit = (_, _) => (),
    grandpaSetId: Option[Long] = None
) extends LazyLogging:

  def attempt(): Either[String, ChainHead] =
    val open = peers().filter(_.isOpen)
    if open.isEmpty then Left("no peer connected")
    else
      open.iterator
        .map(conn => from(conn, conn :: open.filterNot(_ eq conn)))
        .collectFirst {
          case Right(head)                                            => Right(head)
          case Left(why) if why.startsWith(CheckpointSync.ShortAncestry) => Left(why)
        }
        .getOrElse(Left(s"none of ${open.size} peer(s) could serve checkpoint ${checkpoint.toHex.take(18)}"))

  private def from(conn: PeerConnection, statePeers: List[PeerConnection]): Either[String, ChainHead] =
    val result =
      for
        top <- chained(conn, checkpoint, 2)
        checkpointBlock <- top.headOption.map(_._2).toRight("the peer does not have the checkpoint block")
        _ <- Either.cond(!startsAtGenesis(checkpointBlock.header.parent), (), CheckpointSync.AtGenesis)
        target <- top.lift(1).map(Right(_)).getOrElse(
          chained(conn, checkpointBlock.header.parent, 1)
            .flatMap(_.headOption.toRight("the peer does not have the checkpoint's parent"))
        )
        targetHash = target._1
        _ <- Either.cond(chain.bodyMatchesHeader(target._2), (), {
          misbehaved(conn, Offence.InvalidBlock)
          s"start block ${targetHash.toHex.take(18)}: ${ChainError.BodyMismatch.message}"
        })
        targetSlot = target._2.header.slot.toLong
        ancestors <- ancestorsOf(statePeers, target._2.header.parent, targetSlot)
        stateRoot = checkpointBlock.header.parentStateRoot
        _ = logger.info(
          s"checkpoint sync: starting at ${targetHash.toHex.take(18)} slot=$targetSlot, the parent of checkpoint " +
            s"${checkpoint.toHex.take(18)}; ${ancestors.size} ancestor(s); downloading state ${stateRoot.toHex.take(18)}"
        )
        _ <- chain.downloadState(
          targetHash,
          targetSlot,
          stateRoot,
          statePeers,
          StateDownload.request(targetHash, malformed = misbehaved(_, Offence.Malformed)),
          misbehaved(_, Offence.BadProof)
        )
        head <- chain.startAt(target._3, stateRoot, ancestors, grandpaSetId)
      yield head
    result match
      case Left(CheckpointSync.AtGenesis) =>
        logger.info(s"checkpoint ${checkpoint.toHex.take(18)} is the first block: nothing to skip, syncing from genesis")
        Right(chain.best)
      case other =>
        other.left.foreach(why => logger.warn(s"checkpoint sync from ${JamNode.hex(conn.peerKey).take(16)} failed: $why"))
        other

  private def startsAtGenesis(parent: Hash): Boolean =
    chain.blockStore.getHead(io.forge.jam.db.BlockStore.GenesisHead).contains(parent)

  private def chained(conn: PeerConnection, from: Hash, count: Int)
      : Either[String, List[(Hash, io.forge.jam.core.types.block.Block, Array[Byte])]] =
    sync.fetchBlocks(conn, from, count).toRight("no answer to the block request").map { blocks =>
      var expected = from
      blocks.iterator
        .map(bytes => chain.decodeBlock(bytes).toOption.map(b => (chain.headerHashOf(b), b, bytes)))
        .takeWhile {
          case Some((h, b, _)) if h == expected =>
            expected = b.header.parent
            true
          case _ => false
        }
        .flatten
        .toList
    }

  private def ancestorsOf(peers: List[PeerConnection], parent: Hash, startSlot: Long)
      : Either[String, List[Array[Byte]]] =
    val window = chain.config.maxLookupAnchorAge
    val out = List.newBuilder[Array[Byte]]
    var count = 0
    var cursor = parent
    var reached = startSlot
    var complete = startsAtGenesis(parent)
    val untried = peers.iterator.filter(_.isOpen)
    var conn = untried.nextOption()
    while !complete && conn.isDefined do
      val batch = chained(conn.get, cursor, CheckpointSync.AncestorsPerRequest).getOrElse(Nil)
        .map((_, b, _) => (b.header.slot.toLong, b.header.parent, chain.headerCodec.encode(b.header)))
      if batch.isEmpty then conn = untried.nextOption()
      else
        val wanted = batch.takeWhile((slot, _, _) => startSlot - slot <= window)
        out ++= wanted.map(_._3)
        count += wanted.size
        if wanted.size < batch.size then
          out += batch(wanted.size)._3
          complete = true
        else
          val (lastSlot, lastParent, _) = batch.last
          cursor = lastParent
          reached = lastSlot
          complete = startsAtGenesis(cursor)
    if complete then Right(out.result())
    else
      Left(
        s"${CheckpointSync.ShortAncestry}: $count ancestor(s) back to slot $reached, none of ${peers.size} peer(s) " +
          s"had the block before (the window reaches back to slot ${math.max(0L, startSlot - window)})"
      )

object CheckpointSync extends LazyLogging:
  val AncestorsPerRequest: Int = 64

  def untilStarted(
      attempt: () => Either[String, ChainHead],
      running: () => Boolean,
      started: () => Unit,
      retryMillis: Long,
      sleep: Long => Unit = Thread.sleep
  ): Unit =
    var lastFault: Option[String] = None
    try
      while running() do
        val outcome =
          try attempt()
          catch
            case NonFatal(e) =>
              val what = s"${e.getClass.getName}: ${e.getMessage}"
              if !lastFault.contains(what) then
                lastFault = Some(what)
                logger.error(s"checkpoint sync attempt failed: $what; retrying every $retryMillis ms", e)
              else logger.debug(s"checkpoint sync attempt failed again: $what")
              Left(what)
        outcome match
          case Right(_) =>
            try started()
            catch case NonFatal(e) => logger.error("checkpoint sync: handing the started chain over failed", e)
          case Left(why) =>
            logger.debug(s"checkpoint sync not yet possible: $why")
            sleep(retryMillis)
    catch case _: InterruptedException => ()
  private val AtGenesis = "the checkpoint's parent is genesis"
  private val ShortAncestry = "short ancestor set"
