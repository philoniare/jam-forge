package io.forge.jam.node

import io.forge.jam.core.ChainConfig
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.network.{Loopback, PeerStreamHandler}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Nodes on the dev genesis joined in memory ([[Loopback]]): their services run as in a node, with no QUIC endpoint
  * and no slot clock. Node `i` (below six) has dev validator `i`'s Ed25519 key as its network identity, as JAMNP-S
  * expects of a validator.
  */
trait LoopbackFixture extends NodeFixture:
  this: AnyFunSuite & Matchers =>

  protected val devKeys: IndexedSeq[ValidatorKeySet] = (0 until 6).map(ValidatorKeySet.dev)

  protected def eventually(what: String, seconds: Int = 30)(cond: => Boolean): Unit =
    val deadline = System.currentTimeMillis() + seconds * 1000L
    while !cond && System.currentTimeMillis() < deadline do Thread.sleep(20)
    if !cond then fail(s"timed out waiting for: $what")

  protected def withNodes(count: Int, id: String)(body: IndexedSeq[JamNode] => Unit): Unit =
    val genesis = loadGenesis().getOrElse(cancel("dev genesis (jamtestvectors/traces/fuzzy/genesis.json) not available"))
    val spec = ChainSpec(
      id = id,
      config = ChainConfig.TINY,
      genesisHeaderBytes = Some(genesis.header.encode.toArray),
      explicitGenesisHash = None,
      genesisState = genesis.state.keyvals,
      bootnodes = Nil
    )
    val dirs = (0 until count).map(i => tempDir(s"jam-$id-$i"))
    val nodes = dirs.zipWithIndex.map { (dir, i) =>
      new JamNode(spec, NodeConfig(dataDir = dir, slotTicking = false, ed25519Seed = devKeys.lift(i).map(_.ed25519Secret)))
    }
    try
      nodes.foreach(_.chain.initializeOrRestore(spec))
      body(nodes)
    finally
      nodes.foreach(_.shutdown())
      dirs.foreach(cleanup)

  /** Join `a` and `b`, each serving the streams the other opens with `handlers`, and each knowing the other as a
    * peer. Returns a's connection to b and b's to a.
    */
  protected def join(a: JamNode, b: JamNode, handlers: JamNode => Byte => Option[PeerStreamHandler])
      : (Loopback.Connection, Loopback.Connection) =
    val (aToB, bToA) = Loopback.pair(a.identity.publicKeyBytes, b.identity.publicKeyBytes, handlers(a), handlers(b))
    a.distribution.trackConnection(aToB)
    b.distribution.trackConnection(bToA)
    (aToB, bToA)

  /** Author `slots` with every dev key on `nodes.head` and import each block into all of `nodes`. */
  protected def author(nodes: Seq[JamNode], slots: Range): Unit =
    val author = new BlockAuthor(nodes.head.chain, devKeys)
    slots.foreach { slot =>
      val block = author.tryAuthor(slot.toLong).getOrElse(fail(s"author block $slot"))
      val bytes = nodes.head.chain.encodeBlock(block)
      nodes.foreach(_.chain.importBlock(bytes).isRight shouldBe true)
    }
