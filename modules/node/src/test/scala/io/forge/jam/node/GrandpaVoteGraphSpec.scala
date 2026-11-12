package io.forge.jam.node

import io.forge.jam.core.Hashing
import io.forge.jam.core.primitives.Hash
import io.forge.jam.node.GrandpaMessages.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable
import scala.util.Random

/** The voter's vote tally (GHOST, estimate, completable) remembers the chain walks it has made. It must give what a
  * plain walk of every vote gives, on any block tree and any set of votes, and must not read the chain again for
  * blocks it has already walked.
  */
class GrandpaVoteGraphSpec extends AnyFunSuite with Matchers:

  private def h(name: String): Hash = Hashing.blake2b256(name.getBytes("UTF-8"))

  private final case class Node(parent: Hash, slot: Long, root: Hash)

  /** A block tree; counts the header reads the voter makes. */
  private final class Tree extends GrandpaChain:
    val base: Hash = h("base")
    val nodes = mutable.LinkedHashMap(base -> Node(Hash.zero, 10L, h("root-base")))
    var slotReads = 0L
    def add(name: String, parent: Hash, slot: Long): Hash =
      val hash = h(name)
      nodes(hash) = Node(parent, slot, h(s"root-$name"))
      hash
    def vote(x: Hash): Vote = Vote(x, nodes(x).root, nodes(x).slot)
    def finalized: Vote = vote(base)
    def best: Vote = vote(nodes.maxBy(_._2.slot)._1)
    def slotOf(x: Hash): Option[Long] =
      slotReads += 1
      nodes.get(x).map(_.slot)
    def parentOf(x: Hash): Option[Hash] = nodes.get(x).map(_.parent).filter(nodes.contains)
    def stateRootOf(x: Hash): Option[Hash] = nodes.get(x).map(_.root)
    def headerBytes(x: Hash): Option[Array[Byte]] = None
    def epochMarkerKeys(x: Hash): Option[List[Array[Byte]]] = None
    def finalizeBlock(x: Hash): Either[String, Unit] = Right(())

  private val silent = new GrandpaOutbound:
    def broadcastVote(m: VoteMessage): Unit = ()
    def broadcastCommit(round: Long, setId: Long, c: Commit): Unit = ()
    def broadcastState(round: Long, setId: Long, slot: Long): Unit = ()

  private def signed(v: Vote, key: Array[Byte]): SignedVote = SignedVote(v, new Array[Byte](64), key)

  // The tally as a plain walk of every vote's ancestry, with no memory: the definition the voter must match.
  private final class Reference(tree: Tree, set: VoterSet):
    private def path(x: Hash): List[(Hash, Long)] =
      val out = mutable.ListBuffer.empty[(Hash, Long)]
      var cursor: Option[Hash] = Some(x)
      var reached = false
      while !reached && cursor.isDefined do
        val c = cursor.get
        tree.nodes.get(c).map(_.slot) match
          case Some(slot) if slot > set.base.slot =>
            out += c -> slot
            cursor = tree.parentOf(c)
          case Some(_) => reached = c == set.base.hash; cursor = None
          case None    => cursor = None
      if reached then out.toList else Nil
    private def support(votes: Iterable[SignedVote]): Map[Hash, (Long, Int)] =
      val counts = mutable.HashMap.empty[Hash, (Long, Int)]
      votes.foreach { v =>
        if tree.stateRootOf(v.vote.hash).forall(_ == v.vote.stateRoot) then
          path(v.vote.hash).foreach((x, slot) => counts.updateWith(x)(c => Some(slot -> (c.map(_._2).getOrElse(0) + 1))))
      }
      counts.toMap
    private def voteFor(x: Hash, slot: Long): Option[Vote] =
      if x == set.base.hash then Some(set.base) else tree.stateRootOf(x).map(Vote(x, _, slot))
    private def ghost(votes: Iterable[SignedVote]): Option[Vote] =
      if votes.size < set.threshold then None
      else
        support(votes).filter(_._2._2 >= set.threshold).maxByOption(_._2._1) match
          case Some((x, (slot, _))) => voteFor(x, slot)
          case None                 => Some(set.base)
    def prevoteGhost(r: RoundState): Option[Vote] = ghost(r.prevotes.values)
    def estimate(r: RoundState): Option[Vote] =
      prevoteGhost(r).orElse(ghost(r.precommits.values)).map { g =>
        val counts = support(r.precommits.values)
        val missing = set.size - r.precommits.size
        path(g.hash)
          .find((x, _) => counts.get(x).map(_._2).getOrElse(0) + missing >= set.threshold)
          .flatMap((x, slot) => voteFor(x, slot))
          .getOrElse(set.base)
      }
    private def isAncestor(a: Hash, b: Hash): Boolean =
      a == b || {
        var cursor = tree.parentOf(b)
        var found = false
        while !found && cursor.isDefined do
          if cursor.contains(a) then found = true else cursor = cursor.flatMap(tree.parentOf)
        found
      }
    def completable(r: RoundState): Boolean =
      (prevoteGhost(r).orElse(ghost(r.precommits.values)), estimate(r)) match
        case (Some(g), Some(e)) =>
          if e.hash != g.hash then true
          else
            val counts = support(r.precommits.values)
            val missing = set.size - r.precommits.size
            r.precommits.size >= set.threshold && !counts.exists { case (x, (slot, n)) =>
              slot > g.slot && n + missing >= set.threshold && isAncestor(g.hash, x)
            }
        case _ => false

  /** A random tree of `size` blocks above the base: mostly one chain, with forks and forks of forks. */
  private def randomTree(rng: Random, size: Int): (Tree, IndexedSeq[Hash]) =
    val tree = new Tree
    val blocks = mutable.ArrayBuffer(tree.base)
    for i <- 1 to size do
      val parent = if rng.nextInt(4) == 0 then blocks(rng.nextInt(blocks.size)) else blocks.last
      blocks += tree.add(s"b$i-${rng.nextInt()}", parent, tree.nodes(parent).slot + 1 + rng.nextInt(2))
    (tree, blocks.toIndexedSeq)

  test("GHOST, estimate and completable equal a plain walk of every vote, on random trees and votes") {
    val rng = new Random(20261112)
    val keys = (0 until 10).map(i => Hashing.blake2b256(Array[Byte](i.toByte)).bytes.toArray)
    var compared = 0
    var withGhost = 0
    for _ <- 1 to 300 do
      val (tree, blocks) = randomTree(rng, 5 + rng.nextInt(40))
      val set = VoterSet(0, tree.vote(tree.base), keys)
      val voter = new GrandpaVoter(tree, silent, set, None)
      val reference = new Reference(tree, set)
      // Several rounds on one voter, so later tallies run with paths remembered from earlier ones.
      for round <- 1 to 4 do
        val r = new RoundState(round.toLong, 0L)
        def randomVote(): Vote =
          rng.nextInt(20) match
            case 0 => Vote(h(s"unknown-${rng.nextInt()}"), h("x"), 99L) // a block we do not have
            case 1 =>
              val b = blocks(rng.nextInt(blocks.size))
              Vote(b, h("another-root"), tree.nodes(b).slot) // our block under a different state root
            case 2 => tree.vote(tree.base)
            case _ =>
              // Mostly near the tip of some branch, as real votes are.
              tree.vote(blocks(math.max(0, blocks.size - 1 - rng.nextInt(math.min(blocks.size, 8)))))
        keys.take(rng.nextInt(keys.size + 1)).foreach(k => r.prevotes.put(io.forge.jam.core.primitives.Ed25519PublicKey(k), signed(randomVote(), k)))
        keys.take(rng.nextInt(keys.size + 1)).foreach(k => r.precommits.put(io.forge.jam.core.primitives.Ed25519PublicKey(k), signed(randomVote(), k)))
        // Asked twice: the second answer comes from what the first remembered.
        for _ <- 1 to 2 do
          voter.prevoteGhost(r) shouldBe reference.prevoteGhost(r)
          voter.estimate(r) shouldBe reference.estimate(r)
          voter.completable(r) shouldBe reference.completable(r)
        compared += 1
        if reference.prevoteGhost(r).exists(_.hash != tree.base) then withGhost += 1
    compared shouldBe 1200
    withGhost should be > 100 // the comparison is not vacuous: many rounds have a GHOST above the base
  }

  test("a block the chain stops holding is no longer counted, as when it is walked afresh") {
    val tree = new Tree
    val a = tree.add("a", tree.base, 11)
    val b = tree.add("b", a, 12)
    val keys = (0 until 4).map(i => Hashing.blake2b256(Array[Byte](i.toByte)).bytes.toArray)
    val set = VoterSet(0, tree.vote(tree.base), keys)
    val voter = new GrandpaVoter(tree, silent, set, None)
    val r = new RoundState(1L, 0L)
    val voteB = tree.vote(b)
    keys.foreach(k => r.prevotes.put(io.forge.jam.core.primitives.Ed25519PublicKey(k), signed(voteB, k)))
    voter.prevoteGhost(r).map(_.hash) shouldBe Some(b)
    tree.nodes.remove(b)
    voter.prevoteGhost(r) shouldBe Some(set.base) // nothing above the base has a supermajority any more
  }

  test("tallying a round's votes again reads no block header it has read before") {
    val tree = new Tree
    var tip = tree.base
    for i <- 1 to 500 do tip = tree.add(s"c$i", tip, 10L + i)
    val keys = (0 until 100).map(i => Hashing.blake2b256(Array[Byte](i.toByte, 7)).bytes.toArray)
    val set = VoterSet(0, tree.vote(tree.base), keys)
    val voter = new GrandpaVoter(tree, silent, set, None)
    val r = new RoundState(1L, 0L)
    keys.foreach { k =>
      r.prevotes.put(io.forge.jam.core.primitives.Ed25519PublicKey(k), signed(tree.vote(tip), k))
      r.precommits.put(io.forge.jam.core.primitives.Ed25519PublicKey(k), signed(tree.vote(tip), k))
    }
    voter.prevoteGhost(r).map(_.hash) shouldBe Some(tip)
    val firstTally = tree.slotReads
    firstTally should be <= 510L // one walk of the 500 blocks, not one per vote (100 × 500)

    // What a step does, a hundred times over: every tally after the first costs one read per distinct target.
    tree.slotReads = 0
    for _ <- 1 to 100 do
      voter.prevoteGhost(r)
      voter.estimate(r)
      voter.completable(r)
    tree.slotReads should be <= 100L * 12
  }
