package io.forge.jam.core.trie

import io.forge.jam.core.{Hashing, JamBytes}
import io.forge.jam.core.primitives.Hash
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class RangeProofSpec extends AnyFunSuite with Matchers:

  private val MaxKey = JamBytes(Array.fill[Byte](31)(0xff.toByte))

  private def randomKey(rng: Random): JamBytes =
    if rng.nextInt(3) == 0 then
      val a = new Array[Byte](31)
      a(0) = rng.nextInt(4).toByte
      a(1) = rng.nextInt(3).toByte
      a(30) = rng.nextInt(8).toByte
      JamBytes(a)
    else JamBytes(Hashing.blake2b256(Array(rng.nextInt(100000).toByte, rng.nextInt(256).toByte, rng.nextInt(256).toByte)).bytes.take(31))

  private def randomValue(rng: Random): JamBytes =
    JamBytes(Array.fill(if rng.nextBoolean() then rng.nextInt(33) else 33 + rng.nextInt(200))(rng.nextInt(256).toByte))

  private def build(rng: Random, keys: Int): (StateTrie, Vector[(JamBytes, JamBytes)]) =
    val model = (1 to keys).map(_ => randomKey(rng) -> randomValue(rng)).toMap
    val trie = StateTrie.empty(new InMemoryTrieBackend())
    trie.update(model.toSeq.map((k, v) => k -> Some(v)))
    (trie, model.toVector.sortWith((a, b) => JamBytes.compareUnsigned(a._1.toArrayUnsafe, b._1.toArrayUnsafe) < 0))

  private def respond(trie: StateTrie, start: JamBytes, maxSize: Int) =
    val (boundary, pairs) = trie.range(start, MaxKey, maxSize)
    (boundary.map(_.gpEncoding), pairs.toVector)

  private def next(key: JamBytes): Option[JamBytes] =
    val a = key.toArray
    var i = a.length - 1
    while i >= 0 && a(i) == 0xff.toByte do
      a(i) = 0
      i -= 1
    if i < 0 then None
    else
      a(i) = (a(i) + 1).toByte
      Some(JamBytes(a))

  test("an honest server's responses verify, from any start and at any size limit, and walk the whole state") {
    val rng = new Random(1)
    for round <- 1 to 60 do
      val (trie, sorted) = build(rng, rng.nextInt(120))
      val root = trie.rootHash
      for _ <- 1 to 10 do
        val start = if rng.nextBoolean() && sorted.nonEmpty then sorted(rng.nextInt(sorted.size))._1 else randomKey(rng)
        val (boundary, pairs) = respond(trie, start, 200 + rng.nextInt(4000))
        withClue(s"round $round: ") { RangeProof.verify(root, start, boundary, pairs) shouldBe Right(()) }
      var start: Option[JamBytes] = Some(JamBytes(new Array[Byte](31)))
      val got = Vector.newBuilder[(JamBytes, JamBytes)]
      var done = false
      while !done do
        val (boundary, pairs) = respond(trie, start.get, 300 + rng.nextInt(2000))
        RangeProof.verify(root, start.get, boundary, pairs) shouldBe Right(())
        got ++= pairs
        start = pairs.lastOption.flatMap(p => next(p._1))
        done = pairs.isEmpty || start.isEmpty
      got.result() shouldBe sorted
  }

  test("a response with a pair changed, dropped, added or out of place is refused") {
    val rng = new Random(2)
    var tampered = 0
    for round <- 1 to 80 do
      val (trie, sorted) = build(rng, 2 + rng.nextInt(80))
      val root = trie.rootHash
      val start = if rng.nextBoolean() then sorted(rng.nextInt(sorted.size))._1 else randomKey(rng)
      val (boundary, pairs) = respond(trie, start, 400 + rng.nextInt(3000))
      def refused(what: String, b: Seq[Array[Byte]], p: Vector[(JamBytes, JamBytes)]): Unit =
        tampered += 1
        withClue(s"round $round, $what: ") { RangeProof.verify(root, start, b, p).isLeft shouldBe true }
      if pairs.nonEmpty then
        val i = rng.nextInt(pairs.size)
        val (k, v) = pairs(i)
        refused("a value changed", boundary, pairs.updated(i, k -> JamBytes(v.toArray :+ 1.toByte)))
        if v != JamBytes(Array[Byte](9, 9, 9)) then
          refused("a value replaced", boundary, pairs.updated(i, k -> JamBytes(Array[Byte](9, 9, 9))))
        if pairs.size > 1 then
          refused("a pair in the middle or at the start dropped", boundary, pairs.patch(rng.nextInt(pairs.size - 1), Nil, 1))
          refused("pairs out of order", boundary, pairs.reverse)
        val added = randomKey(rng)
        if !pairs.exists(_._1 == added) && JamBytes.compareUnsigned(added.toArrayUnsafe, start.toArrayUnsafe) >= 0 then
          val withAdded = (pairs :+ (added -> randomValue(rng))).sortWith((a, b) => JamBytes.compareUnsigned(a._1.toArrayUnsafe, b._1.toArrayUnsafe) < 0)
          refused("a pair added", boundary, withAdded)
        refused("a boundary node missing", boundary.drop(1), pairs)
        refused("a boundary node altered", boundary.updated(0, boundary.head.updated(40, (boundary.head(40) ^ 1).toByte)), pairs)
      else if sorted.exists(p => JamBytes.compareUnsigned(p._1.toArrayUnsafe, start.toArrayUnsafe) >= 0) then
        fail("an honest server returned nothing although keys remain")
      if sorted.exists(p => JamBytes.compareUnsigned(p._1.toArrayUnsafe, start.toArrayUnsafe) >= 0) then
        refused("nothing returned although keys remain", boundary, Vector.empty)
      tampered += 1
      withClue(s"round $round, the wrong root: ") {
        RangeProof.verify(Hashing.blake2b256(root.bytes), start, boundary, pairs).isLeft shouldBe true
      }
    tampered should be > 300
  }

  test("the empty state, and a range past the last key, verify as empty") {
    RangeProof.verify(Hash.zero, JamBytes(new Array[Byte](31)), Nil, Vector.empty) shouldBe Right(())
    val rng = new Random(3)
    val (trie, sorted) = build(rng, 40)
    next(sorted.last._1).foreach { after =>
      val (boundary, pairs) = respond(trie, after, 4000)
      pairs shouldBe empty
      RangeProof.verify(trie.rootHash, after, boundary, pairs) shouldBe Right(())
    }
    RangeProof.verify(Hash.zero, JamBytes(new Array[Byte](31)), Nil, Vector(sorted.head)).isLeft shouldBe true
  }
