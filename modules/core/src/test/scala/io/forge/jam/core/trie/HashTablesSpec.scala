package io.forge.jam.core.trie

import io.forge.jam.core.trie.HashTables.{NodeTable, ValueTable}
import io.forge.jam.core.primitives.Hash
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable
import scala.util.Random

class HashTablesSpec extends AnyFlatSpec with Matchers:

  /** Keys drawn from a small pool so inserts, hits and removes all recur; some share a first word or have it zero. */
  private def keyPool(rng: Random, n: Int): Vector[Hash] =
    Vector.tabulate(n) { i =>
      val b = rng.nextBytes(32)
      if i % 7 == 0 then java.util.Arrays.fill(b, 0, 8, 0.toByte) // first word 0
      else if i % 5 == 0 then java.util.Arrays.fill(b, 0, 8, 0x11.toByte) // first words collide
      Hash.wrap(b)
    }

  "HashTables.NodeTable" should "agree with a map under random inserts, count changes, payloads and removes" in {
    val rng = new Random(11)
    val keys = keyPool(rng, 3000)
    val table = new NodeTable
    val model = mutable.HashMap[Hash, (Long, Option[TrieNode])]()
    for step <- 1 to 60000 do
      val k = keys(rng.nextInt(keys.size))
      rng.nextInt(4) match
        case 0 =>
          val slot = table.findOrInsert(k.toArrayUnsafe)
          val d = rng.nextInt(5) - 2L
          table.addRef(slot, d)
          val (c, n) = model.getOrElse(k, (0L, None))
          model(k) = (c + d, n)
        case 1 =>
          val node = TrieNode.branch(Hash.wrap(rng.nextBytes(32)), Hash.wrap(rng.nextBytes(32)))
          table.setNode(table.findOrInsert(k.toArrayUnsafe), node)
          model(k) = (model.get(k).fold(0L)(_._1), Some(node))
        case 2 =>
          val slot = table.find(k.toArrayUnsafe)
          if slot >= 0 then table.remove(slot)
          model.remove(k)
        case _ =>
          val slot = table.find(k.toArrayUnsafe)
          model.get(k) match
            case None => slot shouldBe -1
            case Some((c, n)) =>
              slot should be >= 0
              table.ref(slot) shouldBe c
              Option(table.node(slot, k)) shouldBe n
      if step % 10000 == 0 then
        table.size shouldBe model.size
        table.payloadCount shouldBe model.count(_._2._2.isDefined)
        model.foreach { case (k, (c, n)) =>
          val slot = table.find(k.toArrayUnsafe)
          table.ref(slot) shouldBe c
          Option(table.node(slot, k)) shouldBe n
          table.keyHash(slot) shouldBe k
        }
  }

  "HashTables.ValueTable" should "keep each value with its key through growth and removes" in {
    val rng = new Random(12)
    val table = new ValueTable
    val model = mutable.HashMap[Hash, Array[Byte]]()
    for _ <- 1 to 20000 do
      val k = Hash.wrap(rng.nextBytes(32))
      val v = rng.nextBytes(40)
      table.setValue(table.findOrInsert(k.toArrayUnsafe), v)
      model(k) = v
    model.keys.take(10000).toVector.foreach { k =>
      table.remove(table.find(k.toArrayUnsafe))
      model.remove(k)
    }
    table.size shouldBe model.size
    model.foreach((k, v) => table.value(table.find(k.toArrayUnsafe)) shouldBe v)
    table.clear()
    table.size shouldBe 0
    table.find(model.head._1.toArrayUnsafe) shouldBe -1
  }
