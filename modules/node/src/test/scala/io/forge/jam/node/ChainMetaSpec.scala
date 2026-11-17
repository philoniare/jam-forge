package io.forge.jam.node

import io.forge.jam.core.primitives.Hash
import io.forge.jam.db.BlockStore
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files

class ChainMetaSpec extends AnyFunSuite with Matchers:

  test("an invalid mark and a disregarded mark both make a block not adoptable; only the second reads as disregarded") {
    val dir = Files.createTempDirectory("jam-chain-meta")
    val old = Hash(Array.fill[Byte](32)(1))
    val invalid = Hash(Array.fill[Byte](32)(2))
    val disregarded = Hash(Array.fill[Byte](32)(3))
    val both = Hash(Array.fill[Byte](32)(4))
    val clean = Hash(Array.fill[Byte](32)(5))
    def check(meta: ChainMeta): Unit =
      List(old, invalid, disregarded, both).foreach(h => meta.invalid(h) shouldBe true)
      meta.invalid(clean) shouldBe false
      meta.disregarded(old) shouldBe false
      meta.disregarded(invalid) shouldBe false
      meta.disregarded(both) shouldBe false // invalid first: a later disregard does not make it less than invalid
      meta.disregarded(disregarded) shouldBe true
      meta.disregarded(clean) shouldBe false
    try
      val store = BlockStore.open(dir.resolve("blocks"))
      try
        store.setMeta(s"invalid:${old.toHex}", java.nio.ByteBuffer.allocate(8).putLong(1L).array())
        val meta = new ChainMeta(store)
        meta.markInvalid(invalid)
        meta.markDisregarded(disregarded)
        meta.markInvalid(both)
        meta.markDisregarded(both)
        check(meta)
      finally store.close()
      val reopened = BlockStore.open(dir.resolve("blocks"))
      try check(new ChainMeta(reopened))
      finally reopened.close()
    finally
      val files = Files.walk(dir)
      try files.sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p): Unit)
      finally files.close()
  }

  test("a durable GRANDPA round record is written at once, even inside a chain change, and the change does not undo it") {
    val dir = Files.createTempDirectory("jam-chain-meta-round")
    val store = BlockStore.open(dir.resolve("blocks"))
    try
      val meta = new ChainMeta(store)
      def seenFromAnotherThread: Option[Seq[Byte]] =
        val f = java.util.concurrent.CompletableFuture.supplyAsync(() => meta.grandpaRound.map(_.toSeq))
        f.get(5, java.util.concurrent.TimeUnit.SECONDS)
      store.atomically() {
        meta.putGrandpaRound(Array[Byte](1), durable = false)
        meta.putGrandpaRound(Array[Byte](2), durable = true)
        seenFromAnotherThread shouldBe Some(Seq[Byte](2))
        meta.bestSlot = 7L
      }
      seenFromAnotherThread shouldBe Some(Seq[Byte](2))
      meta.putGrandpaRound(Array[Byte](3), durable = false)
      meta.grandpaRound.map(_.toSeq) shouldBe Some(Seq[Byte](3))
    finally store.close()
  }
