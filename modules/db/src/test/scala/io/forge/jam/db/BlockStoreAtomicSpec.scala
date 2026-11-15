package io.forge.jam.db

import java.nio.file.{Files, Path}

import io.forge.jam.core.primitives.Hash
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BlockStoreAtomicSpec extends AnyFunSuite with Matchers:

  private def withStore(f: (BlockStore, Path) => Unit): Unit =
    val dir = Files.createTempDirectory("jam-block-store")
    val store = BlockStore.open(dir)
    try f(store, dir)
    finally
      store.close()
      Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p))

  private def h(b: Int): Hash = Hash(Array.fill(32)(b.toByte))
  private def str(o: Option[Array[Byte]]): Option[String] = o.map(new String(_, "UTF-8"))

  test("metadata written in an atomic scope is read back by its thread and hidden from others until it ends") {
    withStore { (store, _) =>
      store.setMeta("a", "old".getBytes("UTF-8"))
      var seenByOther: Option[String] = None
      var committedBefore = false
      store.atomically(() => committedBefore = true) {
        store.setHead(BlockStore.BestHead, h(1))
        store.setMeta("a", "new".getBytes("UTF-8"))
        store.deleteMeta("gone")
        str(store.getMeta("a")) shouldBe Some("new")
        store.getHead(BlockStore.BestHead) shouldBe Some(h(1))
        val t = new Thread(() => seenByOther = str(store.getMeta("a")))
        t.start(); t.join()
        // Nested scopes join the outer one.
        store.atomically()(store.setMeta("b", Array[Byte](1)))
        store.getMeta("b").map(_.toList) shouldBe Some(List(1.toByte))
      }
      committedBefore shouldBe true
      seenByOther shouldBe Some("old")
      str(store.getMeta("a")) shouldBe Some("new")
      store.getHead(BlockStore.BestHead) shouldBe Some(h(1))
      store.getMeta("b").isDefined shouldBe true
    }
  }

  test("a scope that throws writes nothing") {
    withStore { (store, _) =>
      store.setHead(BlockStore.BestHead, h(1))
      store.setMeta("state_root", h(2).bytes.toArray)
      an[IllegalStateException] should be thrownBy store.atomically() {
        store.setHead(BlockStore.BestHead, h(3))
        throw new IllegalStateException("crash between the head and its state root")
      }
      store.getHead(BlockStore.BestHead) shouldBe Some(h(1))
      store.getMeta("state_root").map(Hash(_)) shouldBe Some(h(2))
    }
  }

  test("an atomic scope's writes survive reopening") {
    val dir = Files.createTempDirectory("jam-block-store-reopen")
    try
      val store = BlockStore.open(dir)
      store.atomically() {
        store.setHead(BlockStore.BestHead, h(5))
        store.setMeta("best_slot", Array[Byte](0, 0, 0, 0, 0, 0, 0, 9))
      }
      store.close()
      val again = BlockStore.open(dir)
      try
        again.getHead(BlockStore.BestHead) shouldBe Some(h(5))
        again.getMeta("best_slot").map(_.last) shouldBe Some(9.toByte)
      finally again.close()
    finally Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p))
  }

  private def bytes(b: Int): Array[Byte] = Array(b.toByte)
  private def names(found: List[(String, Array[Byte])]): List[(String, Int)] = found.map((k, v) => k -> v(0).toInt)

  test("metadata written as a batch is found by key prefix, in key order, after the store is reopened") {
    val dir = Files.createTempDirectory("jam-block-store-scan")
    var store = BlockStore.open(dir)
    try
      store.writeMeta(List("role:a:2" -> bytes(2), "role:a:1" -> bytes(1), "role:ab:1" -> bytes(9), "role:b:1" -> bytes(3)), Nil)
      store.setMeta("role:a:3", bytes(4))
      store.writeMeta(List("role:a:4" -> bytes(5)), List("role:a:2"))
      store.close()
      store = BlockStore.open(dir)
      names(store.scanMeta("role:a:")) shouldBe List("role:a:1" -> 1, "role:a:3" -> 4, "role:a:4" -> 5)
      names(store.scanMeta("role:")).map(_._1) shouldBe List("role:a:1", "role:a:3", "role:a:4", "role:ab:1", "role:b:1")
      store.scanMeta("nothing:") shouldBe Nil
      names(store.scanMeta("role:a:", before = "4")) shouldBe List("role:a:1" -> 1, "role:a:3" -> 4)
    finally
      store.close()
      Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p))
  }

  test("a batch written in an atomic scope is committed with the scope") {
    withStore { (store, _) =>
      store.setMeta("k:gone", bytes(1))
      var seenByOther: List[(String, Int)] = Nil
      store.atomically() {
        store.writeMeta(List("k:new" -> bytes(2)), List("k:gone"))
        store.getMeta("k:new").map(_.toList) shouldBe Some(List(2.toByte))
        store.getMeta("k:gone") shouldBe None
        val t = new Thread(() => seenByOther = names(store.scanMeta("k:")))
        t.start(); t.join()
      }
      seenByOther shouldBe List("k:gone" -> 1)
      names(store.scanMeta("k:")) shouldBe List("k:new" -> 2)
    }
  }
