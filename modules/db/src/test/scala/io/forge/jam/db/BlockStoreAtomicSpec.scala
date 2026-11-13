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
