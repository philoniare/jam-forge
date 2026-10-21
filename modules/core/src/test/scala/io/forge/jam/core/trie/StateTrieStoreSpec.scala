package io.forge.jam.core.trie

import io.forge.jam.core.JamBytes
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.CountDownLatch

/** The service caches of [[StateTrieStore]] follow the pinned root. */
class StateTrieStoreSpec extends AnyFlatSpec with Matchers:

  private def key(i: Int): JamBytes =
    val a = new Array[Byte](31)
    a(0) = i.toByte
    JamBytes.wrap(a)

  private def value(i: Int): JamBytes = JamBytes(Array.fill[Byte](40)(i.toByte))

  /** A store pinned at a one-key root, with a child root saved on top of it, and caches describing the child. */
  private def storeWithChild() =
    val store = new StateTrieStore(new InMemoryTrieBackend)
    val parent = store.bootstrap(Seq(key(1) -> value(1)))
    val trie = store.at(parent)
    trie.update(Seq(key(2) -> Some(value(2))))
    trie.save()
    store.primeKnownServiceIds(Set(1L))
    store.putCachedServiceInfo(1L, value(1))
    store.addKnownServiceId(2L)
    store.putCachedServiceInfo(2L, value(2))
    (store, parent, trie.rootHash)

  "StateTrieStore.commitChild" should "keep the caches an import updated for the child of the pinned root" in {
    val (store, parent, child) = storeWithChild()
    store.commitChild(parent, child)
    store.currentRoot shouldBe child
    store.cachedServiceIds shouldBe Some(Set(1L, 2L))
    store.cachedServiceInfo(2L) shouldBe Some(value(2))
  }

  it should "drop the caches when the parent is not the pinned root" in {
    val (store, _, child) = storeWithChild()
    store.commitChild(child, child)
    store.cachedServiceIds shouldBe None
    store.cachedServiceInfo(1L) shouldBe None
  }

  "StateTrieStore.markCommitted" should "drop the caches when it moves the pin back to an ancestor (reorg)" in {
    val (store, parent, child) = storeWithChild()
    store.commitChild(parent, child)
    // The caches now hold service 2, which exists only on the child; rewinding to the parent must not keep it.
    store.markCommitted(parent)
    store.currentRoot shouldBe parent
    store.cachedServiceIds shouldBe None
    store.cachedServiceInfo(2L) shouldBe None
  }

  it should "keep the caches when the root does not change" in {
    val (store, parent, child) = storeWithChild()
    store.commitChild(parent, child)
    store.markCommitted(child)
    store.cachedServiceInfo(2L) shouldBe Some(value(2))
  }

  "StateTrieStore.primeKnownServiceIds" should "not overwrite an id set that is already there" in {
    val (store, _, _) = storeWithChild()
    store.primeKnownServiceIds(Set(1L)) // a reader that computed its ids before service 2 was added
    store.cachedServiceIds shouldBe Some(Set(1L, 2L))
  }

  "StateTrieStore.addKnownServiceId" should "not lose ids added concurrently" in {
    val store = new StateTrieStore(new InMemoryTrieBackend)
    store.primeKnownServiceIds(Set.empty)
    val threads = 8
    val perThread = 2000
    val start = new CountDownLatch(1)
    val workers = (0 until threads).map { t =>
      new Thread(() => {
        start.await()
        for i <- 0 until perThread do store.addKnownServiceId((t * perThread + i).toLong)
      })
    }
    workers.foreach(_.start())
    start.countDown()
    workers.foreach(_.join())
    store.cachedServiceIds.map(_.size) shouldBe Some(threads * perThread)
  }
