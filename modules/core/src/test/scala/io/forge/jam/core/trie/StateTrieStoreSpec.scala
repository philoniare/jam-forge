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

  /** A store pinned at a one-key root with caches describing it, and a child root saved on top of it. */
  private def storeWithChild() =
    val store = new StateTrieStore(new InMemoryTrieBackend)
    val parent = store.bootstrap(Seq(key(1) -> value(1)))
    val trie = store.at(parent)
    trie.update(Seq(key(2) -> Some(value(2))))
    trie.save()
    store.primeKnownServiceIds(parent, Set(1L))
    store.putCachedServiceInfo(parent, 1L, value(1))
    (store, parent, trie.rootHash)

  private val childChanges = Seq(2L -> Some(value(2)))

  "StateTrieStore.commitChild" should "keep the caches an import updated for the child of the pinned root" in {
    val (store, parent, child) = storeWithChild()
    store.commitChild(parent, child, childChanges)
    store.currentRoot shouldBe child
    store.cachedServiceIds(child) shouldBe Some(Set(1L, 2L))
    store.cachedServiceInfo(child, 1L) shouldBe Some(value(1))
    store.cachedServiceInfo(child, 2L) shouldBe Some(value(2))
    store.cachedServiceIds(parent) shouldBe None
    store.cachedServiceInfo(parent, 1L) shouldBe None
  }

  it should "drop the caches when the parent is not the pinned root" in {
    val (store, _, child) = storeWithChild()
    store.commitChild(child, child, childChanges)
    store.cachedServiceIds(child) shouldBe None
    store.cachedServiceInfo(child, 1L) shouldBe None
    store.cachedServiceInfo(child, 2L) shouldBe None
  }

  "StateTrieStore.markCommitted" should "drop the caches when it moves the pin back to an ancestor (reorg)" in {
    val (store, parent, child) = storeWithChild()
    store.commitChild(parent, child, childChanges)
    // The caches now hold service 2, which exists only on the child; rewinding to the parent must not keep it.
    store.markCommitted(parent)
    store.currentRoot shouldBe parent
    store.cachedServiceIds(parent) shouldBe None
    store.cachedServiceInfo(parent, 2L) shouldBe None
  }

  it should "keep the caches when the root does not change" in {
    val (store, parent, child) = storeWithChild()
    store.commitChild(parent, child, childChanges)
    store.markCommitted(child)
    store.cachedServiceInfo(child, 2L) shouldBe Some(value(2))
  }

  "StateTrieStore.primeKnownServiceIds" should "not overwrite an id set that is already there" in {
    val (store, parent, child) = storeWithChild()
    store.commitChild(parent, child, childChanges)
    store.primeKnownServiceIds(child, Set(1L)) // a set computed some other way must not replace the one kept
    store.cachedServiceIds(child) shouldBe Some(Set(1L, 2L))
  }

  "StateTrieStore's caches" should "neither answer nor take entries for a root other than the pinned one" in {
    val (store, parent, child) = storeWithChild()
    store.cachedServiceInfo(child, 1L) shouldBe None
    store.cachedServiceIds(child) shouldBe None
    store.putCachedServiceInfo(child, 1L, value(9))
    store.putCachedServiceInfos(child, Seq(3L -> value(3)))
    store.primeKnownServiceIds(child, Set(9L))
    store.addKnownServiceId(child, 9L)
    store.removeKnownServiceId(child, 1L)
    store.evictCachedServiceInfo(child, 1L)
    store.cachedServiceInfo(parent, 1L) shouldBe Some(value(1))
    store.cachedServiceInfo(parent, 3L) shouldBe None
    store.cachedServiceIds(parent) shouldBe Some(Set(1L))
  }

  "StateTrieStore.addKnownServiceId" should "not lose ids added concurrently" in {
    val store = new StateTrieStore(new InMemoryTrieBackend)
    val root = store.currentRoot
    store.primeKnownServiceIds(root, Set.empty)
    val threads = 8
    val perThread = 2000
    val start = new CountDownLatch(1)
    val workers = (0 until threads).map { t =>
      new Thread(() => {
        start.await()
        for i <- 0 until perThread do store.addKnownServiceId(root, (t * perThread + i).toLong)
      })
    }
    workers.foreach(_.start())
    start.countDown()
    workers.foreach(_.join())
    store.cachedServiceIds(root).map(_.size) shouldBe Some(threads * perThread)
  }
