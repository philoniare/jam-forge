package io.forge.jam.protocol.state

import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.trie.{InMemoryTrieBackend, StateTrie, StateTrieStore}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ServiceCacheRootSpec extends AnyFunSuite with Matchers:

  private def account(value: Byte): JamBytes = JamBytes(Array.fill[Byte](100)(value))

  private final class States:
    val backend = new InMemoryTrieBackend()
    def state(accounts: (Long, Byte)*): Hash =
      val t = StateTrie.at(backend, Hash.zero)
      t.update(accounts.map((id, v) => StateKey.computeServiceAccountKey(id) -> Some(account(v))))
      t.save()
      t.rootHash
    def view(store: StateTrieStore, root: Hash): TrieBackedJamState =
      val trie = StateTrie.at(backend, root)
      new TrieBackedJamState(trie, ChainConfig.TINY, new ServiceStorageView(trie), Some(store))

  test("a view at another root does not put its accounts into the cache the pinned state is read through") {
    val backend = new InMemoryTrieBackend()
    val key = StateKey.computeServiceAccountKey(7L)
    def stateWith(value: Byte): Hash =
      val t = StateTrie.at(backend, Hash.zero)
      t.update(Seq(key -> Some(JamBytes(Array.fill[Byte](100)(value)))))
      t.save()
      t.rootHash
    val old = stateWith(1)
    val pinned = stateWith(2)
    val store = new StateTrieStore(backend)
    store.markCommitted(pinned)

    def view(root: Hash) =
      val trie = StateTrie.at(backend, root)
      new TrieBackedJamState(trie, ChainConfig.TINY, new ServiceStorageView(trie), Some(store))

    view(old).priorServiceInfoEncoded(7L) shouldBe Some(JamBytes(Array.fill[Byte](100)(1)))
    view(pinned).priorServiceInfoEncoded(7L) shouldBe Some(JamBytes(Array.fill[Byte](100)(2)))
  }

  test("a view at another root does not read the pinned state's cached account") {
    val s = new States
    val old = s.state(7L -> 1)
    val pinned = s.state(7L -> 2)
    val store = new StateTrieStore(s.backend)
    store.markCommitted(pinned)

    s.view(store, pinned).priorServiceInfoEncoded(7L) shouldBe Some(account(2))
    store.cachedServiceInfo(pinned, 7L) shouldBe Some(account(2))
    s.view(store, old).priorServiceInfoEncoded(7L) shouldBe Some(account(1))
    store.cachedServiceInfo(pinned, 7L) shouldBe Some(account(2))
  }

  test("a view at another root lists its own state's services and does not prime the pinned state's id set") {
    val s = new States
    val old = s.state(7L -> 1)
    val pinned = s.state(7L -> 2, 8L -> 3)
    val store = new StateTrieStore(s.backend)
    store.markCommitted(pinned)

    s.view(store, old).serviceIds shouldBe Set(7L)
    store.cachedServiceIds(pinned) shouldBe None
    store.cachedServiceInfo(pinned, 7L) shouldBe None

    s.view(store, pinned).serviceIds shouldBe Set(7L, 8L)
    store.cachedServiceIds(pinned) shouldBe Some(Set(7L, 8L))
    store.cachedServiceInfo(pinned, 7L) shouldBe Some(account(2))
    s.view(store, old).serviceIds shouldBe Set(7L)
    s.view(store, old).priorServiceInfoEncoded(8L) shouldBe None
  }

  test("when the pin moves, the caches follow it: carried to a child with its changes, dropped elsewhere") {
    val s = new States
    val other = s.state(7L -> 1)
    val parent = s.state(7L -> 2, 8L -> 3)
    val child = s.state(7L -> 4, 9L -> 5)
    val store = new StateTrieStore(s.backend)
    store.markCommitted(parent)
    s.view(store, parent).serviceIds shouldBe Set(7L, 8L)

    store.commitChild(parent, child, Seq(7L -> Some(account(4)), 8L -> None, 9L -> Some(account(5))))
    store.currentRoot shouldBe child
    store.cachedServiceIds(child) shouldBe Some(Set(7L, 9L))
    store.cachedServiceInfo(child, 7L) shouldBe Some(account(4))
    store.cachedServiceInfo(child, 8L) shouldBe None
    store.cachedServiceInfo(child, 9L) shouldBe Some(account(5))
    store.cachedServiceIds(parent) shouldBe None
    store.cachedServiceInfo(parent, 7L) shouldBe None
    s.view(store, child).priorServiceInfoEncoded(7L) shouldBe Some(account(4))
    s.view(store, parent).priorServiceInfoEncoded(7L) shouldBe Some(account(2))
    s.view(store, parent).serviceIds shouldBe Set(7L, 8L)

    store.markCommitted(other)
    store.cachedServiceIds(child) shouldBe None
    store.cachedServiceInfo(child, 7L) shouldBe None
    store.cachedServiceIds(other) shouldBe None
    store.cachedServiceInfo(other, 7L) shouldBe None
    s.view(store, other).priorServiceInfoEncoded(7L) shouldBe Some(account(1))
    s.view(store, child).priorServiceInfoEncoded(7L) shouldBe Some(account(4))
    s.view(store, child).serviceIds shouldBe Set(7L, 9L)
    store.cachedServiceIds(other) shouldBe None
    s.view(store, other).serviceIds shouldBe Set(7L)
    store.cachedServiceIds(other) shouldBe Some(Set(7L))

    store.commitChild(parent, child, Seq(7L -> Some(account(4))))
    store.cachedServiceIds(child) shouldBe None
    store.cachedServiceInfo(child, 7L) shouldBe None
  }

  test("readers at two roots never see each other's accounts while the pin moves between them") {
    val s = new States
    val a = s.state(7L -> 1)
    val b = s.state(7L -> 2, 8L -> 3)
    val toB = Seq(7L -> Some(account(2)), 8L -> Some(account(3)))
    val toA = Seq(7L -> Some(account(1)), 8L -> None)
    val store = new StateTrieStore(s.backend)
    store.markCommitted(a)

    val stop = new java.util.concurrent.atomic.AtomicBoolean(false)
    val errors = new java.util.concurrent.ConcurrentLinkedQueue[String]()
    val reads = new java.util.concurrent.atomic.AtomicLong(0)

    val mover = new Thread(() => {
      var i = 0L
      while !stop.get() do
        if i % 7 == 6 then
          store.markCommitted(if store.currentRoot == a then b else a)
        else if store.currentRoot == a then store.commitChild(a, b, toB)
        else store.commitChild(b, a, toA)
        i += 1
    })
    def reader(root: Hash, expected: JamBytes, ids: Set[Long]) = new Thread(() => {
      while !stop.get() do
        val v = s.view(store, root)
        val got = v.priorServiceInfoEncoded(7L)
        if !got.contains(expected) then errors.add(s"view at $root read service 7 as ${got.map(_.toArray.head)}")
        val gotIds = s.view(store, root).serviceIds
        if gotIds != ids then errors.add(s"view at $root listed $gotIds")
        val again = v.priorServiceInfoEncoded(7L)
        if !again.contains(expected) then errors.add(s"view at $root re-read service 7 as ${again.map(_.toArray.head)}")
        reads.incrementAndGet()
    })
    val readers = (0 until 2).flatMap(_ => Seq(reader(a, account(1), Set(7L)), reader(b, account(2), Set(7L, 8L))))
    (mover +: readers).foreach(_.start())
    Thread.sleep(1000)
    stop.set(true)
    (mover +: readers).foreach(_.join())

    withClue(errors.toArray.take(5).mkString("; ")) { errors.size shouldBe 0 }
    reads.get should be > 0L
  }
