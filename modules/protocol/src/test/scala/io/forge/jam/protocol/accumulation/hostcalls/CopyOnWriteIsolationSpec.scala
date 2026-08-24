package io.forge.jam.protocol.accumulation.hostcalls

import io.forge.jam.core.JamBytes
import io.forge.jam.core.trie.{StateTrie, InMemoryTrieBackend}
import io.forge.jam.protocol.accumulation._
import io.forge.jam.protocol.state.ServiceStorageView

import scala.collection.mutable

class CopyOnWriteIsolationSpec extends HostCallTestBase:

  private def stateWith(accounts: Map[Long, ServiceAccount]): PartialState =
    PartialState(
      accounts = accounts,
      stagingSet = mutable.ListBuffer.empty,
      authQueue = mutable.ListBuffer.empty,
      manager = 0L,
      assigners = mutable.ListBuffer.empty,
      delegator = 0L,
      registrar = 0L,
      alwaysAccers = mutable.Map.empty
    )

  test("deepCopy shares the accounts map O(1) but isolates mutations") {
    val original = createTestAccount(balance = 1000L)
    val state = stateWith(Map(1L -> original))

    val snapshot = state.deepCopy()
    // O(1) copy: the heavy accounts map is shared by reference initially.
    assert(snapshot.accounts eq state.accounts)

    // The executor mutates via a functional rebuild, not in place.
    state.accounts = state.accounts.updated(1L, createTestAccount(balance = 9999L))

    // The snapshot keeps the pre-mutation value; the maps have diverged.
    snapshot.accounts(1L).info.balance shouldBe 1000L
    state.accounts(1L).info.balance shouldBe 9999L
    assert(!(snapshot.accounts eq state.accounts))
  }

  test("deepCopy's per-block mutable collections are independent") {
    val state = stateWith(Map.empty)
    state.stagingSet += JamBytes(Array[Byte](1))

    val snapshot = state.deepCopy()
    snapshot.stagingSet += JamBytes(Array[Byte](2))

    state.stagingSet.size shouldBe 1 // original unaffected by the copy's edit
    snapshot.stagingSet.size shouldBe 2
  }

  test("the checkpoint snapshot y is isolated from later x mutations") {
    val ctx = createTestContext(serviceIndex = 100L, balance = 500L)
    ctx.y.accounts(100L).info.balance shouldBe 500L

    ctx.x.accounts = ctx.x.accounts.updated(100L, createTestAccount(balance = 1L))

    ctx.y.accounts(100L).info.balance shouldBe 500L // frozen snapshot unchanged
    ctx.x.accounts(100L).info.balance shouldBe 1L // current state changed
  }

  test("a foreign-service raw read bypasses pending writes (trace-00002244)") {
    val key = JamBytes(Array.fill[Byte](31)(0xab.toByte))
    val preBlock = JamBytes(Array[Byte](1, 1, 1))
    val pendingWrite = JamBytes(Array[Byte](2, 2, 2))

    val trie = StateTrie.empty(new InMemoryTrieBackend())
    trie.update(Seq((key, Some(preBlock)))) // committed pre-block value
    val view = new ServiceStorageView(trie)
    view.putByStateKey(key, pendingWrite) // an in-flight staged write

    val ownService = 100L
    val foreignService = 200L
    val state = stateWith(Map.empty)
    val ctx = new AccumulationContext(
      x = state.deepCopy(),
      initialY = state.deepCopy(),
      serviceIndex = ownService,
      timeslot = 1000L,
      entropy = JamBytes.zeros(32),
      storageView = Some(view)
    )

    // The owning service observes its own in-flight pending write...
    ctx.readRawDataFor(ownService, key) shouldBe Some(pendingWrite)
    // ...but a foreign service must observe only the pre-block (trie) value,
    // never another service's staged pending write.
    ctx.readRawDataFor(foreignService, key) shouldBe Some(preBlock)
  }
