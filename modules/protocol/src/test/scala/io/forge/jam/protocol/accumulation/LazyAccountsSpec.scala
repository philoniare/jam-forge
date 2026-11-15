package io.forge.jam.protocol.accumulation

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.service.ServiceInfo
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class LazyAccountsSpec extends AnyFunSuite with Matchers:

  private def account(balance: Long): ServiceAccount =
    ServiceAccount(
      info = ServiceInfo(
        codeHash = Hash(Array.fill[Byte](32)(1)),
        balance = balance,
        minItemGas = 0L,
        minMemoGas = 0L,
        bytesUsed = 0L,
        depositOffset = 0L,
        items = 0,
        creationSlot = 0L,
        lastAccumulationSlot = 0L,
        parentService = 0L
      ),
      storage = Map.empty,
      preimages = Map.empty[Hash, JamBytes],
      preimageRequests = Map.empty,
      lastAccumulated = 0L
    )

  private final class Source(stored: Map[Long, ServiceAccount]) extends AccountSource:
    var reads = List.empty[Long]
    var enumerations = 0
    def account(id: Long): Option[ServiceAccount] =
      reads ::= id
      stored.get(id)
    def ids: Iterable[Long] =
      enumerations += 1
      stored.keys

  test("an account is read from the source when asked for, and never enumerated by point operations") {
    val source = new Source(Map(1L -> account(10), 2L -> account(20), 3L -> account(30)))
    val accounts: Map[Long, ServiceAccount] = LazyAccounts(source)

    accounts.get(2L).map(_.info.balance) shouldBe Some(20L)
    accounts.contains(9L) shouldBe false
    val changed = accounts.updated(1L, account(11)).removed(3L).updated(7L, account(70))
    changed.get(1L).map(_.info.balance) shouldBe Some(11L)
    changed.contains(3L) shouldBe false
    changed.get(7L).map(_.info.balance) shouldBe Some(70L)
    accounts.get(1L).map(_.info.balance) shouldBe Some(10L)

    source.enumerations shouldBe 0
    source.reads.toSet shouldBe Set(2L, 9L, 1L)
  }

  test("changes are the accounts written or removed since the source") {
    val source = new Source(Map(1L -> account(10), 3L -> account(30)))
    val changed = LazyAccounts(source).updated(1L, account(11)).removed(3L).updated(7L, account(70)).removed(8L)
      .asInstanceOf[LazyAccounts]

    changed.written.map((id, a) => id -> a.info.balance).toMap shouldBe Map(1L -> 11L, 7L -> 70L)
    changed.removedIds.toSet shouldBe Set(3L, 8L)
    source.enumerations shouldBe 0
  }

  test("iterating gives every account of the source with the changes applied") {
    val source = new Source(Map(1L -> account(10), 2L -> account(20), 3L -> account(30)))
    val changed = LazyAccounts(source).updated(1L, account(11)).removed(3L).updated(7L, account(70))

    changed.toList.map((id, a) => id -> a.info.balance).sortBy(_._1) shouldBe List(1L -> 11L, 2L -> 20L, 7L -> 70L)
    changed.size shouldBe 3
  }
