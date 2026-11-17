package io.forge.jam.protocol.accumulation

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.service.ServiceInfo
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AccountChangesSpec extends AnyFunSuite with Matchers:

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

  private def changes(own: Long, created: (Long, ServiceAccount)*): AccountChanges =
    val c = new AccountChanges
    c.accountUpdates(own) = account(own)
    created.foreach { (id, a) =>
      c.accountUpdates(id) = a
      c.createdIds += id
    }
    c

  test("GP: two services creating the same service id make the block invalid, even with equal accounts") {
    val merged = new AccountChanges
    merged.checkAndMerge(changes(1L, 70000L -> account(100L)))
    val e = the[AccountChanges.DuplicateNewServiceId] thrownBy
      merged.checkAndMerge(changes(2L, 70000L -> account(100L)))
    e.serviceId shouldBe 70000L
  }

  test("GP: two services creating the same service id with different accounts make the block invalid") {
    val merged = new AccountChanges
    merged.checkAndMerge(changes(1L, 70000L -> account(100L)))
    an[AccountChanges.DuplicateNewServiceId] should be thrownBy
      merged.checkAndMerge(changes(2L, 70000L -> account(200L)))
  }

  test("services creating different ids merge, with every new account") {
    val merged = new AccountChanges
    merged.checkAndMerge(changes(1L, 70000L -> account(100L)))
    merged.checkAndMerge(changes(2L, 70001L -> account(200L)))
    merged.createdIds.toSet shouldBe Set(70000L, 70001L)
    merged.accountUpdates.keySet.toSet shouldBe Set(1L, 2L, 70000L, 70001L)
  }
