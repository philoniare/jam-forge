package io.forge.jam.protocol.accumulation.hostcalls

import io.forge.jam.protocol.accumulation.HostCall

class HostCallNumberingSpec extends org.scalatest.funsuite.AnyFunSuite with org.scalatest.matchers.should.Matchers:

  test("gp 0.8.0 host-call numbering is pinned exactly") {
    HostCall.GAS shouldBe 0
    HostCall.GROW_HEAP shouldBe 1
    HostCall.FETCH shouldBe 2
    HostCall.LOOKUP shouldBe 3
    HostCall.READ shouldBe 4
    HostCall.WRITE shouldBe 5
    HostCall.INFO shouldBe 6
    HostCall.HISTORICAL_LOOKUP shouldBe 7
    HostCall.EXPORT shouldBe 8
    HostCall.MACHINE shouldBe 9
    HostCall.PEEK shouldBe 10
    HostCall.POKE shouldBe 11
    HostCall.PAGES shouldBe 12
    HostCall.INVOKE shouldBe 13
    HostCall.EXPUNGE shouldBe 14
    HostCall.BLESS shouldBe 15
    HostCall.ASSIGN shouldBe 16
    HostCall.DESIGNATE shouldBe 17
    HostCall.CHECKPOINT shouldBe 18
    HostCall.NEW shouldBe 19
    HostCall.UPGRADE shouldBe 20
    HostCall.TRANSFER shouldBe 21
    HostCall.EJECT shouldBe 22
    HostCall.QUERY shouldBe 23
    HostCall.SOLICIT shouldBe 24
    HostCall.FORGET shouldBe 25
    HostCall.YIELD shouldBe 26
    HostCall.PROVIDE shouldBe 27
    HostCall.LOG shouldBe 100
  }

  test("HostCall.name resolves every pinned identifier (no UNKNOWN fallthrough)") {
    val allIds = Seq(
      HostCall.GAS, HostCall.GROW_HEAP, HostCall.FETCH, HostCall.LOOKUP, HostCall.READ,
      HostCall.WRITE, HostCall.INFO, HostCall.HISTORICAL_LOOKUP, HostCall.EXPORT,
      HostCall.MACHINE, HostCall.PEEK, HostCall.POKE, HostCall.PAGES, HostCall.INVOKE,
      HostCall.EXPUNGE, HostCall.BLESS, HostCall.ASSIGN, HostCall.DESIGNATE,
      HostCall.CHECKPOINT, HostCall.NEW, HostCall.UPGRADE, HostCall.TRANSFER,
      HostCall.EJECT, HostCall.QUERY, HostCall.SOLICIT, HostCall.FORGET, HostCall.YIELD,
      HostCall.PROVIDE, HostCall.LOG
    )
    allIds.foreach(id => HostCall.name(id) should not startWith "UNKNOWN")
    // No accidental numeric collisions.
    allIds.distinct.size shouldBe allIds.size
  }
