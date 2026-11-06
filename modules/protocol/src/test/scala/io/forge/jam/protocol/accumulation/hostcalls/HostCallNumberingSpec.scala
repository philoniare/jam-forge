package io.forge.jam.protocol.accumulation.hostcalls

import io.forge.jam.protocol.accumulation.HostCall

class HostCallNumberingSpec extends org.scalatest.funsuite.AnyFunSuite with org.scalatest.matchers.should.Matchers:

  test("host-call numbering is pinned exactly (compile = 9)") {
    HostCall.GAS shouldBe 0
    HostCall.GROW_HEAP shouldBe 1
    HostCall.FETCH shouldBe 2
    HostCall.LOOKUP shouldBe 3
    HostCall.READ shouldBe 4
    HostCall.WRITE shouldBe 5
    HostCall.INFO shouldBe 6
    HostCall.HISTORICAL_LOOKUP shouldBe 7
    HostCall.EXPORT shouldBe 8
    HostCall.COMPILE shouldBe 9
    HostCall.MACHINE shouldBe 10
    HostCall.PEEK shouldBe 11
    HostCall.POKE shouldBe 12
    HostCall.PAGES shouldBe 13
    HostCall.INVOKE shouldBe 14
    HostCall.EXPUNGE shouldBe 15
    HostCall.BLESS shouldBe 16
    HostCall.ASSIGN shouldBe 17
    HostCall.DESIGNATE shouldBe 18
    HostCall.CHECKPOINT shouldBe 19
    HostCall.NEW shouldBe 20
    HostCall.UPGRADE shouldBe 21
    HostCall.TRANSFER shouldBe 22
    HostCall.EJECT shouldBe 23
    HostCall.QUERY shouldBe 24
    HostCall.SOLICIT shouldBe 25
    HostCall.FORGET shouldBe 26
    HostCall.YIELD shouldBe 27
    HostCall.PROVIDE shouldBe 28
    HostCall.LOG shouldBe 100
  }

  test("HostCall.name resolves every pinned identifier (no UNKNOWN fallthrough)") {
    val allIds = Seq(
      HostCall.GAS, HostCall.GROW_HEAP, HostCall.FETCH, HostCall.LOOKUP, HostCall.READ,
      HostCall.WRITE, HostCall.INFO, HostCall.HISTORICAL_LOOKUP, HostCall.EXPORT,
      HostCall.COMPILE, HostCall.MACHINE, HostCall.PEEK, HostCall.POKE, HostCall.PAGES, HostCall.INVOKE,
      HostCall.EXPUNGE, HostCall.BLESS, HostCall.ASSIGN, HostCall.DESIGNATE,
      HostCall.CHECKPOINT, HostCall.NEW, HostCall.UPGRADE, HostCall.TRANSFER,
      HostCall.EJECT, HostCall.QUERY, HostCall.SOLICIT, HostCall.FORGET, HostCall.YIELD,
      HostCall.PROVIDE, HostCall.LOG
    )
    allIds.foreach(id => HostCall.name(id) should not startWith "UNKNOWN")
    // No accidental numeric collisions.
    allIds.distinct.size shouldBe allIds.size
  }
