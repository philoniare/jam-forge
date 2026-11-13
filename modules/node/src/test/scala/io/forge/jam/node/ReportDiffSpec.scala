package io.forge.jam.node

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ReportDiffSpec extends AnyFunSuite with Matchers:
  private final case class Inner(a: Int, bytes: Array[Byte])
  private final case class Outer(name: String, inner: Inner, items: List[Inner])

  test("names each differing field by its path, through nested case classes, lists and byte arrays") {
    val x = Outer("r", Inner(1, Array[Byte](1, 2)), List(Inner(3, Array[Byte](0))))
    ReportDiff.diff(x, x.copy()) shouldBe Nil
    val y = Outer("r", Inner(2, Array[Byte](1, 2)), List(Inner(3, Array[Byte](9))))
    ReportDiff.diff(x, y) shouldBe List("inner.a: ours=1 theirs=2", "items[0].bytes: ours=00 theirs=09")
    ReportDiff.diff(x, x.copy(items = Nil)) shouldBe List("items: 1 entries ours, 0 theirs")
  }
