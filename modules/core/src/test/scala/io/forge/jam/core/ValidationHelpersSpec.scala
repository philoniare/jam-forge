package io.forge.jam.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ValidationHelpersSpec extends AnyFlatSpec with Matchers:

  private def checkAll[A](cases: List[(A, Boolean)])(f: A => Boolean): Unit =
    for (input, expected) <- cases do
      withClue(s"input=$input: ")(f(input) shouldBe expected)

  "isSortedUnique" should "accept strictly increasing lists only" in {
    checkAll(List(
      List(1, 2, 3, 4, 5) -> true,
      List(1, 3, 2) -> false,
      List(1, 2, 2, 3) -> false,
      List.empty[Int] -> true,
      List(42) -> true
    ))(items => ValidationHelpers.isSortedUnique(items)((a, b) => a - b))
    ValidationHelpers.isSortedUnique(List("a", "b", "c"))((a, b) => a.compareTo(b)) shouldBe true
  }

  "isSortedUniqueBy" should "accept strictly increasing keys only" in {
    case class Item(key: Int, value: String)
    checkAll(List(
      List(Item(1, "a"), Item(2, "b"), Item(3, "c")) -> true,
      List(Item(2, "a"), Item(1, "b")) -> false,
      List(Item(1, "a"), Item(1, "b")) -> false
    ))(items => ValidationHelpers.isSortedUniqueBy(items)(_.key))
  }

  "isSortedUniqueByInt" should "accept strictly increasing int keys only" in {
    case class Item(id: Int)
    checkAll(List(
      List(Item(10), Item(20), Item(30)) -> true,
      List(Item(30), Item(10)) -> false,
      List(Item(10), Item(10)) -> false,
      List.empty[Item] -> true
    ))(items => ValidationHelpers.isSortedUniqueByInt(items)(_.id))
  }

  "isSortedUniqueByLong" should "accept strictly increasing long keys only, across the full range" in {
    case class Item(id: Long)
    checkAll(List(
      List(Item(100L), Item(200L), Item(300L)) -> true,
      List(Item(300L), Item(100L)) -> false,
      List(Item(Long.MinValue), Item(0L), Item(Long.MaxValue)) -> true
    ))(items => ValidationHelpers.isSortedUniqueByLong(items)(_.id))
  }

  "isSortedUniqueByBytes" should "accept strictly increasing byte arrays only, compared unsigned" in {
    checkAll(List(
      List(Array[Byte](0, 0, 1), Array[Byte](0, 0, 2), Array[Byte](0, 1, 0)) -> true,
      List(Array[Byte](0, 1, 0), Array[Byte](0, 0, 1)) -> false,
      List(Array[Byte](0x01), Array[Byte](0xff.toByte)) -> true // 0xFF (255 unsigned) > 0x01
    ))(items => ValidationHelpers.isSortedUniqueByBytes(items)(identity))
  }

  "isSortedUniqueByJamBytes" should "accept strictly increasing JamBytes keys only" in {
    def jb(bs: Int*): JamBytes = JamBytes(bs.map(_.toByte).toArray)
    checkAll(List(
      List(jb(0, 1), jb(0, 2), jb(1, 0)) -> true,
      List(jb(1, 0), jb(0, 1)) -> false,
      List(jb(1, 2), jb(1, 2)) -> false
    ))(items => ValidationHelpers.isSortedUniqueByJamBytes(items)(identity))
  }

  "isSortedBy" should "accept non-decreasing keys, duplicates included" in {
    case class Item(value: Int)
    checkAll(List(
      List(Item(1), Item(1), Item(2), Item(3)) -> true,
      List(Item(2), Item(1)) -> false,
      List.empty[Item] -> true,
      List(Item(42)) -> true
    ))(items => ValidationHelpers.isSortedBy(items)(_.value))
    ValidationHelpers.isSortedBy(List("alpha", "beta", "gamma"))(identity) shouldBe true
  }
