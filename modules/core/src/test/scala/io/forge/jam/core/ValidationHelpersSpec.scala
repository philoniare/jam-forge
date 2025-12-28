package io.forge.jam.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ValidationHelpersSpec extends AnyFlatSpec with Matchers:
  "isSortedUnique" should "return true for sorted unique list" in {
    val items = List(1, 2, 3, 4, 5)
    ValidationHelpers.isSortedUnique(items)((a, b) => a - b) shouldBe true
  }

  it should "return false for unsorted list" in {
    val items = List(1, 3, 2)
    ValidationHelpers.isSortedUnique(items)((a, b) => a - b) shouldBe false
  }

  it should "return false for list with duplicates" in {
    val items = List(1, 2, 2, 3)
    ValidationHelpers.isSortedUnique(items)((a, b) => a - b) shouldBe false
  }

  it should "return true for empty list" in {
    ValidationHelpers.isSortedUnique(List.empty[Int])((a, b) => a - b) shouldBe true
  }

  it should "return true for single-element list" in {
    ValidationHelpers.isSortedUnique(List(42))((a, b) => a - b) shouldBe true
  }

  it should "work with string comparator" in {
    val items = List("a", "b", "c")
    ValidationHelpers.isSortedUnique(items)((a, b) => a.compareTo(b)) shouldBe true
  }

  "isSortedUniqueBy" should "return true for sorted unique by key" in {
    case class Item(key: Int, value: String)
    val items = List(Item(1, "a"), Item(2, "b"), Item(3, "c"))
    ValidationHelpers.isSortedUniqueBy(items)(_.key) shouldBe true
  }

  it should "return false for unsorted by key" in {
    case class Item(key: Int, value: String)
    val items = List(Item(2, "a"), Item(1, "b"))
    ValidationHelpers.isSortedUniqueBy(items)(_.key) shouldBe false
  }

  it should "return false for duplicate keys" in {
    case class Item(key: Int, value: String)
    val items = List(Item(1, "a"), Item(1, "b"))
    ValidationHelpers.isSortedUniqueBy(items)(_.key) shouldBe false
  }

  "isSortedUniqueByInt" should "return true for sorted unique int keys" in {
    case class Item(id: Int)
    val items = List(Item(10), Item(20), Item(30))
    ValidationHelpers.isSortedUniqueByInt(items)(_.id) shouldBe true
  }

  it should "return false for unsorted int keys" in {
    case class Item(id: Int)
    val items = List(Item(30), Item(10))
    ValidationHelpers.isSortedUniqueByInt(items)(_.id) shouldBe false
  }

  it should "return false for duplicate int keys" in {
    case class Item(id: Int)
    val items = List(Item(10), Item(10))
    ValidationHelpers.isSortedUniqueByInt(items)(_.id) shouldBe false
  }

  it should "work with empty list" in {
    ValidationHelpers.isSortedUniqueByInt(List.empty[(Int, String)])(x => x._1) shouldBe true
  }

  "isSortedUniqueByLong" should "return true for sorted unique long keys" in {
    case class Item(id: Long)
    val items = List(Item(100L), Item(200L), Item(300L))
    ValidationHelpers.isSortedUniqueByLong(items)(_.id) shouldBe true
  }

  it should "return false for unsorted long keys" in {
    case class Item(id: Long)
    val items = List(Item(300L), Item(100L))
    ValidationHelpers.isSortedUniqueByLong(items)(_.id) shouldBe false
  }

  it should "handle large long values" in {
    case class Item(id: Long)
    val items = List(Item(Long.MinValue), Item(0L), Item(Long.MaxValue))
    ValidationHelpers.isSortedUniqueByLong(items)(_.id) shouldBe true
  }

  "isSortedUniqueByBytes" should "return true for sorted unique byte arrays" in {
    case class Item(data: Array[Byte])
    val items = List(
      Item(Array[Byte](0, 0, 1)),
      Item(Array[Byte](0, 0, 2)),
      Item(Array[Byte](0, 1, 0))
    )
    ValidationHelpers.isSortedUniqueByBytes(items)(_.data) shouldBe true
  }

  it should "return false for unsorted byte arrays" in {
    case class Item(data: Array[Byte])
    val items = List(
      Item(Array[Byte](0, 1, 0)),
      Item(Array[Byte](0, 0, 1))
    )
    ValidationHelpers.isSortedUniqueByBytes(items)(_.data) shouldBe false
  }

  it should "use unsigned comparison" in {
    case class Item(data: Array[Byte])
    // 0xFF (255 unsigned) > 0x01
    val items = List(
      Item(Array[Byte](0x01)),
      Item(Array[Byte](0xff.toByte))
    )
    ValidationHelpers.isSortedUniqueByBytes(items)(_.data) shouldBe true
  }

  "isSortedUniqueByJamBytes" should "return true for sorted unique JamBytes keys" in {
    case class Item(data: JamBytes)
    val items = List(
      Item(JamBytes(Array[Byte](0, 1))),
      Item(JamBytes(Array[Byte](0, 2))),
      Item(JamBytes(Array[Byte](1, 0)))
    )
    ValidationHelpers.isSortedUniqueByJamBytes(items)(_.data) shouldBe true
  }

  it should "return false for unsorted JamBytes keys" in {
    case class Item(data: JamBytes)
    val items = List(
      Item(JamBytes(Array[Byte](1, 0))),
      Item(JamBytes(Array[Byte](0, 1)))
    )
    ValidationHelpers.isSortedUniqueByJamBytes(items)(_.data) shouldBe false
  }

  it should "return false for duplicate JamBytes keys" in {
    case class Item(data: JamBytes)
    val items = List(
      Item(JamBytes(Array[Byte](1, 2))),
      Item(JamBytes(Array[Byte](1, 2)))
    )
    ValidationHelpers.isSortedUniqueByJamBytes(items)(_.data) shouldBe false
  }

  "isSortedBy" should "return true for sorted list with duplicates" in {
    case class Item(value: Int)
    val items = List(Item(1), Item(1), Item(2), Item(3))
    ValidationHelpers.isSortedBy(items)(_.value) shouldBe true
  }

  it should "return false for unsorted list" in {
    case class Item(value: Int)
    val items = List(Item(2), Item(1))
    ValidationHelpers.isSortedBy(items)(_.value) shouldBe false
  }

  it should "return true for empty list" in {
    ValidationHelpers.isSortedBy(List.empty[(Int, String)])(x => x._1) shouldBe true
  }

  it should "return true for single-element list" in {
    case class Item(value: Int)
    ValidationHelpers.isSortedBy(List(Item(42)))(_.value) shouldBe true
  }

  it should "work with string keys" in {
    case class Item(name: String)
    val items = List(Item("alpha"), Item("beta"), Item("gamma"))
    ValidationHelpers.isSortedBy(items)(_.name) shouldBe true
  }
