package io.forge.jam.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class JamBytesListSpec extends AnyFlatSpec with Matchers:

  "JamBytesList.empty" should "create an empty list" in {
    val list = JamBytesList.empty
    list.size shouldBe 0
    list.isEmpty shouldBe true
    list.nonEmpty shouldBe false
  }

  "JamBytesList.apply" should "create a list from varargs" in {
    val b1 = JamBytes(Array[Byte](1, 2, 3))
    val b2 = JamBytes(Array[Byte](4, 5, 6))
    val list = JamBytesList(b1, b2)
    list.size shouldBe 2
    list(0).toArray shouldBe Array[Byte](1, 2, 3)
    list(1).toArray shouldBe Array[Byte](4, 5, 6)
  }

  "JamBytesList.fromSeq" should "create a list from a sequence" in {
    val seq = Seq(JamBytes(Array[Byte](1)), JamBytes(Array[Byte](2)))
    val list = JamBytesList.fromSeq(seq)
    list.size shouldBe 2
  }

  "JamBytesList.from" should "create a list from an iterable" in {
    val iter = List(JamBytes(Array[Byte](1)), JamBytes(Array[Byte](2)))
    val list = JamBytesList.from(iter)
    list.size shouldBe 2
  }

  "size and length" should "return the same value" in {
    val list = JamBytesList(JamBytes(Array[Byte](1)), JamBytes(Array[Byte](2)))
    list.size shouldBe list.length
    list.size shouldBe 2
  }

  "isEmpty and nonEmpty" should "correctly report list state" in {
    val empty = JamBytesList.empty
    empty.isEmpty shouldBe true
    empty.nonEmpty shouldBe false

    val nonEmpty = JamBytesList(JamBytes(Array[Byte](1)))
    nonEmpty.isEmpty shouldBe false
    nonEmpty.nonEmpty shouldBe true
  }

  "apply" should "return item at index" in {
    val list = JamBytesList(JamBytes(Array[Byte](1, 2)), JamBytes(Array[Byte](3, 4)))
    list(0).toArray shouldBe Array[Byte](1, 2)
    list(1).toArray shouldBe Array[Byte](3, 4)
  }

  "get" should "return Some for valid index and None for invalid" in {
    val list = JamBytesList(JamBytes(Array[Byte](1)))
    list.get(0) shouldBe defined
    list.get(0).get.toArray shouldBe Array[Byte](1)
    list.get(1) shouldBe None
    list.get(-1) shouldBe None
  }

  "head" should "return the first item" in {
    val list = JamBytesList(JamBytes(Array[Byte](1)), JamBytes(Array[Byte](2)))
    list.head.toArray shouldBe Array[Byte](1)
  }

  "headOption" should "return Some(first) or None" in {
    JamBytesList.empty.headOption shouldBe None
    JamBytesList(JamBytes(Array[Byte](42))).headOption.get.toArray shouldBe Array[Byte](42)
  }

  "last" should "return the last item" in {
    val list = JamBytesList(JamBytes(Array[Byte](1)), JamBytes(Array[Byte](2)))
    list.last.toArray shouldBe Array[Byte](2)
  }

  "lastOption" should "return Some(last) or None" in {
    JamBytesList.empty.lastOption shouldBe None
    JamBytesList(JamBytes(Array[Byte](42))).lastOption.get.toArray shouldBe Array[Byte](42)
  }

  "add" should "append an item to the list" in {
    val list = JamBytesList.empty
    list.add(JamBytes(Array[Byte](1))) shouldBe true
    list.size shouldBe 1
    list(0).toArray shouldBe Array[Byte](1)
  }

  "+=" should "append an item and return this" in {
    val list = JamBytesList.empty
    (list += JamBytes(Array[Byte](1))) shouldBe list
    list.size shouldBe 1
  }

  "add at index" should "insert an item at a specific position" in {
    val list = JamBytesList(JamBytes(Array[Byte](1)), JamBytes(Array[Byte](3)))
    list.add(1, JamBytes(Array[Byte](2)))
    list.size shouldBe 3
    list(1).toArray shouldBe Array[Byte](2)
  }

  "addAll" should "add all items from a collection" in {
    val list = JamBytesList.empty
    list.addAll(Seq(JamBytes(Array[Byte](1)), JamBytes(Array[Byte](2))))
    list.size shouldBe 2
  }

  "removeAt" should "remove and return the item at index" in {
    val list = JamBytesList(JamBytes(Array[Byte](1)), JamBytes(Array[Byte](2)))
    val removed = list.removeAt(0)
    removed.toArray shouldBe Array[Byte](1)
    list.size shouldBe 1
    list(0).toArray shouldBe Array[Byte](2)
  }

  "update" should "replace item at index and return old value" in {
    val list = JamBytesList(JamBytes(Array[Byte](1)))
    val old = list.update(0, JamBytes(Array[Byte](99)))
    old.toArray shouldBe Array[Byte](1)
    list(0).toArray shouldBe Array[Byte](99)
  }

  "clear" should "remove all items" in {
    val list = JamBytesList(JamBytes(Array[Byte](1)), JamBytes(Array[Byte](2)))
    list.clear()
    list.size shouldBe 0
    list.isEmpty shouldBe true
    list.contains(JamBytes(Array[Byte](1, 2, 3))) shouldBe true
    list.contains(JamBytes(Array[Byte](4, 5, 6))) shouldBe false
  }

  "indexOf" should "return index of first matching item" in {
    val list = JamBytesList(
      JamBytes(Array[Byte](1)),
      JamBytes(Array[Byte](2)),
      JamBytes(Array[Byte](1))
    )
    list.indexOf(JamBytes(Array[Byte](1))) shouldBe 0
    list.indexOf(JamBytes(Array[Byte](2))) shouldBe 1
    list.indexOf(JamBytes(Array[Byte](99))) shouldBe -1
  }

  "lastIndexOf" should "return index of last matching item" in {
    val list = JamBytesList(
      JamBytes(Array[Byte](1)),
      JamBytes(Array[Byte](2)),
      JamBytes(Array[Byte](1))
    )
    list.lastIndexOf(JamBytes(Array[Byte](1))) shouldBe 2
    list.lastIndexOf(JamBytes(Array[Byte](99))) shouldBe -1
  }

  "iterator" should "iterate over all items" in {
    val list = JamBytesList(JamBytes(Array[Byte](1)), JamBytes(Array[Byte](2)))
    val collected = list.iterator.toList
    collected.size shouldBe 2
    collected(0).toArray shouldBe Array[Byte](1)
    collected(1).toArray shouldBe Array[Byte](2)
  }

  "foreach" should "apply function to each item" in {
    val list = JamBytesList(JamBytes(Array[Byte](1)), JamBytes(Array[Byte](2)))
    var sum = 0
    list.foreach(b => sum += b.toArray(0))
    sum shouldBe 3
  }

  "map" should "transform items" in {
    val list = JamBytesList(JamBytes(Array[Byte](1)), JamBytes(Array[Byte](2)))
    val mapped = list.map(b => b.toArray(0).toInt)
    mapped shouldBe Seq(1, 2)
  }

  "foldLeft" should "accumulate values" in {
    val list = JamBytesList(JamBytes(Array[Byte](1)), JamBytes(Array[Byte](2)))
    val sum = list.foldLeft(0)((acc, b) => acc + b.toArray(0))
    sum shouldBe 3
  }

  "toList" should "convert to immutable List" in {
    val list = JamBytesList(JamBytes(Array[Byte](1)), JamBytes(Array[Byte](2)))
    val result = list.toList
    result.size shouldBe 2
    result.head.toArray shouldBe Array[Byte](1)
  }

  "toSeq" should "convert to immutable Seq" in {
    val list = JamBytesList(JamBytes(Array[Byte](1)))
    val result = list.toSeq
    result.size shouldBe 1
  }

  "toVector" should "convert to Vector" in {
    val list = JamBytesList(JamBytes(Array[Byte](1)))
    val result = list.toVector
    result.size shouldBe 1
  }

  "clone" should "create an independent copy" in {
    val original = JamBytesList(JamBytes(Array[Byte](1, 2, 3)))
    val cloned = original.clone()

    cloned.size shouldBe original.size
    cloned(0).toArray shouldBe original(0).toArray

    // Modify original
    original.add(JamBytes(Array[Byte](4)))

    // Clone should be unaffected
    cloned.size shouldBe 1
  }

  "encode" should "produce bytes with length prefix" in {
    val list = JamBytesList(JamBytes(Array[Byte](1, 2)), JamBytes(Array[Byte](3, 4)))
    val encoded = list.encode()
    // First byte should be length (2) in compact integer format
    encoded(0) shouldBe 2.toByte
    // Followed by the items
    encoded(1) shouldBe 1.toByte
    encoded(2) shouldBe 2.toByte
    encoded(3) shouldBe 3.toByte
    encoded(4) shouldBe 4.toByte
  }

  "encodeWithoutLength" should "produce bytes without length prefix" in {
    val list = JamBytesList(JamBytes(Array[Byte](1)), JamBytes(Array[Byte](2)))
    val encoded = list.encodeWithoutLength()
    encoded shouldBe Array[Byte](1, 2)
  }

  "fromBytes" should "decode a list with length prefix" in {
    // 2 items, each 2 bytes
    val data = Array[Byte](2, 1, 2, 3, 4)
    val (list, consumed) = JamBytesList.fromBytes(data, 0, 2)
    list.size shouldBe 2
    list(0).toArray shouldBe Array[Byte](1, 2)
    list(1).toArray shouldBe Array[Byte](3, 4)
    consumed shouldBe 5
  }

  "fromBytes with offset" should "start decoding at specified offset" in {
    val data = Array[Byte](99, 99, 1, 0x42)
    val (list, consumed) = JamBytesList.fromBytes(data, 2, 1)
    list.size shouldBe 1
    list(0).toArray shouldBe Array[Byte](0x42)
    consumed shouldBe 2
  }

  "fromBytesWithDecoder" should "use custom decoder for each item" in {
    // 2 items, each variable length (1 byte length + data)
    val data = Array[Byte](2, 2, 0xa, 0xb, 1, 0xc)
    val (list, consumed) = JamBytesList.fromBytesWithDecoder(
      data,
      0,
      (arr, off) =>
        val len = arr(off)
        val bytes = java.util.Arrays.copyOfRange(arr, off + 1, off + 1 + len)
        (JamBytes(bytes), 1 + len)
    )
    list.size shouldBe 2
    list(0).toArray shouldBe Array[Byte](0xa, 0xb)
    list(1).toArray shouldBe Array[Byte](0xc)
    consumed shouldBe 6
  }

  "equals" should "compare by content" in {
    val list1 = JamBytesList(JamBytes(Array[Byte](1, 2)))
    val list2 = JamBytesList(JamBytes(Array[Byte](1, 2)))
    val list3 = JamBytesList(JamBytes(Array[Byte](3, 4)))

    list1 shouldBe list2
    list1 should not be list3
  }

  "equals" should "return false for different types" in {
    val list = JamBytesList(JamBytes(Array[Byte](1)))
    list.equals("not a list") shouldBe false
    list.equals(null) shouldBe false
  }

  "hashCode" should "be consistent with equals" in {
    val list1 = JamBytesList(JamBytes(Array[Byte](1, 2)))
    val list2 = JamBytesList(JamBytes(Array[Byte](1, 2)))
    list1.hashCode shouldBe list2.hashCode
  }

  "toString" should "provide a readable representation" in {
    val list = JamBytesList(JamBytes(Array[Byte](1)))
    val str = list.toString
    str should include("JamBytesList")
    str should include("size=1")
  }

  "toString with many items" should "truncate" in {
    val list = JamBytesList.from((0 until 10).map(i => JamBytes(Array[Byte](i.toByte))))
    val str = list.toString
    str should include("more")
  }
