package io.forge.jam.protocol.state

import io.forge.jam.core.JamBytes
import io.forge.jam.core.trie.{StateTrie, InMemoryTrieBackend}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ServiceStorageViewSpec extends AnyFlatSpec with Matchers:

  private def key(first: Int, second: Int): JamBytes =
    val a = new Array[Byte](31)
    a(0) = first.toByte
    a(1) = second.toByte
    JamBytes(a)

  "ServiceStorageView.enumerate" should "filter pending writes by the requested prefix" in {
    val view = new ServiceStorageView(StateTrie.empty(new InMemoryTrieBackend()))
    val inPrefix = key(0xab, 0x01)
    val outOfPrefix = key(0xcd, 0x02)
    view.putByStateKey(inPrefix, JamBytes(Array[Byte](1, 2, 3)))
    view.putByStateKey(outOfPrefix, JamBytes(Array[Byte](4, 5, 6)))

    // Prefix = first byte 0xab (8 bits).
    val keys = view.enumerate(key(0xab, 0x00), 8).map(_._1).toList

    keys should contain(inPrefix)
    keys should not contain outOfPrefix
  }
