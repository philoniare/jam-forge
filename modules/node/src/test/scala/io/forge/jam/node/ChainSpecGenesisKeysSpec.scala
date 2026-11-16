package io.forge.jam.node

import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.protocol.state.KeyValue
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ChainSpecGenesisKeysSpec extends AnyFunSuite with Matchers:

  /** A 336-byte validator entry whose Ed25519 key (bytes 32 to 63) is all `id`. */
  private def entry(id: Int): Array[Byte] =
    Array.fill[Byte](32)(0x06) ++ Array.fill[Byte](32)(id.toByte) ++ Array.fill[Byte](272)(0)

  private def key(chapter: Int): JamBytes = JamBytes(chapter.toByte +: Array.fill[Byte](30)(0))

  private def spec(kappa: Array[Byte], gamma: Array[Byte]): ChainSpec =
    ChainSpec(
      id = "t",
      config = ChainConfig.TINY,
      genesisHeaderBytes = None,
      explicitGenesisHash = Some(Hash(Array.fill[Byte](32)(9))),
      genesisState = List(KeyValue(key(8), JamBytes(kappa)), KeyValue(key(4), JamBytes(gamma))),
      bootnodes = Nil
    )

  test("genesis validators and next-epoch validators are read from counted lists") {
    val kappa = 2.toByte +: (entry(1) ++ entry(2))
    val gamma = (2.toByte +: (entry(3) ++ entry(4))) ++ Array.fill[Byte](200)(0x7f)
    val (current, next) = spec(kappa, gamma).genesisValidatorKeys
    current.map(_.toSeq) shouldBe Seq(Seq.fill(32)(1.toByte), Seq.fill(32)(2.toByte))
    next.map(_.toSeq) shouldBe Seq(Seq.fill(32)(3.toByte), Seq.fill(32)(4.toByte))
  }

  test("lists of V entries with no count are read too, though an entry starts with a byte that reads as a count") {
    val kappa = (1 to 6).flatMap(entry).toArray
    val gamma = (11 to 16).flatMap(entry).toArray ++ Array.fill[Byte](200)(0x7f)
    val (current, next) = spec(kappa, gamma).genesisValidatorKeys
    current.map(_.head) shouldBe (1 to 6).map(_.toByte)
    next.map(_.head) shouldBe (11 to 16).map(_.toByte)
  }

  test("a state without validators gives no keys") {
    spec(Array.emptyByteArray, Array.emptyByteArray).genesisValidatorKeys shouldBe ((IndexedSeq.empty, IndexedSeq.empty))
  }
