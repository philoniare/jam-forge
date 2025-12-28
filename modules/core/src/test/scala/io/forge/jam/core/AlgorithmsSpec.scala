package io.forge.jam.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import primitives.Hash
import io.circe.*
import io.circe.parser.*
import scala.io.Source

class AlgorithmsSpec extends AnyFlatSpec with Matchers:
  private val shuffleTestVectorsPath = "jamtestvectors/shuffle/shuffle_tests.json"
  private val trieTestVectorsPath = "jamtestvectors/trie/trie.json"

  case class ShuffleTestCase(input: Int, entropy: String, output: List[Int])
  case class TrieTestCase(input: Map[String, String], output: String)

  given Decoder[ShuffleTestCase] = Decoder.instance { cursor =>
    for
      input <- cursor.downField("input").as[Int]
      entropy <- cursor.downField("entropy").as[String]
      output <- cursor.downField("output").as[List[Int]]
    yield ShuffleTestCase(input, entropy, output)
  }

  given Decoder[TrieTestCase] = Decoder.instance { cursor =>
    for
      input <- cursor.downField("input").as[Map[String, String]]
      output <- cursor.downField("output").as[String]
    yield TrieTestCase(input, output)
  }

  private def loadShuffleTestCases(): List[ShuffleTestCase] =
    val source = Source.fromFile(shuffleTestVectorsPath)
    try
      val json = source.mkString
      decode[List[ShuffleTestCase]](json) match
        case Right(cases) => cases
        case Left(error) => fail(s"Failed to parse shuffle test vectors: $error")
    finally
      source.close()

  private def loadTrieTestCases(): List[TrieTestCase] =
    val source = Source.fromFile(trieTestVectorsPath)
    try
      val json = source.mkString
      decode[List[TrieTestCase]](json) match
        case Right(cases) => cases
        case Left(error) => fail(s"Failed to parse trie test vectors: $error")
    finally
      source.close()

  private lazy val shuffleTestCases = loadShuffleTestCases()

  "Shuffle.jamComputeShuffle" should "pass all test vectors from shuffle_tests.json" in {
    shuffleTestCases.foreach { testCase =>
      withClue(s"size=${testCase.input}: ") {
        val entropy = Hash.fromHex(testCase.entropy).toOption.get
        val result = Shuffle.jamComputeShuffle(testCase.input, entropy)
        result shouldBe testCase.output
      }
    }
  }

  private lazy val trieTestCases = loadTrieTestCases()

  "MerkleTrie.merkle" should "pass all test vectors from trie.json" in {
    trieTestCases.zipWithIndex.foreach {
      case (testCase, index) =>
        withClue(s"case $index (${testCase.input.size} entries): ") {
          val entries = testCase.input.map {
            case (k, v) =>
              JamBytes.fromHexUnsafe(k) -> JamBytes.fromHexUnsafe(v)
          }
          val result = MerkleTrie.merkle(entries)
          val expected = Hash.fromHex(testCase.output).toOption.get
          result.bytes shouldBe expected.bytes
        }
    }
  }
