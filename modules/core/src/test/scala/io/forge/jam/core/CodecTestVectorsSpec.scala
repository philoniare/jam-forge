package io.forge.jam.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.circe.Decoder
import io.circe.parser.*
import io.forge.jam.core.json.decoders.given
import io.forge.jam.core.json.simpletypes.given
import io.forge.jam.core.json.complextypes.given
import io.forge.jam.core.types.header.Header
import io.forge.jam.core.types.workpackage.{WorkPackage, WorkReport}
import io.forge.jam.core.types.workitem.WorkItem
import io.forge.jam.core.types.workresult.WorkResult
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.block.{Block, Extrinsic}
import io.forge.jam.core.types.extrinsic.{AssuranceExtrinsic, Preimage, GuaranteeExtrinsic, Dispute}
import io.forge.jam.core.types.tickets.TicketEnvelope
import _root_.scodec.{Codec, Attempt, DecodeResult}
import _root_.scodec.bits.BitVector
import scala.io.Source
import java.nio.file.{Files, Paths}

class CodecTestVectorsSpec extends AnyFlatSpec with Matchers:

  private val basePath = "jamtestvectors/codec"

  // Test case with scodec Codec that can depend on config
  private case class TestCase[A](
    name: String,
    jsonDecoder: Decoder[A],
    codecFactory: ChainConfig => Codec[A]
  )

  // List-based extrinsic test cases with version byte prefix
  private case class ListTestCase[A](
    name: String,
    versionByte: Byte,
    jsonDecoder: Decoder[List[A]],
    codecFactory: ChainConfig => Codec[A]
  )

  // Helper to create config-independent codec factory
  private def static[A](codec: Codec[A]): ChainConfig => Codec[A] = _ => codec

  // Get scodec codecs from the types
  private val testCases: List[TestCase[?]] = List(
    TestCase("header_0", summon[Decoder[Header]], c => Header.headerCodec(c.validatorCount, c.epochLength)),
    TestCase("header_1", summon[Decoder[Header]], c => Header.headerCodec(c.validatorCount, c.epochLength)),
    TestCase("refine_context", summon[Decoder[Context]], static(summon[Codec[Context]])),
    TestCase("work_item", summon[Decoder[WorkItem]], static(summon[Codec[WorkItem]])),
    TestCase("work_result_0", summon[Decoder[WorkResult]], static(summon[Codec[WorkResult]])),
    TestCase("work_result_1", summon[Decoder[WorkResult]], static(summon[Codec[WorkResult]])),
    TestCase("work_package", summon[Decoder[WorkPackage]], static(summon[Codec[WorkPackage]])),
    TestCase("work_report", summon[Decoder[WorkReport]], static(summon[Codec[WorkReport]])),
    TestCase("extrinsic", summon[Decoder[Extrinsic]], c => Extrinsic.extrinsicCodec(c)),
    TestCase("block", summon[Decoder[Block]], c => Block.blockCodec(c)),
    TestCase("disputes_extrinsic", summon[Decoder[Dispute]], c => Dispute.codec(c.votesPerVerdict))
  )

  private val listTestCases: List[ListTestCase[?]] = List(
    ListTestCase("assurances_extrinsic", 0x02, summon[Decoder[List[AssuranceExtrinsic]]], c => AssuranceExtrinsic.codec(c.coresCount)),
    ListTestCase("tickets_extrinsic", 0x03, summon[Decoder[List[TicketEnvelope]]], static(summon[Codec[TicketEnvelope]])),
    ListTestCase("preimages_extrinsic", 0x03, summon[Decoder[List[Preimage]]], static(summon[Codec[Preimage]])),
    ListTestCase("guarantees_extrinsic", 0x01, summon[Decoder[List[GuaranteeExtrinsic]]], static(summon[Codec[GuaranteeExtrinsic]]))
  )

  private def loadJson(path: String): String =
    val source = Source.fromFile(path)
    try source.mkString
    finally source.close()

  private def loadBinary(path: String): JamBytes =
    JamBytes(Files.readAllBytes(Paths.get(path)))

  private def runTestsForConfig(configName: String): Unit =
    val config = if configName == "tiny" then ChainConfig.TINY else ChainConfig.FULL
    val configPath = s"$basePath/$configName"
    testCases.foreach { tc =>
      runTest(configPath, config, tc)
    }
    listTestCases.foreach { tc =>
      runListTest(configPath, config, tc)
    }

  private def runTest[A](configPath: String, config: ChainConfig, tc: TestCase[A]): Unit =
    val jsonFile = s"$configPath/${tc.name}.json"
    val binFile = s"$configPath/${tc.name}.bin"
    val json = loadJson(jsonFile)
    val expectedBinary = loadBinary(binFile)
    val codec = tc.codecFactory(config)

    // Test 1: JSON -> encode -> matches .bin
    decode[A](json)(using tc.jsonDecoder) match
      case Right(parsed) =>
        codec.encode(parsed) match
          case Attempt.Successful(bits) =>
            val encoded = JamBytes.fromByteVector(bits.bytes)
            withClue(s"Encoding mismatch for ${tc.name}:\nExpected ${expectedBinary.length} bytes, got ${encoded.length} bytes\n") {
              encoded shouldBe expectedBinary
            }

            // Test 2: .bin -> decode -> should equal JSON-decoded value
            codec.decode(BitVector(expectedBinary.toByteVector)) match
              case Attempt.Successful(DecodeResult(decoded, remainder)) =>
                withClue(s"Decode didn't consume all bytes for ${tc.name}:\n") {
                  remainder.isEmpty shouldBe true
                }
                withClue(s"JSON-decoded != binary-decoded for ${tc.name}:\n") {
                  decoded shouldBe parsed
                }
              case Attempt.Failure(err) =>
                fail(s"Failed to decode $binFile: $err")

          case Attempt.Failure(err) =>
            fail(s"Failed to encode for ${tc.name}: $err")

      case Left(error) =>
        fail(s"Failed to parse $jsonFile: $error")

  private def runListTest[A](configPath: String, config: ChainConfig, tc: ListTestCase[A]): Unit =
    val jsonFile = s"$configPath/${tc.name}.json"
    val binFile = s"$configPath/${tc.name}.bin"
    val json = loadJson(jsonFile)
    val expectedBinary = loadBinary(binFile)
    val codec = tc.codecFactory(config)

    decode[List[A]](json)(using tc.jsonDecoder) match
      case Right(items) =>
        // Test 1: JSON -> encode -> matches .bin
        val encodedItems = items.flatMap { item =>
          codec.encode(item) match
            case Attempt.Successful(bits) => Some(JamBytes.fromByteVector(bits.bytes))
            case Attempt.Failure(err) =>
              fail(s"Failed to encode item for ${tc.name}: $err")
              None
        }
        val concatenated = encodedItems.foldLeft(JamBytes.empty)(_ ++ _)
        val encoded = JamBytes(Array(tc.versionByte)) ++ concatenated
        withClue(s"Encoding mismatch for ${tc.name}:\nExpected ${expectedBinary.length} bytes, got ${encoded.length} bytes\n") {
          encoded shouldBe expectedBinary
        }

        // Test 2: .bin -> decode each item -> should equal JSON-decoded items
        var bits = BitVector(expectedBinary.toByteVector).drop(8) // skip version byte
        val decodedItems = items.indices.map { _ =>
          codec.decode(bits) match
            case Attempt.Successful(DecodeResult(item, remainder)) =>
              bits = remainder
              item
            case Attempt.Failure(err) =>
              fail(s"Failed to decode item for ${tc.name}: $err")
        }.toList
        withClue(s"JSON-decoded != binary-decoded for ${tc.name}:\n") {
          decodedItems shouldBe items
        }

      case Left(error) =>
        fail(s"Failed to parse $jsonFile: $error")

  "Codec TINY" should "encode and decode all test vectors correctly" in {
    runTestsForConfig("tiny")
  }

  "Codec FULL" should "encode and decode all test vectors correctly" in {
    runTestsForConfig("full")
  }
