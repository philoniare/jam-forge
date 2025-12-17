package io.forge.jam.protocol.erasure

import io.circe.{Decoder, HCursor}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.AppendedClues.convertToClueful
import io.forge.jam.core.ChainConfig
import io.forge.jam.crypto.ErasureCoding
import io.forge.jam.protocol.TestFileLoader

/**
 * Tests for Erasure Coding using Reed-Solomon via native JNI bindings and the jamtestvectors.
 */
class ErasureCodingTest extends AnyFunSuite with Matchers:

  val TinyConfig: ChainConfig = ChainConfig.TINY
  val FullConfig: ChainConfig = ChainConfig.FULL

  // Erasure coding piece size (W_E) = 2 * number of original shards
  // For TINY: 2 * 2 = 4
  // For FULL: 2 * 342 = 684
  def basicSizeForConfig(config: ChainConfig): Int =
    if config.validatorCount == 6 then 4
    else if config.validatorCount == 1023 then 684
    else config.coresCount * 2

  // Test vector case class
  case class ErasureCodingTestVector(data: Array[Byte], shards: Array[Array[Byte]])

  object ErasureCodingTestVector:
    given Decoder[ErasureCodingTestVector] = new Decoder[ErasureCodingTestVector]:
      def apply(c: HCursor): Decoder.Result[ErasureCodingTestVector] =
        for
          dataHex <- c.downField("data").as[String]
          shardsHex <- c.downField("shards").as[List[String]]
        yield ErasureCodingTestVector(
          hexToBytes(dataHex),
          shardsHex.map(hexToBytes).toArray
        )

  private def hexToBytes(hex: String): Array[Byte] =
    val cleanHex = hex.stripPrefix("0x")
    cleanHex.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray

  private def bytesToHex(bytes: Array[Byte]): String =
    "0x" + bytes.map(b => f"${b & 0xff}%02x").mkString

  test("Native library should be available") {
    ErasureCoding.isAvailable shouldBe true
  }

  test("Conformance test vectors - tiny config") {
    val folderPath = "erasure/tiny"
    val basicSize = basicSizeForConfig(TinyConfig)
    val validatorCount = TinyConfig.validatorCount

    val testFilesResult = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)
    testFilesResult
      .isRight shouldBe true withClue s"Failed to list tiny test vectors: ${testFilesResult.left.getOrElse("")}"

    val testFiles = testFilesResult.getOrElse(List.empty)
    testFiles should not be empty withClue "Should have test files in tiny folder"

    for filename <- testFiles do
      withClue(s"Test vector $filename: ") {
        val vectorResult = TestFileLoader.loadJsonFromTestVectors[ErasureCodingTestVector](folderPath, filename)
        vectorResult.isRight shouldBe true withClue s"Failed to load: ${vectorResult.left.getOrElse("")}"

        val vector = vectorResult.toOption.get

        // Chunk the data
        val chunkResult = ErasureCoding.chunk(vector.data, basicSize, validatorCount)
        chunkResult.isRight shouldBe true withClue s"Chunk failed: ${chunkResult.left.map(_.getMessage).getOrElse("")}"

        val chunks = chunkResult.toOption.get

        // Verify shard count
        chunks.length shouldBe vector.shards.length withClue "Shard count mismatch"

        // Verify each shard matches expected
        for i <- chunks.indices do
          withClue(s"Shard $i mismatch - got ${bytesToHex(chunks(i))}, expected ${bytesToHex(vector.shards(i))}: ") {
            chunks(i) shouldBe vector.shards(i)
          }
      }
  }

  test("Conformance test vectors - full config") {
    val folderPath = "erasure/full"
    val basicSize = basicSizeForConfig(FullConfig)
    val validatorCount = FullConfig.validatorCount

    val testFilesResult = TestFileLoader.getTestFilenamesFromTestVectors(folderPath)
    testFilesResult
      .isRight shouldBe true withClue s"Failed to list full test vectors: ${testFilesResult.left.getOrElse("")}"

    val testFiles = testFilesResult.getOrElse(List.empty)
    testFiles should not be empty withClue "Should have test files in full folder"

    for filename <- testFiles do
      withClue(s"Test vector $filename: ") {
        val vectorResult = TestFileLoader.loadJsonFromTestVectors[ErasureCodingTestVector](folderPath, filename)
        vectorResult.isRight shouldBe true withClue s"Failed to load: ${vectorResult.left.getOrElse("")}"

        val vector = vectorResult.toOption.get

        // Chunk the data
        val chunkResult = ErasureCoding.chunk(vector.data, basicSize, validatorCount)
        chunkResult.isRight shouldBe true withClue s"Chunk failed: ${chunkResult.left.map(_.getMessage).getOrElse("")}"

        val chunks = chunkResult.toOption.get

        // Verify shard count
        chunks.length shouldBe vector.shards.length withClue "Shard count mismatch"

        // Verify each shard matches expected
        for i <- chunks.indices do
          withClue(s"Shard $i mismatch: ") {
            chunks(i) shouldBe vector.shards(i)
          }
      }
  }

  test("Basic encode/recover roundtrip") {
    val originalCount = 2
    val recoveryCount = 4
    val totalShards = originalCount + recoveryCount

    // Create original shards
    val original = Array(
      Array[Byte](0, 15),
      Array[Byte](30, 45)
    )

    // Encode - returns original + recovery shards
    val encodeResult = ErasureCoding.encode(original, recoveryCount)
    encodeResult.isRight shouldBe true
    val allShards = encodeResult.toOption.get
    allShards.length shouldBe totalShards

    // First originalCount shards should be the original data
    allShards(0) shouldBe original(0)
    allShards(1) shouldBe original(1)

    // Recover using only recovery shards (last recoveryCount shards, need originalCount of them)
    val recoveryShards = (originalCount until originalCount + originalCount).map { i =>
      ErasureCoding.Shard(allShards(i), i)
    }.toArray

    val recoverResult = ErasureCoding.recover(originalCount, recoveryCount, recoveryShards)
    recoverResult.isRight shouldBe true
    val recovered = recoverResult.toOption.get

    // Verify
    recovered.length shouldBe originalCount
    recovered(0) shouldBe original(0)
    recovered(1) shouldBe original(1)
  }

  test("Encode/recover with various configurations") {
    // (dataLength, originalCount, recoveryCount)
    val testCases = Seq(
      (4, 2, 4),
      (16, 2, 4),
      (32, 2, 4),
      (64, 4, 6),
      (100, 5, 10)
    )

    for (dataLength, originalCount, recoveryCount) <- testCases do
      val totalShards = originalCount + recoveryCount
      withClue(s"Config ($dataLength, $originalCount, $recoveryCount): ") {
        val shardSize = (dataLength + originalCount - 1) / originalCount
        val adjustedShardSize = if shardSize % 2 == 0 then shardSize else shardSize + 1

        val originalData = (0 until dataLength).map(i => ((i * 15) % 256).toByte).toArray

        val original = ErasureCoding.split(originalData, adjustedShardSize)
          .take(originalCount)
          .map { shard =>
            if shard.length < adjustedShardSize then
              shard ++ Array.fill(adjustedShardSize - shard.length)(0.toByte)
            else
              shard
          }

        val encodeResult = ErasureCoding.encode(original, recoveryCount)
        encodeResult.isRight shouldBe true
        val allShards = encodeResult.toOption.get
        allShards.length shouldBe totalShards

        // Use recovery shards (indices originalCount to totalShards)
        val shards = (originalCount until originalCount + originalCount).map { i =>
          ErasureCoding.Shard(allShards(i), i)
        }.toArray

        val recoverResult = ErasureCoding.recover(originalCount, recoveryCount, shards)
        recoverResult.isRight shouldBe true
        val recovered = recoverResult.toOption.get

        for i <- 0 until originalCount do
          recovered(i) shouldBe original(i)
      }
  }

  test("Should fail with insufficient shards") {
    val originalCount = 5
    val recoveryCount = 8

    val original = (0 until originalCount).map(i => Array[Byte]((i * 10).toByte, (i * 10 + 1).toByte)).toArray

    val encodeResult = ErasureCoding.encode(original, recoveryCount)
    encodeResult.isRight shouldBe true
    val allShards = encodeResult.toOption.get

    // Only provide 2 shards when we need 5
    val insufficientShards = Array(
      ErasureCoding.Shard(allShards(0), 0),
      ErasureCoding.Shard(allShards(1), 1)
    )

    val recoverResult = ErasureCoding.recover(originalCount, recoveryCount, insufficientShards)
    recoverResult.isLeft shouldBe true
    recoverResult.left.toOption.get shouldBe ErasureCoding.InvalidShardsCount
  }

  test("Split and join roundtrip") {
    val testData = "hello world, this is a test".getBytes("UTF-8")
    val n = 4

    val split = ErasureCoding.split(testData, n)
    val joined = ErasureCoding.join(split)

    val paddedLength = ((testData.length + n - 1) / n) * n
    joined.length shouldBe paddedLength
    joined.take(testData.length) shouldBe testData
  }

  test("Transpose roundtrip") {
    val testData = Array(
      Array(1, 2, 3, 4),
      Array(5, 6, 7, 8),
      Array(9, 10, 11, 12),
      Array(13, 14, 15, 16)
    )

    val transposed = ErasureCoding.transpose(testData)
    val transposedBack = ErasureCoding.transpose(transposed)

    transposedBack shouldBe testData
  }

  test("Chunk and reconstruct roundtrip (tiny config)") {
    val basicSize = basicSizeForConfig(TinyConfig)
    val validatorCount = TinyConfig.validatorCount
    val originalCount = basicSize / 2
    val dataLength = 4104

    val originalData = (0 until dataLength).map(i => ((i * 15) % 256).toByte).toArray

    val chunkResult = ErasureCoding.chunk(originalData, basicSize, validatorCount)
    chunkResult.isRight shouldBe true
    val chunks = chunkResult.toOption.get
    chunks.length shouldBe validatorCount

    // Use some recovery shards (indices originalCount onwards)
    val shards =
      (originalCount until originalCount + originalCount).map(i => ErasureCoding.Shard(chunks(i), i)).toArray

    val reconstructResult = ErasureCoding.reconstruct(shards, basicSize, validatorCount)
    reconstructResult.isRight shouldBe true
    val reconstructed = reconstructResult.toOption.get

    reconstructed shouldBe originalData
  }

  test("Invalid basic size should fail") {
    val data = Array[Byte](1, 2, 3, 4)

    val result = ErasureCoding.chunk(data, 3, 5)
    result.isLeft shouldBe true
    result.left.toOption.get shouldBe a[ErasureCoding.InvalidBasicSize]
  }
