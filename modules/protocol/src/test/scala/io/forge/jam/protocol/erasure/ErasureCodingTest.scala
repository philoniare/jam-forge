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

  // Erasure coding piece size = 2 * ecOriginalShards(validatorCount)
  // For TINY (6 validators): 2 * 3 = 6.  For FULL (1023): 2 * 342 = 684.
  def basicSizeForConfig(config: ChainConfig): Int =
    ChainConfig.ecPieceSizeFor(config.validatorCount)

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

  test("Chunk and reconstruct roundtrip (full config)") {
    val basicSize = basicSizeForConfig(FullConfig) // 684
    val validatorCount = FullConfig.validatorCount // 1023
    val originalCount = basicSize / 2 // 342
    val dataLength = 4104 // 342 * 12, divisible across the original shards

    val originalData = (0 until dataLength).map(i => ((i * 15) % 256).toByte).toArray

    val chunkResult = ErasureCoding.chunk(originalData, basicSize, validatorCount)
    chunkResult.isRight shouldBe true
    val chunks = chunkResult.toOption.get
    chunks.length shouldBe validatorCount

    // Reconstruct from a recovery-only shard set (indices originalCount onward).
    val shards =
      (originalCount until originalCount + originalCount).map(i => ErasureCoding.Shard(chunks(i), i)).toArray
    val reconstructResult = ErasureCoding.reconstruct(shards, basicSize, validatorCount)
    reconstructResult.isRight shouldBe true
    reconstructResult.toOption.get shouldBe originalData
  }

  test("reconstruct with no shards fails (InvalidShardsCount)") {
    val result = ErasureCoding.reconstruct(Array.empty[ErasureCoding.Shard], 4, 6)
    result.isLeft shouldBe true
    result.left.toOption.get shouldBe ErasureCoding.InvalidShardsCount
  }

  test("reconstruct with ragged shards fails (RaggedShards)") {
    // >= originalCount(2) shards, but with mismatched lengths.
    val shards = Array(
      ErasureCoding.Shard(Array[Byte](1, 2, 3, 4), 0),
      ErasureCoding.Shard(Array[Byte](1, 2), 1)
    )
    val result = ErasureCoding.reconstruct(shards, 4, 6)
    result.isLeft shouldBe true
    result.left.toOption.get shouldBe ErasureCoding.RaggedShards
  }

  test("chunk with too-small validator count fails (InvalidRecoveryCount)") {
    val result = ErasureCoding.chunk(Array[Byte](1, 2, 3, 4), 6, 2) // 3 originals > 2 validators
    result.isLeft shouldBe true
    result.left.toOption.get shouldBe a[ErasureCoding.InvalidRecoveryCount]
  }

  // chunk and reconstruct code every 2-byte position in one native call; these check them against coding each
  // position on its own, which is how GP defines C_k and R_k: position p of every original shard is a one-word piece
  // of basicSize bytes, coded with k = 1.
  private def chunkPerPosition(data: Array[Byte], basicSize: Int, validatorCount: Int): Array[Array[Byte]] =
    val k = Math.max(1, (data.length + basicSize - 1) / basicSize)
    val padded = data.padTo(k * basicSize, 0.toByte)
    val perPosition = (0 until k).map { p =>
      val piece = Array.tabulate[Byte](basicSize)(b => padded((b / 2) * 2 * k + 2 * p + b % 2))
      ErasureCoding.chunk(piece, basicSize, validatorCount).toOption.get
    }
    Array.tabulate(validatorCount)(v => perPosition.flatMap(_(v)).toArray)

  private def reconstructPerPosition(shards: Array[ErasureCoding.Shard], basicSize: Int, validatorCount: Int) =
    val originalCount = basicSize / 2
    val words = (shards(0).data.length + 1) / 2
    val perPosition = (0 until words).map { p =>
      val atP = shards.map(s => ErasureCoding.Shard(s.data.slice(2 * p, 2 * p + 2), s.index))
      ErasureCoding.reconstruct(atP, basicSize, validatorCount).toOption.get
    }
    Array.tabulate[Byte](originalCount * 2 * words) { b =>
      val i = b / (2 * words)
      val p = (b % (2 * words)) / 2
      perPosition(p)(2 * i + b % 2)
    }

  test("chunk and reconstruct match per-position coding (full config)") {
    val basicSize = basicSizeForConfig(FullConfig)
    val validatorCount = FullConfig.validatorCount
    val originalCount = basicSize / 2
    val rng = new scala.util.Random(3)
    // Lengths on both sides of whole 32-word blocks, with and without padding.
    for len <- Seq(1, 683, 684, 685, 684 * 31, 684 * 33 - 5, 684 * 64) do
      withClue(s"length $len: ") {
        val data = rng.nextBytes(len)
        val chunks = ErasureCoding.chunk(data, basicSize, validatorCount).toOption.get
        chunks shouldBe chunkPerPosition(data, basicSize, validatorCount)

        val recoveryOnly = (originalCount until validatorCount).map(i => ErasureCoding.Shard(chunks(i), i)).toArray
        val mixed = (0 until validatorCount by 2).map(i => ErasureCoding.Shard(chunks(i), i)).toArray
        for shards <- Seq(recoveryOnly, mixed) do
          val rebuilt = ErasureCoding.reconstruct(shards, basicSize, validatorCount).toOption.get
          rebuilt shouldBe reconstructPerPosition(shards, basicSize, validatorCount)
          rebuilt.take(len) shouldBe data
      }
  }

  test("reconstruct pads odd-length shards like per-position coding (tiny config)") {
    val basicSize = basicSizeForConfig(TinyConfig)
    val validatorCount = TinyConfig.validatorCount
    val chunks = ErasureCoding.chunk(Array.tabulate[Byte](40)(_.toByte), basicSize, validatorCount).toOption.get
    val odd = (3 until 6).map(i => ErasureCoding.Shard(chunks(i).dropRight(1), i)).toArray
    ErasureCoding.reconstruct(odd, basicSize, validatorCount).toOption.get shouldBe
      reconstructPerPosition(odd, basicSize, validatorCount)
  }

  test("reconstruct with duplicate shard indices fails (RecoverFailed)") {
    val chunks = ErasureCoding.chunk(Array.tabulate[Byte](12)(_.toByte), 6, 6).toOption.get
    val shards = Array(3, 3, 4).map(i => ErasureCoding.Shard(chunks(i), i))
    ErasureCoding.reconstruct(shards, 6, 6) shouldBe Left(ErasureCoding.RecoverFailed)
  }

  test("Conformance test vectors - reconstruct from recovery shards") {
    for (folder, config) <- Seq("erasure/tiny" -> TinyConfig, "erasure/full" -> FullConfig) do
      val basicSize = basicSizeForConfig(config)
      val originalCount = basicSize / 2
      for filename <- TestFileLoader.getTestFilenamesFromTestVectors(folder).toOption.get do
        withClue(s"$folder/$filename: ") {
          val vector = TestFileLoader.loadJsonFromTestVectors[ErasureCodingTestVector](folder, filename).toOption.get
          val shards = vector.shards.zipWithIndex.drop(originalCount).map((d, i) => ErasureCoding.Shard(d, i))
          val rebuilt = ErasureCoding.reconstruct(shards, basicSize, config.validatorCount).toOption.get
          rebuilt.take(vector.data.length) shouldBe vector.data
          rebuilt.drop(vector.data.length).forall(_ == 0) shouldBe true
        }
  }

  test("chunkSegments matches chunk on each segment, and reconstructSegments gives every segment back") {
    for (config, lengths) <- Seq(FullConfig -> Seq(4104, 4000, 1, 684 * 7), TinyConfig -> Seq(4104, 13)) do
      val basicSize = basicSizeForConfig(config)
      val validatorCount = config.validatorCount
      val originalCount = basicSize / 2
      val rng = new scala.util.Random(17)
      for len <- lengths; count <- Seq(1, 5, 48) do
        withClue(s"validators $validatorCount, $count segments of $len bytes: ") {
          val segments = IndexedSeq.fill(count)(rng.nextBytes(len))
          val shards = ErasureCoding.chunkSegments(segments, basicSize, validatorCount).toOption.get
          val alone = segments.map(ErasureCoding.chunk(_, basicSize, validatorCount).toOption.get)
          for v <- 0 until validatorCount do
            shards.column(v).map(_.toSeq) shouldBe alone.map(_(v).toSeq)

          for picked <- Seq(originalCount until validatorCount, 0 until validatorCount by 2) do
            val held = picked.take(originalCount).map(v => ErasureCoding.Shard(shards.joinedColumn(v), v)).toArray
            val rebuilt = ErasureCoding.reconstructSegments(held, count, basicSize, validatorCount).toOption.get
            rebuilt.map(_.take(len).toSeq) shouldBe segments.map(_.toSeq)
            rebuilt.map(_.toSeq) shouldBe segments.indices.map { j =>
              ErasureCoding.reconstruct(
                picked.take(originalCount).map(v => ErasureCoding.Shard(alone(j)(v), v)).toArray,
                basicSize,
                validatorCount
              ).toOption.get.toSeq
            }
        }
  }

  test("chunkSegments of segments of different lengths matches chunk on each") {
    val basicSize = basicSizeForConfig(TinyConfig)
    val segments = IndexedSeq(Array.tabulate[Byte](40)(_.toByte), Array.tabulate[Byte](7)(_.toByte))
    val shards = ErasureCoding.chunkSegments(segments, basicSize, TinyConfig.validatorCount).toOption.get
    for v <- 0 until TinyConfig.validatorCount; j <- segments.indices do
      shards.chunk(v, j) shouldBe ErasureCoding.chunk(segments(j), basicSize, TinyConfig.validatorCount).toOption.get(v)
  }

  test("Conformance test vectors - chunkSegments gives each vector's shards") {
    for (folder, config) <- Seq("erasure/tiny" -> TinyConfig, "erasure/full" -> FullConfig) do
      val basicSize = basicSizeForConfig(config)
      for filename <- TestFileLoader.getTestFilenamesFromTestVectors(folder).toOption.get do
        withClue(s"$folder/$filename: ") {
          val vector = TestFileLoader.loadJsonFromTestVectors[ErasureCodingTestVector](folder, filename).toOption.get
          // The vector's data as three segments of one length: each must code to the vector's shards.
          val shards =
            ErasureCoding.chunkSegments(IndexedSeq.fill(3)(vector.data), basicSize, config.validatorCount).toOption.get
          for v <- vector.shards.indices; j <- 0 until 3 do
            shards.chunk(v, j) shouldBe vector.shards(v)
        }
  }
