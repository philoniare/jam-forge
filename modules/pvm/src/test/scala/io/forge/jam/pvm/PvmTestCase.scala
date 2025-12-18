package io.forge.jam.pvm

import io.circe.*

/**
 * Status of PVM execution.
 */
enum PvmStatus:
  case Panic
  case Halt
  case PageFault

object PvmStatus:
  given Decoder[PvmStatus] = Decoder.decodeString.emap {
    case "panic" => Right(PvmStatus.Panic)
    case "halt" => Right(PvmStatus.Halt)
    case "page-fault" => Right(PvmStatus.PageFault)
    case other => Left(s"Invalid status: $other")
  }

/**
 * Page map entry describing a memory region.
 */
case class PageMapEntry(
  address: Long,
  length: Long,
  isWritable: Boolean
)

object PageMapEntry:
  given Decoder[PageMapEntry] = Decoder.instance { c =>
    for
      address <- c.downField("address").as[Long]
      length <- c.downField("length").as[Long]
      isWritable <- c.downField("is-writable").as[Boolean]
    yield PageMapEntry(address, length, isWritable)
  }

/**
 * Memory contents at a specific address.
 */
case class MemoryEntry(
  address: Long,
  contents: Array[Byte]
)

object MemoryEntry:
  given Decoder[MemoryEntry] = Decoder.instance { c =>
    for
      address <- c.downField("address").as[Long]
      contents <- c.downField("contents").as[List[Int]].map(_.map(_.toByte).toArray)
    yield MemoryEntry(address, contents)
  }

/**
 * PVM test case loaded from JSON.
 */
case class PvmTestCase(
  name: String,
  initialRegs: Array[Long],
  initialPc: Int,
  initialPageMap: List[PageMapEntry],
  initialMemory: List[MemoryEntry],
  initialGas: Long,
  program: Array[Byte],
  expectedStatus: PvmStatus,
  expectedRegs: Array[Long],
  expectedPc: Int,
  expectedMemory: List[MemoryEntry],
  expectedGas: Long,
  expectedPageFaultAddress: Option[Long]
)

object PvmTestCase:
  private given unsignedLongDecoder: Decoder[Long] = Decoder.instance { c =>
    c.focus match
      case Some(json) if json.isNumber =>
        json.asNumber match
          case Some(num) =>
            // Try to get as Long directly first
            num.toLong match
              case Some(l) => Right(l)
              case None =>
                // For values > Long.MAX_VALUE, use BigInt
                num.toBigInt match
                  case Some(bi) if bi > Long.MaxValue =>
                    // Interpret as unsigned - wrap around
                    Right((bi - BigInt(2).pow(64)).toLong)
                  case Some(bi) =>
                    Right(bi.toLong)
                  case None =>
                    Left(DecodingFailure(s"Cannot decode $num as Long", c.history))
          case None =>
            Left(DecodingFailure("Expected number", c.history))
      case _ =>
        Left(DecodingFailure("Expected number", c.history))
  }

  given Decoder[PvmTestCase] = Decoder.instance { c =>
    for
      name <- c.downField("name").as[String]
      initialRegs <- c.downField("initial-regs").as[List[Long]](Decoder.decodeList(unsignedLongDecoder))
      initialPc <- c.downField("initial-pc").as[Int]
      initialPageMap <- c.downField("initial-page-map").as[List[PageMapEntry]]
      initialMemory <- c.downField("initial-memory").as[List[MemoryEntry]]
      initialGas <- c.downField("initial-gas").as[Long](unsignedLongDecoder)
      program <- c.downField("program").as[List[Int]].map(_.map(_.toByte).toArray)
      expectedStatus <- c.downField("expected-status").as[PvmStatus]
      expectedRegs <- c.downField("expected-regs").as[List[Long]](Decoder.decodeList(unsignedLongDecoder))
      expectedPc <- c.downField("expected-pc").as[Int]
      expectedMemory <- c.downField("expected-memory").as[List[MemoryEntry]]
      expectedGas <- c.downField("expected-gas").as[Long](unsignedLongDecoder)
      expectedPageFaultAddress <- c.downField("expected-page-fault-address").as[Option[Long]](Decoder.decodeOption(unsignedLongDecoder))
    yield PvmTestCase(
      name,
      initialRegs.toArray,
      initialPc,
      initialPageMap,
      initialMemory,
      initialGas,
      program,
      expectedStatus,
      expectedRegs.toArray,
      expectedPc,
      expectedMemory,
      expectedGas,
      expectedPageFaultAddress
    )
  }
