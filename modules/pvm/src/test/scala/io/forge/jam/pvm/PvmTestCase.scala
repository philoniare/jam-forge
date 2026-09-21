package io.forge.jam.pvm

import io.circe.*

/**
 * Status of PVM execution
 */
enum PvmStatus:
  case Panic
  case Halt
  case PageFault
  case Ecalli
  case OutOfGas

object PvmStatus:
  given Decoder[PvmStatus] = Decoder.decodeString.emap {
    case "panic" => Right(PvmStatus.Panic)
    case "halt" => Right(PvmStatus.Halt)
    case "page-fault" => Right(PvmStatus.PageFault)
    case "ecalli" => Right(PvmStatus.Ecalli)
    case "out-of-gas" => Right(PvmStatus.OutOfGas)
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

enum PvmStep:
  case Run
  case Map(address: Long, length: Long, isWritable: Boolean)
  case Write(address: Long, contents: Array[Byte])
  case SetReg(reg: Int, value: Long)
  case Assert(
    status: Option[PvmStatus],
    pageFaultAddress: Option[Long],
    hostcall: Option[Long],
    gas: Option[Long],
    pc: Option[Int],
    regs: List[Option[Long]],
    memory: Option[List[MemoryEntry]]
  )

/**
 * PVM test case loaded from JSON.
 */
case class PvmTestCase(
  name: String,
  initialPc: Int,
  initialGas: Long,
  program: Array[Byte],
  steps: List[PvmStep],
  blockGasCosts: List[(Int, Long)]
):

  private def prologue: List[PvmStep] =
    val i = steps.indexWhere(_ == PvmStep.Run)
    if i < 0 then steps else steps.take(i)

  private def lastAssert: Option[PvmStep.Assert] =
    steps.reverse.collectFirst { case a: PvmStep.Assert => a }

  def initialRegs: Array[Long] =
    val regs = new Array[Long](13)
    prologue.foreach {
      case PvmStep.SetReg(r, v) if r < 13 => regs(r) = v
      case _ => ()
    }
    regs

  def initialPageMap: List[PageMapEntry] =
    prologue.collect { case PvmStep.Map(a, l, w) => PageMapEntry(a, l, w) }

  def initialMemory: List[MemoryEntry] =
    prologue.collect { case PvmStep.Write(a, c) => MemoryEntry(a, c) }

  def expectedStatus: PvmStatus = lastAssert.flatMap(_.status).getOrElse(PvmStatus.Panic)
  def expectedPc: Int = lastAssert.flatMap(_.pc).getOrElse(initialPc)
  def expectedGas: Long = lastAssert.flatMap(_.gas).getOrElse(initialGas)
  def expectedMemory: List[MemoryEntry] = lastAssert.flatMap(_.memory).getOrElse(Nil)
  def expectedPageFaultAddress: Option[Long] = lastAssert.flatMap(_.pageFaultAddress)
  def expectedRegs: Array[Long] =
    val regs = initialRegs
    lastAssert.foreach(_.regs.zipWithIndex.foreach { (v, i) =>
      if i < 13 then v.foreach(regs(i) = _)
    })
    regs

object PvmTestCase:
  /** Register values are serialized as unsigned 64-bit; wrap into a Long. */
  private given unsignedLongDecoder: Decoder[Long] = Decoder.instance { c =>
    c.focus match
      case Some(json) if json.isNumber =>
        json.asNumber match
          case Some(num) =>
            num.toLong match
              case Some(l) => Right(l)
              case None =>
                num.toBigInt match
                  case Some(bi) if bi > Long.MaxValue => Right((bi - BigInt(2).pow(64)).toLong)
                  case Some(bi) => Right(bi.toLong)
                  case None => Left(DecodingFailure(s"Cannot decode $num as Long", c.history))
          case None => Left(DecodingFailure("Expected number", c.history))
      case _ => Left(DecodingFailure("Expected number", c.history))
  }

  private given Decoder[PvmStep] = Decoder.instance { c =>
    c.keys.map(_.toList) match
      case Some("run" :: Nil) => Right(PvmStep.Run)
      case Some("map" :: Nil) =>
        val m = c.downField("map")
        for
          address <- m.downField("address").as[Long]
          length <- m.downField("length").as[Long]
          isWritable <- m.downField("is-writable").as[Boolean]
        yield PvmStep.Map(address, length, isWritable)
      case Some("write" :: Nil) =>
        val w = c.downField("write")
        for
          address <- w.downField("address").as[Long]
          contents <- w.downField("contents").as[List[Int]].map(_.map(_.toByte).toArray)
        yield PvmStep.Write(address, contents)
      case Some("set-reg" :: Nil) =>
        val s = c.downField("set-reg")
        for
          reg <- s.downField("reg").as[Int]
          value <- s.downField("value").as[Long](unsignedLongDecoder)
        yield PvmStep.SetReg(reg, value)
      case Some("assert" :: Nil) =>
        val a = c.downField("assert")
        for
          status <- a.downField("status").as[Option[PvmStatus]]
          pageFaultAddress <- a.downField("page-fault-address").as[Option[Long]](Decoder.decodeOption(unsignedLongDecoder))
          hostcall <- a.downField("hostcall").as[Option[Long]](Decoder.decodeOption(unsignedLongDecoder))
          gas <- a.downField("gas").as[Option[Long]](Decoder.decodeOption(unsignedLongDecoder))
          pc <- a.downField("pc").as[Option[Int]]
          regs <- a.downField("regs").as[Option[List[Option[Long]]]](
            Decoder.decodeOption(Decoder.decodeList(Decoder.decodeOption(unsignedLongDecoder)))
          )
          memory <- a.downField("memory").as[Option[List[MemoryEntry]]]
        yield PvmStep.Assert(status, pageFaultAddress, hostcall, gas, pc, regs.getOrElse(Nil), memory)
      case other => Left(DecodingFailure(s"Unrecognized step: $other", c.history))
  }

  given Decoder[PvmTestCase] = Decoder.instance { c =>
    for
      name <- c.downField("name").as[String]
      initialPc <- c.downField("initial-pc").as[Int]
      initialGas <- c.downField("initial-gas").as[Long](unsignedLongDecoder)
      program <- c.downField("program").as[List[Int]].map(_.map(_.toByte).toArray)
      steps <- c.downField("steps").as[List[PvmStep]]
      blockGasCosts <- c.downField("block-gas-costs").as[Option[List[Json]]].map(_.getOrElse(Nil)).flatMap { entries =>
        entries.foldLeft[Decoder.Result[List[(Int, Long)]]](Right(Nil)) { (acc, json) =>
          for
            list <- acc
            cur = json.hcursor
            pc <- cur.downField("pc").as[Int]
            cost <- cur.downField("cost").as[Long]
          yield list :+ (pc, cost)
        }
      }
    yield PvmTestCase(name, initialPc, initialGas, program, steps, blockGasCosts)
  }
