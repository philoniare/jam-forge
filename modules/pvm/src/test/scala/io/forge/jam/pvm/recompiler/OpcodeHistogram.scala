package io.forge.jam.pvm.recompiler

import io.forge.jam.pvm.program.{ProgramBlob, InstructionDecoder}
import io.forge.jam.pvm.Opcode
import java.io.File
import scala.io.Source
import io.circe.parser.decode
import io.forge.jam.pvm.PvmTestCase

object OpcodeHistogram:
  def main(args: Array[String]): Unit =
    val base = Option(System.getProperty("jam.base.dir")).getOrElse(".")
    val dir = new File(s"$base/modules/pvm/src/test/resources/pvm")
    val files = Option(dir.listFiles((_, n) => n.endsWith(".json"))).getOrElse(Array.empty[File]).sorted
    val counts = scala.collection.mutable.Map.empty[Int, Int]
    var vectors = 0
    var undecodable = 0
    for f <- files do
      val content = Source.fromFile(f).mkString
      decode[PvmTestCase](content) match
        case Left(_) => undecodable += 1
        case Right(tc) =>
          vectors += 1
          ProgramBlob.fromCodeAndJumpTable(tc.program) match
            case None => undecodable += 1
            case Some(blob) =>
              var off = 0
              while off < blob.code.length do
                val (instr, skip) = InstructionDecoder.decode(blob.code, blob.bitmask, off)
                counts(instr.opcode.value) = counts.getOrElse(instr.opcode.value, 0) + 1
                off += (if skip <= 0 then 1 else skip)
    val byName = Opcode.values.map(o => o.value -> o.toString).toMap
    println(s"vectors=$vectors undecodable=$undecodable distinct-opcodes=${counts.size}")
    counts.toSeq.sortBy(-_._2).foreach { case (op, c) =>
      println(f"$c%8d  ${byName.getOrElse(op, s"op$op")}%-32s ($op)")
    }
