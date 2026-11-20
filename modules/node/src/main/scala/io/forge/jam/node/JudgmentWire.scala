package io.forge.jam.node

import java.nio.{ByteBuffer, ByteOrder}

object JudgmentWire:

  val JudgmentLength: Int = 4 + 2 + 1 + 32 + 64

  private val HeadLength = 4 + 2 + 1

  private val SignatureLength = 64
  private val EntryLength = 2 + SignatureLength
  private val Signers = 2 to 3

  def valid(judgment: Array[Byte]): Boolean = judgment(6) == 1

  def encodeGuarantee(slot: Long, signed: List[(Int, Array[Byte])]): Option[Array[Byte]] =
    Option.when(Signers.contains(signed.size) && signed.forall(_._2.length == SignatureLength)) {
      val out = ByteBuffer.allocate(5 + EntryLength * signed.size).order(ByteOrder.LITTLE_ENDIAN)
      out.putInt(slot.toInt).put(signed.size.toByte)
      signed.foreach((validator, signature) => out.putShort(validator.toShort).put(signature))
      out.array()
    }

  def decodeGuarantee(message: Array[Byte]): Option[(Long, List[(Int, Array[Byte])])] =
    Option.when(message.length >= 5 && Signers.contains(message(4).toInt) && message.length == 5 + EntryLength * message(4)) {
      val in = ByteBuffer.wrap(message).order(ByteOrder.LITTLE_ENDIAN)
      val slot = in.getInt & 0xffffffffL
      val signed = List.fill(in.get().toInt) {
        val validator = in.getShort & 0xffff
        val signature = new Array[Byte](SignatureLength)
        in.get(signature)
        validator -> signature
      }
      (slot, signed)
    }

  def encodeInline(judgment: Array[Byte], guarantee: Option[Array[Byte]]): Option[Array[Byte]] =
    if judgment.length != JudgmentLength then None
    else if valid(judgment) then Some(judgment)
    else
      guarantee
        .filter(decodeGuarantee(_).isDefined)
        .map(g => judgment.take(HeadLength) ++ g ++ judgment.drop(HeadLength))

  def decodeBatch(message: Array[Byte]): Option[List[(Array[Byte], Option[Array[Byte]])]] =
    val judgments = List.newBuilder[(Array[Byte], Option[Array[Byte]])]
    var at = 0
    var wellFormed = message.nonEmpty
    while wellFormed && at < message.length do
      val guaranteeAt = at + HeadLength
      val countAt = guaranteeAt + 4
      val guaranteeLength =
        if guaranteeAt > message.length then -1
        else if message(guaranteeAt - 1) == 1 then 0
        else if message(guaranteeAt - 1) == 0 && countAt < message.length && Signers.contains(message(countAt).toInt) then
          5 + EntryLength * message(countAt)
        else -1
      val end = at + JudgmentLength + guaranteeLength
      if guaranteeLength < 0 || end > message.length then wellFormed = false
      else
        val rest = guaranteeAt + guaranteeLength
        judgments += (message.slice(at, guaranteeAt) ++ message.slice(rest, end)) ->
          Option.when(guaranteeLength > 0)(message.slice(guaranteeAt, rest))
        at = end
    Option.when(wellFormed)(judgments.result())

  enum Part:
    case Judgment, Guarantee, Batch, Malformed

  final class Stream:
    private var guaranteeDue = false

    def next(message: Array[Byte]): Part =
      val due = guaranteeDue
      guaranteeDue = message.length == JudgmentLength && !valid(message)
      if message.length == JudgmentLength then Part.Judgment
      else if due && decodeGuarantee(message).isDefined then Part.Guarantee
      else if decodeBatch(message).isDefined then Part.Batch
      else Part.Malformed
