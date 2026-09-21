package io.forge.jam.core.scodec

import scodec.*
import scodec.bits.*
import scodec.codecs.*
import io.forge.jam.core.primitives.*
import io.forge.jam.core.types.tickets.TicketMark
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.service.ServiceInfo
import io.forge.jam.core.scodec.CodecDecodingException

/**
 * scodec codecs for FullJamState encoding/decoding.
 *
 * Provides bidirectional codecs for JAM state components with binary compatibility.
 */
object FullJamStateCodecs:

  val BandersnatchRingCommitmentSize: Int = 144
  val ValidatorKeySize: Int = 336
  val MetadataSize: Int = 128
  val TicketMarkSize: Int = Hash.Size + 1

  /** Codec for timeslot (tau) - 4 bytes little-endian uint32. */
  val timeslotCodec: Codec[Long] = uint32L.xmap(_.toLong, _ & 0xFFFFFFFFL)

  /** Codec for entropy pool - exactly 4 hashes (128 bytes total). */
  val entropyPoolCodec: Codec[List[Hash]] =
    JamCodecs.fixedSizeList(JamCodecs.hashCodec, 4)

  /**
   * Codec for `ValidatorsData`.
   */
  def validatorListCodec(count: Int): Codec[List[ValidatorKey]] =
    JamCodecs.compactPrefixedList(summon[Codec[ValidatorKey]])

  /** Codec for authorization pools: per-core compact length prefix + N hashes. */
  def authPoolsCodec(coresCount: Int): Codec[List[List[Hash]]] =
    val poolCodec: Codec[List[Hash]] = JamCodecs.compactPrefixedList(JamCodecs.hashCodec)
    JamCodecs.fixedSizeList(poolCodec, coresCount)

  /** Codec for authorization queues: fixed-size coresCount * queueSize * 32 bytes. */
  def authQueuesCodec(coresCount: Int, queueSize: Int): Codec[List[List[Hash]]] =
    val queueCodec: Codec[List[Hash]] = JamCodecs.fixedSizeList(JamCodecs.hashCodec, queueSize)
    JamCodecs.fixedSizeList(queueCodec, coresCount)

  /** Codec for accumulation history: per-slot compact count + N x 32-byte hashes. */
  def accumulationHistoryCodec(epochLength: Int): Codec[List[List[ByteVector]]] =
    val byteVectorCodec: Codec[ByteVector] = fixedSizeBytes(Hash.Size.toLong, bytes)
    val slotCodec: Codec[List[ByteVector]] = JamCodecs.compactPrefixedList(byteVectorCodec)
    JamCodecs.fixedSizeList(slotCodec, epochLength)

  sealed trait TicketsOrKeysData

  object TicketsOrKeysData:
    final case class Tickets(tickets: List[TicketMark]) extends TicketsOrKeysData
    final case class Keys(keys: List[BandersnatchPublicKey]) extends TicketsOrKeysData

  private def ticketsOrKeysCodec(epochLength: Int): Codec[TicketsOrKeysData] =
    val ticketsListCodec: Codec[List[TicketMark]] = JamCodecs.fixedSizeList(JamCodecs.ticketMarkCodec, epochLength)
    val keysListCodec: Codec[List[BandersnatchPublicKey]] =
      JamCodecs.fixedSizeList(JamCodecs.bandersnatchPublicKeyCodec, epochLength)

    discriminated[TicketsOrKeysData]
      .by(byte)
      .subcaseP(0) { case t: TicketsOrKeysData.Tickets => t }(
        ticketsListCodec.xmap(TicketsOrKeysData.Tickets.apply, _.tickets)
      )
      .subcaseP(1) { case k: TicketsOrKeysData.Keys => k }(
        keysListCodec.xmap(TicketsOrKeysData.Keys.apply, _.keys)
      )

  /** Codec for safrole gamma state (gammaK + gammaZ + gammaS + gammaA). */
  def safroleGammaStateCodec(
    validatorCount: Int,
    epochLength: Int
  ): Codec[(List[ValidatorKey], ByteVector, TicketsOrKeysData, List[TicketMark])] =
    val gammaKCodec = validatorListCodec(validatorCount)
    val gammaZCodec: Codec[ByteVector] = fixedSizeBytes(BandersnatchRingCommitmentSize.toLong, bytes)
    val gammaSCodec = ticketsOrKeysCodec(epochLength)
    val gammaACodec: Codec[List[TicketMark]] = JamCodecs.compactPrefixedList(JamCodecs.ticketMarkCodec)

    (gammaKCodec :: gammaZCodec :: gammaSCodec :: gammaACodec).xmap(
      { case (k, z, s, a) => (k, z, s, a) },
      t => (t._1, t._2, t._3, t._4)
    )

  /** Codec for reports with 0/1 discriminator for Option. */
  def reportsCodec[A](coresCount: Int)(using assignmentCodec: Codec[A]): Codec[List[Option[A]]] =
    val optionalAssignmentCodec = JamCodecs.optionCodec(assignmentCodec)
    JamCodecs.fixedSizeList(optionalAssignmentCodec, coresCount)

  /** Use ServiceInfo codec from its companion object. */
  val serviceInfoCodec: Codec[ServiceInfo] = summon[Codec[ServiceInfo]]

  /**
   * Decode a state value and require it to be consumed exactly.
   */
  def decodeExact[A](codec: Codec[A], bytes: Array[Byte], label: String): A =
    codec.decode(BitVector(bytes)) match
      case Attempt.Successful(result) =>
        if result.remainder.nonEmpty then
          throw new CodecDecodingException(
            s"$label: ${result.remainder.bytes.size} trailing byte(s) after a valid value"
          )
        result.value
      case Attempt.Failure(err) =>
        throw new CodecDecodingException(s"$label: ${err.messageWithContext}")

  def decodeAuthPools(bytes: Array[Byte], coresCount: Int): List[List[Hash]] =
    decodeExact(authPoolsCodec(coresCount), bytes, "decodeAuthPools")

  def decodeAuthQueues(bytes: Array[Byte], coresCount: Int, queueSize: Int): List[List[Hash]] =
    decodeExact(authQueuesCodec(coresCount, queueSize), bytes, "decodeAuthQueues")

  def decodeAccumulationHistory(bytes: Array[Byte], epochLength: Int): List[List[ByteVector]] =
    decodeExact(accumulationHistoryCodec(epochLength), bytes, "decodeAccumulationHistory")

  def decodeServiceInfo(bytes: Array[Byte]): ServiceInfo =
    decodeExact(serviceInfoCodec, bytes, "decodeServiceInfo")

  /** Codec for last accumulation outputs: compact list of (u32LE serviceId, 32-byte hash). */
  val lastAccumulationOutputsCodec: Codec[List[(Long, ByteVector)]] =
    val entryCodec: Codec[(Long, ByteVector)] =
      (uint32L :: fixedSizeBytes(Hash.Size.toLong, bytes)).xmap(
        { case (id, bv) => (id.toLong & 0xFFFFFFFFL, bv) },
        { case (id, bv) => (id & 0xFFFFFFFFL, bv) }
      )
    JamCodecs.compactPrefixedList(entryCodec)

  def decodeLastAccumulationOutputs(bytes: Array[Byte]): List[(Long, ByteVector)] =
    decodeExact(lastAccumulationOutputsCodec, bytes, "decodeLastAccumulationOutputs")

  def encodeLastAccumulationOutputs(outputs: List[(Long, ByteVector)]): ByteVector =
    // Sort by service ID before encoding as per Gray Paper
    val sorted = outputs.sortBy(_._1)
    lastAccumulationOutputsCodec.encode(sorted).require.bytes

