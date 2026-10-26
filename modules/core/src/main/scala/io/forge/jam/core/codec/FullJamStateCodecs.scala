package io.forge.jam.core.codec

import io.forge.jam.core.codec.codecs.*
import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.*
import io.forge.jam.core.types.tickets.TicketMark
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.service.ServiceInfo

/**
 * Codecs for FullJamState encoding/decoding.
 *
 * Provides bidirectional codecs for JAM state components with binary compatibility.
 */
object FullJamStateCodecs:

  val BandersnatchRingCommitmentSize: Int = 144
  val ValidatorKeySize: Int = 336
  val MetadataSize: Int = 128
  val TicketMarkSize: Int = Hash.Size + 1

  /** Codec for timeslot (tau) - 4 bytes little-endian uint32. */
  val timeslotCodec: Codec[Long] = uint32L

  /** Codec for entropy pool - exactly 4 hashes (128 bytes total). */
  val entropyPoolCodec: Codec[List[Hash]] =
    JamCodecs.fixedSizeList(JamCodecs.hashCodec, 4)

  /**
   * Codec for `ValidatorsData`.
   */
  def validatorListCodec: Codec[IndexedSeq[ValidatorKey]] =
    JamCodecs.compactPrefixedVector(summon[Codec[ValidatorKey]])

  /** Codec for authorization pools: per-core compact length prefix + N hashes. */
  def authPoolsCodec(coresCount: Int): Codec[List[List[Hash]]] =
    val poolCodec: Codec[List[Hash]] = JamCodecs.compactPrefixedList(JamCodecs.hashCodec)
    JamCodecs.fixedSizeList(poolCodec, coresCount)

  /** Codec for authorization queues: fixed-size coresCount * queueSize * 32 bytes. */
  def authQueuesCodec(coresCount: Int, queueSize: Int): Codec[List[List[Hash]]] =
    val queueCodec: Codec[List[Hash]] = JamCodecs.fixedSizeList(JamCodecs.hashCodec, queueSize)
    JamCodecs.fixedSizeList(queueCodec, coresCount)

  /** Codec for accumulation history: per-slot compact count + N x 32-byte hashes. */
  def accumulationHistoryCodec(epochLength: Int): Codec[List[List[JamBytes]]] =
    val slotCodec: Codec[List[JamBytes]] = JamCodecs.compactPrefixedList(bytes(Hash.Size))
    JamCodecs.fixedSizeList(slotCodec, epochLength)

  sealed trait TicketsOrKeysData

  object TicketsOrKeysData:
    final case class Tickets(tickets: List[TicketMark]) extends TicketsOrKeysData
    final case class Keys(keys: List[BandersnatchPublicKey]) extends TicketsOrKeysData

  /** gamma_S: 0 = tickets, 1 = fallback keys, each exactly E entries (GP 0.8.0 §safrole eq. (6.5)). */
  private def ticketsOrKeysCodec(epochLength: Int): Codec[TicketsOrKeysData] =
    val ticketsListCodec: Codec[List[TicketMark]] = JamCodecs.fixedSizeList(JamCodecs.ticketMarkCodec, epochLength)
    val keysListCodec: Codec[List[BandersnatchPublicKey]] =
      JamCodecs.fixedSizeList(JamCodecs.bandersnatchPublicKeyCodec, epochLength)
    Codec.of(
      (w, v) =>
        v match
          case TicketsOrKeysData.Tickets(t) =>
            w.u8(0)
            ticketsListCodec.write(w, t)
          case TicketsOrKeysData.Keys(k) =>
            w.u8(1)
            keysListCodec.write(w, k),
      r =>
        r.u8() match
          case 0     => TicketsOrKeysData.Tickets(ticketsListCodec.read(r))
          case 1     => TicketsOrKeysData.Keys(keysListCodec.read(r))
          case other => r.fail(s"invalid TicketsOrKeys discriminator $other")
    )

  /** Codec for safrole gamma state (gammaK + gammaZ + gammaS + gammaA). */
  def safroleGammaStateCodec(
    epochLength: Int
  ): Codec[(IndexedSeq[ValidatorKey], JamBytes, TicketsOrKeysData, List[TicketMark])] =
    val gammaKCodec = validatorListCodec
    val gammaZCodec: Codec[JamBytes] = bytes(BandersnatchRingCommitmentSize)
    val gammaSCodec = ticketsOrKeysCodec(epochLength)
    val gammaACodec: Codec[List[TicketMark]] = JamCodecs.compactPrefixedList(JamCodecs.ticketMarkCodec)
    gammaKCodec :: gammaZCodec :: gammaSCodec :: gammaACodec

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
    codec.decodeExact(bytes) match
      case Right(value) => value
      case Left(err)    => throw new CodecDecodingException(s"$label: $err")

  def decodeAuthPools(bytes: Array[Byte], coresCount: Int): List[List[Hash]] =
    decodeExact(authPoolsCodec(coresCount), bytes, "decodeAuthPools")

  def decodeAuthQueues(bytes: Array[Byte], coresCount: Int, queueSize: Int): List[List[Hash]] =
    decodeExact(authQueuesCodec(coresCount, queueSize), bytes, "decodeAuthQueues")

  def decodeAccumulationHistory(bytes: Array[Byte], epochLength: Int): List[List[JamBytes]] =
    decodeExact(accumulationHistoryCodec(epochLength), bytes, "decodeAccumulationHistory")

  def decodeServiceInfo(bytes: Array[Byte]): ServiceInfo =
    decodeExact(serviceInfoCodec, bytes, "decodeServiceInfo")

  /** Codec for last accumulation outputs: compact list of (u32LE serviceId, 32-byte hash). */
  val lastAccumulationOutputsCodec: Codec[List[(Long, JamBytes)]] =
    JamCodecs.compactPrefixedList(uint32L :: bytes(Hash.Size))

  def decodeLastAccumulationOutputs(bytes: Array[Byte]): List[(Long, JamBytes)] =
    decodeExact(lastAccumulationOutputsCodec, bytes, "decodeLastAccumulationOutputs")

  def encodeLastAccumulationOutputs(outputs: List[(Long, JamBytes)]): JamBytes =
    // Sort by service ID before encoding as per Gray Paper
    val sorted = outputs.sortBy(_._1)
    lastAccumulationOutputsCodec.encodeJam(sorted)

