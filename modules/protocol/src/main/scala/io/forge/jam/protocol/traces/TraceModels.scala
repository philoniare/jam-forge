package io.forge.jam.protocol.traces

import _root_.scodec.{Codec, Attempt, DecodeResult, Err}
import _root_.scodec.bits.{BitVector, ByteVector}
import _root_.scodec.codecs.*
import io.circe.{Decoder, HCursor}
import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.scodec.JamCodecs
import io.forge.jam.core.types.block.Block
import io.forge.jam.core.types.header.Header

/**
 * A key-value pair representing a storage entry.
 * Keys are exactly 31 bytes as per the schema.
 */
final case class KeyValue(
  key: JamBytes,
  value: JamBytes
)

object KeyValue:
  val KEY_SIZE: Int = 31

  given Codec[KeyValue] =
    (fixedSizeBytes(KEY_SIZE.toLong, bytes) ::
     variableSizeBytes(JamCodecs.compactInt, bytes)).xmap(
      { case (keyBv, valueBv) =>
        KeyValue(
          key = JamBytes.fromByteVector(keyBv),
          value = JamBytes.fromByteVector(valueBv)
        )
      },
      kv => (kv.key.toByteVector, kv.value.toByteVector)
    )

  given Decoder[KeyValue] = new Decoder[KeyValue]:
    def apply(c: HCursor): Decoder.Result[KeyValue] =
      for
        keyHex <- c.downField("key").as[String]
        valueHex <- c.downField("value").as[String]
      yield
        val key = JamBytes.fromHexUnsafe(keyHex)
        val value = JamBytes.fromHexUnsafe(valueHex)
        KeyValue(key, value)

/**
 * Raw state representation containing state root and key-value pairs.
 */
final case class RawState(
  stateRoot: Hash,
  keyvals: List[KeyValue]
)

object RawState:
  /**
   * Create an empty raw state.
   */
  def empty: RawState = RawState(Hash.zero, List.empty)

  given Codec[RawState] =
    (JamCodecs.hashCodec ::
     JamCodecs.compactPrefixedList(summon[Codec[KeyValue]])).xmap(
      { case (stateRoot, keyvals) =>
        RawState(stateRoot = stateRoot, keyvals = keyvals)
      },
      rs => (rs.stateRoot, rs.keyvals)
    )

  given Decoder[RawState] = new Decoder[RawState]:
    def apply(c: HCursor): Decoder.Result[RawState] =
      for
        stateRootHex <- c.downField("state_root").as[String]
        keyvals <- c.downField("keyvals").as[List[KeyValue]]
      yield
        val stateRoot = Hash(JamBytes.fromHexUnsafe(stateRootHex).toArray)
        RawState(stateRoot, keyvals)

/**
 * A single trace step representing a block import operation.
 * Contains pre-state, the block to import, and expected post-state.
 */
final case class TraceStep(
  preState: RawState,
  block: Block,
  postState: RawState
)

object TraceStep:
  /**
   * Create a codec for TraceStep with config parameters.
   */
  def codec(validatorCount: Int, epochLength: Int, coresCount: Int, votesPerVerdict: Int): Codec[TraceStep] =
    (summon[Codec[RawState]] ::
     Block.blockCodec(validatorCount, epochLength, coresCount, votesPerVerdict) ::
     summon[Codec[RawState]]).xmap(
      { case (preState, block, postState) =>
        TraceStep(preState = preState, block = block, postState = postState)
      },
      ts => (ts.preState, ts.block, ts.postState)
    )

  /**
   * Create a JSON decoder for TraceStep.
   * Block and Header decoders need to be provided from the test scope.
   */
  def decoder(using blockDecoder: Decoder[Block]): Decoder[TraceStep] = new Decoder[TraceStep]:
    def apply(c: HCursor): Decoder.Result[TraceStep] =
      for
        preState <- c.downField("pre_state").as[RawState]
        block <- c.downField("block").as[Block]
        postState <- c.downField("post_state").as[RawState]
      yield TraceStep(preState, block, postState)

/**
 * Genesis state for a trace, containing initial header and state.
 */
final case class Genesis(
  header: Header,
  state: RawState
)

object Genesis:
  /**
   * Create a codec for Genesis with config parameters.
   */
  def codec(validatorCount: Int, epochLength: Int): Codec[Genesis] =
    (Header.headerCodec(validatorCount, epochLength) ::
     summon[Codec[RawState]]).xmap(
      { case (header, state) =>
        Genesis(header = header, state = state)
      },
      g => (g.header, g.state)
    )

  /**
   * Create a JSON decoder for Genesis.
   * Header decoder needs to be provided from the test scope.
   */
  def decoder(using headerDecoder: Decoder[Header]): Decoder[Genesis] = new Decoder[Genesis]:
    def apply(c: HCursor): Decoder.Result[Genesis] =
      for
        header <- c.downField("header").as[Header]
        state <- c.downField("state").as[RawState]
      yield Genesis(header, state)

/**
 * Configuration for tiny chain spec used by all traces.
 * Uses ChainConfig.TINY for standard parameters.
 */
object TinyConfig:
  val CONFIG: ChainConfig = ChainConfig.TINY
  val VALIDATORS_COUNT: Int = CONFIG.validatorCount
  val EPOCH_LENGTH: Int = CONFIG.epochLength
  val CORES_COUNT: Int = CONFIG.coresCount
  val PREIMAGE_EXPUNGE_DELAY: Int = CONFIG.preimageExpungePeriod
  val MAX_TICKET_ATTEMPTS: Int = CONFIG.ticketsPerValidator

  // Safrole-specific parameters
  val TICKET_CUTOFF: Int = CONFIG.ticketCutoff
  val RING_SIZE: Int = 6

  // Bandersnatch ring commitment size
  val BANDERSNATCH_RING_COMMITMENT_SIZE: Int = 144
