package io.forge.jam.protocol.traces

import io.circe.{Decoder, HCursor}
import io.forge.jam.core.{ChainConfig, JamBytes, codec}
import io.forge.jam.core.codec.{JamEncoder, JamDecoder, encode}
import io.forge.jam.core.primitives.Hash
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

  /**
   * Create KeyValue from bytes starting at offset.
   * Returns the KeyValue and number of bytes consumed.
   */
  def fromBytes(data: Array[Byte], offset: Int = 0): (KeyValue, Int) =
    val key = JamBytes(data.slice(offset, offset + KEY_SIZE))
    val (valueLength, valueLengthBytes) = codec.decodeCompactInteger(data, offset + KEY_SIZE)
    val valueEnd = offset + KEY_SIZE + valueLengthBytes + valueLength.toInt
    val value = JamBytes(data.slice(offset + KEY_SIZE + valueLengthBytes, valueEnd))
    (KeyValue(key, value), KEY_SIZE + valueLengthBytes + valueLength.toInt)

  /**
   * Encode KeyValue to bytes.
   * Key is fixed 31 bytes, value is length-prefixed.
   */
  def encode(kv: KeyValue): JamBytes =
    val keyBytes = kv.key.toArray
    val lengthBytes = codec.encodeCompactInteger(kv.value.length.toLong)
    val valueBytes = kv.value.toArray
    JamBytes(keyBytes ++ lengthBytes ++ valueBytes)

  given JamEncoder[KeyValue] with
    def encode(a: KeyValue): JamBytes = KeyValue.encode(a)

  given JamDecoder[KeyValue] with
    def decode(bytes: JamBytes, offset: Int): (KeyValue, Int) =
      fromBytes(bytes.toArray, offset)

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
   * Create RawState from bytes starting at offset.
   * Returns the RawState and number of bytes consumed.
   */
  def fromBytes(data: Array[Byte], offset: Int = 0): (RawState, Int) =
    var currentOffset = offset
    val stateRoot = Hash(data.slice(currentOffset, currentOffset + 32))
    currentOffset += 32

    val (keyvalsLength, keyvalsLengthBytes) = codec.decodeCompactInteger(data, currentOffset)
    currentOffset += keyvalsLengthBytes

    val keyvals = scala.collection.mutable.ListBuffer[KeyValue]()
    for _ <- 0 until keyvalsLength.toInt do
      val (kv, kvBytes) = KeyValue.fromBytes(data, currentOffset)
      keyvals += kv
      currentOffset += kvBytes

    (RawState(stateRoot, keyvals.toList), currentOffset - offset)

  /**
   * Encode RawState to bytes.
   */
  def encode(state: RawState): JamBytes =
    val builder = JamBytes.newBuilder
    builder ++= state.stateRoot.bytes
    builder ++= codec.encodeCompactInteger(state.keyvals.length.toLong)
    for kv <- state.keyvals do
      builder ++= KeyValue.encode(kv).toArray
    builder.result()

  /**
   * Create an empty raw state.
   */
  def empty: RawState = RawState(Hash.zero, List.empty)

  given JamEncoder[RawState] with
    def encode(a: RawState): JamBytes = RawState.encode(a)

  given JamDecoder[RawState] with
    def decode(bytes: JamBytes, offset: Int): (RawState, Int) =
      fromBytes(bytes.toArray, offset)

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
   * Create TraceStep from bytes with config parameters.
   */
  def fromBytes(
    data: Array[Byte],
    offset: Int = 0,
    validatorCount: Int = TinyConfig.VALIDATORS_COUNT,
    epochLength: Int = TinyConfig.EPOCH_LENGTH,
    coresCount: Int = TinyConfig.CORES_COUNT,
    votesPerVerdict: Int = TinyConfig.VALIDATORS_COUNT
  ): (TraceStep, Int) =
    var currentOffset = offset

    val (preState, preStateBytes) = RawState.fromBytes(data, currentOffset)
    currentOffset += preStateBytes

    val blockDecoder = Block.decoder(validatorCount, epochLength, coresCount, votesPerVerdict)
    val (block, blockBytes) = blockDecoder.decode(JamBytes(data), currentOffset)
    currentOffset += blockBytes

    val (postState, postStateBytes) = RawState.fromBytes(data, currentOffset)
    currentOffset += postStateBytes

    (TraceStep(preState, block, postState), currentOffset - offset)

  /**
   * Encode TraceStep to bytes.
   */
  def encodeStep(step: TraceStep): JamBytes =
    val builder = JamBytes.newBuilder
    builder ++= RawState.encode(step.preState).toArray
    builder ++= step.block.encode.toArray
    builder ++= RawState.encode(step.postState).toArray
    builder.result()

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

  given JamEncoder[TraceStep] with
    def encode(a: TraceStep): JamBytes = encodeStep(a)

/**
 * Genesis state for a trace, containing initial header and state.
 */
final case class Genesis(
  header: Header,
  state: RawState
)

object Genesis:
  /**
   * Create Genesis from bytes with config parameters.
   */
  def fromBytes(
    data: Array[Byte],
    offset: Int = 0,
    validatorCount: Int = TinyConfig.VALIDATORS_COUNT,
    epochLength: Int = TinyConfig.EPOCH_LENGTH
  ): (Genesis, Int) =
    var currentOffset = offset

    val headerDecoder = Header.decoder(validatorCount, epochLength)
    val (header, headerBytes) = headerDecoder.decode(JamBytes(data), currentOffset)
    currentOffset += headerBytes

    val (state, stateBytes) = RawState.fromBytes(data, currentOffset)
    currentOffset += stateBytes

    (Genesis(header, state), currentOffset - offset)

  /**
   * Encode Genesis to bytes.
   */
  def encodeGenesis(genesis: Genesis): JamBytes =
    val builder = JamBytes.newBuilder
    builder ++= genesis.header.encode.toArray
    builder ++= RawState.encode(genesis.state).toArray
    builder.result()

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

  given JamEncoder[Genesis] with
    def encode(a: Genesis): JamBytes = encodeGenesis(a)

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
