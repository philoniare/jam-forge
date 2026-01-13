package io.forge.jam.conformance

import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.scodec.JamCodecs
import io.forge.jam.core.primitives.{Hash, Timeslot}
import io.forge.jam.core.types.block.Block
import io.forge.jam.core.types.header.Header
import io.forge.jam.protocol.traces.KeyValue
import io.forge.jam.conformance.ConformanceCodecs.{encode, decodeAs}
import spire.math.{UByte, UInt}
import _root_.scodec.Codec

/**
 * Feature bits for protocol negotiation.
 */
object Features:
  /** Target has access to block ancestry of up to L items */
  val ANCESTRY: Int = 0x01

  /** Simple forking: forks are supported with max depth 1 */
  val FORKS: Int = 0x02

  /** All supported features for M1 conformance */
  val ALL_M1: Int = ANCESTRY | FORKS

/**
 * Protocol version information.
 */
final case class Version(
  major: UByte,
  minor: UByte,
  patch: UByte
)

object Version:
  val JAM_VERSION: Version = Version(UByte(0), UByte(7), UByte(2))
  val APP_VERSION: Version = Version(UByte(0), UByte(1), UByte(2))

  given Codec[Version] =
    (JamCodecs.ubyteCodec :: JamCodecs.ubyteCodec :: JamCodecs.ubyteCodec).xmap(
      { case (major, minor, patch) => Version(major, minor, patch) },
      v => (v.major, v.minor, v.patch)
    )

/**
 * Peer information exchanged during handshake.
 *
 * @param fuzzVersion Fuzzer protocol version
 * @param fuzzFeatures Supported features bitmask
 * @param jamVersion JAM protocol version
 * @param appVersion Application version
 * @param appName Application name
 */
final case class PeerInfo(
  fuzzVersion: UByte,
  fuzzFeatures: UInt,
  jamVersion: Version,
  appVersion: Version,
  appName: String
)

object PeerInfo:
  val DISCRIMINANT: Int = 0x00

  /** Create target PeerInfo with standard values */
  def forTarget(features: Int = Features.ALL_M1): PeerInfo =
    PeerInfo(
      fuzzVersion = UByte(1),
      fuzzFeatures = UInt(features),
      jamVersion = Version.JAM_VERSION,
      appVersion = Version.APP_VERSION,
      appName = "jam-forge"
    )

  given Codec[PeerInfo] =
    import _root_.scodec.codecs.*
    val stringCodec: Codec[String] = variableSizeBytesLong(JamCodecs.compactInteger, _root_.scodec.codecs.utf8)

    (JamCodecs.ubyteCodec :: JamCodecs.uintCodec :: summon[Codec[Version]] ::
      summon[Codec[Version]] :: stringCodec).xmap(
      {
        case (fuzzVersion, fuzzFeatures, jamVersion, appVersion, appName) =>
          PeerInfo(fuzzVersion, fuzzFeatures, jamVersion, appVersion, appName)
      },
      p => (p.fuzzVersion, p.fuzzFeatures, p.jamVersion, p.appVersion, p.appName)
    )

/**
 * Ancestry item containing slot and header hash.
 */
final case class AncestryItem(
  slot: Timeslot,
  headerHash: Hash
)

object AncestryItem:
  given Codec[AncestryItem] =
    import _root_.scodec.codecs.*
    (uint32L :: JamCodecs.hashCodec).xmap(
      { case (slotVal, headerHash) => AncestryItem(Timeslot(slotVal.toInt), headerHash) },
      a => (a.slot.value.toLong & 0xffffffffL, a.headerHash)
    )

/**
 * Initialize message containing genesis-like header, state, and ancestry.
 */
final case class Initialize(
  header: Header,
  keyvals: List[KeyValue],
  ancestry: List[AncestryItem]
)

object Initialize:
  val DISCRIMINANT: Int = 0x01

  def codec(config: ChainConfig): Codec[Initialize] =
    (Header.headerCodec(config) ::
      JamCodecs.compactPrefixedList(summon[Codec[KeyValue]]) ::
      JamCodecs.compactPrefixedList(summon[Codec[AncestryItem]])).xmap(
      { case (header, keyvals, ancestry) => Initialize(header, keyvals, ancestry) },
      i => (i.header, i.keyvals, i.ancestry)
    )

/**
 * StateRoot response message containing 32-byte state root hash.
 */
final case class StateRoot(hash: Hash)

object StateRoot:
  val DISCRIMINANT: Int = 0x02

  given Codec[StateRoot] =
    JamCodecs.hashCodec.xmap(
      hash => StateRoot(hash),
      sr => sr.hash
    )

/**
 * ImportBlock request message wrapping a Block.
 */
final case class ImportBlock(block: Block)

object ImportBlock:
  val DISCRIMINANT: Int = 0x03

  def codec(config: ChainConfig): Codec[ImportBlock] =
    Block.blockCodec(config.validatorCount, config.epochLength, config.coresCount, config.votesPerVerdict).xmap(
      block => ImportBlock(block),
      ib => ib.block
    )

/**
 * GetState request message containing header hash.
 */
final case class GetState(headerHash: Hash)

object GetState:
  val DISCRIMINANT: Int = 0x04

  given Codec[GetState] =
    JamCodecs.hashCodec.xmap(
      hash => GetState(hash),
      gs => gs.headerHash
    )

/**
 * State response message containing key-value pairs.
 */
final case class State(keyvals: List[KeyValue])

object State:
  val DISCRIMINANT: Int = 0x05

  given Codec[State] =
    JamCodecs.compactPrefixedList(summon[Codec[KeyValue]]).xmap(
      keyvals => State(keyvals),
      s => s.keyvals
    )

/**
 * Error response message with UTF8 string.
 */
final case class Error(message: String)

object Error:
  val DISCRIMINANT: Int = 0xff

  given Codec[Error] =
    import _root_.scodec.codecs.*
    variableSizeBytesLong(JamCodecs.compactInteger, utf8).xmap(
      message => Error(message),
      e => e.message
    )

/**
 * Sealed trait representing all protocol messages.
 */
sealed trait ProtocolMessage

object ProtocolMessage:
  final case class PeerInfoMsg(info: PeerInfo) extends ProtocolMessage
  final case class InitializeMsg(init: Initialize) extends ProtocolMessage
  final case class StateRootMsg(root: StateRoot) extends ProtocolMessage
  final case class ImportBlockMsg(importBlock: ImportBlock) extends ProtocolMessage
  final case class GetStateMsg(getState: GetState) extends ProtocolMessage
  final case class StateMsg(state: State) extends ProtocolMessage
  final case class ErrorMsg(error: Error) extends ProtocolMessage

  /**
   * Encode a protocol message with discriminant prefix.
   */
  def encodeMessage(msg: ProtocolMessage): JamBytes =
    msg match
      case PeerInfoMsg(info) =>
        JamBytes(Array(PeerInfo.DISCRIMINANT.toByte)) ++ info.encode
      case StateRootMsg(root) =>
        JamBytes(Array(StateRoot.DISCRIMINANT.toByte)) ++ root.encode
      case StateMsg(state) =>
        JamBytes(Array(State.DISCRIMINANT.toByte)) ++ state.encode
      case ErrorMsg(error) =>
        JamBytes(Array(Error.DISCRIMINANT.toByte)) ++ error.encode
      case _ =>
        throw new IllegalArgumentException(s"Cannot encode message type: $msg")

  /**
   * Encode a protocol message with 32-bit LE length prefix.
   */
  def encodeWithFrame(msg: ProtocolMessage): JamBytes =
    val body = encodeMessage(msg)
    val lengthPrefix = JamBytes(JamCodecs.encodeU32LE(UInt(body.length)))
    lengthPrefix ++ body

  /**
   * Decode a protocol message from bytes (without length prefix).
   * The discriminant byte indicates message type.
   */
  def decodeMessage(bytes: JamBytes, offset: Int, config: ChainConfig): (ProtocolMessage, Int) =
    val discriminant = bytes.toArray(offset) & 0xff
    val bodyOffset = offset + 1

    discriminant match
      case PeerInfo.DISCRIMINANT =>
        val (info, consumed) = bytes.decodeAs[PeerInfo](bodyOffset)
        (PeerInfoMsg(info), 1 + consumed)

      case Initialize.DISCRIMINANT =>
        given Codec[Initialize] = Initialize.codec(config)
        val (init, consumed) = bytes.decodeAs[Initialize](bodyOffset)
        (InitializeMsg(init), 1 + consumed)

      case StateRoot.DISCRIMINANT =>
        val (root, consumed) = bytes.decodeAs[StateRoot](bodyOffset)
        (StateRootMsg(root), 1 + consumed)

      case ImportBlock.DISCRIMINANT =>
        given Codec[ImportBlock] = ImportBlock.codec(config)
        val (importBlock, consumed) = bytes.decodeAs[ImportBlock](bodyOffset)
        (ImportBlockMsg(importBlock), 1 + consumed)

      case GetState.DISCRIMINANT =>
        val (getState, consumed) = bytes.decodeAs[GetState](bodyOffset)
        (GetStateMsg(getState), 1 + consumed)

      case State.DISCRIMINANT =>
        val (state, consumed) = bytes.decodeAs[State](bodyOffset)
        (StateMsg(state), 1 + consumed)

      case Error.DISCRIMINANT =>
        val (error, consumed) = bytes.decodeAs[Error](bodyOffset)
        (ErrorMsg(error), 1 + consumed)

      case other =>
        throw new IllegalArgumentException(s"Unknown message discriminant: 0x${other.toHexString}")
