package io.forge.jam.protocol.state

import io.forge.jam.core.codec.Codec
import io.forge.jam.core.codec.codecs.*
import io.circe.{Decoder, HCursor}
import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.codec.JamCodecs

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
    (bytes(KEY_SIZE) ::
     compactBytes).xmap(
      { case (keyBv, valueBv) =>
        KeyValue(
          key = keyBv,
          value = valueBv
        )
      },
      kv => (kv.key, kv.value)
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
