package io.forge.jam.protocol.authorization

import io.forge.jam.core.{ChainConfig, JamBytes, codec, constants}
import io.forge.jam.core.codec.{JamEncoder, JamDecoder, encode}
import io.forge.jam.core.primitives.{Hash, CoreIndex}
import io.forge.jam.core.json.JsonHelpers.{parseHash, parseHashListList}
import io.circe.{Decoder, HCursor}
import spire.math.UShort

/**
 * Types for the Authorization State Transition Function.
 *
 * The Authorization STF manages authorization pools per core, handling
 * authorization consumption and queue rotation based on timeslot.
 */
object AuthorizationTypes:

  /** Fixed size of authorization queues (inner list) */
  val AuthQueueSize: Int = constants.Q

  /** Maximum size of authorization pools per core */
  val PoolSize: Int = constants.O

  /**
   * An authorization entry with core index and authorization hash.
   * Fixed size: 34 bytes (2 bytes for core + 32 bytes for authHash)
   */
  final case class Auth(
    core: CoreIndex,
    authHash: Hash
  )

  object Auth:
    val Size: Int = 2 + Hash.Size // 34 bytes

    given JamEncoder[Auth] with
      def encode(a: Auth): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= codec.encodeU16LE(a.core.value)
        builder ++= a.authHash.bytes
        builder.result()

    given JamDecoder[Auth] with
      def decode(bytes: JamBytes, offset: Int): (Auth, Int) =
        val arr = bytes.toArray
        val core = CoreIndex(codec.decodeU16LE(arr, offset))
        val authHash = Hash(arr.slice(offset + 2, offset + 2 + Hash.Size))
        (Auth(core, authHash), Size)

    given Decoder[Auth] =
      Decoder.instance { cursor =>
        for
          coreValue <- cursor.get[Long]("core")
          authHashHex <- cursor.get[String]("auth_hash")
          authHash <- parseHash(authHashHex)
        yield Auth(CoreIndex(coreValue.toInt), authHash)
      }

  /**
   * Input to the Authorization STF.
   */
  final case class AuthInput(
    slot: Long,
    auths: List[Auth]
  )

  object AuthInput:
    given JamEncoder[AuthInput] with
      def encode(a: AuthInput): JamBytes =
        val builder = JamBytes.newBuilder
        // slot is 4 bytes (u32)
        builder ++= codec.encodeU32LE(spire.math.UInt(a.slot.toInt))
        // auths - variable-size list with compact integer length prefix
        builder ++= codec.encodeCompactInteger(a.auths.length.toLong)
        for auth <- a.auths do
          builder ++= auth.encode
        builder.result()

    given JamDecoder[AuthInput] with
      def decode(bytes: JamBytes, offset: Int): (AuthInput, Int) =
        val arr = bytes.toArray
        var pos = offset
        // slot is 4 bytes (u32)
        val slot = codec.decodeU32LE(arr, pos).toLong
        pos += 4
        // auths - variable-size list with compact integer length prefix
        val (authsLength, authsLengthBytes) = codec.decodeCompactInteger(arr, pos)
        pos += authsLengthBytes
        val auths = (0 until authsLength.toInt).map { _ =>
          val (auth, consumed) = Auth.given_JamDecoder_Auth.decode(bytes, pos)
          pos += consumed
          auth
        }.toList
        (AuthInput(slot, auths), pos - offset)

    given Decoder[AuthInput] =
      Decoder.instance { cursor =>
        for
          slot <- cursor.get[Long]("slot")
          auths <- cursor.get[List[Auth]]("auths")
        yield AuthInput(slot, auths)
      }

  /**
   * Authorization state containing pools and queues per core.
   * - authPools: variable-size inner lists (0..8 hashes per core)
   * - authQueues: fixed-size inner lists (80 hashes per core)
   */
  final case class AuthState(
    authPools: List[List[Hash]],
    authQueues: List[List[Hash]]
  )

  object AuthState:
    /**
     * Create a config-aware decoder for AuthState.
     */
    def decoder(config: ChainConfig): JamDecoder[AuthState] = new JamDecoder[AuthState]:
      private val coreCount = config.coresCount
      def decode(bytes: JamBytes, offset: Int): (AuthState, Int) =
        val arr = bytes.toArray
        var pos = offset

        // authPools - fixed size outer (coreCount), variable inner (compact length + 32-byte hashes)
        val authPools = (0 until coreCount).map { _ =>
          val (poolLength, poolLengthBytes) = codec.decodeCompactInteger(arr, pos)
          pos += poolLengthBytes
          val pool = (0 until poolLength.toInt).map { _ =>
            val hash = Hash(arr.slice(pos, pos + Hash.Size))
            pos += Hash.Size
            hash
          }.toList
          pool
        }.toList

        // authQueues - fixed size outer (coreCount), fixed size inner (80 x 32-byte hashes)
        val authQueues = (0 until coreCount).map { _ =>
          val queue = (0 until AuthQueueSize).map { _ =>
            val hash = Hash(arr.slice(pos, pos + Hash.Size))
            pos += Hash.Size
            hash
          }.toList
          queue
        }.toList

        (AuthState(authPools, authQueues), pos - offset)

    given JamEncoder[AuthState] with
      def encode(a: AuthState): JamBytes =
        val builder = JamBytes.newBuilder
        // AuthPools: outer list is fixed-size (core-count), inner list is variable-size (0..8)
        // No outer length prefix, inner uses compact integer length prefix
        for pool <- a.authPools do
          builder ++= codec.encodeCompactInteger(pool.length.toLong)
          for hash <- pool do
            builder ++= hash.bytes

        // AuthQueues: outer list is fixed-size (core-count), inner list is fixed-size (80)
        // No length prefixes at all
        for queue <- a.authQueues do
          for hash <- queue do
            builder ++= hash.bytes

        builder.result()

    given Decoder[AuthState] =
      Decoder.instance { cursor =>
        for
          authPoolsOpt <- cursor.get[List[List[String]]]("auth_pools")
          authQueuesOpt <- cursor.get[List[List[String]]]("auth_queues")
          authPools <- parseHashListList(authPoolsOpt)
          authQueues <- parseHashListList(authQueuesOpt)
        yield AuthState(authPools, authQueues)
      }

  /**
   * Test case for Authorization STF containing input, pre-state, and post-state.
   */
  final case class AuthCase(
    input: AuthInput,
    preState: AuthState,
    postState: AuthState
  )

  object AuthCase:
    /**
     * Create a config-aware decoder for AuthCase.
     */
    def decoder(config: ChainConfig): JamDecoder[AuthCase] = new JamDecoder[AuthCase]:
      def decode(bytes: JamBytes, offset: Int): (AuthCase, Int) =
        var pos = offset
        val (input, inputBytes) = AuthInput.given_JamDecoder_AuthInput.decode(bytes, pos)
        pos += inputBytes
        val stateDecoder = AuthState.decoder(config)
        val (preState, preStateBytes) = stateDecoder.decode(bytes, pos)
        pos += preStateBytes
        // output is always null in encoding (per encode method)
        val (postState, postStateBytes) = stateDecoder.decode(bytes, pos)
        pos += postStateBytes
        (AuthCase(input, preState, postState), pos - offset)

    given JamEncoder[AuthCase] with
      def encode(a: AuthCase): JamBytes =
        val builder = JamBytes.newBuilder
        builder ++= a.input.encode
        builder ++= a.preState.encode
        // NULL output encodes to empty byte array (nothing)
        builder ++= a.postState.encode
        builder.result()

    given Decoder[AuthCase] =
      Decoder.instance { cursor =>
        for
          input <- cursor.get[AuthInput]("input")
          preState <- cursor.get[AuthState]("pre_state")
          postState <- cursor.get[AuthState]("post_state")
        yield AuthCase(input, preState, postState)
      }
