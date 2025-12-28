package io.forge.jam.protocol.authorization

import io.forge.jam.core.{ChainConfig, constants}
import io.forge.jam.core.primitives.{Hash, CoreIndex}
import io.forge.jam.core.json.JsonHelpers.{parseHash, parseHashListList}
import io.circe.Decoder
import spire.math.UShort
import _root_.scodec.Codec
import _root_.scodec.codecs.*
import io.forge.jam.core.scodec.JamCodecs

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

    given Codec[Auth] =
      (uint16L :: JamCodecs.hashCodec).xmap(
        { case (coreInt, authHash) =>
          Auth(CoreIndex(UShort(coreInt)), authHash)
        },
        auth => (auth.core.toInt, auth.authHash)
      )

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
    given Codec[AuthInput] =
      (uint32L :: JamCodecs.compactPrefixedList(summon[Codec[Auth]])).xmap(
        { case (slotInt, auths) =>
          AuthInput(slotInt & 0xFFFFFFFFL, auths)
        },
        input => (input.slot & 0xFFFFFFFFL, input.auths)
      )

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
     * Create a config-aware codec for AuthState.
     */
    def codec(config: ChainConfig): Codec[AuthState] =
      val coreCount = config.coresCount

      // authPools: fixed-size outer list, variable-size inner lists with compact prefix
      val authPoolsCodec: Codec[List[List[Hash]]] =
        JamCodecs.fixedSizeList(JamCodecs.compactPrefixedList(JamCodecs.hashCodec), coreCount)

      // authQueues: fixed-size outer list, fixed-size inner lists
      val authQueuesCodec: Codec[List[List[Hash]]] =
        JamCodecs.fixedSizeList(JamCodecs.fixedSizeList(JamCodecs.hashCodec, AuthQueueSize), coreCount)

      (authPoolsCodec :: authQueuesCodec).xmap(
        { case (authPools, authQueues) =>
          AuthState(authPools, authQueues)
        },
        state => (state.authPools, state.authQueues)
      )

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
     * Create a config-aware codec for AuthCase.
     */
    def codec(config: ChainConfig): Codec[AuthCase] =
      val stateCodec = AuthState.codec(config)

      (summon[Codec[AuthInput]] :: stateCodec :: stateCodec).xmap(
        { case (input, preState, postState) =>
          AuthCase(input, preState, postState)
        },
        authCase => (authCase.input, authCase.preState, authCase.postState)
      )

    given Decoder[AuthCase] =
      Decoder.instance { cursor =>
        for
          input <- cursor.get[AuthInput]("input")
          preState <- cursor.get[AuthState]("pre_state")
          postState <- cursor.get[AuthState]("post_state")
        yield AuthCase(input, preState, postState)
      }
