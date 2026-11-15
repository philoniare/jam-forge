package io.forge.jam.protocol.traces


import io.forge.jam.protocol.state.RawState
import io.forge.jam.core.codec.Codec
import io.circe.{Decoder, HCursor}
import io.forge.jam.core.types.block.Block
import io.forge.jam.core.types.header.Header

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
  def codec(epochLength: Int, coresCount: Int): Codec[TraceStep] =
    (summon[Codec[RawState]] ::
     Block.blockCodec(epochLength, coresCount) ::
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
  def codec(epochLength: Int): Codec[Genesis] =
    (Header.headerCodec(epochLength) ::
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
