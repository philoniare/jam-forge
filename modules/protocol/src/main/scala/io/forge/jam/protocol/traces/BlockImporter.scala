package io.forge.jam.protocol.traces

import io.forge.jam.core.{ChainConfig, JamBytes, Hashing}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.block.Block
import io.forge.jam.protocol.safrole.SafroleTransition
import io.forge.jam.protocol.safrole.SafroleTypes.*

/**
 * Result of a block import operation.
 */
sealed trait ImportResult

object ImportResult:
  final case class Success(
    postState: RawState,
    computedFullState: FullJamState,
    safroleState: Option[SafroleState] = None
  ) extends ImportResult

  final case class Failure(
    error: ImportError,
    message: String = ""
  ) extends ImportResult

/**
 * Errors that can occur during block import.
 */
enum ImportError:
  case InvalidHeader
  case InvalidParent
  case InvalidSlot
  case InvalidStateRoot
  case SafroleError
  case AssuranceError
  case AuthorizationError
  case DisputeError
  case HistoryError
  case PreimageError
  case ReportError
  case StatisticsError
  case AccumulationError
  case UnknownError

/**
 * BlockImporter handles importing blocks and applying state transitions.
 *
 * 1. Safrole - Block production and VRF validation (includes Disputes)
 * 2. Assurances - Process availability assurances
 * 3. Reports - Process work reports (guarantees)
 * 4. Accumulation - Execute PVM accumulation
 * 5. History - Update recent blocks history
 * 6. Authorizations - Update authorization pools
 * 7. Preimages - Handle preimage provisioning
 * 8. Statistics - Update chain statistics
 */
class BlockImporter(config: ChainConfig = ChainConfig.TINY):

  /**
   * Imports a block and applies Safrole state transition.
   * Returns the computed post-state with updated state root.
   *
   * @param block The block to import
   * @param preState The state before the block (raw keyvals)
   * @return ImportResult indicating success with new state or failure with error
   */
  def importBlock(block: Block, preState: RawState): ImportResult =
    try
      // Step 1: Decode full pre-state from keyvals
      val fullPreState = FullJamState.fromKeyvals(preState.keyvals, config)

      // Step 2: Extract Safrole input from block
      val safroleInput = InputExtractor.extractSafroleInput(block, fullPreState.toSafroleState())

      // Step 3: Run Safrole STF
      val safrolePreState = fullPreState.toSafroleState()
      val (safrolePostState, safroleOutput) = SafroleTransition.stf(safroleInput, safrolePreState, config)

      if safroleOutput.err.isDefined then
        return ImportResult.Failure(
          ImportError.SafroleError,
          s"Safrole STF error: ${safroleOutput.err.get}"
        )

      // Step 4: Merge Safrole post-state back into full state
      val mergedState = fullPreState.withSafroleState(safrolePostState)

      // Step 5: Encode merged state back to keyvals
      val postKeyvals = mergedState.toKeyvals(config)

      // Add other keyvals that weren't decoded (non-Safrole components)
      val allPostKeyvals = postKeyvals ++ fullPreState.otherKeyvals

      // Step 6: Compute state root via Merkle trie
      val stateRoot = StateMerklization.stateMerklize(allPostKeyvals)

      val rawPostState = RawState(stateRoot, allPostKeyvals)

      ImportResult.Success(rawPostState, mergedState, Some(safrolePostState))
    catch
      case e: Exception =>
        e.printStackTrace()
        ImportResult.Failure(ImportError.UnknownError, e.getMessage)

  /**
   * Imports a block and returns just the computed SafroleState for comparison.
   * This is useful for trace testing where we want to compare typed state.
   */
  def importBlockForSafrole(block: Block, preState: RawState): (Option[SafroleState], Option[String]) =
    try
      importBlock(block, preState) match
        case ImportResult.Success(_, _, safroleState) => (safroleState, None)
        case ImportResult.Failure(_, message) => (None, Some(message))
    catch
      case e: Exception =>
        (None, Some(s"Exception: ${e.getMessage}"))

  /**
   * Validates that a block import produces the expected post-state.
   * Used for testing against trace vectors.
   */
  def validateBlockImport(
    block: Block,
    preState: RawState,
    expectedPostState: RawState
  ): Boolean =
    importBlock(block, preState) match
      case ImportResult.Success(actualPostState, _, _) =>
        // Compare computed post-state root with expected
        actualPostState.stateRoot == expectedPostState.stateRoot
      case ImportResult.Failure(_, _) =>
        false

/**
 * Extracts STF inputs from block and state.
 */
object InputExtractor:
  import io.forge.jam.core.types.tickets.TicketEnvelope

  /**
   * Extract SafroleInput from block and state.
   * The entropy source in the header is a VRF signature from which we extract the output.
   */
  def extractSafroleInput(block: Block, state: SafroleState): SafroleInput =
    val header = block.header
    val tickets = block.extrinsic.tickets

    // The header.entropySource is a 96-byte Bandersnatch IETF VRF signature.
    // For now, we extract the first 32 bytes as a simple approximation.
    // In production, this would use native crypto to extract the VRF output.
    val entropyBytes = header.entropySource.toArray
    val vrfOutput = if entropyBytes.length >= 32 then
      Hash(entropyBytes.take(32))
    else
      Hash(entropyBytes.padTo(32, 0.toByte))

    SafroleInput(
      slot = header.slot.value.toLong,
      entropy = vrfOutput,
      extrinsic = tickets
    )

  /**
   * Extract SafroleInput from block and FullJamState.
   */
  def extractSafroleInput(block: Block, state: FullJamState): SafroleInput =
    extractSafroleInput(block, state.toSafroleState())

/**
 * Encoder for converting typed state structures back to raw keyvals.
 */
object StateEncoder:
  // This would encode the full FullJamState back to keyvals.
  // For now, FullJamState.toKeyvals handles the Safrole-related state.
  // A complete implementation would encode all 16+ state components.
  def encodeFullState(state: FullJamState, config: ChainConfig = ChainConfig.TINY): List[KeyValue] =
    state.toKeyvals(config)
