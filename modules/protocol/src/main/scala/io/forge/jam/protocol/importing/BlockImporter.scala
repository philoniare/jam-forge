package io.forge.jam.protocol.importing


import io.forge.jam.protocol.state.{KeyValue, RawState}
import io.forge.jam.protocol.pipeline.StageTimers

import io.forge.jam.core.{ChainConfig, JamBytes}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.block.Block
import io.forge.jam.protocol.report.ReportTypes.*
import io.forge.jam.protocol.pipeline.{BlockPipeline, PipelineError}
import io.forge.jam.protocol.state.{ServiceStorageView, TrieBackedJamState}
import io.forge.jam.core.trie.{StateTrieStore, InMemoryTrieBackend}
import org.slf4j.LoggerFactory

sealed trait ImportResult

object ImportResult:
  /** @param headerHash the imported block's header hash, for callers that index blocks by it */
  final case class Success(
      postStateRoot: Hash,
      headerHash: Hash
  )(
      val accumulateCosts: Map[Long, io.forge.jam.protocol.pvmhost.AccumulateCost] = Map.empty
  ) extends ImportResult

  /** The block was checked and is invalid: importing it again gives the same answer. */
  final case class Invalid(
      error: ImportError,
      message: String = "",
      cause: Option[PipelineError] = None
  ) extends ImportResult

  /** The import itself failed (an exception of ours). It says nothing about the block, which may import on a later
    * try.
    */
  final case class Failed(message: String, cause: Throwable) extends ImportResult

/** Why a block is invalid.
  */
enum ImportError:
  case InvalidHeader
  case InvalidStateRoot
  case SafroleError
  case AssuranceError
  case DisputeError
  case PreimageError
  case ReportError
  case SignatureError
  case AccumulationError

/** BlockImporter handles importing blocks and applying all state transitions.
  *
  * Uses a unified JamState pipeline where state flows sequentially through all
  * 9 STFs:
  *   1. Safrole - Block production and VRF validation
  *   2. Disputes - Process dispute verdicts
  *   3. Assurances - Process availability assurances
  *   4. Reports - Process work reports (guarantees)
  *   5. Accumulation - Execute PVM accumulation
  *   6. History - Update recent blocks history
  *   7. Authorizations - Update authorization pools
  *   8. Preimages - Handle preimage provisioning
  *   9. Statistics - Update chain statistics
  *
  * @param config
  *   The chain configuration
  * @param skipAncestryValidation
  *   When true, skip anchor recency validation in Reports STF.
  */
class BlockImporter(
    config: ChainConfig,
    skipAncestryValidation: Boolean = false,
    externalTrieStore: Option[StateTrieStore] = None,
    gcAfterImport: Boolean = true
):
  private val logger = LoggerFactory.getLogger(getClass)

  // Shared PVM module cache across block imports to avoid recompiling same service code.
  private val sharedExecutor =
    new io.forge.jam.protocol.accumulation.AccumulationExecutor(config)

  // Re-entrancy guard for the single-threaded-per-instance contract on importBlock.
  private val importInFlight = new java.util.concurrent.atomic.AtomicBoolean(false)

  // Held while the trie backend is changed (bootstrap, save, gc) and while materializePostState walks it, so a walk
  // run alongside a later import never sees a backend in mid-update.
  private val backendLock = new Object

  private val trieStore: StateTrieStore =
    externalTrieStore.getOrElse(new StateTrieStore(new InMemoryTrieBackend))

  private val ownsStore: Boolean = externalTrieStore.isEmpty

  def store: StateTrieStore = trieStore

  def currentTrieRoot: Hash = trieStore.currentRoot

  /** Imports a block and applies all state transitions using the unified
    * JamState pipeline. Returns the computed post-state with updated state
    * root.
    *
    * @param block
    *   The block to import
    * @param preState
    *   The state before the block (raw keyvals)
    * @return
    *   ImportResult indicating success with new state or failure with error
    */
  def importBlock(
      block: Block,
      preState: RawState,
      ancestry: Ancestry = Ancestry.none,
      extrinsicHashChecked: Boolean = false
  ): ImportResult =
    if !importInFlight.compareAndSet(false, true) then
      throw new IllegalStateException(
        "BlockImporter.importBlock invoked concurrently on the same instance; " +
          "callers must serialize imports per instance (e.g. via synchronized)"
      )
    try StageTimers.time("total")(importBlockUnguarded(block, preState, ancestry, extrinsicHashChecked))
    finally importInFlight.set(false)

  private def importBlockUnguarded(
      block: Block,
      preState: RawState,
      ancestry: Ancestry,
      extrinsicHashChecked: Boolean
  ): ImportResult =
    try
      if block.header.parentStateRoot != preState.stateRoot then
        return ImportResult.Invalid(
          ImportError.InvalidStateRoot,
          "Block's parent_state_root does not match pre-state root"
        )

      if trieStore.currentRoot != block.header.parentStateRoot then
        if ownsStore then
          backendLock.synchronized(trieStore.bootstrap(preState.keyvals.map(kv => (kv.key, kv.value))))
        else
          val parentRoot = block.header.parentStateRoot
          val repinned = backendLock.synchronized {
            val held = parentRoot == Hash.zero || trieStore.backend.readNode(parentRoot).isDefined
            if held then trieStore.markCommitted(parentRoot)
            held
          }
          if !repinned then
            val message = s"parent state ${parentRoot.toHex.take(18)} is not in the state store " +
              s"(pinned at ${trieStore.currentRoot.toHex.take(18)})"
            logger.warn(s"block import failed: $message")
            return ImportResult.Failed(message, new IllegalStateException(message))
          logger.warn(s"state store was pinned elsewhere; re-pinned at parent state ${parentRoot.toHex.take(18)}")
        if trieStore.currentRoot != block.header.parentStateRoot then
          return ImportResult.Invalid(
            ImportError.InvalidStateRoot,
            s"Bootstrapped state root ${trieStore.currentRoot} does not match " +
              s"block parent_state_root ${block.header.parentStateRoot}"
          )
      val trie = StageTimers.time("trie-at")(trieStore.at(block.header.parentStateRoot))

      val storageView = new ServiceStorageView(trie)
      val view = new TrieBackedJamState(trie, config, storageView, Some(trieStore))
      finish(block, trie, view) { whileChecksRun =>
        BlockPipeline.execute(
          block,
          view,
          skipAncestryValidation,
          Some(sharedExecutor),
          ancestry,
          whileChecksRun,
          extrinsicHashChecked
        )
      }
    catch
      case e: Throwable if BlockImporter.isFatalCrash(e) =>
        throw e
      case e: Throwable =>
        logger.error("Unexpected error during block import", e)
        ImportResult.Failed(Option(e.getMessage).getOrElse(e.getClass.getSimpleName), e)

  private def finish(block: Block, trie: io.forge.jam.core.trie.StateTrie, view: TrieBackedJamState)(
      pipeline: (io.forge.jam.protocol.pipeline.BlockPipelineResult => Unit) => Either[
        PipelineError,
        io.forge.jam.protocol.pipeline.BlockPipelineResult
      ]
  ): ImportResult =
    val storageView = view.storage
    // The state update for a block whose steps passed: every write hashed into the block's own trie handle. It
    // runs while the block's signature checks finish on other threads and touches nothing shared; the handle is
    // saved below only once those checks have passed.
    def stageUpdates(): Unit =
      // One update for state and storage writes, so nodes on shared paths are rebuilt and hashed once.
      // A key in both takes the storage view's value, as when they were applied one after the other.
      val updates = StageTimers.time("commit-encode") {
        val u = view.commitUpdates()
        u ++= storageView.commitUpdates()
        u
      }
      StageTimers.time("trie-update")(trie.update(updates.toIndexedSeq))

    StageTimers.time("pipeline")(pipeline(_ => stageUpdates())) match
      case Left(error) =>
        ImportResult.Invalid(mapPipelineError(error), error.message, Some(error))

      case Right(result) =>
        backendLock.synchronized {
          StageTimers.time("trie-save")(trie.save())
          trieStore.commitChild(block.header.parentStateRoot, trie.rootHash, view.accountRecordChanges)
          if gcAfterImport then StageTimers.time("trie-gc")(trieStore.gc())
        }

        ImportResult.Success(trie.rootHash, result.headerHash)(result.accumulateCosts)

  def open(root: Hash): Option[BlockImporter.OpenState] =
    try
      if trieStore.currentRoot != root then None
      else
        val trie = trieStore.at(root)
        Some(new BlockImporter.OpenState(root, trie, new TrieBackedJamState(trie, config, new ServiceStorageView(trie), Some(trieStore))))
    catch case _: IllegalStateException => None

  def importExecuted(block: Block, executed: BlockImporter.Executed): ImportResult =
    if !importInFlight.compareAndSet(false, true) then
      throw new IllegalStateException("BlockImporter.importExecuted invoked while another import is in flight")
    try
      StageTimers.time("total") {
        val state = executed.state
        if state.root != block.header.parentStateRoot || trieStore.currentRoot != state.root then
          val message = "the state the block was built on is not the importer's"
          ImportResult.Failed(message, new IllegalStateException(message))
        else
          try
            finish(block, state.trie, state.view) { whileChecksRun =>
              BlockPipeline.executeAfterReports(
                block,
                state.view,
                executed.priorSlot,
                executed.safrole,
                executed.availableReports,
                Some(sharedExecutor),
                whileChecksRun
              )
            }
          catch
            case e: Throwable if BlockImporter.isFatalCrash(e) => throw e
            case e: Throwable =>
              logger.error("Unexpected error during the import of an authored block", e)
              ImportResult.Failed(Option(e.getMessage).getOrElse(e.getClass.getSimpleName), e)
      }
    finally importInFlight.set(false)

  /** Maps pipeline errors to import errors.
    */
  private def mapPipelineError(error: PipelineError): ImportError = error match
    case PipelineError.SafroleErr(_)            => ImportError.SafroleError
    case PipelineError.DisputeErr(_)            => ImportError.DisputeError
    case PipelineError.AssuranceErr(_)          => ImportError.AssuranceError
    case PipelineError.ReportErr(_)             => ImportError.ReportError
    case PipelineError.PreimageErr(_)           => ImportError.PreimageError
    case PipelineError.HeaderErr(_)             => ImportError.InvalidHeader
    case PipelineError.InvalidOffendersMark     => ImportError.InvalidHeader
    case PipelineError.BadSignatureBatch        => ImportError.SignatureError
    case PipelineError.NewServiceIdConflict(_)  => ImportError.AccumulationError

  /** The state at the last imported root. Safe to run alongside a later import, though it then reads that import's
    * state if the import commits first; callers that need a given block's state must order the two themselves.
    */
  def materializePostState(): RawState =
    backendLock.synchronized {
      val root = trieStore.currentRoot
      val pairs = trieStore.at(root).getKeyValues(JamBytes.empty, 0)
      RawState(root, pairs.map { case (k, v) => KeyValue(k, v) })
    }

object BlockImporter:
  def isFatalCrash(t: Throwable): Boolean = t.isInstanceOf[VirtualMachineError]

  final class OpenState private[importing] (
      val root: Hash,
      private[importing] val trie: io.forge.jam.core.trie.StateTrie,
      val view: TrieBackedJamState
  )

  final case class Executed(
      state: OpenState,
      priorSlot: Long,
      safrole: io.forge.jam.protocol.safrole.SafroleTypes.SafroleOutputData,
      availableReports: List[io.forge.jam.core.types.workpackage.WorkReport]
  )
