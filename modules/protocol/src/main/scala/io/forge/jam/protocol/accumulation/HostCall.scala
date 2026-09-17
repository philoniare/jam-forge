package io.forge.jam.protocol.accumulation

/**
 * Host call identifiers for accumulation.
 */
object HostCall:

  // ===========================================================================
  // General Host Calls (0-13)
  // ===========================================================================

  /** gas (0): Returns remaining gas in register r7 */
  val GAS: Int = 0

  val GROW_HEAP: Int = 1

  /** fetch (2): Fetch various data based on selector */
  val FETCH: Int = 2

  /** lookup (3): Look up preimage by hash */
  val LOOKUP: Int = 3

  /** read (4): Read from service storage */
  val READ: Int = 4

  /** write (5): Write to service storage */
  val WRITE: Int = 5

  /** info (6): Get service account info (96 bytes) */
  val INFO: Int = 6

  // ===========================================================================
  // Refine-Only Host Calls (7-14)
  // ===========================================================================

  /** historical_lookup (7): Look up historical preimage data */
  val HISTORICAL_LOOKUP: Int = 7

  /** export (8): Export a segment to the work report output */
  val EXPORT: Int = 8

  /** machine (9): Create a new inner PVM instance */
  val MACHINE: Int = 9

  /** peek (10): Read memory from an inner PVM instance */
  val PEEK: Int = 10

  /** poke (11): Write memory to an inner PVM instance */
  val POKE: Int = 11

  /** pages (12): Modify page access rights of an inner PVM instance */
  val PAGES: Int = 12

  /** invoke (13): Execute an inner PVM instance */
  val INVOKE: Int = 13

  /** expunge (14): Remove an inner PVM instance */
  val EXPUNGE: Int = 14

  // ===========================================================================
  // Accumulate-Specific Host Calls (15-27)
  // ===========================================================================

  /** bless (15): Set privileged services (manager, assigners, delegator, registrar, always-acc) */
  val BLESS: Int = 15

  /** assign (16): Set core assigner and authorization queue (privileged) */
  val ASSIGN: Int = 16

  /** designate (17): Set validator queue (privileged) */
  val DESIGNATE: Int = 17

  /** checkpoint (18): Save current state x to checkpoint y */
  val CHECKPOINT: Int = 18

  /** new (19): Create new service account */
  val NEW: Int = 19

  /** upgrade (20): Upgrade service code hash */
  val UPGRADE: Int = 20

  /** transfer (21): Queue a deferred transfer */
  val TRANSFER: Int = 21

  /** eject (22): Eject (remove) another service account */
  val EJECT: Int = 22

  /** query (23): Query preimage request status */
  val QUERY: Int = 23

  /** solicit (24): Request a preimage */
  val SOLICIT: Int = 24

  /** forget (25): Forget a preimage request */
  val FORGET: Int = 25

  /** yield (26): Set accumulation output hash */
  val YIELD: Int = 26

  /** provide (27): Provide a preimage for another service */
  val PROVIDE: Int = 27

  // ===========================================================================
  // Debug Host Call
  // ===========================================================================

  /** log (100): Debug logging (JIP-1) */
  val LOG: Int = 100

  /** Get human-readable name for host call ID */
  def name(id: Int): String = id match
    case GAS => "GAS"
    case GROW_HEAP => "GROW_HEAP"
    case FETCH => "FETCH"
    case LOOKUP => "LOOKUP"
    case READ => "READ"
    case WRITE => "WRITE"
    case INFO => "INFO"
    case HISTORICAL_LOOKUP => "HISTORICAL_LOOKUP"
    case EXPORT => "EXPORT"
    case MACHINE => "MACHINE"
    case PEEK => "PEEK"
    case POKE => "POKE"
    case PAGES => "PAGES"
    case INVOKE => "INVOKE"
    case EXPUNGE => "EXPUNGE"
    case BLESS => "BLESS"
    case ASSIGN => "ASSIGN"
    case DESIGNATE => "DESIGNATE"
    case CHECKPOINT => "CHECKPOINT"
    case NEW => "NEW"
    case UPGRADE => "UPGRADE"
    case TRANSFER => "TRANSFER"
    case EJECT => "EJECT"
    case QUERY => "QUERY"
    case SOLICIT => "SOLICIT"
    case FORGET => "FORGET"
    case YIELD => "YIELD"
    case PROVIDE => "PROVIDE"
    case LOG => "LOG"
    case _ => s"UNKNOWN($id)"
