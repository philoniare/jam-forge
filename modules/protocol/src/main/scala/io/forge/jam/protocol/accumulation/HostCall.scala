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
  // Refine-Only Host Calls (7-15).
  // ===========================================================================

  /** historical_lookup (7): Look up historical preimage data */
  val HISTORICAL_LOOKUP: Int = 7

  /** export (8): Export a segment to the work report output */
  val EXPORT: Int = 8

  /** compile (9): Store a program module, from memory or a preimage */
  val COMPILE: Int = 9

  /** machine (10): Create a new inner PVM instance from a compiled module */
  val MACHINE: Int = 10

  /** peek (11): Read memory from an inner PVM instance */
  val PEEK: Int = 11

  /** poke (12): Write memory to an inner PVM instance */
  val POKE: Int = 12

  /** pages (13): Modify page access rights of an inner PVM instance */
  val PAGES: Int = 13

  /** invoke (14): Execute an inner PVM instance */
  val INVOKE: Int = 14

  /** expunge (15): Remove an inner PVM instance or a compiled module */
  val EXPUNGE: Int = 15

  // ===========================================================================
  // Accumulate-Specific Host Calls (16-28)
  // ===========================================================================

  /** bless (16): Set privileged services (manager, assigners, delegator, registrar, always-acc) */
  val BLESS: Int = 16

  /** assign (17): Set core assigner and authorization queue (privileged) */
  val ASSIGN: Int = 17

  /** designate (18): Set validator queue (privileged) */
  val DESIGNATE: Int = 18

  /** checkpoint (19): Save current state x to checkpoint y */
  val CHECKPOINT: Int = 19

  /** new (20): Create new service account */
  val NEW: Int = 20

  /** upgrade (21): Upgrade service code hash */
  val UPGRADE: Int = 21

  /** transfer (22): Queue a deferred transfer */
  val TRANSFER: Int = 22

  /** eject (23): Eject (remove) another service account */
  val EJECT: Int = 23

  /** query (24): Query preimage request status */
  val QUERY: Int = 24

  /** solicit (25): Request a preimage */
  val SOLICIT: Int = 25

  /** forget (26): Forget a preimage request */
  val FORGET: Int = 26

  /** yield (27): Set accumulation output hash */
  val YIELD: Int = 27

  /** provide (28): Provide a preimage for another service */
  val PROVIDE: Int = 28

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
    case COMPILE => "COMPILE"
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
