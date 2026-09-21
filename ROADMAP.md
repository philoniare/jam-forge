# Project Roadmap

## Vision

Build a production-grade JAM implementation in Scala 3 that provides:
- Complete protocol conformance with the Gray Paper specification
- Reduced code complexity through functional programming
- Type-safe API leveraging Scala's advanced type system
- Full compatibility with official JAM test vectors
- Integration with the JVM ecosystem

### Milestones

#### ✅ M1: IMPORTER (Completed)

State-transitioning conformance tests pass and can import blocks.

**Status:** **COMPLETED** ✅

#### 🟡 M2: AUTHORER (Node-side feature-complete; cross-client demonstration pending)

Fully conformant and can produce blocks (including networking and off-chain components).

**Status:** **NODE-SIDE FEATURE-COMPLETE** 🟡

**M2 checklist:**

- [x] CE 131/132 — Safrole ticket distribution, with proxy forwarding (`TicketService`)
- [x] CE 142/143 — inbound announce + fetch + serve wired; outbound announce
      (`PreimageService.announce`) awaits a local preimage-submission entrypoint
      (there is no such entrypoint today, which is why it isn't called from
      production code yet — see `PreimageFlowSpec` for coverage of the wired half)
- [x] CE 136 — work-report request (served from known-report cache)
- [x] CE 129 — state request, with `StateTrie.range` boundary-node support (node-level
      coverage in `StateRangeSpec` is fixture-gated on the `jam-conformance` submodule,
      which is SSH-gated, so it does not run in CI)
- [x] CE 147 — bundle request (served from guarantor's held-bundle cache)
- [x] CE 148 — segment request (`SegmentPool`)
- [x] CE 146 — builder bundle submission, accepted into the guarantee pipeline
- [x] Honest UP 0 fork-tree semantics — `ChainManager` tracks real leaves, pruned at finality
- [x] Honest UP 0 finality semantics — finalized head carries its real persisted state root; each peer's announced `Final` is tracked; devnet finality depth is a `ChainSpec` knob (a documented GRANDPA stand-in — implementing GRANDPA itself is an explicit non-goal, since jamnp-s defines no finality-vote streams)
- [ ] CE 131 proxy-send wiring in `JamNode.authorSlot` (currently passes `_ => None` for validator-index→connection lookup; broadcast fallback works, so tickets still reach every peer, but the direct-proxy path is unwired)

#### 📋 M3: HALF-SPEED (Planned)

Conformance and 50% of required performance.

#### 📋 M4: FULL-SPEED (Planned)

Conformance and 100% of required performance.

#### 📋 M5: SECURE (Planned)

Fully audited implementation.