# Architecture

## Overview

JamForge is a functional-first implementation of the JAM (Join-Accumulate Machine) protocol using Scala 3. The implementation prioritizes type safety, immutability, and clear separation of concerns through a modular architecture.

## Design Philosophy

### Functional Programming First
- Immutable data structures throughout
- Pure functions for core protocol logic
- Type classes for polymorphic behavior
- Algebraic data types (ADTs) for protocol types

### Type Safety
- Scala 3 GADTs for instruction encoding
- Opaque types for domain primitives
- Compile-time validation where possible
- Explicit error handling via Either/Option

### Code Simplicity
- Leverage Scala 3 features to reduce boilerplate
- ~70% fewer lines of code vs. imperative implementations
- Clear mapping between code and Gray Paper specification
- Minimal abstraction layers

## Module Structure

Eight sbt modules, with no cycles:

| Module | Depends on | Role |
|---|---|---|
| `jam-core` | | Types, codec, state trie and its backend interface, configuration |
| `jam-crypto` | core | Bandersnatch VRF, Ed25519, BLS and erasure coding over native libraries |
| `jam-pvm` | core | PolkaVM: interpreter, memory, native recompiler interface |
| `jam-protocol` | core, crypto, pvm | State transition functions, block pipeline, refinement |
| `jam-db` | core | RocksDB: state trie backend, block store, shard store |
| `jam-network` | core, crypto | JAMNP-S over QUIC |
| `jam-node` | core, crypto, protocol, network, db | The node: chain, sync, validator roles |
| `jam-conformance` | core, crypto, protocol | Fuzz target and trace runner |

`jam-node` and `jam-conformance` are the two applications. Each resolves the PVM settings
(`-Djam.pvm.*`), the RocksDB memory budget (`-Djam.db.*`) and the repository base directory (`-Djam.base.dir`)
once at startup and installs them (`ProcessSettings`); the libraries read the installed values. Tests and tools
that install nothing get the same values from the system properties at first use. The logging backend (logback)
is a dependency of the applications only; libraries have it for their tests.

### jam-core

**Purpose:** Foundation module providing core protocol types and serialization.

**Key Components:**
- **Types**: Block, Header, Extrinsic, WorkPackage, WorkReport, etc.
- **Codec System**: Codec type classes for binary serialization
- **Primitives**: Hash types, unsigned integers (via Spire)
- **Constants**: Protocol constants from Gray Paper
- **Configuration**: Chain configuration loading

**Package:** `io.forge.jam.core`

**Dependencies:**
- Spire (unsigned integers)
- Circe (JSON parsing)
- io.forge.jam.core.codec (binary encoding, in-house)

### jam-crypto

**Purpose:** Cryptographic operations required by the JAM protocol.

**Key Components:**
- **Bandersnatch VRF**: Ring VRF signatures via native Rust library
- **Ed25519**: Signature operations via native ed25519-zebra library
- **Erasure Coding**: Reed-Solomon coding via native Rust library
- **Hashing**: Blake2b, Keccak implementations

**Package:** `io.forge.jam.crypto`

**Dependencies:**
- jam-core
- Native libraries (Rust via JNI):
  - `libbandersnatch_vrfs_wrapper`
  - `libed25519_zebra_wrapper`
  - `liberasure_coding_wrapper`

**Native Library Build:**
Native libraries are automatically built by sbt during compilation using Cargo.

### jam-pvm

**Purpose:** PolkaVM virtual machine for executing work package code.

**Key Components:**
- **Opcode System**: RISC-V instruction set using Scala 3 GADTs
- **Memory Model**: Page-based memory with access control
- **Execution Engine**: Instruction handlers and VM state management
- **Program Loading**: Bytecode parsing from program binary format
- **Gas Metering**: Resource accounting for execution

**Package:** `io.forge.jam.pvm`

**Dependencies:**
- jam-core

### jam-protocol

**Purpose:** State transition functions (STFs), the block import pipeline, and work-package refinement.

**Packages** (under `io.forge.jam.protocol`):
- `safrole`, `dispute`, `assurance`, `report`, `accumulation`, `history`, `authorization`, `preimage`,
  `statistics`: one STF each. An STF has a pure `stfInternal` on its own case-class state (what the test vectors
  exercise) and a `stfView` that runs it against the trie-backed state.
- `pipeline`: `BlockPipeline`, which applies the STFs to a block in order, and the header checks.
- `importing`: `BlockImporter`, which runs the pipeline on a state and commits the result to the trie.
- `state`: the trie-backed state view (`TrieBackedJamState`), service storage staging, state keys and the full-state
  codec.
- `pvmhost`: what accumulation, refinement and is-authorized share to run guest code: the instance interface, the
  interpreter and native runners, host-call numbers, gas costs and result codes.
- `refine`: work-package validation, refinement and report computation.
- `traces`: the models of recorded traces (`TraceStep`, `Genesis`).

**Dependencies:**
- jam-core
- jam-crypto
- jam-pvm

### jam-db

**Purpose:** Persistence on RocksDB.

**Key Components:**
- `RocksDbTrieBackend`: the state trie's nodes and values (implements `StateTrieBackend` from jam-core), with
  per-block journals so replaced state can be pruned
- `BlockStore`: blocks, headers, the parent-to-children index and chain metadata, written in atomic batches
- `ShardStore`: erasure-coded shard custody
- `RocksDbMemory`: one memory budget shared by the stores of a process

**Package:** `io.forge.jam.db`

### jam-network

**Purpose:** JAMNP-S transport: QUIC (Netty incubator codec) with TLS 1.3 mutual authentication on Ed25519
certificates, and the UP/CE stream kinds.

**Package:** `io.forge.jam.network`

### jam-node

**Purpose:** The node: chain management, sync, and the validator roles.

**Key Components:**
- `ChainManager`: owns the canonical chain (import, side branches and reorgs, finality). It delegates to
  `ForkChoice` (branch weight, equivocation and validity marks), `HistoryPruner` (pruning behind finality),
  `UnverifiedBlocks` (caps on side-branch blocks whose seal cannot be checked) and `ChainMeta` (every metadata key
  in the block store and its encoding).
- `ChainReader`: the read-only view of the chain that the roles depend on.
- Roles: `BlockAuthor`, `GuarantorService`, `AssurerService`, `AuditorService`, `TicketService`,
  `PreimageService`; finality: `GrandpaService`, `BeefyService`; `SyncService`.
- `JamNode`: wires the above to the network and the slot clock; `PeerConnectivity` keeps its connections.

**Package:** `io.forge.jam.node`

### jam-conformance

**Purpose:** Protocol conformance testing: the fuzz target and the trace runner.

**Key Components:**
- **Fuzz server**: Unix domain socket server speaking the conformance protocol (`FuzzServer`, `ProtocolHandler`)
- **Trace runner**: replays JSON and binary traces and checks every state root
- **Warmup**: JVM warmup over recorded traces, with CRaC checkpoint/restore

**Package:** `io.forge.jam.conformance`

**Dependencies:**
- jam-core
- jam-crypto
- jam-protocol
- CRaC (checkpoint/restore)

## Data Flow

### Block Import Pipeline

1. **Block Receipt**
   - Receive block from network or test vector
   - Deserialize using codec type class

2. **Header Validation**
   - Verify seal signature
   - Check block ancestry
   - Validate VRF proofs (Safrole)

3. **State Transition Execution** (`BlockPipeline.execute`)
   ```
   Prior state
        ↓
   Safrole: epoch step, then the header's seal and markers against it, then tickets
        ↓
   Disputes
        ↓
   Assurances
        ↓
   Reports
        ↓
   Accumulation
        ↓
   History
        ↓
   Authorizations
        ↓
   Preimages
        ↓
   Statistics
        ↓
   Posterior state
   ```
   The pipeline is plain sequential code over one mutable state view: a step that finds the block invalid ends it
   with that step's error. Signature checks that decide only validity (seal, entropy VRF, ticket proofs, guarantee
   and assurance signatures) run on a shared pool while the steps continue, and are awaited before the import
   succeeds.

4. **State Root Verification**
   - Compute state merklization
   - Compare with block header

5. **Block Finalization**
   - Update chain state
   - Persist to storage

### Work Package Execution

1. **Work Package Receipt**
   - Extract from block extrinsic
   - Validate authorization

2. **PVM Execution**
   - Load work item bytecode into PolkaVM
   - Execute with gas metering
   - Handle host calls (service interactions)

3. **Work Report Generation**
   - Collect execution results
   - Generate availability chunks (erasure coding)
   - Produce work report with signatures

4. **Accumulation**
   - Process work reports in Accumulation STF
   - Finalize service state updates
   - Update validator statistics

## Codec System

The codec system provides type-safe binary serialization conforming to the JAM specification (GP 0.8.0 appendix C).
It lives in `io.forge.jam.core.codec` and is a small byte-array library with no third-party dependency.

### Codecs

`Codec[A]` writes into a growable `ByteWriter` and reads from a bounds-checked `ByteReader`, so composed codecs
allocate no intermediate buffers:

```scala
import io.forge.jam.core.codec.{ByteReader, ByteWriter, Codec}

trait Codec[A]:
  def write(out: ByteWriter, value: A): Unit
  def read(in: ByteReader): A

  def encode(value: A): Array[Byte]
  def decode(bytes: Array[Byte]): Either[String, A]                 // ignores trailing bytes
  def decodeExact(bytes: Array[Byte]): Either[String, A]            // rejects trailing bytes
  def decodeAt(bytes: Array[Byte], offset: Int): Either[String, (A, Int)]  // value and bytes consumed
```

Inside a codec, malformed input throws `CodecDecodingException`; the `decode*` entry points turn it into `Left`.

### Composition

Fields compose with `::` into tuples, mapped onto case classes with `xmap` or `as`:

```scala
import io.forge.jam.core.codec.codecs.*

given Codec[Block] = (Codec[Header] :: Codec[Extrinsic]).as[Block]
```

Hot per-block state components write their fields directly with `Codec.of((w, v) => ..., r => ...)`.

### JAM Codecs

`codecs` holds the primitives (`uint8`, `uint16L`, `uint32L`, `int64L`, `compactInt`, `compactBytes`, `bytes(n)`,
`option`, `fixedSizeList`, `compactPrefixedList`, ...) and `JamCodecs` the givens for JAM types (`Hash`, keys,
`UInt`, ...). The compact natural encoding (eq. C.6) is implemented once, in `ByteWriter.compact` and
`ByteReader.compact`, and rejects non-minimal encodings on decode.

## State Management

### State Representation

JAM state is represented as an immutable case class (`FullJamState`) containing:
- Block chain state (best block, finalized block)
- Validator epoch state
- Service accounts
- Authorization pool
- Preimage storage
- Pending reports and disputes
- Historical MMR

### State Updates

Each STF is a function from its pre-state to its post-state; block import applies them to a trie-backed state view (`TrieBackedJamState`) that re-encodes only the components a block changed:

```scala
val result: Either[PipelineError, BlockPipelineResult] = BlockPipeline.execute(block, view)
```

### State Merklization

State is merklized into a single root hash for verification:
- Each state component is hashed independently
- Hashes are combined into a Merkle tree
- Root hash included in block header

## Error Handling

### Result Types

Operations that can fail return `Either[Error, Result]`:
```scala
def transition(state: State, block: Block): Either[PipelineError, State]
```

### Error Categories

- **Codec Errors**: Deserialization failures
- **Validation Errors**: Invalid block/transaction data
- **Execution Errors**: PVM runtime failures
- **State Errors**: Invalid state transitions

### Error Propagation

Errors propagate through monadic composition:
```scala
for
  header <- decodeHeader(bytes)
  valid <- validateHeader(header)
  newState <- applyBlock(state, valid)
yield newState
```

## Performance Considerations

### JVM Optimizations

- JIT compilation for hot paths
- Efficient collection usage (Vector, LazyList)
- Minimal allocations in tight loops

### Native Library Performance

- Cryptographic operations in native code
- Zero-copy data passing where possible
- Batch operations for VRF and erasure coding

## Design Decisions

### Why Scala 3?

- **GADTs**: Perfect for instruction encoding with type-safe width parameters
- **Opaque Types**: Zero-cost abstractions for domain types
- **Type Classes**: Extensible encoding/decoding without runtime overhead
- **Inline Functions**: Compile-time code generation for performance
- **Functional Core**: Immutability and pure functions match protocol specification

### Why Native Crypto?

- Avoid reimplementation bugs in complex cryptography
- Leverage existing audited Rust implementations
- Performance benefits of native code

### Why Immutable State?

- Thread safety without locks
- Easier reasoning about state transitions
- Natural fit with functional programming
- Simplified testing and debugging
