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

The codebase is organized into five main modules with clear dependencies:

```
┌─────────────────────────────────────────────────────┐
│                  jam-conformance                    │
│         (Conformance Server & Testing)              │
└─────────────────┬───────────────────────────────────┘
                  │
┌─────────────────▼───────────────────────────────────┐
│                  jam-protocol                       │
│   (State Transition Functions & Block Pipeline)     │
└─────────┬───────────────┬───────────────────────────┘
          │               │
┌─────────▼──────┐  ┌────▼────────────────────────────┐
│   jam-crypto   │  │          jam-pvm                │
│  (Cryptography)│  │    (PolkaVM Execution)          │
└────────┬───────┘  └────┬────────────────────────────┘
         │               │
         └───────┬───────┘
                 │
         ┌───────▼──────────────────────────────────┐
         │           jam-core                       │
         │  (Types, Encoding, Constants)            │
         └──────────────────────────────────────────┘
```

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
- Cats (functional abstractions)

### jam-protocol

**Purpose:** State transition functions (STFs) and block import pipeline.

**Key Components:**
- **State Transition Functions:**
  - Safrole (block production)
  - Assurances (availability attestation)
  - Authorizations (service authorization)
  - Reports (work report processing)
  - Disputes (validator disagreements)
  - History (block ancestry tracking)
  - Preimages (data storage)
  - Statistics (validator metrics)
  - Accumulation (work result finalization)

- **Block Pipeline**: End-to-end block import and validation
- **State Management**: Full JAM state representation and merklization

**Package:** `io.forge.jam.protocol`

**Dependencies:**
- jam-core
- jam-crypto
- jam-pvm
- Monocle (lens-based state updates)

### jam-conformance

**Purpose:** Protocol conformance testing and validation server.

**Key Components:**
- **Conformance Server**: Unix domain socket server for protocol testing
- **Block Importer**: Processes test vectors and validates state transitions
- **State Codec**: Encoding/decoding of full JAM state
- **Fuzzing Support**: Integration with jam-conformance fuzzing tools

**Package:** `io.forge.jam.conformance`

**Dependencies:**
- jam-core
- jam-crypto
- jam-protocol
- FS2 (streaming and Unix socket IO)
- Cats Effect (async/effect management)

## Data Flow

### Block Import Pipeline

1. **Block Receipt**
   - Receive block from network or test vector
   - Deserialize using codec type class

2. **Header Validation**
   - Verify seal signature
   - Check block ancestry
   - Validate VRF proofs (Safrole)

3. **State Transition Execution**
   ```
   Initial State
        ↓
   Safrole STF (block production)
        ↓
   Disputes STF (validator disputes)
        ↓
   Assurances STF (availability)
        ↓
   Reports STF (work reports)
        ↓
   Accumulation STF (finalization)
        ↓
   History STF (ancestry)
        ↓
   Authorizations STF (service auth)
        ↓
   Preimages STF (data storage)
        ↓
   Statistics STF (metrics)
        ↓
   Final State
   ```

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

State transitions use lenses (Monocle) for type-safe updates:

```scala
val updatedState = SafroleTransition.transition(
  currentState,
  newBlock,
  config
)
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

### Future Optimizations

- Parallel STF execution where independent
- Incremental state merklization
- Caching of frequently accessed state

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

## Future Architecture Evolution

### Planned Improvements

1. **Parallel STF Execution**: Execute independent STFs concurrently
2. **Incremental Merklization**: Only recompute changed state components
3. **Storage Abstraction**: Pluggable storage backends (RocksDB)
4. **Network Layer**: P2P networking for full node operation
5. **RPC Interface**: JSON-RPC API for client applications