# 🚀 JAM-Forge

> A Scala 3 implementation of the JAM (Join-Accumulate Machine) protocol - a potential successor to the Polkadot Relay chain

[![Scala Version](https://img.shields.io/badge/scala-3.3.7-red.svg)](https://www.scala-lang.org/)
[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)
[![Run Tests](https://github.com/philoniare/jam-forge/actions/workflows/run-tests.yml/badge.svg)](https://github.com/philoniare/jam-forge/actions/workflows/run-tests.yml)
[![codecov](https://codecov.io/gh/philoniare/jam-forge/branch/main/graph/badge.svg)](https://codecov.io/gh/philoniare/jam-forge)

---

## 📖 Overview

JAM-Forge is an idiomatic Scala 3 implementation of the **JAM (Join-Accumulate Machine)** protocol, as specified in the [Gray Paper](https://graypaper.com/). This implementation leverages functional programming patterns, advanced type systems, and the JVM ecosystem to provide a robust, type-safe, and maintainable blockchain protocol client.

### ✨ Key Highlights

- 🎯 **Functional-First Design**: Immutable data structures, type classes, and algebraic data types
- 🔒 **Type Safety**: Compile-time guarantees using Scala 3's advanced type system
- 📦 **Modular Architecture**: Clean separation between core types, PVM, cryptography, and protocol logic
- ✅ **Test Vector Compliant**: Full compatibility with official JAM test vectors
- 🔧 **JVM Ecosystem**: Seamless integration with existing Java/Scala tooling and infrastructure

---

## 🏗️ Architecture

JAM-Forge is organized into eight core modules:

### 📦 Modules

| Module | Description | Package |
|--------|-------------|---------|
| **jam-core** 🔷 | Core JAM protocol types and binary encoding/decoding | `io.forge.jam.core` |
| **jam-crypto** 🔐 | Cryptographic operations (Bandersnatch VRF, Ed25519, Erasure Coding) | `io.forge.jam.crypto` |
| **jam-pvm** 🖥️ | PolkaVM implementation (RISC-V virtual machine) | `io.forge.jam.pvm` |
| **jam-protocol** 📜 | State transition functions (Safrole, Statistics, Accumulation, etc.) | `io.forge.jam.protocol` |
| **jam-network** 🌐 | JAMNP-S networking (QUIC transport, all stream kinds) | `io.forge.jam.network` |
| **jam-db** 🗄️ | Persistent storage (RocksDB-backed state and block stores) | `io.forge.jam.db` |
| **jam-node** 🖧 | Full node: authoring, guaranteeing, assuring, auditing, sync | `io.forge.jam.node` |
| **jam-conformance** 🧪 | Conformance testing server for cross-implementation validation | `io.forge.jam.conformance` |

---

## 🚀 Getting Started

### 📋 Prerequisites

- ☕ **JDK 24**
- 🦀 **Rust & Cargo** (for building native cryptographic libraries)
- 📦 **sbt 1.9+** (Scala build tool)

### 🔧 Installation

1. **Clone the repository:**
   ```bash
   git clone https://github.com/philoniare/jam-forge.git
   cd jam-forge
   ```

2. **Initialize submodules:**
   ```bash
   git submodule update --init --recursive
   ```

3. **Build the project:**
   ```bash
   sbt compile
   ```

   Native cryptographic libraries (Bandersnatch VRF, Ed25519-Zebra, Erasure Coding) will be automatically built on first compilation.

---

## 🔨 Build Commands

```bash
# 🏗️ Build all modules
sbt compile

# 🧪 Run all tests
sbt test

# 📊 Run tests for a specific module
sbt "core/test"
sbt "protocol/test"
sbt "crypto/test"

# 🎯 Run a single test class
sbt "core/testOnly io.forge.jam.core.ShuffleTest"

# 📈 Run tests with code coverage
sbt clean coverage test coverageAggregate

# 🧹 Clean build artifacts
sbt clean

# 📦 Build conformance server JAR
sbt "conformance/assembly"

# ⚡ Run benchmarks
sbt benchmark
```

---

## 🖧 Running a Node

Usage: `jam-node [-c <config path>] [--chain <chain>] <command>`, with the commands `run`, `gen-keys`, `list-keys`,
`dump-spec` and `help`. The image is `ghcr.io/philoniare/jam-forge-node`; to follow the public jamtoaster testnet:

```bash
docker run -d --name jam-forge --network host -v jam-forge-data:/data \
  ghcr.io/philoniare/jam-forge-node:testnet --chain toaster run --port 40000
```

To run a validator, keep its seed in the keystore and name it by its peer ID:

```bash
docker run --rm -v jam-forge-data:/data ghcr.io/philoniare/jam-forge-node:testnet --chain toaster gen-keys validator
# prints "Peer ID: e…" and "Bandersnatch key: …"
docker run -d --name jam-forge --network host -v jam-forge-data:/data \
  ghcr.io/philoniare/jam-forge-node:testnet --chain toaster run --port 40000 --peer-id e… --finality-mode grandpa
```

- `--chain` is `polkadot` (the default, a placeholder without validators yet), `toaster` or `toaster:<network>`
  (fetched from `https://jamtoaster.network/chainspec/<network>.json`, `public` by default), `dev` or
  `dev:<base port>` (the dev chain, validator i at port 40000 + i or base port + i), or a spec file.
  `dump-spec <file>` writes the spec a `--chain` names.
- A chain's data lives in `<data path>/<chain id>` (`run -d`, default `~/.local/share/jam-forge`, in the image
  `/data/jam-forge`) and its keystore in `<config path>/<chain id>/keys` (`-c`, default `~/.config/jam-forge`).
  Seeds are 32-byte `*.seed` files. `gen-keys [name]` writes a new one (and never replaces an existing file);
  `list-keys` prints each seed's peer ID and Bandersnatch key; `run --key-seed-file <file>` names more.
- `run --peer-id` makes the node the validator whose seed has that peer ID (its Ed25519 key is the network
  identity); `--dev-validator i` runs dev validator i with its address and port from the genesis state; `--mode
  ordinary` follows the chain with the identity `--peer-id` names. Without `--peer-id` an ordinary node takes a new
  identity each start. Proxy mode (`--mode proxy`, `--proxy`) is not implemented.
- Finality: `--finality-mode dummy` (the default: each new best block finalizes its parent, without
  consensus) or `grandpa`. `--finality-peers N` and `--announced-finality` take finality from peers' announcements
  instead.
- The RPC (JIP-2) runs on `[::]:19800` unless the node is a validator (`--rpc` to enable it there) or `--no-rpc` is
  given; `--rpc-port` and `--rpc-listen-ip` move it. Submission and segment recovery are served to loopback clients
  only unless `--rpc-methods unsafe` (`safe` refuses them to everyone); `--rpc-allow-origin <origin>` lets a web page
  use it.
- `--port` (UDP, default any free port), `--listen-ip` (default `::`), `--external-ip` (the address peers reach the
  node at behind NAT), `--bootnode <peer id>@<host>:<port>`, `--telemetry <host:port>` (JIP-3), `--pvm-backend
  interpreter|compiler` (default: the compiler, our recompiler, where available) and `-t`/`--temp` (a throwaway data
  directory).
- Also: `--sync-from <header hash>[:<set id>]` joins at a trusted checkpoint; `--prune-history`;
  `--metrics-port 9615 --metrics-listen-ip 0.0.0.0` serves Prometheus metrics at `/metrics` and a health check at
  `/health` (loopback only without `--metrics-listen-ip`); `--bandersnatch-key` and `--ed25519-key` (32-byte JIP-5
  secret seeds, as hex or a file) replace those keys of the validator's seed. `jam-node run --help` lists them all.
- The image runs as uid/gid 10001, which owns `/data`. A volume an earlier image (which ran as root) wrote must be
  handed over once: `docker run --rm -u 0 -v jam-forge-data:/data --entrypoint chown <image> -R 10001:10001 /data`.

---

## 🧪 Testing

### Test Vectors

JAM-Forge uses official test vectors from the `jamtestvectors` submodule to validate correctness:

- **tiny** (6 validators): Fast testing configuration
- **full** (1023 validators): Production-scale configuration

### Code Coverage

JAM-Forge uses [scoverage](https://github.com/scoverage/sbt-scoverage) for code coverage analysis:

```bash
# Run tests with coverage
sbt clean coverage test coverageAggregate

# View HTML report
open target/scala-3.3.7/scoverage-report/index.html
```

**Coverage targets**: 70% statement coverage | Excludes: benchmark code

---

## 🔐 Cryptography

JAM-Forge integrates native Rust libraries for cryptographic operations:

| Library | Purpose | Wrapper |
|---------|---------|---------|
| 🔑 **Bandersnatch VRF** | Ring VRF signatures for Safrole | `bandersnatch-vrfs-wrapper` |
| ✍️ **Ed25519-Zebra** | Ed25519 signatures | `ed25519-zebra-wrapper` |
| 🧩 **Erasure Coding** | Data availability encoding | `erasure-coding-wrapper` |

All native libraries are built automatically during compilation and loaded via JNI.

---

## 📚 Documentation

For full project documentation, visit **[jamforge.xyz](http://jamforge.xyz/)**

### Core Concepts

- **JAM Codec**: Type-class based binary serialization over byte arrays (`io.forge.jam.core.codec`)
- **State Transition Functions**: Implementations of Safrole, Statistics, Accumulation, and other STF components
- **PolkaVM**: RISC-V virtual machine with gas metering and full instruction set support

---

## 🌟 Features

### ✅ Implemented

- ✅ Complete JAM binary codec for all protocol types
- ✅ PolkaVM virtual machine with RISC-V instruction set
- ✅ State transition functions (Safrole, Statistics, History, etc.)
- ✅ Bandersnatch VRF integration
- ✅ Erasure coding for data availability
- ✅ Test vector compliance (tiny & full configurations)
- ✅ Conformance testing server

### 🚧 In Progress

- 🚧 Full node implementation
- 🚧 Networking layer
- 🚧 Block production and validation
- 🚧 P2P gossip protocol

---

## 📜 License

This project is licensed under the Apache 2.0 License - see the [LICENSE](LICENSE) file for details.

---

## 🙏 Acknowledgments

- The JAM protocol specification team
- The Polkadot ecosystem community

---

## 💳 Addresses

- **Polkadot Asset Hub:** `15tgSQA8ge7S5jZDevXPdiTXjRV1toyD57XSJvLYqSVQbTAg`
- **Kusama Asset Hub:** `HTzxPEwTDrtPrN9TzHSPWzP2Pmc1BEFSzdhYHd9m9gP9vuF`

---

<div align="center">
  <strong>Built with ❤️ using Scala 3</strong>
  <br>
  <sub>For the decentralized future 🌐</sub>
</div>
