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

- ☕ **Java 21+** (required for JVM)
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

- **JAM Codec**: Type-class based binary serialization using `scodec`
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

<div align="center">
  <strong>Built with ❤️ using Scala 3</strong>
  <br>
  <sub>For the decentralized future 🌐</sub>
</div>
