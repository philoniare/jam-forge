# Development Guide

## Prerequisites

### Required

- **Java Development Kit (JDK) 24** 
  ```bash
  # Check Java version
  java -version

  # Install via SDKMAN (recommended)
  curl -s "https://get.sdkman.io" | bash
  sdk install java 24.0.2-tem
  ```

- **Scala Build Tool (sbt) 1.9+**
  ```bash
  # macOS
  brew install sbt

  # Linux
  curl -fL https://github.com/sbt/sbt/releases/download/v1.9.7/sbt-1.9.7.tgz | tar xz

  # Check version
  sbt --version
  ```

- **Rust toolchain (for native libraries)**
  ```bash
  # Install Rust
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

  # Check installation
  cargo --version
  rustc --version
  ```

## Getting Started

### Clone the Repository

```bash
# Clone with submodules (includes graypaper and jamtestvectors)
cd /jam
git submodule update --init --recursive
```

### Initial Build

The first build will:
1. Download Scala compiler and dependencies
2. Build native Rust libraries (Bandersnatch VRF, Ed25519, Erasure Coding)
3. Compile all Scala modules

```bash
# Full build
sbt compile

# Build and run all tests
sbt test
```
## Project Structure

```
jam/
├── build.sbt                 # Main build configuration
├── project/                  # sbt build definitions
│   ├── build.properties      # sbt version
│   └── plugins.sbt           # sbt plugins
├── modules/
│   ├── core/                 # Core types and encoding
│   │   └── src/
│   │       ├── main/scala/   # Source code
│   │       └── test/scala/   # Tests
│   ├── crypto/               # Cryptography
│   │   ├── src/
│   │   └── native/           # Rust native libraries
│   │       ├── bandersnatch-vrfs-wrapper/
│   │       ├── ed25519-zebra-wrapper/
│   │       └── erasure-coding-wrapper/
│   ├── pvm/                  # PolkaVM virtual machine
│   ├── protocol/             # State transition functions
│   └── conformance/          # Conformance testing
├── graypaper/                # Gray Paper (submodule)
├── jamtestvectors/           # Official test vectors (submodule)
└── jam-conformance/          # Conformance testing tools (submodule)
```

## Development Workflow

### Code Style

The project uses Scalafmt for code formatting:

```bash
# Format all source files
sbt scalafmtAll

# Check formatting without modifying
sbt scalafmtCheckAll
```

Configuration in `.scalafmt.conf`:
- 2-space indentation
- 120-character line width
- Scala 3 syntax

### Running Specific Tests

```bash
# Run all tests
sbt test

# Run tests for a specific module
sbt "core/test"

# Run a single test class
sbt "core/testOnly io.forge.jam.core.codec.BlockCodecTest"

# Run tests matching a pattern
sbt "protocol/testOnly *SafroleTest"

# Run with increased logging
sbt "set Test / logLevel := Level.Debug" test
```

### Working with Native Libraries

#### Rebuilding Native Libraries

Native libraries are automatically rebuilt if missing. To force a rebuild:

```bash
# Remove existing libraries
rm -rf modules/crypto/native/build/

# Rebuild
sbt crypto/compile
```

#### Debugging Native Library Issues

```bash
# Build manually with debug output
cd modules/crypto/native/bandersnatch-vrfs-wrapper
cargo clean
cargo build --release --verbose

# Check library is loadable (macOS)
otool -L target/release/libbandersnatch_vrfs_wrapper.dylib

# Check library is loadable (Linux)
ldd target/release/libbandersnatch_vrfs_wrapper.so
```

Set `RUST_LOG` environment variable for Rust library debug output:
```bash
RUST_LOG=debug sbt test
```

### Updating Test Vectors

Test vectors are in the `jamtestvectors` submodule:

```bash
# Update to latest test vectors
cd jamtestvectors
git pull origin main
cd ..

# Run tests with new vectors
sbt test
```

### Benchmarking

Run performance benchmarks:

```bash
# Run benchmark suite
sbt benchmark

# The benchmark runs TracesBenchmark which measures STF performance
```

The benchmark suites (trie/DB, signature verification, erasure coding, PVM) and baseline figures are in
[BENCHMARKS.md](BENCHMARKS.md); `sbt benchComponents` runs the first three.

## Building for Production

### Conformance Server

Create a standalone JAR with all dependencies:

```bash
# Build fat JAR
sbt "conformance/assembly"

# Output: modules/conformance/target/scala-3.3.7/jam-conformance.jar

# Run the server (listens on /tmp/jam_target.sock; the native libraries come from the sbt build)
java --enable-native-access=ALL-UNNAMED --sun-misc-unsafe-memory-access=allow \
  -Djava.library.path=modules/crypto/native/build/linux \
  -jar modules/conformance/target/scala-3.3.7/jam-conformance.jar
```

### Docker Build

```bash
# Build Docker image (scripts/build-and-push.sh --skip-push does the same and smoke-tests the jar)
docker build -t jam-forge:latest .

# Run it the way W3F's target.py does: the JAM_FUZZ env selects the fuzz server, replays the bundled warmup traces
# through it, then opens the socket in the shared directory. JAM_WARMUP=0 skips the warmup.
mkdir -p /tmp/jam_fuzz && chmod 777 /tmp/jam_fuzz
docker run --rm -v /tmp/jam_fuzz:/tmp/jam_fuzz \
  -e JAM_FUZZ=1 -e JAM_FUZZ_SPEC=tiny -e JAM_FUZZ_DATA_PATH=/tmp/jam_fuzz/data \
  -e JAM_FUZZ_SOCK_PATH=/tmp/jam_fuzz/fuzz.sock \
  jam-forge:latest
```

## Testing Against Conformance Server

### Running Local Conformance Tests

```bash
# Start the conformance server (same JVM flags and warmup as the image), replay a public trace against it and
# print the fuzz-perf stats
scripts/fuzz-perf-local.sh jam-forge fallback

# In another terminal, run fuzzing tests
cd jam-conformance
python3 scripts/fuzz-workflow.py --target jam-forge --mode exploratory
```

## Debugging

### Enable Debug Logging

Set log level in `src/main/resources/logback.xml` or via environment:

```bash
# Run with debug logging
LOG_LEVEL=debug sbt test

# Or set in test code
System.setProperty("org.slf4j.simpleLogger.defaultLogLevel", "debug")
```