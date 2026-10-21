# Testing Guide

## Testing Philosophy

The JAM Scala implementation uses multiple testing strategies to ensure correctness:

1. **Test Vector Compliance**: Official `jamtestvectors` validate protocol conformance
2. **Unit Tests**: Component-level testing with ScalaTest
3. **Property-Based Testing**: ScalaCheck for codec laws and invariants
4. **Integration Tests**: End-to-end block import and state transitions
5. **Conformance Testing**: Cross-implementation validation via fuzzing

## Test Organization

### Module Structure

```
modules/
├── core/src/test/scala/
│   ├── codec/           # Encoding/decoding tests
│   ├── types/           # Type validation tests
│   └── ShuffleTest.scala
├── crypto/src/test/scala/
│   ├── BandersnatchTest.scala
│   ├── Ed25519Test.scala
│   └── ErasureCodingTest.scala
├── pvm/src/test/scala/
│   ├── opcodes/         # Instruction tests
│   └── execution/       # VM execution tests
├── protocol/src/test/scala/
│   ├── safrole/         # Safrole STF tests
│   ├── statistics/      # Statistics STF tests
│   └── traces/          # Full block import tests
└── conformance/src/test/scala/   # Fuzz server (socket-level), warmup, fuzz-proto and trace tests
```

## Running Tests

### All Tests

```bash
# Run all tests across all modules
sbt test

# Run with specific log level
LOG_LEVEL=debug sbt test

# Run with detailed output
sbt "testOnly * -- -oF"
```

### Module-Specific Tests

```bash
# Run tests for a single module
sbt "core/test"
sbt "crypto/test"
sbt "protocol/test"
sbt "pvm/test"
sbt "conformance/test"
```

### Individual Test Classes

```bash
# Run a specific test class
sbt "testOnly io.forge.jam.core.ShuffleTest"
sbt "testOnly io.forge.jam.protocol.safrole.SafroleTransitionTest"

# Run multiple test classes
sbt "testOnly io.forge.jam.core.*CodecTest"
```

### Test Pattern Matching

```bash
# Run tests matching a pattern
sbt "testOnly *Safrole*"
sbt "testOnly *Codec*"

# Run tests in a specific package
sbt "testOnly io.forge.jam.protocol.statistics.*"
```

## Test Vectors

### Official Test Vectors

Located in `jamtestvectors/` submodule:

```
jamtestvectors/
├── codec/           # Binary encoding test cases
├── erasure/         # Erasure coding vectors
├── shuffle/         # Validator shuffle vectors
└── stf/             # State transition vectors
    ├── safrole/
    ├── statistics/
    ├── authorizations/
    ├── assurances/
    ├── reports/
    ├── disputes/
    ├── history/
    └── preimages/
```

## Conformance Testing

### Local Conformance Server

The conformance server provides a Unix socket interface for testing:

```bash
# Build the jar, then start the server (see DEVELOPMENT.md for the JVM flags)
sbt "conformance/assembly"
java --enable-native-access=ALL-UNNAMED --sun-misc-unsafe-memory-access=allow \
  -Djava.library.path=modules/crypto/native/build/linux \
  -jar modules/conformance/target/scala-3.3.7/jam-conformance.jar

# Server listens on: /tmp/jam_target.sock (--socket-path changes it)

# Or start it as the Docker image does and replay a public trace into it (fuzz-perf stats)
scripts/fuzz-perf-local.sh jam-forge fallback
```

### Running Conformance Tests

```bash
# From jam-conformance directory
cd jam-conformance/scripts

# Run fuzzing against Scala implementation
python3 fuzz-workflow.py --target jam-forge --mode exploratory

# Run with trace output
python3 fuzz-workflow.py --target jam-forge --mode trace
```

## Benchmarking

### Running Benchmarks

```bash
# Run benchmark suite
sbt benchmark

# This executes TracesBenchmark
```

### Coverage Goals

- Core module: >90% line coverage
- Protocol module: >85% line coverage
- Crypto module: >80% (native code excluded)
- PVM module: >80% line coverage

## Continuous Integration

### GitHub Actions

Tests run automatically on:
- Push to main branch