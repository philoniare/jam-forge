# Benchmarks

In-repo performance tests cover PVM/gas, the state trie and DB, signature
verification and erasure coding. Each suite is a plain `main` under `src/test/.../benchmark/` and prints
`RESULT <name> <value> <unit>` lines, so runs can be diffed or grepped.

| Category | Suite | Command |
|---|---|---|
| PVM / gas | `PvmThroughputBench`, `PvmMixedBench`, `RecompilerThroughputBench` | `sbt "pvm/Test/runMain io.forge.jam.pvm.benchmark.PvmThroughputBench"` |
| Trie / DB | `TrieDbBench` (RocksDB and in-memory backends) | `sbt benchTrieDb` |
| Signature verification | `SignatureVerifyBench` (Ed25519, Bandersnatch IETF and ring VRF) | `sbt benchSignatures` |
| Erasure coding | `ErasureCodingBench` (full config: 1023 validators, 342 needed) | `sbt benchErasureCoding` |
| Block import | `TracesBenchmark`, `Phase0Benchmark`, `PolkajamComparisonBenchmark` | `sbt benchmark` |
| Hashing | `Blake2bBench` | `sbt "core/Test/runMain io.forge.jam.core.benchmark.Blake2bBench"` |

`sbt benchComponents` runs the trie/DB, signature and erasure-coding suites in turn.

## Options

- `TrieDbBench`: `--keys=N` (1,000,000), `--blocks=B` (100), `--dirty=K` (1,000 keys per block, 10% of them new),
  `--reads=R` (100,000), `--backend=rocksdb|memory|both`. Each block does update, save, commit and gc, as
  `BlockImporter` does; the first tenth of the blocks is warmup. Values are 36 bytes (regular leaves).
- `SignatureVerifyBench`: any of `ed25519`, `ietf`, `ring`. Ring figures are for ring size 6 (tiny, 3 tickets) and
  1023 (full, 16 tickets).
- `ErasureCodingBench`: any of `bundle`, `segment`, `segments`. Reconstruct "parity" uses only the 342 recovery
  shards (the worst case), "systematic" the first 342.

Example: `sbt "benchTrieDb --keys=100000 --backend=rocksdb"`.

## Baseline

2026-10-22, main at 81c1954, cloud VM: 4 vCPU Intel Xeon @ 2.10 GHz (AVX-512), JDK 24, one run each. Cloud VM CPUs
vary by day, so compare only figures taken on the same machine.

Trie / DB (1M keys, 100 blocks of 1,000 dirty keys):

| Metric | RocksDB | In-memory |
|---|---|---|
| Populate 1M keys | 44,321 ms | 11,397 ms |
| Block, mean | 483 ms | 74 ms |
| Block, p50 / p99 | 481 / 623 ms | 51 / 202 ms |
| of which update / save / gc | 125 / 130 / 228 ms | 40 / 25 / 9 ms |
| Read, warm | 110 µs | 11.7 µs |
| Read, after reopen | 93 µs | |
| Disk size | 657 MB | |

Signature verification:

| Metric | Value |
|---|---|
| Ed25519 verify, single | 53.4 µs/sig |
| Ed25519 verify, batch of 64 (one thread) | 25.3 µs/sig |
| Ed25519 `verifyAll`, 1023 assurances | 8.8 ms |
| Bandersnatch IETF sign | 612 µs |
| Bandersnatch IETF verify | 1,002 µs |
| Ring commitment, 6 / 1023 keys | 2.0 / 95.5 ms |
| Ring proof, 6 / 1023 | 231 / 804 ms |
| Ring verify, 6 / 1023 | 5.7 / 5.9 ms/proof |
| Ring verify, 3 tickets parallel (tiny) | 6.2 ms |
| Ring verify, 16 tickets parallel (full) | 27.9 ms |

Erasure coding (full config):

| Metric | Value |
|---|---|
| Bundle encode, 13,791,360 B (W_B) | 119 ms |
| Bundle reconstruct, parity only | 187 ms |
| Bundle reconstruct, systematic | 74 ms |
| One segment (4,104 B) encode | 244 µs |
| One segment reconstruct, parity only | 731 µs |
| 3,120 segments encode (`chunkSegments`) | 134 ms |
| 3,120 segments reconstruct (`reconstructSegments`) | 188 ms |
