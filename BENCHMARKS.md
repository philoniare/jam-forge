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
| Block import, before/after | `ImportBench` (replay with root check; heap soak) | `sbt benchImport` (see below) |
| Node heap soak | `NodeSoak` (two nodes over loopback JAMNP) | `sbt soakNodes` (see below) |
| Hashing | `Blake2bBench` | `sbt "core/Test/runMain io.forge.jam.core.benchmark.Blake2bBench"` |
| PVM heap growth (`grow_heap`) | `GrowHeapBench` | `sbt benchGrowHeap` |

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

PVM Heap Growth:
| Scenario | Before | After |
|---|---|---|
| fuzzy-ramp: 20 grows of 1-3 pages, then doubling to ~128 MiB (30 calls) | 64.2 / 65.7 ms | 16.0 / 15.6 ms |
| small-steps: 4,096 grows of 2 pages (32 MiB) | 27,556 / 27,452 ms | 2.4 / 2.7 ms |
| small-on-large: one 32 MiB grow, then 200 grows of 2 pages | 1,772 / 1,748 ms | 1.9 / 2.0 ms |

## Block import bench and heap soak

For before/after figures in performance and memory PRs. Run the same command on `main` and on the branch, on the
same machine, and quote both. None of this runs under `sbt test`.

`ImportBench` replays binary traces the way the conformance target handles ImportBlock: decode the framed message,
`ImportBlockStep` on a per-session `StateStore` and `BlockImporter`, wait for the post-state. Each replay of a trace
is a new session, so the first block of a trace includes building the trie from the seed state. Every state root is
compared with the fixture (a block the fixture rejects must be rejected); any mismatch is printed and the run exits 1.

```bash
# TINY, fuzz-report corpus (41 traces, 82 blocks, 38 of them blocks the fixture rejects)
sbt benchImport
# TINY, jamtestvectors traces (8 traces, 1,000 blocks)
sbt "benchImport --traces=jamtestvectors/traces --per-trace"

# FULL (1023 validators, 341 cores): generate the workload once, on main, then replay it
sbt "benchGenTrace --spec=full --blocks=60 --out=target/bench/full-trace"
sbt "benchImport --spec=full --traces=target/bench/full-trace"

# Heap soak: 20,000 imports, heap after a forced GC every 1,000
sbt "benchImport --mode=soak --imports=20000"

# Production GC and heap for the forked JVM (default is the JVM's own: G1, a quarter of RAM)
sbt -Djam.bench.jvmOpts="-XX:+UseZGC -Xms4g -Xmx4g" benchImport
```

Options (`--key=value`): `--traces=dir[,dir]` (a trace directory or a directory of them, relative to the repo root;
`genesis.bin` is used when present, else the first step's pre-state), `--spec=tiny|full|fuzz`, `--warmup=2` untimed
passes, `--reps=5` timed passes, `--per-trace`. Soak: `--imports=20000`, `--sample-every=1000`, `--mutate` (also
import a bad-seal sibling and re-import the previous block, as a fuzzer does; unchecked), `--max-growth-mb=2`,
`--csv=file`.

Output: `RESULT import ALL blocks=<per pass> n=<timed samples> mean= p50= p90= p99= max= ms` is wall time per
block; `RESULT import-cpu ALL ...` is CPU time of the importing thread per block. The CPU line leaves out work done
on other threads (parallel signature checks, the post-state read), but it moves far less than wall time when the
machine is busy, so quote both and trust `import-cpu` for small differences. The soak prints CSV
`imports,elapsed_s,heap_after_gc_mb,sessions` and a `SOAK ... growth_mb_per_10k_imports= verdict=FLAT|GROWING` line
(least-squares slope over the second half of the samples); it exits 1 on GROWING or on a root mismatch.

The FULL workload comes from `FuzzSpecTraceGen` (deterministic for a given seed): generated blocks with guarantees
and assurances, almost no service code, no tickets and, at 60 blocks, no epoch change. Its expected roots are the
ones the generating build computed, so generate it on `main` and replay the same files on the branch.

`NodeSoak` runs two in-process nodes (RocksDB each, loopback JAMNP, tiny `fuzzy` genesis): A authors every slot,
assures and audits; B guarantees and audits; a work package is submitted every `--wp-every=4` slots.

```bash
sbt "soakNodes --slots=4000 --sample-every=100"
```

Options: `--slots=1000`, `--sample-every=100`, `--wp-every=4`, `--data=dir` (kept; default is a temp directory,
deleted), `--max-growth-mb=2` (per 1,000 slots), `--csv=file`. It prints CSV with heap after GC, both data-directory
sizes and authored/finalized/guarantee/assurance counts, then a `SOAK ... growth_mb_per_1k_slots= verdict=` line.
Not exercised: GRANDPA voting, peer churn, reorgs, more than two nodes.

Sample, 2026-11-12, main at 4e0a2825, Apple M1 Max (10 cores, 32 GB), OpenJDK 26.0.2, ZGC 4 GB, one run each, with
other sbt jobs running on the machine, so treat the absolute figures as noisy:

| Run | Blocks | wall mean / p50 / p90 / p99 | importing-thread CPU mean / p50 / p90 / p99 |
|---|---|---|---|
| TINY fuzz-reports 0.8.0 | 82 x 5 | 10.4 / 7.1 / 21.6 / 66.1 ms | 2.9 / 2.3 / 5.9 / 8.5 ms |
| TINY jamtestvectors traces | 1,000 x 3 | 4.8 / 3.1 / 9.6 / 25.8 ms | 2.3 / 1.6 / 4.3 / 10.2 ms |
| FULL generated, 60 blocks | 60 x 5 | 16.5 / 13.3 / 29.9 / 38.2 ms | 12.4 / 7.8 / 21.6 / 24.9 ms |

Load average was about 60 during these runs. A run of the same three commands twenty minutes earlier gave wall
means of 7.5, 8.3 and 36.6 ms, which is the size of the wall-time noise here.

Heap soak (`--mode=soak --imports=20000 --sample-every=2000 --mutate`): 128-132 MB after GC at every sample from
2,000 imports on, -0.29 MB per 10k imports, FLAT.

Node soak (`soakNodes --slots=400 --sample-every=50`, 341 s): 400 of 400 slots authored, 100 guarantees and 594
assurances included, finalized to slot 395; heap after GC 86 MB at slot 50, 92 MB at 100-200, 96 MB at 250-400;
16 MB per 1,000 slots over the second half, so GROWING and exit 1. Too short a run to say whether that is a leak or
caches still filling; use 4,000 slots or more for a real answer.
