# Benchmarks

In-repo performance tests cover PVM/gas, the state trie and DB, signature
verification and erasure coding. Each suite is a plain `main` under `src/test/.../benchmark/` and prints
`RESULT <name> <value> <unit>` lines, so runs can be diffed or grepped.

| Category | Suite | Command |
|---|---|---|
| PVM / gas | `PvmThroughputBench`, `PvmMixedBench`, `RecompilerThroughputBench` | `sbt "pvm/Test/runMain io.forge.jam.pvm.benchmark.PvmThroughputBench"` |
| Trie / DB | `TrieDbBench` (RocksDB and in-memory backends; `--backend=node` drives RocksDB as the node does) | `sbt benchTrieDb` |
| Signature verification | `SignatureVerifyBench` (Ed25519, Bandersnatch IETF and ring VRF) | `sbt benchSignatures` |
| Erasure coding | `ErasureCodingBench` (full config: 1023 validators, 342 needed) | `sbt benchErasureCoding` |
| Block import | `TracesBenchmark`, `Phase0Benchmark`, `PolkajamComparisonBenchmark` | `sbt benchmark` |
| Block import, before/after | `ImportBench` (replay with root check; heap soak) | `sbt benchImport` (see below) |
| Block import against state size | `ServiceScaleBench` (a trace replayed on a state with N extra service accounts) | `sbt "protocol/Test/runMain io.forge.jam.protocol.benchmark.ServiceScaleBench --services=0,10000,100000"` |
| Node heap soak | `NodeSoak` (two nodes over loopback JAMNP) | `sbt soakNodes` (see below) |
| Trie, large batches | `TrieBatchBench` (in-memory backend: populate 1M keys, insert 1M more as one batch and in 10k batches; `--keys=N`, `--reps=R`) | `sbt "core/Test/runMain io.forge.jam.core.benchmark.TrieBatchBench"` |
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

Large trie batches (`TrieBatchBench`, 1M keys, in-memory backend), 2026-11-22, cloud VM (4 vCPU Xeon @ 2.10 GHz),
JDK 24, ParallelGC 10 GB, median of 3 passes. "Before" rebuilds a batch on one thread; "after" rebuilds the subtrees
at depth 5 in parallel (batches of 4,096 keys or more). Same final root. PolkaJam is `polkajam benchmark`
(nightly-2026-11-22) on the same VM, one run, which reports one insert figure.

| Workload | Before | After | PolkaJam |
|---|---|---|---|
| Populate 1M keys | 3,744 ms | 2,183 ms | 1,242 ms |
| Insert 1M more, one batch | 6,535 ms | 3,220 ms | 1,738 ms |
| Insert 1M more, 10k batches | 16,795 ms | 9,075 ms | |

Seal and entropy-source verification in block import, same VM and day: fuzz-perf mean ms per block with each
verification split over the native library's threads ("split", as before) and done on the thread that started it
("on caller", as now), two alternating runs each. Run-to-run noise on this VM is about 10%.

| Trace | Split | On caller |
|---|---|---|
| fallback | 1.43, 1.52 | 1.41, 1.26 |
| safrole | 5.26, 4.91 | 5.41, 4.71 |
| storage | 2.32, 2.39 | 1.93, 2.12 |
| storage_light | 1.97, 2.13 | 1.84, 1.79 |

With nothing else running the split verification is the quicker one (`SignatureVerifyBench ietf`: 403 against 674 us
for one, 654 against 758 us for a pair started together), which is why only block import verifies on the caller.

## The published measurement (fuzz-perf)

`jam-conformance/fuzz-perf` is what implementations are compared on: a fresh target process, the first 100 blocks
of each of `fallback`, `safrole`, `storage` and `storage_light` sent over the fuzz socket, and the time from each
ImportBlock to its reply. `scripts/fuzz-perf-local.sh jam-forge` reproduces it with the image's JVM flags and warmup
corpus and prints mean / p50 / p99 / max per trace. Because the process is fresh, startup warmup counts: check the
target log (`target/fuzz-perf/jam-forge/<trace>.target.log`) for `Warmup complete: ... 0 replays skipped`.

2026-11-13, Apple M1 Max, OpenJDK 26, ZGC 4 GB, one run, ms per block. "Cold" is the target started without
warmup; "Reference" is the published 0.7.2 reference figure on W3F's Threadripper 3970X, a different machine, for
scale only.

| Trace | Cold: mean / max | Warm: mean / p50 / p99 / max | Reference mean | Reference interpreter mean |
|---|---|---|---|---|
| fallback | 4.05 / 249 | 0.66 / 0.65 / 0.97 / 1.25 | 0.97 | 0.85 |
| safrole | 7.34 / 390 | 2.95 / 4.26 / 6.41 / 10.1 | 5.29 | 5.54 |
| storage | 8.47 / 251 | 3.34 / 2.34 / 10.6 / 11.7 | 1.54 | 4.40 |
| storage_light | 6.81 / 249 | 2.28 / 2.13 / 5.44 / 5.67 | 1.43 | 2.73 |

Stage breakdown of `storage` (`benchImport --traces=jamtestvectors/traces/storage --stages`): accumulation is 71%
of import time, and 63% of import time is inside the PVM run of a service (3.3 ms per invocation, about 2.2 M gas at
0.7 Ggas/s). Host calls are 6% of that PVM time; instance setup is 64 µs. Trie update, save and gc together are 3%.

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

`--stages` adds a per-stage breakdown over the timed passes: one `RESULT stage <name> calls= total_ms= mean_us=
share=` line per pipeline stage (`safrole`, `reports`, `accumulation`, ...), the commit stages (`commit-encode`,
`trie-update`, `trie-save`, `trie-gc`), accumulation's parts (`acc/extract`, `acc/stf`, `acc/service-pvm`, ...) and
the PVM (`pvm/instance`, `pvm/host-<id>` per host call, and `pvm/gas`, whose `total_ms` column is the gas used).
`share` is of `total`. The timers are off otherwise (`StageTimers`; `-Djam.stageTimers=true` turns them on in any
process), and cost one clock read per stage when on.

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

## One node at full size (`FullSizeBench`)

No machine holds 1,023 validators, so this drives one real node at that size with the other validators played by
the harness (in-memory connections, real signatures and VRF proofs).

```
sbt "benchGenTrace --spec=full --blocks=12 --busy-cores=341 --out=target/bench/full-busy-trace"
sbt "node/Test/runMain io.forge.jam.node.bench.FullSizeBench"
```

`--busy-cores=341` makes a chain in which every core gets a report in one block and all 341 become available in
the next. The harness replays it into a node and measures: (1) auditing a block of 341 reports with a tranche-0
announcement from each of the 1,023 validators and a judgment for each of their 10,230 picks; (2) finding a shard
among 1,022 peers of which the report's three guarantors hold it, in connection order and guarantors first;
(3) ordering 1,022 peers by their faults.

16 November 2026: the bench VM (x86-64, 8 vCPU; three runs, which agree within a few percent) and one run on an
M-series Mac.

| Measure | Bench VM | Mac |
|---|---|---|
| A block's 1,023 announcements, wall | 0.78 s | 0.58 s |
| Audit thread CPU per announcement (its VRF proof is checked there) | 0.63 ms | 0.44 ms |
| The block's 10,230 judgments, wall | 1.5 s | 1.1 s |
| Audit thread CPU per judgment | 55 us | 58 us (114 us with every report re-checked per message) |
| Block audited after (a tranche is 8 s) | 2.2 s | 1.7 s |
| Peers asked for a shard, connection order / guarantors first | 277 / 4.0 | 277 / 3.9 |
| Shard lookup median, connection order / guarantors first (in memory: no network round trips) | 14 ms / 0.30 ms | 18.5 ms / 0.25 ms |
| Ordering 1,022 peers: nobody faulted / 10 faulted / all faulted | 1.9 / 106 / 226 us a call | 2.6 / 95 / 252 us a call |
