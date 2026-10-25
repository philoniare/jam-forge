# Generated warmup traces

The fuzz target replays these at startup, next to jamtestvectors' preimages traces, so that the JIT compiles the
reports, assurances and accumulation paths before the fuzzer connects (`WarmupRunner`, `JAM_TRACES_DIR`). The
Dockerfile copies them to `/app/warmup-traces/`, and `scripts/fuzz-perf-local.sh` uses them too.

Each is 200 tiny-spec blocks from `BlockGenerator` (fallback sealing, guarantees, assurances, accumulation),
starting from a jamtestvectors genesis with the dev validators swapped in. Post-state roots are our own importer's.

- `generated_preimages`: from the `preimages` genesis (the storage/preimages service).
- `generated_fuzzy`: from the `fuzzy` genesis (the fuzzy service).

Regenerate with:

    sbt "protocol/Test/runMain io.forge.jam.protocol.benchmark.FuzzSpecTraceGen --out=warmup-traces/generated_preimages --spec=tiny --genesis=preimages"
    sbt "protocol/Test/runMain io.forge.jam.protocol.benchmark.FuzzSpecTraceGen --out=warmup-traces/generated_fuzzy --spec=tiny --genesis=fuzzy"
