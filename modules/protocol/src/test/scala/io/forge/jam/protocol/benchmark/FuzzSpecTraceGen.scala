package io.forge.jam.protocol.benchmark

import io.circe.Decoder
import io.forge.jam.core.{ChainConfig, Hashing}
import io.forge.jam.core.types.header.Header
import io.forge.jam.protocol.TestFileLoader
import io.forge.jam.protocol.state.FuzzSpecGenesis
import io.forge.jam.protocol.traces.{BlockImporter, Genesis, ImportResult, RawState, TraceStep}

import java.nio.file.{Files, Paths}

/**
 * Writes a binary trace (`genesis.bin` plus one `NNNNNNNN.bin` per block, the jamtestvectors/traces layout) for a
 * chain spec other than tiny, so a fuzz target can be replayed under e.g. `JAM_FUZZ_SPEC=fuzz`.
 *
 * Genesis is a tiny trace's genesis (`--genesis`, default `fuzzy`) re-sized by [[FuzzSpecGenesis]] for V = 3C dev validators. Blocks come from
 * [[BlockGenerator]] (fallback sealing, guarantees, assurances, accumulation) and each post-state root is the one
 * our own importer computes, so the trace is only as right as this client: replay it against other targets to
 * compare.
 *
 * Usage: sbt "protocol/Test/runMain io.forge.jam.protocol.benchmark.FuzzSpecTraceGen --out=<dir> [--spec=fuzz]
 * [--blocks=200] [--staging=N] [--genesis=fuzzy] [--seed=42]"
 *
 * `--staging=N` puts only the first N validators in the genesis staging set (iota), as if a `designate` had
 * resized the set: N keys enter gamma_P at the first epoch change and kappa at the second.
 */
object FuzzSpecTraceGen:

  def main(args: Array[String]): Unit =
    val opts = args.flatMap(a => a.stripPrefix("--").split("=", 2) match
      case Array(k, v) => Some(k -> v)
      case _ => None).toMap
    val out = Paths.get(opts.getOrElse("out", throw new IllegalArgumentException("--out=<dir> is required")))
    val config = ChainConfig.fromName(opts.getOrElse("spec", "fuzz")).fold(e => throw new IllegalArgumentException(e), identity)
    val blocks = opts.get("blocks").map(_.toInt).getOrElse(200)
    Files.createDirectories(out)

    given Decoder[Genesis] = Genesis.decoder
    val tiny = TestFileLoader
      .loadJsonFromTestVectors[Genesis](s"traces/${opts.getOrElse("genesis", "fuzzy")}", "genesis")
      .fold(e => throw new IllegalStateException(s"tiny genesis: $e"), identity)

    val validatorCount = 3 * config.coresCount
    val keys = Array.tabulate(validatorCount)(DevKeyStore.getValidator)
    val validators = keys.toList.map(k => FuzzSpecGenesis.devValidatorKey(k.bandersnatchPublic, k.ed25519Public))
    val staging = validators.take(opts.get("staging").map(_.toInt).getOrElse(validatorCount))
    val genesisState = FuzzSpecGenesis.build(tiny.state, config, validators, staging)
    val genesis = Genesis(tiny.header, genesisState)
    val genesisCodec = Genesis.codec(config.validatorCount, config.epochLength)
    Files.write(out.resolve("genesis.bin"), genesisCodec.encode(genesis).toArray)
    val genesisHash = Hashing.blake2b256(Header.headerCodec(config.validatorCount, config.epochLength).encode(tiny.header))
    println(s"genesis: ${validators.size} validators (${staging.size} staged), ${config.coresCount} cores, root ${genesisState.stateRoot.toHex}")

    val importer = new BlockImporter(config)
    val store = importer.store
    store.bootstrap(genesisState.keyvals.map(kv => (kv.key, kv.value)))
    val generator = new BlockGenerator(config, keys, store, seed = opts.get("seed").map(_.toLong).getOrElse(42L),
      genesisHeaderHash = Some(genesisHash))
    val stepCodec = TraceStep.codec(config.validatorCount, config.epochLength, config.coresCount, config.votesPerVerdict)

    var root = genesisState.stateRoot
    var slot = tiny.header.slot.value.toLong + 1
    var written = 0
    var guarantees = 0
    var assurances = 0
    while written < blocks do
      val block = generator.generateBlock(root, slot)
      importer.importBlock(block, RawState(root, Nil)) match
        case ImportResult.Success(postRoot, _) =>
          written += 1
          guarantees += block.extrinsic.guarantees.size
          assurances += block.extrinsic.assurances.size
          val step = TraceStep(RawState(root, Nil), block, RawState(postRoot, Nil))
          Files.write(out.resolve(f"$written%08d.bin"), stepCodec.encode(step).toArray)
          root = postRoot
        case ImportResult.Failure(err, msg) =>
          throw new IllegalStateException(s"own block at slot $slot rejected: $err $msg")
      slot += 1
    println(
      s"wrote $written blocks (slots up to ${slot - 1}, epochs ${slot / config.epochLength + 1}), " +
        s"$guarantees guarantees, $assurances assurances, final root ${root.toHex}"
    )
    sys.exit(0)
