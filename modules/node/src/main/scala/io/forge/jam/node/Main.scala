package io.forge.jam.node

import java.nio.file.Paths

import io.forge.jam.core.JamBytes

/** CLI entry point:
  * {{{
  * jam-node --spec <chain-spec.json> --data <data-dir> [--port N]
  *          [--seed <64-hex Ed25519 seed>] [--era-start <unix-seconds>]
  *          [--peer <64-hex-ed25519>@host:port ...] [--author <validator-index>]
  * }}}
  */
object Main:

  def main(args: Array[String]): Unit =
    val (peerArgs, rest) = extractRepeated("--peer", args.toList)
    val opts = parseArgs(rest, Map.empty)

    val specPath = opts.getOrElse("spec", fail("--spec <chain-spec.json> is required"))
    val dataDir = opts.getOrElse("data", fail("--data <dir> is required"))

    val loadedSpec = ChainSpec.load(Paths.get(specPath)) match
      case Right(s)  => s
      case Left(err) => fail(s"failed to load chain spec: $err")

    val extraBootnodes = peerArgs.map(Bootnode.parse)
    val spec =
      if extraBootnodes.isEmpty then loadedSpec
      else loadedSpec.copy(bootnodes = loadedSpec.bootnodes ++ extraBootnodes)

    val config = NodeConfig(
      dataDir = Paths.get(dataDir),
      listenPort = opts.get("port").map(_.toInt).getOrElse(0),
      ed25519Seed = opts.get("seed").map(s => JamBytes.fromHexUnsafe(s).toArray),
      eraStartSeconds = opts
        .get("era-start")
        .map(_.toLong)
        .getOrElse(SlotClock.JamCommonEraSeconds)
    )

    val node = new JamNode(spec, config).start()
    opts.get("author").foreach { idx =>
      node.enableAuthoring(Seq(ValidatorKeySet.dev(idx.toInt)))
    }
    node.onSlot { slot =>
      // Authoring hooks in here; for now surface liveness.
      if slot % 10 == 0 then
        println(s"slot $slot best=${node.chain.best.slot} (${node.chain.best.hash.toHex.take(18)})")
    }

    Runtime.getRuntime.addShutdownHook(new Thread(() => node.shutdown()))
    Thread.currentThread().join()

  /** Pulls every occurrence of a repeatable `--flag value` pair out of
    * `args`, preserving the relative order of what's left for `parseArgs`.
    */
  private def extractRepeated(flag: String, args: List[String]): (List[String], List[String]) =
    args match
      case Nil => (Nil, Nil)
      case f :: value :: rest if f == flag =>
        val (values, remaining) = extractRepeated(flag, rest)
        (value :: values, remaining)
      case other :: rest =>
        val (values, remaining) = extractRepeated(flag, rest)
        (values, other :: remaining)

  private def parseArgs(args: List[String], acc: Map[String, String]): Map[String, String] =
    args match
      case Nil => acc
      case key :: value :: rest if key.startsWith("--") =>
        parseArgs(rest, acc + (key.stripPrefix("--") -> value))
      case other :: _ =>
        fail(s"unexpected argument: $other")

  private def fail(msg: String): Nothing =
    System.err.println(msg)
    sys.exit(1)
