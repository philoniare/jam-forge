package io.forge.jam.node

import java.nio.file.Paths

import io.forge.jam.core.JamBytes

/** CLI entry point:
  * {{{
  * jam-node --spec <chain-spec.json> --data <data-dir> [--port N]
  *          [--seed <64-hex Ed25519 seed>] [--era-start <unix-seconds>]
  * }}}
  */
object Main:

  def main(args: Array[String]): Unit =
    val opts = parseArgs(args.toList, Map.empty)

    val specPath = opts.getOrElse("spec", fail("--spec <chain-spec.json> is required"))
    val dataDir = opts.getOrElse("data", fail("--data <dir> is required"))

    val spec = ChainSpec.load(Paths.get(specPath)) match
      case Right(s)  => s
      case Left(err) => fail(s"failed to load chain spec: $err")

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
    node.onSlot { slot =>
      // Authoring hooks in here; for now surface liveness.
      if slot % 10 == 0 then
        println(s"slot $slot best=${node.chain.best.slot} (${node.chain.best.hash.toHex.take(18)})")
    }

    Runtime.getRuntime.addShutdownHook(new Thread(() => node.shutdown()))
    Thread.currentThread().join()

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
