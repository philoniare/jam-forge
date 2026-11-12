package io.forge.jam.node

import java.net.InetSocketAddress
import java.nio.file.{Files, Path, Paths}
import java.nio.file.attribute.PosixFilePermissions
import java.security.SecureRandom

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.JamBytes

/** CLI entry point:
  * {{{
  * jam-node --spec <chain-spec.json> --data <data-dir> [--port N]
  *          [--seed <64-hex Ed25519 seed> | --seed-file <path>] [--era-start <unix-seconds>]
  *          [--peer <64-hex-ed25519>@host:port ...] [--author <validator-index>]
  *          [--telemetry <host:port>] [--reuse-port] [--grandpa] [--beefy] [--submit-port N]
  *          [--finality-peers N] [--prune-history]
  * }}}
  *
  * With `--author i` and no `--seed`, the node's network identity is dev
  * validator i's Ed25519 key, as JAMNP-S expects of validators; peers then
  * map the connection to validator index i (CE 131 proxying). Native crypto
  * libraries are looked up under `-Djam.base.dir` (default: the working
  * directory); a missing one stops the node before it joins the network.
  * `--submit-port N` opens the loopback preimage endpoint (`POST /preimage/<service-id>`).
  * `--seed-file` keeps the network identity across restarts: the seed is read from the file, or generated and
  * written there (owner-only) when the file does not exist. `--prune-history` deletes finalized blocks once they
  * are more than a lookup-anchor age behind finality.
  */
object Main extends LazyLogging:

  def main(args: Array[String]): Unit =
    try run(args)
    catch
      case e: Throwable =>
        System.err.println(s"jam-node failed to start: $e")
        e.printStackTrace()
        sys.exit(1)

  private def run(args: Array[String]): Unit =
    val (peerArgs, afterPeers) = extractRepeated("--peer", args.toList)
    val (flags, rest) = afterPeers.partition(a => Set("--reuse-port", "--grandpa", "--beefy", "--prune-history").contains(a))
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

    // Derive authoring keys first: this loads the native Bandersnatch and
    // Ed25519 libraries, so a missing library fails here, not after start.
    val authorKeys = opts.get("author").map(idx => ValidatorKeySet.dev(idx.toInt))

    val config = NodeConfig(
      dataDir = Paths.get(dataDir),
      listenPort = opts.get("port").map(_.toInt).getOrElse(0),
      ed25519Seed = opts
        .get("seed")
        .map(s => JamBytes.fromHexUnsafe(s).toArray)
        .orElse(opts.get("seed-file").map(p => loadOrCreateSeed(Paths.get(p))))
        .orElse(authorKeys.map(_.ed25519Secret)),
      eraStartSeconds = opts
        .get("era-start")
        .map(_.toLong)
        .getOrElse(SlotClock.JamCommonEraSeconds),
      reusePort = flags.contains("--reuse-port"),
      grandpa = flags.contains("--grandpa"),
      pruneFinalizedHistory = flags.contains("--prune-history"),
      finalityPeers = opts.get("finality-peers").map(_.toInt),
      telemetry = opts.get("telemetry").map(parseHostPort),
      submitPort = opts.get("submit-port").map(_.toInt)
    )

    val node = new JamNode(spec, config).start()
    authorKeys.foreach { k =>
      node.enableAuthoring(Seq(k))
      node.enableGuaranteeing(Seq(k))
      node.enableAssuring(Seq(k))
      node.enableAuditing(Seq(k))
    }
    if config.grandpa then node.enableGrandpa(authorKeys)
    if flags.contains("--beefy") then node.enableBeefy(authorKeys)
    node.onSlot { slot =>
      val best = node.chain.best
      val fin = node.chain.finalized
      logger.info(
        s"slot $slot best=${best.slot} ${best.hash.toHex} finalized=${fin.slot} " +
          s"peers=${node.distribution.peerCount}"
      )
    }

    Runtime.getRuntime.addShutdownHook(new Thread(() => node.shutdown()))
    Thread.currentThread().join()

  /** The 32-byte seed in `path` (hex), or a new random one written there, readable by the owner only. */
  private def loadOrCreateSeed(path: Path): Array[Byte] =
    if Files.exists(path) then
      val seed = JamBytes.fromHexUnsafe(Files.readString(path).trim).toArray
      if seed.length != 32 then fail(s"$path: expected a 32-byte hex seed, got ${seed.length} bytes")
      seed
    else
      val seed = new Array[Byte](32)
      new SecureRandom().nextBytes(seed)
      Option(path.toAbsolutePath.getParent).foreach(Files.createDirectories(_))
      val ownerOnly = PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rw-------"))
      try Files.createFile(path, ownerOnly)
      catch case _: UnsupportedOperationException => Files.createFile(path)
      Files.writeString(path, JamBytes(seed).toHex + "\n")
      seed

  private def parseHostPort(s: String): InetSocketAddress =
    val colon = s.lastIndexOf(':')
    if colon <= 0 then fail(s"expected host:port, got: $s")
    new InetSocketAddress(s.substring(0, colon), s.substring(colon + 1).toInt)

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
