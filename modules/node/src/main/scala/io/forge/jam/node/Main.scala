package io.forge.jam.node

import java.net.InetSocketAddress
import java.nio.file.{Files, Path, Paths}
import java.nio.file.attribute.PosixFilePermissions
import java.security.SecureRandom

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.JamBytes

/** CLI entry point:
  * {{{
  * jam-node --data <data-dir> [--spec <chain-spec.json or URL>] [--port N (default 40000)]
  *          [--seed <64-hex Ed25519 seed> | --seed-file <path>] [--era-start <unix-seconds>]
  *          [--peer <64-hex-ed25519>@host:port ...]
  *          [--author <validator-index> | --validator-seed <path>]
  *          [--bandersnatch-key <hex or path>] [--ed25519-key <hex or path>]
  *          [--telemetry <host:port>] [--reuse-port] [--grandpa] [--beefy] [--submit-port N]
  *          [--finality-peers N] [--prune-history]
  * jam-node --data <data-dir> [--author <index> | --validator-seed <path>]
  *          [--bandersnatch-key <hex or path>] [--ed25519-key <hex or path>] --print-keys
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
  *
  * `--validator-seed <path>` runs the node as the validator whose keys derive (JIP-5) from the 32-byte hex seed in
  * that file, generated there (owner-only) when the file does not exist. Its Ed25519 key is also the network
  * identity, so it cannot be combined with `--seed` or `--seed-file`. `--print-keys` prints the public keys of
  * `--author` or `--validator-seed` as JSON and exits without starting the node.
  *
  * `--bandersnatch-key` and `--ed25519-key` give the validator's Bandersnatch and Ed25519 keys: 32-byte secret
  * seeds (JIP-5 `bandersnatch_secret_seed` and `ed25519_secret_seed`), each as hex or as the path of a file holding
  * the hex (a file keeps it out of the process list). They replace those keys of `--author` or `--validator-seed`;
  * without either, the keys not given come from `<data-dir>/validator.seed`, generated on first start. The Ed25519
  * key is also the node's network identity.
  *
  * A node with validator keys also votes in GRANDPA, as if `--grandpa` were given.
  *
  * `--spec` may be an http(s) URL: the spec is downloaded at every start into `<data-dir>/spec.json`, and the copy
  * there is used when the download fails. Without `--spec` the node joins the public jamtoaster testnet.
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
    val (flags, rest) = afterPeers.partition(a => Set("--reuse-port", "--grandpa", "--beefy", "--prune-history", "--print-keys").contains(a))
    val opts = parseArgs(rest, Map.empty)

    if opts.contains("author") && opts.contains("validator-seed") then
      fail("--author and --validator-seed are exclusive: give one source of validator keys")
    if opts.contains("validator-seed") && (opts.contains("seed") || opts.contains("seed-file")) then
      fail("--validator-seed sets the network identity too: drop --seed/--seed-file")

    // Derive validator keys first: this loads the native Bandersnatch and
    // Ed25519 libraries, so a missing library fails here, not after start.
    // With only some keys given, the others come from a seed kept in the data directory.
    val givenKeys = List("bandersnatch-key", "ed25519-key").filter(opts.contains)
    val validatorSeed = opts.get("validator-seed").orElse {
      if givenKeys.nonEmpty && !opts.contains("author") then
        if opts.contains("seed") || opts.contains("seed-file") then
          fail(s"--${givenKeys.head} makes this node a validator, whose network identity is its Ed25519 key: drop --seed/--seed-file")
        val dir = opts.getOrElse("data", fail(s"--${givenKeys.head} needs --data <dir> to keep the node's other keys in"))
        Some(Paths.get(dir, "validator.seed").toString)
      else None
    }
    val authorKeys = opts
      .get("author")
      .map(idx => ValidatorKeySet.dev(idx.toInt))
      .orElse(validatorSeed.map(p => ValidatorKeySet.fromSeed(loadOrCreateSeed(Paths.get(p)))))
      .map(keys =>
        opts.get("bandersnatch-key").fold(keys)(k => ValidatorKeySet.withBandersnatchSeed(keys, readKey("bandersnatch-key", k)))
      )
      .map(keys => opts.get("ed25519-key").fold(keys)(k => ValidatorKeySet.withEd25519Seed(keys, readKey("ed25519-key", k))))

    if flags.contains("--print-keys") then
      authorKeys match
        case Some(k) => println(publicKeysJson(k)); return
        case None    => fail("--print-keys needs --author, --validator-seed, --bandersnatch-key or --ed25519-key")

    val specPath = opts.getOrElse("spec", DefaultSpecUrl)
    val dataDir = opts.getOrElse("data", fail("--data <dir> is required"))

    val loadedSpec = ChainSpec.load(localSpec(specPath, Paths.get(dataDir))) match
      case Right(s)  => s
      case Left(err) => fail(s"failed to load chain spec: $err")

    val extraBootnodes = peerArgs.map(Bootnode.parse)
    val spec =
      if extraBootnodes.isEmpty then loadedSpec
      else loadedSpec.copy(bootnodes = loadedSpec.bootnodes ++ extraBootnodes)

    val config = NodeConfig(
      dataDir = Paths.get(dataDir),
      listenPort = opts.get("port").map(_.toInt).getOrElse(DefaultPort),
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
      grandpa = flags.contains("--grandpa") || authorKeys.nonEmpty,
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

  /** A validator's public keys and the JAMNP-S peer name its Ed25519 key gives, as JSON. */
  private def publicKeysJson(k: ValidatorKeySet): String =
    def hex(b: Array[Byte]) = "0x" + JamBytes(b).toHex
    s"""{
       |  "bandersnatch": "${hex(k.bandersnatchPublic)}",
       |  "ed25519": "${hex(k.ed25519Public)}",
       |  "bls": "${hex(k.blsPublic)}",
       |  "peer_id": "${io.forge.jam.network.NodeIdentity.altName(k.ed25519Public)}"
       |}""".stripMargin

  /** `spec` itself, or for an http(s) URL the file it is downloaded to in `dataDir` (the last good download when
    * this one fails).
    */
  private def localSpec(spec: String, dataDir: Path): Path =
    if !spec.startsWith("http://") && !spec.startsWith("https://") then Paths.get(spec)
    else
      val target = dataDir.resolve("spec.json")
      try
        Files.createDirectories(dataDir)
        val client = java.net.http.HttpClient
          .newBuilder()
          .followRedirects(java.net.http.HttpClient.Redirect.NORMAL)
          .connectTimeout(java.time.Duration.ofSeconds(20))
          .build()
        val request = java.net.http.HttpRequest
          .newBuilder(java.net.URI.create(spec))
          .timeout(java.time.Duration.ofSeconds(60))
          .build()
        val response = client.send(request, java.net.http.HttpResponse.BodyHandlers.ofByteArray())
        if response.statusCode() != 200 then throw new java.io.IOException(s"HTTP ${response.statusCode()}")
        val tmp = dataDir.resolve("spec.json.tmp")
        Files.write(tmp, response.body())
        Files.move(tmp, target, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
        logger.info(s"chain spec downloaded from $spec")
      catch
        case e: Exception =>
          if Files.exists(target) then logger.warn(s"chain spec download from $spec failed ($e); using $target")
          else fail(s"could not download the chain spec from $spec: $e")
      target

  /** Chain spec used when `--spec` is not given: the public jamtoaster testnet. */
  private val DefaultSpecUrl = "https://jamtoaster.network/chainspec/public.json"

  /** Port the node listens on unless `--port` says otherwise. */
  private val DefaultPort = 40000

  /** A 32-byte key given as hex, or as the path of a file holding the hex. */
  private def readKey(option: String, hexOrPath: String): Array[Byte] =
    val (hex, from) =
      if hexOrPath.matches("(0x)?[0-9a-fA-F]{64}") then (hexOrPath, s"--$option")
      else
        val path = Paths.get(hexOrPath)
        if !Files.exists(path) then fail(s"$path: no such file (expected a 32-byte hex key or the path of one)")
        (Files.readString(path).trim, path.toString)
    val key = JamBytes.fromHex(hex).fold(e => fail(s"$from: not hex: $e"), _.toArray)
    if key.length != 32 then fail(s"$from: expected a 32-byte hex key, got ${key.length} bytes")
    key

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
