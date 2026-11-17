package io.forge.jam.node

import java.net.InetSocketAddress
import java.nio.file.{Files, Path, Paths}
import java.nio.file.attribute.PosixFilePermissions
import java.security.SecureRandom

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.JamBytes
import io.forge.jam.crypto.BandersnatchVrf
import io.forge.jam.db.RocksDbMemory
import io.forge.jam.pvm.PvmSettings

/** CLI entry point:
  * {{{
  * jam-node --data <data-dir> [--spec <chain-spec.json or URL>] [--port N (default 40000)]
  *          [--seed <64-hex Ed25519 seed> | --seed-file <path>] [--era-start <unix-seconds>]
  *          [--peer <64-hex-ed25519>@host:port ...]
  *          [--author <validator-index> | --validator-seed <path>]
  *          [--bandersnatch-key <hex or path>] [--ed25519-key <hex or path>]
  *          [--telemetry <host:port>] [--reuse-port] [--grandpa] [--beefy] [--submit-port N]
  *          [--finality-peers N | --announced-finality] [--prune-history]
  *          [--fault-tamper-reports]   (testing only: guarantee wrong reports, to exercise disputes)
  * jam-node --data <data-dir> [--author <index> | --validator-seed <path>]
  *          [--bandersnatch-key <hex or path>] [--ed25519-key <hex or path>] --print-keys
  * }}}
  *
  * With `--author i` the node's network identity is dev validator i's Ed25519
  * key (JIP-5 trivial seed for i), as JAMNP-S expects of validators; peers then
  * map the connection to validator index i (CE 131 proxying). `--seed` or
  * `--seed-file` with it is refused: it would give the node another identity.
  * An unknown option, an option without its value, or an argument that is no
  * option is refused before anything is opened. `--seed` on the command line is
  * visible in the process list (a warning says so): `--seed-file` is the way to
  * keep it out. Native crypto
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

  /** Whether the node runs GRANDPA. A validator votes. A node without validator keys observes: it finalizes what a
    * justification signed by a supermajority of the voter set proves (CE 130, CE 153) and nothing else. Only an
    * operator who asks for it gets finality from unsigned UP 0 announcements instead: `--announced-finality` (a
    * quorum of the current validators' connections) or `--finality-peers N` (any N peers).
    */
  private[node] def grandpaWanted(asked: Boolean, validator: Boolean, unsignedFinality: Boolean): Boolean =
    asked || validator || !unsignedFinality

  def main(args: Array[String]): Unit =
    try run(args)
    catch
      case e: Throwable =>
        System.err.println(s"jam-node failed to start: $e")
        e.printStackTrace()
        sys.exit(1)

  private def installProcessSettings(): Unit =
    PvmSettings.process.install(PvmSettings.process.fromSystemProperties)
    RocksDbMemory.Settings.process.install(RocksDbMemory.Settings.process.fromSystemProperties)
    BandersnatchVrf.baseDir.install(BandersnatchVrf.baseDir.fromSystemProperties)

  private def run(args: Array[String]): Unit =
    val commandLine = parseCommandLine(args.toList).fold(fail, identity)
    if commandLine.flags.contains("--help") then
      println(Usage)
      return
    installProcessSettings()
    val peerArgs = commandLine.repeated("--peer")
    val rpcOrigins = commandLine.repeated("--rpc-allow-origin")
    val flags = commandLine.flags
    val opts = commandLine.options
    if opts.contains("seed") then
      System.err.println("warning: --seed is visible in the process list to every user of this machine; give --seed-file <path> instead")

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

    val syncFrom = opts.get("sync-from").map(v => parseSyncFrom(v).fold(fail, identity))
    val config = NodeConfig(
      dataDir = Paths.get(dataDir),
      listenPort = opts.get("port").map(_.toInt).getOrElse(DefaultPort),
      ed25519Seed = opts
        .get("seed")
        .map(s => JamBytes.fromHexUnsafe(s.stripPrefix("0x")).toArray)
        .orElse(opts.get("seed-file").map(p => loadOrCreateSeed(Paths.get(p))))
        .orElse(authorKeys.map(_.ed25519Secret)),
      eraStartSeconds = opts
        .get("era-start")
        .map(_.toLong)
        .getOrElse(SlotClock.JamCommonEraSeconds),
      reusePort = flags.contains("--reuse-port"),
      grandpa = grandpaWanted(
        asked = flags.contains("--grandpa"),
        validator = authorKeys.nonEmpty,
        unsignedFinality = opts.contains("finality-peers") || flags.contains("--announced-finality")
      ),
      pruneFinalizedHistory = flags.contains("--prune-history"),
      tamperReports = flags.contains("--fault-tamper-reports"),
      finalityPeers = opts.get("finality-peers").map(_.toInt),
      telemetry = opts.get("telemetry").map(parseHostPort),
      submitPort = opts.get("submit-port").map(_.toInt),
      syncFrom = syncFrom.map(_._1),
      syncFromSetId = syncFrom.flatMap(_._2)
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
    opts.get("rpc-port").foreach(p => node.enableRpc(p.toInt, opts.getOrElse("rpc-listen-ip", "127.0.0.1"), rpcOrigins))
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

  private[node] def parseSyncFrom(value: String): Either[String, (io.forge.jam.core.primitives.Hash, Option[Long])] =
    val (hex, setId) = value.indexOf(':') match
      case -1 => (value, None)
      case i  => (value.substring(0, i), Some(value.substring(i + 1)))
    val bytes = JamBytes.fromHex(hex.stripPrefix("0x")).toOption.map(_.toArray)
    bytes match
      case None => Left(s"--sync-from: not a hex header hash: $hex")
      case Some(b) if b.length != 32 => Left(s"--sync-from: a header hash is 32 bytes, got ${b.length}")
      case Some(b) =>
        val hash = io.forge.jam.core.primitives.Hash(b)
        setId match
          case None => Right((hash, None))
          case Some(id) =>
            id.toLongOption.filter(_ >= 0) match
              case Some(n) => Right((hash, Some(n)))
              case None => Left(s"--sync-from: the GRANDPA set id after ':' must be a non-negative integer, got '$id'")

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
      if hexOrPath.matches("(0x)?[0-9a-fA-F]{64}") then
        System.err.println(s"warning: --$option given as hex is visible in the process list; give the path of a file instead")
        (hexOrPath, s"--$option")
      else
        val path = Paths.get(hexOrPath)
        if !Files.exists(path) then fail(s"$path: no such file (expected a 32-byte hex key or the path of one)")
        warnIfExposed(path)
        (Files.readString(path).trim, path.toString)
    val key = JamBytes.fromHex(hex).fold(e => fail(s"$from: not hex: $e"), _.toArray)
    if key.length != 32 then fail(s"$from: expected a 32-byte hex key, got ${key.length} bytes")
    key

  /** The 32-byte seed in `path` (hex), or a new random one written there, readable by the owner only. */
  private def loadOrCreateSeed(path: Path): Array[Byte] =
    if Files.exists(path) then
      warnIfExposed(path)
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

  private[node] def exposedTo(path: Path): Option[String] =
    try
      val perms = Files.getPosixFilePermissions(path)
      import java.nio.file.attribute.PosixFilePermission.*
      val who = List(GROUP_READ -> "its group", OTHERS_READ -> "everyone").collect { case (p, w) if perms.contains(p) => w }
      Option.when(who.nonEmpty)(who.mkString(" and "))
    catch case _: UnsupportedOperationException => None

  private def warnIfExposed(path: Path): Unit =
    exposedTo(path).foreach(who =>
      System.err.println(s"warning: $path holds a secret key and is readable by $who; restrict it with: chmod 600 $path")
    )

  private def parseHostPort(s: String): InetSocketAddress =
    val colon = s.lastIndexOf(':')
    if colon <= 0 then fail(s"expected host:port, got: $s")
    new InetSocketAddress(s.substring(0, colon), s.substring(colon + 1).toInt)

  private[node] final case class CommandLine(
      options: Map[String, String],
      repeated: Map[String, List[String]],
      flags: Set[String]
  )

  private val ValueOptions = Set(
    "data", "spec", "port", "seed", "seed-file", "era-start", "author", "validator-seed", "bandersnatch-key",
    "ed25519-key", "telemetry", "submit-port", "finality-peers", "sync-from", "rpc-port", "rpc-listen-ip"
  )

  private val RepeatedOptions = Set("--peer", "--rpc-allow-origin")

  private val Flags = Set(
    "--reuse-port", "--grandpa", "--announced-finality", "--beefy", "--prune-history", "--print-keys",
    "--fault-tamper-reports", "--help"
  )

  private val Numbers: Map[String, (Long, Long)] = Map(
    "port" -> (0L, 65535L),
    "rpc-port" -> (0L, 65535L),
    "submit-port" -> (0L, 65535L),
    "author" -> (0L, Int.MaxValue.toLong),
    "finality-peers" -> (1L, Int.MaxValue.toLong),
    "era-start" -> (0L, Long.MaxValue)
  )

  private[node] def parseCommandLine(args: List[String]): Either[String, CommandLine] =
    def missing(option: String) = s"$option needs a value"
    @scala.annotation.tailrec
    def loop(rest: List[String], acc: CommandLine): Either[String, CommandLine] =
      rest match
        case Nil => Right(acc)
        case flag :: more if Flags.contains(flag) => loop(more, acc.copy(flags = acc.flags + flag))
        case option :: more if RepeatedOptions.contains(option) =>
          more match
            case value :: after if !value.startsWith("--") =>
              loop(after, acc.copy(repeated = acc.repeated.updated(option, acc.repeated(option) :+ value)))
            case _ => Left(missing(option))
        case option :: more if option.startsWith("--") && ValueOptions.contains(option.drop(2)) =>
          val name = option.drop(2)
          more match
            case value :: after if !value.startsWith("--") =>
              if acc.options.contains(name) then Left(s"$option is given twice")
              else loop(after, acc.copy(options = acc.options.updated(name, value)))
            case _ => Left(missing(option))
        case option :: _ if option.startsWith("-") =>
          Left(s"unknown option: $option (jam-node --help lists the options)")
        case other :: _ => Left(s"unexpected argument: $other (every argument is an --option)")
    loop(args, CommandLine(Map.empty, Map.empty.withDefaultValue(Nil), Set.empty)).flatMap(checked)

  private def checked(cl: CommandLine): Either[String, CommandLine] =
    val opts = cl.options
    def has(name: String) = opts.contains(name)
    val numbers = Numbers.toList.flatMap { (name, range) =>
      opts.get(name).map { value =>
        value.toLongOption.filter(n => n >= range._1 && n <= range._2).toRight(
          s"--$name takes a whole number from ${range._1} to ${range._2}, not '$value'"
        )
      }
    }
    for
      _ <- numbers.collectFirst { case Left(e) => e }.toLeft(())
      _ <- opts.get("seed").filterNot(_.matches("(0x)?[0-9a-fA-F]{64}")).map(_ =>
        "--seed takes a 32-byte Ed25519 seed as 64 hex digits (or use --seed-file <path>)"
      ).toLeft(())
      _ <- Either.cond(!(has("seed") && has("seed-file")), (), "--seed and --seed-file both name the network identity: give one")
      _ <- Either.cond(
        !(has("author") && has("validator-seed")),
        (),
        "--author and --validator-seed are exclusive: give one source of validator keys"
      )
      _ <- Either.cond(
        !(has("validator-seed") && (has("seed") || has("seed-file"))),
        (),
        "--validator-seed sets the network identity too: drop --seed/--seed-file"
      )
      _ <- Either.cond(
        !(has("author") && (has("seed") || has("seed-file"))),
        (),
        "--author i runs as dev validator i, whose Ed25519 key is the network identity: drop --seed/--seed-file " +
          "(for keys of your own, use --validator-seed, or --ed25519-key with --bandersnatch-key)"
      )
    yield cl

  private[node] val Usage: String =
    """jam-node --data <data-dir> [--spec <chain-spec.json or URL>] [--port N (default 40000)]
      |         [--seed <64-hex Ed25519 seed> | --seed-file <path>] [--era-start <unix-seconds>]
      |         [--peer <64-hex-ed25519>@host:port ...]
      |         [--author <validator-index> | --validator-seed <path>]
      |         [--bandersnatch-key <hex or path>] [--ed25519-key <hex or path>]
      |         [--telemetry <host:port>] [--reuse-port] [--grandpa] [--beefy] [--submit-port N]
      |         [--finality-peers N | --announced-finality] [--prune-history]
      |         [--sync-from <header hash>[:<set id>]]
      |         [--rpc-port N [--rpc-listen-ip IP (default 127.0.0.1)] [--rpc-allow-origin <origin> ...]]
      |         [--fault-tamper-reports]
      |jam-node --data <data-dir> [--author <index> | --validator-seed <path>]
      |         [--bandersnatch-key <hex or path>] [--ed25519-key <hex or path>] --print-keys
      |jam-node --help""".stripMargin

  private def fail(msg: String): Nothing =
    System.err.println(msg)
    sys.exit(1)
