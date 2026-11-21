package io.forge.jam.node

import java.net.{InetAddress, InetSocketAddress}
import java.nio.file.{Files, Path, Paths}

import com.typesafe.scalalogging.LazyLogging
import io.forge.jam.core.JamBytes
import io.forge.jam.crypto.BandersnatchVrf
import io.forge.jam.db.RocksDbMemory
import io.forge.jam.network.NodeIdentity
import io.forge.jam.pvm.PvmSettings

/** CLI entry point. */
object Main extends LazyLogging:

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
    val cl = parseCommandLine(args.toList).fold(usageError, identity)
    cl.command match
      case None if cl.flags.contains("--help") => println(Usage)
      case None if cl.flags.contains("--version") => println(VersionText)
      case None if cl.flags.contains("-V") => println(ShortVersionText)
      case None => usageError(s"a command is required\n\n$Usage")
      case Some(Help) => println(cl.arguments.headOption.flatMap(CommandUsage.get).getOrElse(Usage))
      case Some(command) if cl.flags.contains("--help") => println(CommandUsage(command))
      case Some(GenKeys) => genKeys(cl)
      case Some(ListKeys) => listKeys(cl)
      case Some(DumpSpec) => dumpSpec(cl)
      case Some(_) => runNode(cl)

  private def chainOf(cl: CommandLine): (String, ChainSpec) =
    val chain = cl.global.getOrElse("chain", if cl.options.contains("dev-validator") then "dev" else Chains.Default)
    val json = Chains.resolve(chain).fold(fail, identity)
    (json, ChainSpec.fromJson(json).fold(e => fail(s"Error loading chain spec $chain: $e"), identity))

  private def configDirOf(cl: CommandLine, chainId: String): Path =
    cl.global.get("config-path").map(Paths.get(_)).getOrElse(defaultBase(config = true)).resolve(chainId)

  private def genKeys(cl: CommandLine): Unit =
    val (_, spec) = chainOf(cl)
    val file = Keystore.fileFor(Keystore.dir(configDirOf(cl, spec.id)), cl.arguments.headOption, java.time.LocalDateTime.now())
    val seed = Keystore.create(file).fold(fail, identity)
    Keystore.describe(seed).foreach(println)

  private def listKeys(cl: CommandLine): Unit =
    val (_, spec) = chainOf(cl)
    val (seeds, errors) = Keystore.load(Keystore.dir(configDirOf(cl, spec.id)), required = true)
    for seed <- seeds; line <- Keystore.describe(seed) do println(s"${seed.path}: $line")
    if errors.nonEmpty then
      errors.foreach(e => System.err.println(e))
      fail("Error: Failed to load keys")

  private def dumpSpec(cl: CommandLine): Unit =
    val (json, _) = chainOf(cl)
    Chains.write(json, Paths.get(cl.arguments.head)).fold(fail, identity)

  private def runNode(cl: CommandLine): Unit =
    val flags = cl.flags
    val opts = cl.options
    systemProperties(cl).foreach((name, value) => System.setProperty(name, value))
    installProcessSettings()
    if opts.get("pvm-backend").contains("compiler") && !io.forge.jam.pvm.ExecutionMode.nativeAvailable then
      fail(
        "--pvm-backend compiler: the recompiler library is not available; name it with " +
          "-Djam.pvm.recompiler.lib=<path to libpvm_recompiler>, " +
          "or use --pvm-backend interpreter"
      )
    val listenIp = opts.get("listen-ip").map(v => parseIp("--listen-ip", v).fold(fail, identity)).getOrElse(AnyAddress)
    if !listenIp.isAnyLocalAddress && !isOwnAddress(listenIp) then
      fail(s"--listen-ip ${listenIp.getHostAddress} is not an address of this machine (:: or 0.0.0.0 listens on all of them)")

    val (_, loadedSpec) = chainOf(cl)
    if loadedSpec.genesisValidatorKeys._1.isEmpty then
      fail(s"chain ${loadedSpec.id} has no validators in its genesis state: it cannot be run (choose another with --chain)")
    val configDir = configDirOf(cl, loadedSpec.id)

    val temporaryData = Option.when(flags.contains("--temp"))(newTempDataDir())
    val dataDir = temporaryData.getOrElse(
      opts.get("data-path").map(Paths.get(_)).getOrElse(defaultBase(config = false)).resolve(loadedSpec.id)
    )
    val running = new java.util.concurrent.atomic.AtomicReference[JamNode]()
    Runtime.getRuntime.addShutdownHook(
      new Thread(
        () =>
          Option(running.get).foreach(_.shutdown())
          temporaryData.foreach(deleteTree)
        ,
        "jam-shutdown"
      )
    )
    logger.info(s"Chain ID: ${loadedSpec.id}")
    logger.info(s"Config dir: $configDir")
    logger.info(s"Data dir: $dataDir${if temporaryData.isDefined then " (--temp: deleted on exit)" else ""}")

    val keysDir = Keystore.dir(configDir)
    val (stored, unreadable) = Keystore.load(keysDir, required = false)
    val named = cl.repeated("key-seed-file").map(p => Keystore.read(Paths.get(p)))
    val loadErrors = unreadable ++ named.collect { case Left(e) => e }
    if loadErrors.nonEmpty then
      loadErrors.foreach(e => System.err.println(e))
      fail("Error: Failed to load keys")
    val seeds = stored ++ named.collect { case Right(s) => s }
    seeds.foreach(s => warnIfExposed(s.path))

    val devIndex = opts.get("dev-validator").map(_.toInt)
    devIndex.foreach { i =>
      val validators = loadedSpec.genesisValidatorKeys._1.size
      if i >= validators then fail(s"--dev-validator $i: the chain's genesis state has $validators validators")
    }
    val peerSeed = opts.get("peer-id").filter(_ => devIndex.isEmpty).map { name =>
      val key = NodeIdentity.fromAltName(name).get
      seeds.find(s => java.util.Arrays.equals(s.keys.ed25519Public, key)).getOrElse(fail(s"Error: Don't have secret key for peer ID $name"))
    }
    val ownKeys = List("bandersnatch-key", "ed25519-key").filter(opts.contains)
    val validator = opts.get("mode") match
      case Some(mode) => mode == "validator"
      case None       => devIndex.isDefined || peerSeed.isDefined || ownKeys.nonEmpty
    val baseKeys: Option[ValidatorKeySet] =
      if !validator then None
      else
        devIndex.map(ValidatorKeySet.dev).orElse(peerSeed.map(_.keys)).orElse {
          seeds match
            case Nil         => None
            case List(only)  => Some(only.keys)
            case several     =>
              fail(s"${several.size} seeds in $keysDir and --key-seed-file: name the validator's with --peer-id")
        }
    if ownKeys.nonEmpty && baseKeys.isEmpty then
      fail(
        s"--${ownKeys.head} replaces a key of the validator's seed: give --peer-id or --dev-validator, or keep the seed in " +
          s"$keysDir (gen-keys)"
      )
    val authorKeys = baseKeys
      .map(keys =>
        opts.get("bandersnatch-key").fold(keys)(k => ValidatorKeySet.withBandersnatchSeed(keys, readKey("bandersnatch-key", k)))
      )
      .map(keys => opts.get("ed25519-key").fold(keys)(k => ValidatorKeySet.withEd25519Seed(keys, readKey("ed25519-key", k))))
    for keys <- authorKeys; seed <- peerSeed if !java.util.Arrays.equals(keys.ed25519Public, seed.keys.ed25519Public) do
      fail("--ed25519-key gives the validator another network identity than --peer-id names: give one of them")
    if validator && authorKeys.isEmpty then
      logger.warn(s"validator mode, but no validator key: no seed in $keysDir and none named; running without duties")

    val identitySeed = authorKeys.map(_.ed25519Secret).orElse(peerSeed.map(_.keys.ed25519Secret))
    val devEndpoint = devIndex.map(i =>
      loadedSpec.genesisValidatorEndpoints.lift(i).flatten.getOrElse(fail(s"--dev-validator $i: the genesis state names no endpoint for it"))
    )
    val externalIp = devEndpoint.map(_.getAddress).orElse(opts.get("external-ip").map(v => parseIp("--external-ip", v).fold(fail, identity)))
    val listenPort = devEndpoint.map(_.getPort).orElse(opts.get("port").map(_.toInt)).getOrElse(0)

    val bootnodes = cl.repeated("bootnode").map(Bootnode.parse)
    val spec = if bootnodes.isEmpty then loadedSpec else loadedSpec.copy(bootnodes = loadedSpec.bootnodes ++ bootnodes)
    val finality = finalityOf(cl)
    val syncFrom = opts.get("sync-from").map(v => parseSyncFrom(v).fold(fail, identity))
    val config = NodeConfig(
      dataDir = dataDir,
      listenPort = listenPort,
      listenIp = listenIp,
      externalIp = externalIp,
      ed25519Seed = identitySeed,
      eraStartSeconds = opts.get("era-start").map(_.toLong).getOrElse(SlotClock.JamCommonEraSeconds),
      reusePort = flags.contains("--reuse-port"),
      grandpa = finality == Finality.Grandpa,
      dummyFinality = finality == Finality.Dummy,
      pruneFinalizedHistory = flags.contains("--prune-history"),
      tamperReports = flags.contains("--fault-tamper-reports"),
      inlineJudgmentGuarantee = flags.contains("--inline-judgment-guarantee"),
      finalityPeers = opts.get("finality-peers").map(_.toInt),
      telemetry = opts.get("telemetry").map(parseHostPort(_).fold(fail, a => new InetSocketAddress(a.getHostString, a.getPort))),
      submitPort = opts.get("submit-port").map(_.toInt),
      syncFrom = syncFrom.map(_._1),
      syncFromSetId = syncFrom.flatMap(_._2)
    )

    try io.forge.jam.vrfs.NativeCpu.check()
    catch case e: UnsupportedOperationException => fail(e.getMessage)
    val node = new JamNode(spec, config).start()
    running.set(node)
    logger.info(s"This node is ${node.identity.altName}@${externalIp.getOrElse(InetAddress.getLoopbackAddress).getHostAddress}:${node.listenPort}")
    authorKeys.foreach { k =>
      node.enableAuthoring(Seq(k))
      node.enableGuaranteeing(Seq(k))
      node.enableAssuring(Seq(k))
      node.enableAuditing(Seq(k))
    }
    if config.grandpa then node.enableGrandpa(authorKeys)
    if flags.contains("--beefy") then node.enableBeefy(authorKeys)
    if flags.contains("--rpc") || (!flags.contains("--no-rpc") && !validator) then
      val methods = opts.get("rpc-methods").flatMap(RpcMethods.parse).getOrElse(RpcMethods.Auto)
      node.enableRpc(opts.get("rpc-port").map(_.toInt).getOrElse(DefaultRpcPort), opts.getOrElse("rpc-listen-ip", "::"), cl.repeated("rpc-allow-origin"), methods)
    opts.get("metrics-port").foreach { p =>
      val host = opts.get("metrics-listen-ip").map(v => parseIp("--metrics-listen-ip", v).fold(fail, identity).getHostAddress)
      node.enableMetrics(p.toInt, host.getOrElse("127.0.0.1"))
    }
    node.onSlot { slot =>
      val best = node.chain.best
      val fin = node.chain.finalized
      logger.info(
        s"slot $slot best=${best.slot} ${best.hash.toHex} finalized=${fin.slot} " +
          s"peers=${node.distribution.peerCount}"
      )
    }

    Thread.currentThread().join()

  enum Finality:
    case Dummy, Grandpa, Peers

  private[node] def finalityOf(cl: CommandLine): Finality =
    cl.options.get("finality-mode") match
      case Some("grandpa") => Finality.Grandpa
      case Some(_)         => Finality.Dummy
      case None =>
        if cl.options.contains("finality-peers") || cl.flags.contains("--announced-finality") then Finality.Peers
        else Finality.Dummy

  private[node] def defaultBase(
      config: Boolean,
      env: String => Option[String] = sys.env.get,
      home: String = sys.props.getOrElse("user.home", ".")
  ): Path =
    val base =
      if config then env("XDG_CONFIG_HOME").filter(_.nonEmpty).map(Paths.get(_)).getOrElse(Paths.get(home, ".config"))
      else env("XDG_DATA_HOME").filter(_.nonEmpty).map(Paths.get(_)).getOrElse(Paths.get(home, ".local", "share"))
    base.resolve(AppDirName)

  private val AppDirName = "jam-forge"

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

  private[node] def newTempDataDir(): Path = Files.createTempDirectory("jam-node-")

  private[node] def deleteTree(dir: Path): Unit =
    if Files.exists(dir, java.nio.file.LinkOption.NOFOLLOW_LINKS) then
      val walk = Files.walk(dir)
      try
        walk.sorted(java.util.Comparator.reverseOrder()).forEach { p =>
          try Files.deleteIfExists(p): Unit
          catch case e: java.io.IOException => System.err.println(s"could not delete $p: $e")
        }
      finally walk.close()

  private[node] val ShortVersionText: String = s"${BuildInfo.name} ${BuildInfo.version}"

  private[node] val VersionText: String =
    s"$ShortVersionText${BuildInfo.gitSha.fold("")(sha => s" ($sha)")}\nGP ${JamNode.GrayPaperVersion}"

  private[node] def systemProperties(cl: CommandLine): Map[String, String] =
    cl.options.get("pvm-backend").map { backend =>
      PvmSettings.ExecutionModeProperty -> (if backend == "interpreter" then "interpreted" else "recompiled")
    }.toMap

  private[node] def parseIp(option: String, value: String): Either[String, InetAddress] =
    val dotted = """(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})""".r
    val literal = value.stripPrefix("[").stripSuffix("]")
    val parsed = literal match
      case dotted(a, b, c, d) if List(a, b, c, d).forall(_.toInt <= 255) =>
        Some(InetAddress.getByAddress(List(a, b, c, d).map(_.toInt.toByte).toArray))
      case v6 if v6.contains(':') =>
        try Some(InetAddress.ofLiteral(v6))
        catch case _: IllegalArgumentException => None
      case _ => None
    parsed match
      case None => Left(s"$option takes an IPv4 or IPv6 address (such as ::, 0.0.0.0, 192.0.2.1 or 2001:db8::1), not '$value'")
      case Some(ip) if ip.isMulticastAddress => Left(s"$option: $value is a multicast address")
      case Some(ip) => Right(ip)

  private def isOwnAddress(ip: InetAddress): Boolean =
    ip.isLoopbackAddress || (try java.net.NetworkInterface.getByInetAddress(ip) != null
    catch case _: java.io.IOException => false)

  private val AnyAddress: InetAddress = InetAddress.getByAddress(new Array[Byte](16))

  private val DefaultRpcPort = 19800

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

  private[node] def parseHostPort(s: String): Either[String, InetSocketAddress] =
    val colon = s.lastIndexOf(':')
    val port = s.substring(colon + 1).toIntOption.filter(p => p > 0 && p <= 65535)
    if colon <= 0 || port.isEmpty then Left(s"--telemetry takes <HOST:PORT>, not '$s'")
    else Right(InetSocketAddress.createUnresolved(s.substring(0, colon).stripPrefix("[").stripSuffix("]"), port.get))

  private[node] final case class CommandLine(
      global: Map[String, String],
      command: Option[String],
      options: Map[String, String],
      repeated: Map[String, List[String]],
      flags: Set[String],
      arguments: List[String]
  )

  private val Run = "run"
  private val GenKeys = "gen-keys"
  private val ListKeys = "list-keys"
  private val DumpSpec = "dump-spec"
  private val Help = "help"

  private final case class Takes(
      values: Map[String, String],
      repeated: Map[String, String],
      flags: Map[String, String],
      maxArguments: Int
  )

  private def longs(names: String*): Map[String, String] = names.map(n => s"--$n" -> n).toMap
  private val HelpFlags = Map("-h" -> "--help", "--help" -> "--help")

  private val GlobalTakes = Takes(
    values = longs("config-path", "chain") + ("-c" -> "config-path"),
    repeated = Map.empty,
    flags = HelpFlags ++ Map("-V" -> "-V", "--version" -> "--version"),
    maxArguments = 0
  )

  private val RunTakes = Takes(
    values = longs(
      "data-path", "mode", "peer-id", "external-ip", "listen-ip", "port", "telemetry", "dev-validator",
      "pvm-backend", "finality-mode", "rpc-listen-ip", "rpc-port",
      "sync-from", "metrics-port", "metrics-listen-ip", "rpc-methods", "finality-peers", "submit-port", "era-start",
      "bandersnatch-key", "ed25519-key"
    ) + ("-d" -> "data-path"),
    repeated = longs("key-seed-file", "bootnode", "rpc-allow-origin"),
    flags = HelpFlags ++ Map("-t" -> "--temp") ++ List(
      "--temp", "--rpc", "--no-rpc",
      "--prune-history", "--announced-finality", "--beefy", "--reuse-port", "--inline-judgment-guarantee",
      "--fault-tamper-reports"
    ).map(f => f -> f),
    maxArguments = 0
  )

  private val CommandTakes: Map[String, Takes] = Map(
    Run -> RunTakes,
    GenKeys -> Takes(Map.empty, Map.empty, HelpFlags, maxArguments = 1),
    ListKeys -> Takes(Map.empty, Map.empty, HelpFlags, maxArguments = 0),
    DumpSpec -> Takes(Map.empty, Map.empty, HelpFlags, maxArguments = 1),
    Help -> Takes(Map.empty, Map.empty, Map.empty, maxArguments = 1)
  )

  private[node] def parseCommandLine(args: List[String]): Either[String, CommandLine] =
    final case class Read(values: Map[String, String], repeated: Map[String, List[String]], flags: Set[String], arguments: List[String])
    def read(rest: List[String], takes: Takes, where: String, global: Boolean): Either[String, (Read, List[String])] =
      @scala.annotation.tailrec
      def loop(rest: List[String], acc: Read): Either[String, (Read, List[String])] =
        rest match
          case Nil => Right((acc, Nil))
          case token :: more if token.startsWith("-") && token != "-" =>
            val (spelling, inline) = token.indexOf('=') match
              case -1 => (token, None)
              case i  => (token.substring(0, i), Some(token.substring(i + 1)))
            def value: Either[String, (String, List[String])] = inline match
              case Some(v) => Right((v, more))
              case None =>
                more match
                  case v :: after if !v.startsWith("--") && !takes.flags.contains(v) => Right((v, after))
                  case _ => Left(s"a value is required for '$spelling' but none was supplied")
            if takes.flags.contains(spelling) && inline.isEmpty then loop(more, acc.copy(flags = acc.flags + takes.flags(spelling)))
            else if takes.values.contains(spelling) then
              val name = takes.values(spelling)
              value match
                case Left(e) => Left(e)
                case Right(_) if acc.values.contains(name) => Left(s"the argument '--$name' cannot be used multiple times")
                case Right((v, after)) => loop(after, acc.copy(values = acc.values.updated(name, v)))
            else if takes.repeated.contains(spelling) then
              val name = takes.repeated(spelling)
              value match
                case Left(e) => Left(e)
                case Right((v, after)) => loop(after, acc.copy(repeated = acc.repeated.updated(name, acc.repeated(name) :+ v)))
            else
              val hint =
                if global && (RunTakes.values.contains(spelling) || RunTakes.flags.contains(spelling) || RunTakes.repeated.contains(spelling))
                then s" ('$spelling' is an option of run: give it after the command)"
                else if !global && GlobalTakes.values.contains(spelling) then s" ('$spelling' comes before the command)"
                else ""
              Left(s"unexpected argument '$token' found$hint ($where --help lists the options)")
          case _ :: _ if global => Right((acc, rest))
          case argument :: more =>
            if acc.arguments.size >= takes.maxArguments then Left(s"unexpected argument '$argument' found ($where --help lists the arguments)")
            else loop(more, acc.copy(arguments = acc.arguments :+ argument))
      loop(rest, Read(Map.empty, Map.empty.withDefaultValue(Nil), Set.empty, Nil))

    read(args, GlobalTakes, "jam-node", global = true).flatMap { (globals, rest) =>
      val noCommand = CommandLine(globals.values, None, Map.empty, Map.empty.withDefaultValue(Nil), globals.flags, Nil)
      rest match
        case _ if globals.flags.nonEmpty => Right(noCommand)
        case Nil                         => Right(noCommand)
        case command :: more =>
          CommandTakes.get(command).toRight(s"unrecognized subcommand '$command' (jam-node --help lists the commands)").flatMap { takes =>
            read(more, takes, s"jam-node $command", global = false).map { (r, _) =>
              CommandLine(globals.values, Some(command), r.values, r.repeated, r.flags, r.arguments)
            }
          }
    }.flatMap(checked)

  private def checked(cl: CommandLine): Either[String, CommandLine] =
    val opts = cl.options
    def has(name: String) = opts.contains(name)
    def flag(name: String) = cl.flags.contains(name)
    def oneOf(name: String, values: List[String]): Either[String, Unit] =
      opts.get(name).filterNot(values.contains).map(v =>
        s"invalid value '$v' for '--$name'\n  [possible values: ${values.mkString(", ")}]"
      ).toLeft(())
    def ip(name: String, check: InetAddress => Option[String] = _ => None): Either[String, Unit] =
      opts.get(name).fold(Right(())) { value => parseIp(s"--$name", value).flatMap(address => check(address).toLeft(())) }
    def conflict(a: Boolean, b: Boolean, message: String): Either[String, Unit] = Either.cond(!(a && b), (), message)
    val numbers = Numbers.toList.flatMap { (name, range) =>
      opts.get(name).map { value =>
        value.toLongOption.filter(n => n >= range._1 && n <= range._2).toRight(
          s"invalid value '$value' for '--$name': a whole number from ${range._1} to ${range._2}"
        )
      }
    }
    for
      _ <- Either.cond(!cl.command.contains(DumpSpec) || cl.flags.contains("--help") || cl.arguments.size == 1, (),
        "the following required argument was not provided: <SPEC_FILE> (jam-node --chain <CHAIN> dump-spec <SPEC_FILE>)")
      _ <- cl.command.filter(_ == Help).flatMap(_ => cl.arguments.headOption).filterNot(CommandUsage.contains).map(c =>
        s"unrecognized subcommand '$c'"
      ).toLeft(())
      _ <- numbers.collectFirst { case Left(e) => e }.toLeft(())
      _ <- oneOf("mode", List("ordinary", "validator"))
      _ <- oneOf("pvm-backend", PvmBackends)
      _ <- oneOf("finality-mode", List("dummy", "grandpa"))
      _ <- oneOf("rpc-methods", List("safe", "unsafe", "auto"))
      _ <- opts.get("peer-id").filter(NodeIdentity.fromAltName(_).isEmpty).map(v =>
        s"invalid value '$v' for '--peer-id <PEER_ID>': expected 'e' and 52 base-32 digits (as gen-keys and list-keys print it)"
      ).toLeft(())
      _ <- cl.repeated("bootnode").find(b => scala.util.Try(Bootnode.parse(b)).isFailure).map(b =>
        s"invalid value '$b' for '--bootnode <BOOTNODE>': expected <PEER_ID>@<HOST>:<PORT>"
      ).toLeft(())
      _ <- opts.get("telemetry").map(parseHostPort).collect { case Left(e) => e }.toLeft(())
      _ <- conflict(flag("--temp"), has("data-path"), "the argument '--temp' cannot be used with '--data-path <DATA_PATH>'")
      _ <- conflict(flag("--rpc"), flag("--no-rpc"), "the argument '--rpc' cannot be used with '--no-rpc'")
      _ <- conflict(
        has("finality-mode"),
        has("finality-peers") || flag("--announced-finality"),
        "--finality-mode cannot be used with --finality-peers or --announced-finality, which take finality from peers instead"
      )
      _ <- conflict(
        opts.get("mode").contains("ordinary"),
        has("bandersnatch-key") || has("ed25519-key") || has("dev-validator"),
        "--mode ordinary runs no validator: drop --dev-validator, --bandersnatch-key and --ed25519-key"
      )
      _ <- ip("listen-ip")
      _ <- ip("rpc-listen-ip")
      _ <- ip(
        "external-ip",
        a => Option.when(a.isAnyLocalAddress)(s"--external-ip is the address peers reach this node at: not ${opts("external-ip")}")
      )
      _ <- Either.cond(!has("metrics-listen-ip") || has("metrics-port"), (), "--metrics-listen-ip needs --metrics-port")
      _ <- ip("metrics-listen-ip")
    yield cl

  private val Numbers: Map[String, (Long, Long)] = Map(
    "port" -> (0L, 65535L),
    "rpc-port" -> (0L, 65535L),
    "submit-port" -> (0L, 65535L),
    "metrics-port" -> (0L, 65535L),
    "dev-validator" -> (0L, 65535L),
    "finality-peers" -> (1L, Int.MaxValue.toLong),
    "era-start" -> (0L, Long.MaxValue)
  )

  private val PvmBackends = List("interpreter", "compiler")

  private[node] val Usage: String =
    """JAM-Forge node
      |
      |Usage: jam-node [OPTIONS] <COMMAND>
      |
      |Commands:
      |  dump-spec  Dump the spec of the chain given by the `--chain` option
      |  gen-keys   Generate a new secret key seed and print the derived session keys
      |  list-keys  List all session keys we have the secret key for
      |  run        Run a JAM-Forge node
      |  help       Print this message or the help of the given subcommand(s)
      |
      |Options:
      |  -c, --config-path <CONFIG_PATH>  Base config path
      |      --chain <CHAIN>              Chain to run. "polkadot", "toaster", "toaster:<NETWORK>", "dev",
      |                                   "dev:<BASE_PORT>", or the path of a chain spec file. Default is "polkadot"
      |                                   ("dev" with `run --dev-validator`). "toaster" fetches the chain spec over
      |                                   HTTPS; if the network is not given it defaults to "public"
      |  -h, --help                       Print help
      |  -V, --version                    Print version""".stripMargin

  private val RunUsage: String =
    """Run a JAM-Forge node
      |
      |Usage: jam-node [-c <CONFIG_PATH>] [--chain <CHAIN>] run [OPTIONS]
      |
      |Options:
      |  -d, --data-path <DATA_PATH>        Base data path; the chain's data is in <DATA_PATH>/<chain id>
      |  -t, --temp                         Use a temporary data directory, removed on exit. Conflicts with
      |                                     `--data-path`
      |      --key-seed-file <FILE>         A secret key seed file (32 bytes), besides <config>/<chain id>/keys/*.seed;
      |                                     may be given more than once
      |      --mode <MODE>                  ordinary or validator; validator when --peer-id, --dev-validator or a
      |                                     validator key is given. Proxy mode is not implemented
      |      --peer-id <PEER_ID>            Peer ID of this node: the seed whose key it is. Without it an ordinary
      |                                     node takes a new peer ID each start
      |      --external-ip <EXTERNAL_IP>    IP address other nodes reach this node at
      |      --listen-ip <LISTEN_IP>        IP address to listen on [default: ::]
      |      --port <PORT>                  UDP port to listen on; 0 means any unused port [default: 0]
      |      --telemetry <HOST:PORT>        Telemetry endpoint (JIP-3)
      |      --bootnode <BOOTNODE>          A bootnode, <PEER_ID>@<HOST>:<PORT>; may be given more than once
      |      --dev-validator <VAL_INDEX>    Run as the dev validator with this index: its keys, and its peer ID,
      |                                     address and port in the genesis state; the default chain becomes dev
      |      --pvm-backend <PVM_BACKEND>    interpreter, or compiler (the recompiler: native code; needs
      |                                     -Djam.pvm.recompiler.lib). Default: the compiler where available
      |      --finality-mode <MODE>         dummy (the best block's parent is finalized without consensus) or
      |                                     grandpa [default: dummy]
      |      --rpc                          Enable the RPC server (off by default for a validator)
      |      --no-rpc                       Disable the RPC server (on by default otherwise)
      |      --rpc-listen-ip <RPC_LISTEN_IP>  IP address for the RPC server to listen on [default: ::]
      |      --rpc-port <RPC_PORT>          Port for the RPC server to listen on; 0 means any unused port
      |                                     [default: 19800]
      |  -h, --help                         Print help
      |
      |Other options:
      |      --sync-from <HASH>[:<SET_ID>]  Join at this block, trusted, instead of replaying from genesis
      |      --prune-history                Delete finalized blocks a lookup-anchor age behind finality
      |      --metrics-port <PORT>          Prometheus metrics at GET /metrics, and GET /health (JSON; 200 while the
      |                                     node follows the chain, 503 when not)
      |      --metrics-listen-ip <IP>       Address the metrics listen on [default: 127.0.0.1]
      |      --rpc-methods <METHODS>        safe, unsafe or auto: whether the RPC serves work-package, bundle and
      |                                     preimage submission and segment recovery; auto serves them to loopback
      |                                     clients only [default: auto]
      |      --rpc-allow-origin <ORIGIN>    A browser origin the RPC accepts (e.g. http://localhost:3000); may be
      |                                     given more than once
      |      --finality-peers <N>           Finalize what N peers announce as final (UP 0), instead of --finality-mode
      |      --announced-finality           Finalize what a quorum of the current validators announce as final
      |      --bandersnatch-key <KEY>       The validator's Bandersnatch secret seed (JIP-5), as hex or a file holding
      |                                     it, in place of its seed's
      |      --ed25519-key <KEY>            The validator's Ed25519 secret seed, likewise; also its peer ID
      |      --beefy                        Run the BEEFY prototype
      |      --submit-port <PORT>           Loopback preimage endpoint (POST /preimage/<service-id>)
      |      --reuse-port                   SO_REUSEPORT on the listening socket
      |      --era-start <UNIX_SECONDS>     Start of the JAM common era, for devnets whose genesis starts now
      |      --inline-judgment-guarantee    CE 145 with the guarantee inside the judgment (JAMNP's newer form)
      |      --fault-tamper-reports         Testing only: guarantee wrong reports, to exercise disputes""".stripMargin

  private val CommandUsage: Map[String, String] = Map(
    Run -> RunUsage,
    GenKeys ->
      """Generate a new secret key seed and print the derived session keys
        |
        |Usage: jam-node [-c <CONFIG_PATH>] [--chain <CHAIN>] gen-keys [FILE_NAME]
        |
        |Arguments:
        |  [FILE_NAME]  Secret key seed file name, in <config>/<chain id>/keys unless absolute. If not specified, a
        |               file name containing the current time will be generated. An existing file is not replaced""".stripMargin,
    ListKeys ->
      """List all session keys we have the secret key for (the seeds in <config>/<chain id>/keys)
        |
        |Usage: jam-node [-c <CONFIG_PATH>] [--chain <CHAIN>] list-keys""".stripMargin,
    DumpSpec ->
      """Dump the spec of the chain given by the `--chain` option
        |
        |Usage: jam-node [--chain <CHAIN>] dump-spec <SPEC_FILE>
        |
        |Arguments:
        |  <SPEC_FILE>  Where to write the chain spec file""".stripMargin,
    Help -> Usage
  )

  private def usageError(msg: String): Nothing =
    System.err.println(s"error: $msg")
    sys.exit(2)

  private def fail(msg: String): Nothing =
    System.err.println(msg)
    sys.exit(1)
