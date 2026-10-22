package io.forge.jam.node

import java.net.InetSocketAddress
import java.nio.file.{Files, Path}

import io.circe.parser.decode
import io.circe.{Decoder, HCursor}
import io.forge.jam.core.{ChainConfig, Hashing, JamBytes}
import io.forge.jam.core.primitives.Hash
import io.forge.jam.protocol.traces.KeyValue

/** A bootnode address: the peer's Ed25519 key (hex) plus host and port. */
final case class Bootnode(ed25519Hex: String, host: String, port: Int):
  def address: InetSocketAddress = new InetSocketAddress(host, port)

  def ed25519Key: Array[Byte] = JamBytes.fromHexUnsafe(ed25519Hex).toArray

object Bootnode:
  /** Format: `<64-hex-ed25519>@host:port`, or the JAMNP-S name `e<52 base-32 digits>@host:port` (JIP-4). */
  def parse(s: String): Bootnode =
    val at = s.indexOf('@')
    val keyHex =
      if at == 64 then s.substring(0, at)
      else
        io.forge.jam.network.NodeIdentity
          .fromAltName(s.substring(0, math.max(at, 0)))
          .map(JamBytes(_).toHex.stripPrefix("0x"))
          .getOrElse(throw new IllegalArgumentException(s"bootnode must be <ed25519-hex|e-name>@host:port, got: $s"))
    val addr = s.substring(at + 1)
    val colon = addr.lastIndexOf(':')
    require(colon > 0, s"bootnode address must be host:port, got: $addr")
    Bootnode(keyHex, addr.substring(0, colon), addr.substring(colon + 1).toInt)

/** Chain specification: identifies the network (genesis) and provides the
  * genesis state.
  *
  * JSON shape:
  * {{{
  * {
  *   "id": "dev",
  *   "config": "tiny" | "full" | "fuzz",   // optional with JIP-4 "protocol_parameters"
  *   "genesis_header": "0x…",          // optional if genesis_header_hash given
  *   "genesis_header_hash": "0x…",     // optional; derived from header if absent
  *   "genesis_state": [{"key": "0x…31-bytes…", "value": "0x…"}, …],  // or a JIP-4 {"key": "value"} object
  *   "bootnodes": ["<ed25519-hex>@host:port", …],
  *   "devnet_finality_depth": 5,       // optional, default 5; see field doc below
  *   "devnet_finality_quorum": 5       // optional, default 2V/3+1; see field doc below
  * }
  * }}}
  */
final case class ChainSpec(
    id: String,
    config: ChainConfig,
    genesisHeaderBytes: Option[Array[Byte]],
    explicitGenesisHash: Option[Hash],
    genesisState: List[KeyValue],
    bootnodes: List[Bootnode],
    /** Devnet stand-in for GRANDPA: finalize the block this many below the
      * best head (0 disables)...
      */
    devnetFinalityDepth: Int = 5,
    /** ...once this many nodes (this one plus peers whose announced best
      * descends from it) hold it. None means 2V/3+1 for the config's V
      * validators, the GRANDPA supermajority, so no minority partition can
      * finalize alone.
      */
    devnetFinalityQuorum: Option[Int] = None
):
  def finalityQuorum: Int =
    devnetFinalityQuorum.getOrElse(2 * config.validatorCount / 3 + 1)

  val genesisHeaderHash: Hash =
    explicitGenesisHash.orElse(genesisHeaderBytes.map(Hashing.blake2b256)).getOrElse {
      throw new IllegalArgumentException(
        "chain spec needs genesis_header or genesis_header_hash"
      )
    }

  /** First 8 hex nibbles of the genesis header hash (ALPN suffix). */
  def alpnPrefix: String = genesisHeaderHash.toHex.stripPrefix("0x").take(8)

object ChainSpec:

  private def parseHex(s: String): Array[Byte] =
    JamBytes.fromHexUnsafe(s).toArray

  /** Config of a JIP-4 spec without `config`: the one whose C and E match `protocol_parameters`, where C is E_2 at
    * byte 24 and E is E_4 at byte 30 (after B_I, B_L, B_S and D; the layout polkajam's `dump-spec` writes).
    */
  private def configFromParameters(hex: String): Either[String, ChainConfig] =
    val p = parseHex(hex)
    if p.length < 34 then Left("protocol_parameters too short")
    else
      val cores = io.forge.jam.core.LittleEndian.get(p, 24, 2).toInt
      val epoch = io.forge.jam.core.LittleEndian.get(p, 30, 4).toInt
      ChainConfig.byName.values
        .find(c => c.coresCount == cores && c.epochLength == epoch)
        .toRight(s"no known config with C=$cores, E=$epoch")

  /** `genesis_state` as a list of {key, value} or, as in JIP-4, an object of key -> value. */
  private def genesisState(c: HCursor): Decoder.Result[List[KeyValue]] =
    c.downField("genesis_state").focus match
      case Some(j) if j.isObject =>
        j.as[Map[String, String]].map(_.toList.map((k, v) => KeyValue(JamBytes(parseHex(k)), JamBytes(parseHex(v)))))
      case _ => c.getOrElse[List[KeyValue]]("genesis_state")(Nil)

  given Decoder[ChainSpec] = (c: HCursor) =>
    for
      id <- c.get[String]("id")
      configName <- c.get[Option[String]]("config")
      parameters <- c.get[Option[String]]("protocol_parameters")
      header <- c.get[Option[String]]("genesis_header")
      headerHash <- c.get[Option[String]]("genesis_header_hash")
      state <- genesisState(c)
      bootnodes <- c.getOrElse[List[String]]("bootnodes")(Nil)
      devnetFinalityDepth <- c.getOrElse[Int]("devnet_finality_depth")(5)
      devnetFinalityQuorum <- c.get[Option[Int]]("devnet_finality_quorum")
    yield ChainSpec(
      id = id,
      config = configName
        .map(ChainConfig.fromName)
        .orElse(parameters.map(configFromParameters))
        .getOrElse(Left("chain spec needs config or protocol_parameters"))
        .fold(e => throw new IllegalArgumentException(e), identity),
      genesisHeaderBytes = header.map(parseHex),
      explicitGenesisHash = headerHash.map(h => Hash(parseHex(h))),
      genesisState = state,
      bootnodes = bootnodes.map(Bootnode.parse),
      devnetFinalityDepth = devnetFinalityDepth,
      devnetFinalityQuorum = devnetFinalityQuorum
    )

  def fromJson(json: String): Either[String, ChainSpec] =
    decode[ChainSpec](json).left.map(_.getMessage)

  def load(path: Path): Either[String, ChainSpec] =
    fromJson(Files.readString(path))
