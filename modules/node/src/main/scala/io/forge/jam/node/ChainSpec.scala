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

object Bootnode:
  /** Format: `<64-hex-ed25519>@host:port`. */
  def parse(s: String): Bootnode =
    val at = s.indexOf('@')
    require(at == 64, s"bootnode must be <ed25519-hex>@host:port, got: $s")
    val addr = s.substring(at + 1)
    val colon = addr.lastIndexOf(':')
    require(colon > 0, s"bootnode address must be host:port, got: $addr")
    Bootnode(s.substring(0, at), addr.substring(0, colon), addr.substring(colon + 1).toInt)

/** Chain specification: identifies the network (genesis) and provides the
  * genesis state.
  *
  * JSON shape:
  * {{{
  * {
  *   "id": "dev",
  *   "config": "tiny" | "full",
  *   "genesis_header": "0x…",          // optional if genesis_header_hash given
  *   "genesis_header_hash": "0x…",     // optional; derived from header if absent
  *   "genesis_state": [{"key": "0x…31-bytes…", "value": "0x…"}, …],
  *   "bootnodes": ["<ed25519-hex>@host:port", …]
  * }
  * }}}
  */
final case class ChainSpec(
    id: String,
    config: ChainConfig,
    genesisHeaderBytes: Option[Array[Byte]],
    explicitGenesisHash: Option[Hash],
    genesisState: List[KeyValue],
    bootnodes: List[Bootnode]
):
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

  given Decoder[ChainSpec] = (c: HCursor) =>
    for
      id <- c.get[String]("id")
      configName <- c.get[String]("config")
      header <- c.get[Option[String]]("genesis_header")
      headerHash <- c.get[Option[String]]("genesis_header_hash")
      state <- c.getOrElse[List[KeyValue]]("genesis_state")(Nil)
      bootnodes <- c.getOrElse[List[String]]("bootnodes")(Nil)
    yield ChainSpec(
      id = id,
      config = configName match
        case "tiny" => ChainConfig.TINY
        case "full" => ChainConfig.FULL
        case other  => throw new IllegalArgumentException(s"unknown config: $other")
      ,
      genesisHeaderBytes = header.map(parseHex),
      explicitGenesisHash = headerHash.map(h => Hash(parseHex(h))),
      genesisState = state,
      bootnodes = bootnodes.map(Bootnode.parse)
    )

  def fromJson(json: String): Either[String, ChainSpec] =
    decode[ChainSpec](json).left.map(_.getMessage)

  def load(path: Path): Either[String, ChainSpec] =
    fromJson(Files.readString(path))
