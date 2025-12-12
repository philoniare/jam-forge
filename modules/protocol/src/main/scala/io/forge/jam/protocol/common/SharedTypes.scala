package io.forge.jam.protocol.common

import io.forge.jam.core.JamBytes
import io.forge.jam.core.codec.{JamEncoder, JamDecoder}
import io.forge.jam.core.primitives.Hash
import io.circe.Decoder
import JsonHelpers.parseHash

/**
 * Shared types used across multiple STFs.
 */
object SharedTypes:

  /**
   * A reported work package with its hash and exports root.
   * Fixed size: 64 bytes (32-byte hash + 32-byte exports root)
   *
   * Used by: History, Report STFs
   */
  final case class ReportedWorkPackage(
    hash: Hash,
    exportsRoot: Hash
  )

  object ReportedWorkPackage:
    val Size: Int = Hash.Size * 2 // 64 bytes

    given JamEncoder[ReportedWorkPackage] with
      def encode(a: ReportedWorkPackage): JamBytes =
        JamBytes(a.hash.bytes ++ a.exportsRoot.bytes)

    given JamDecoder[ReportedWorkPackage] with
      def decode(bytes: JamBytes, offset: Int): (ReportedWorkPackage, Int) =
        val arr = bytes.toArray
        val hash = Hash(arr.slice(offset, offset + Hash.Size))
        val exportsRoot = Hash(arr.slice(offset + Hash.Size, offset + Size))
        (ReportedWorkPackage(hash, exportsRoot), Size)

    given Decoder[ReportedWorkPackage] =
      Decoder.instance { cursor =>
        for
          hashHex <- cursor.get[String]("hash")
          exportsRootHex <- cursor.get[String]("exports_root")
          hash <- parseHash(hashHex)
          exportsRoot <- parseHash(exportsRootHex)
        yield ReportedWorkPackage(hash, exportsRoot)
      }
