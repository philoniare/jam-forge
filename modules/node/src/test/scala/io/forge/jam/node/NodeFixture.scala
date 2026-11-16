package io.forge.jam.node

import java.nio.file.{Files, Path, Paths}

import io.circe.Decoder
import io.circe.parser.decode
import io.forge.jam.protocol.traces.Genesis

/** What node specs share: the dev genesis of the test vectors and temporary data directories. */
trait NodeFixture:

  protected val baseDir: Path = sys.props.get("jam.base.dir").map(Paths.get(_)).getOrElse(Paths.get("."))
  protected val genesisPath: Path = baseDir.resolve("jamtestvectors/traces/fuzzy/genesis.json")

  /** The dev genesis, when the test vectors are checked out. */
  protected def loadGenesis(): Option[Genesis] =
    if !Files.exists(genesisPath) then None
    else
      given Decoder[Genesis] = Genesis.decoder
      decode[Genesis](Files.readString(genesisPath)).toOption

  protected def tempDir(prefix: String): Path = Files.createTempDirectory(prefix)

  /** Delete `dir` and everything in it. */
  protected def cleanup(dir: Path): Unit =
    if Files.exists(dir) then
      Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(f => Files.deleteIfExists(f))
