package io.forge.jam.node

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import io.circe.{Json, JsonObject}
import io.circe.parser.parse as parseJson
import io.forge.jam.core.JamBytes
import io.forge.jam.core.codec.JamCodecs

object Chains:

  val Default = "polkadot"

  val DevBasePort = 40000

  private val ToasterUrl = "https://jamtoaster.network/chainspec/"
  private val ToasterDefaultNetwork = "public"

  def resolve(chain: String, fetch: String => Either[String, String] = httpGet): Either[String, String] =
    chain match
      case "polkadot" => Right(builtIn("polkadot"))
      case "dev"      => Right(builtIn("dev"))
      case s"dev:$port" =>
        port.toIntOption.filter(p => p > 0 && p <= 65535).toRight(s"--chain dev:<BASE_PORT>: not a port: '$port'")
          .flatMap(base => rebaseDevPorts(builtIn("dev"), base))
      case "toaster"              => toaster(ToasterDefaultNetwork, fetch)
      case s"toaster:$network"    => toaster(network, fetch)
      case path =>
        val file = Paths.get(path)
        if Files.isRegularFile(file) then Right(Files.readString(file))
        else Left(s"Error loading chain spec file: $path: No such file")

  private def toaster(network: String, fetch: String => Either[String, String]): Either[String, String] =
    if network.isEmpty || !network.matches("[A-Za-z0-9_.-]+") then Left(s"--chain toaster:<NETWORK>: not a network name: '$network'")
    else
      val url = s"$ToasterUrl$network.json"
      fetch(url).left.map(e => s"Error fetching toaster chain spec from $url: $e")

  private def builtIn(name: String): String =
    val in = getClass.getResourceAsStream(s"/chains/$name.json")
    if in == null then throw new IllegalStateException(s"built-in chain spec $name is missing from the jar")
    try new String(in.readAllBytes(), StandardCharsets.UTF_8)
    finally in.close()

  def rebaseDevPorts(specJson: String, base: Int): Either[String, String] =
    for
      json <- parseJson(specJson).left.map(_.getMessage)
      spec <- json.asObject.toRight("chain spec is not a JSON object")
      state <- spec("genesis_state").flatMap(_.asObject).toRight("chain spec has no genesis_state object")
    yield
      val rebased = state.toMap.map { (key, value) =>
        key -> value.asString.filter(_ => ValidatorLists.contains(chapter(key))).fold(value)(hex =>
          Json.fromString(withPorts(JamBytes.fromHexUnsafe(hex).toArray, base).map(JamBytes(_).toHex.stripPrefix("0x")).getOrElse(hex))
        )
      }
      Json.fromJsonObject(spec.add("genesis_state", Json.fromJsonObject(JsonObject.fromIterable(sortedByKey(rebased))))).spaces2

  private val ValidatorLists = Set(4, 7, 8, 9)
  private val RecordBytes = 336
  private val MetadataOffset = 208
  private val Loopback = Array.fill[Byte](10)(0) ++ Array[Byte](-1, -1, 127, 0, 0, 1)

  private def chapter(keyHex: String): Int =
    val key = JamBytes.fromHexUnsafe(keyHex).toArray
    if key.nonEmpty && key.drop(1).forall(_ == 0) then key(0) & 0xff else -1

  private def withPorts(value: Array[Byte], base: Int): Option[Array[Byte]] =
    val (count, used) =
      try JamCodecs.decodeCompactInteger(value, 0)
      catch case _: Exception => (-1L, 0)
    if count < 0 || value.length < used + count * RecordBytes then None
    else
      val out = value.clone()
      for i <- 0 until count.toInt do
        val at = used + i * RecordBytes + MetadataOffset
        val port = (out(at + 16) & 0xff) | ((out(at + 17) & 0xff) << 8)
        if java.util.Arrays.equals(out, at, at + 16, Loopback, 0, 16) && port >= DevBasePort && port < DevBasePort + 1023 then
          val moved = base + port - DevBasePort
          out(at + 16) = moved.toByte
          out(at + 17) = (moved >> 8).toByte
      Some(out)

  private def sortedByKey(entries: Map[String, Json]): List[(String, Json)] = entries.toList.sortBy(_._1)

  def write(specJson: String, file: Path): Either[String, Unit] =
    parseJson(specJson).left.map(_.getMessage).map { json =>
      Option(file.toAbsolutePath.getParent).foreach(Files.createDirectories(_))
      Files.writeString(file, json.spaces2 + "\n"): Unit
    }

  private def httpGet(url: String): Either[String, String] =
    try
      val client = java.net.http.HttpClient
        .newBuilder()
        .followRedirects(java.net.http.HttpClient.Redirect.NORMAL)
        .connectTimeout(java.time.Duration.ofSeconds(20))
        .build()
      val request = java.net.http.HttpRequest.newBuilder(java.net.URI.create(url)).timeout(java.time.Duration.ofSeconds(60)).build()
      val response = client.send(request, java.net.http.HttpResponse.BodyHandlers.ofString())
      if response.statusCode() == 200 then Right(response.body()) else Left(s"HTTP ${response.statusCode()}")
    catch case e: Exception => Left(e.toString)
