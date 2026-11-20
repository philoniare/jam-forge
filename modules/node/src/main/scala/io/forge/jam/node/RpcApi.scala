package io.forge.jam.node

import java.util.Base64
import java.util.concurrent.{ConcurrentHashMap, ThreadLocalRandom}

import scala.jdk.CollectionConverters.*

import io.circe.Json
import io.circe.parser.parse
import io.forge.jam.core.{ChainConfig, Hashing, JamBytes, LittleEndian}
import io.forge.jam.core.codec.JamCodecs.encode
import io.forge.jam.core.primitives.Hash
import io.forge.jam.core.types.workpackage.WorkReport
import io.forge.jam.core.trie.{MissingTrieNodeException, StateTrie}
import io.forge.jam.protocol.pvmhost.ConstantsBlob
import io.forge.jam.protocol.state.{StateKey, StateKeys}

final case class RpcError(code: Int, message: String, data: Json = Json.Null)

object RpcError:
  val UnsafeMethodCode: Int = -32601

  val LimitExceededCode: Int = -32005

  def invalidParams(why: String): RpcError = RpcError(-32602, "Invalid params", Json.fromString(why))
  val methodNotFound: RpcError = RpcError(-32601, "Method not found")
  val invalidRequest: RpcError = RpcError(-32600, "Invalid request")
  val parseError: RpcError = RpcError(-32700, "Parse error")
  def other(why: String): RpcError = RpcError(0, why)
  def unsafeRefused(method: String): RpcError =
    RpcError(
      UnsafeMethodCode,
      s"Method not allowed: $method is unsafe and this RPC serves safe methods only; " +
        "start the node with --rpc-methods unsafe to serve it",
      Json.fromString(s"$method is unsafe and this RPC serves safe methods only (--rpc-methods)")
    )
  val limitExceeded: RpcError =
    RpcError(LimitExceededCode, "Limit exceeded", Json.fromString("too many requests from this address; retry later"))
  def blockUnavailable(h: Hash): RpcError =
    RpcError(1, s"The block with header hash ${h.toHex} is not available", RpcApi.blob(h.bytes.toArray))
  def reportUnavailable(h: Hash): RpcError =
    RpcError(2, s"The work-report with hash ${h.toHex} is not available", RpcApi.blob(h.bytes.toArray))
  val segmentUnavailable: RpcError = RpcError(3, "A segment could not be recovered")

final class RpcSubscription(
    val id: Long,
    val method: String,
    val finalized: Boolean,
    val current: ChainHead => (Json, Json),
    val key: String,
    val everyRefresh: Boolean = false
):
  @volatile var sent: Json = null

  @volatile var readAt: Hash = null

final class RpcSession(val send: Json => Unit, val unsafeAllowed: Boolean = true):
  val subscriptions = new ConcurrentHashMap[java.lang.Long, RpcSubscription]()

final class RpcApi(
    chain: ChainManager,
    genesisHash: Hash,
    peerCount: () => Int,
    syncing: () => Boolean,
    submitPreimage: (Long, Array[Byte]) => Either[String, Hash],
    submitWorkPackage: (Int, Array[Byte], Seq[Array[Byte]]) => Either[String, Unit] = (_, _, _) =>
      Left("this node takes no work-packages"),
    submitBundle: (Int, Array[Byte]) => Either[String, Unit] = (_, _) => Left("this node takes no work-packages"),
    knownReport: Hash => Option[WorkReport] = _ => None,
    fetchSegments: (Hash, Seq[Int]) => Option[IndexedSeq[Array[Byte]]] = (_, _) => None,
    slow: java.util.concurrent.Executor = (r: Runnable) => r.run()
):
  import RpcApi.*

  private val config: ChainConfig = chain.config

  private[node] val computations = new java.util.concurrent.atomic.AtomicLong()

  def handle(text: String, session: RpcSession): Option[Json] =
    val answer = new java.util.concurrent.CompletableFuture[Option[Json]]()
    var answered = false
    handle(text, session, j => { answered = true; answer.complete(Some(j)): Unit }, () => if !answered then answer.complete(None): Unit)
    answer.get(60, java.util.concurrent.TimeUnit.SECONDS)

  def handle(text: String, session: RpcSession, respond: Json => Unit, done: () => Unit = () => ()): Unit =
    def finish(response: Option[Json]): Unit =
      response.foreach(respond)
      done()
    parse(text) match
      case Left(_) => finish(Some(failure(Json.Null, RpcError.parseError)))
      case Right(json) =>
        json.asObject match
          case None => finish(Some(failure(Json.Null, RpcError.invalidRequest)))
          case Some(request) =>
            val id = request("id")
            val parsed = for
              method <- request("method").flatMap(_.asString).toRight(RpcError.invalidRequest)
              _ <- Either.cond(session.unsafeAllowed || !Unsafe.contains(method), (), RpcError.unsafeRefused(method))
              params <- request("params") match
                case None                 => Right(Vector.empty)
                case Some(p) if p.isNull  => Right(Vector.empty)
                case Some(p) if p.isArray => Right(p.asArray.get)
                case Some(_)              => Left(RpcError.invalidParams("parameters are positional: an Array"))
            yield (method, params)
            def run(): Unit =
              val result = parsed.flatMap { (method, params) =>
                try call(method, new Params(params), session)
                catch case e: Exception => Left(RpcError.other(String.valueOf(e.getMessage)))
              }
              finish(id.map(i => result.fold(failure(i, _), success(i, _))))
            if parsed.exists((method, _) => Slow.contains(method)) then
              try slow.execute(() => run())
              catch
                case _: java.util.concurrent.RejectedExecutionException =>
                  finish(id.map(failure(_, RpcError.other("the node is shutting down"))))
            else run()

  private def call(method: String, p: Params, session: RpcSession): Either[RpcError, Json] =
    method match
      case "parameters"     => Right(parameters)
      case "bestBlock" =>
        val head = chain.best
        Right(descriptor(head.hash, head.slot))
      case "finalizedBlock" =>
        val head = chain.finalized
        Right(descriptor(head.hash, head.slot))
      case "parent" =>
        for
          h <- p.hash(0)
          _ <- slotOf(h)
          parent <- chain.parentHash(h).toRight(RpcError.blockUnavailable(h))
          slot <- slotOf(parent)
        yield descriptor(parent, slot)
      case "stateRoot" =>
        for h <- p.hash(0); root <- chain.postStateRoot(h).toRight(RpcError.blockUnavailable(h))
        yield blob(root.bytes.toArray)
      case "beefyRoot" =>
        for
          h <- p.hash(0)
          view <- chain.stateViewAt(h).toRight(RpcError.blockUnavailable(h))
          root <- whole(h)(view.beta.history.lastOption.map(b => blob(b.beefyRoot.bytes.toArray)).getOrElse(Json.Null))
        yield root
      case "statistics" => for h <- p.hash(0); v <- atState(h)(statistics) yield v
      case "stateValue" => for h <- p.hash(0); k <- p.stateKey(1); v <- atState(h)(raw(_, k)) yield v
      case "serviceData" => for h <- p.hash(0); id <- p.service(1); v <- atState(h)(serviceData(_, id)) yield v
      case "serviceValue" =>
        for h <- p.hash(0); id <- p.service(1); k <- p.blob(2); v <- atState(h)(serviceValue(_, id, k)) yield v
      case "servicePreimage" =>
        for h <- p.hash(0); id <- p.service(1); x <- p.hash(2); v <- atState(h)(servicePreimage(_, id, x)) yield v
      case "servicePreimageLen" =>
        for
          h <- p.hash(0); id <- p.service(1); x <- p.hash(2)
          v <- atState(h)(preimageBytes(_, id, x).map(b => Json.fromInt(b.length)).getOrElse(Json.Null))
        yield v
      case "serviceRequest" =>
        for h <- p.hash(0); id <- p.service(1); x <- p.hash(2); len <- p.number(3); v <- atState(h)(serviceRequest(_, id, x, len))
        yield v
      case "listServices" => for h <- p.hash(0); v <- atState(h)(listServices) yield v
      case "syncState" =>
        Right(Json.obj("num_peers" -> Json.fromInt(peerCount()), "status" -> syncStatus))
      case "submitPreimage" =>
        for
          requester <- p.service(0)
          preimage <- p.blob(1)
          _ <- submitPreimage(requester, preimage).left.map(RpcError.other)
        yield Json.Null

      case "submitWorkPackage" =>
        for
          core <- p.number(0)
          workPackage <- p.blob(1)
          extrinsics <- p.blobs(2)
          _ <- submitWorkPackage(core.toInt, workPackage, extrinsics).left.map(RpcError.other)
        yield Json.Null
      case "submitWorkPackageBundle" =>
        for
          core <- p.number(0)
          bundle <- p.blob(1)
          _ <- submitBundle(core.toInt, bundle).left.map(RpcError.other)
        yield Json.Null
      case "workPackageStatus" =>
        for h <- p.hash(0); wp <- p.hash(1); anchor <- p.hash(2); status <- workPackageStatus(h, wp, anchor)
        yield status
      case "workReport" =>
        for h <- p.hash(0); report <- workReport(h).toRight(RpcError.reportUnavailable(h))
        yield blob(report.encode.toArray)
      case "fetchSegments" =>
        for root <- p.hash(0); indices <- p.indices(1); found <- segments(Some(root), indices) yield found
      case "fetchWorkPackageSegments" =>
        for wp <- p.hash(0); indices <- p.indices(1); found <- segments(segmentsRootOf(wp), indices) yield found

      case "subscribeBestBlock"      => subscribe(session, method, finalized = false, "", None)
      case "subscribeFinalizedBlock" => subscribe(session, method, finalized = true, "", None)
      case "subscribeStatistics" =>
        for f <- p.flag(0); id <- subscribe(session, method, f, "", inState(statistics)) yield id
      case "subscribeStateValue" =>
        for k <- p.stateKey(0); f <- p.flag(1); id <- subscribe(session, method, f, k.toHex, inState(raw(_, k))) yield id
      case "subscribeServiceData" =>
        for s <- p.service(0); f <- p.flag(1); id <- subscribe(session, method, f, s.toString, inState(serviceData(_, s)))
        yield id
      case "subscribeServiceValue" =>
        for
          s <- p.service(0); k <- p.blob(1); f <- p.flag(2)
          id <- subscribe(session, method, f, s"$s ${JamBytes.toHex(k)}", inState(serviceValue(_, s, k)))
        yield id
      case "subscribeServicePreimage" =>
        for
          s <- p.service(0); x <- p.hash(1); f <- p.flag(2)
          id <- subscribe(session, method, f, s"$s ${x.toHex}", inState(servicePreimage(_, s, x)))
        yield id
      case "subscribeServiceRequest" =>
        for
          s <- p.service(0); x <- p.hash(1); len <- p.number(2); f <- p.flag(3)
          id <- subscribe(session, method, f, s"$s ${x.toHex} $len", inState(serviceRequest(_, s, x, len)))
        yield id
      case "subscribeWorkPackageStatus" =>
        for
          wp <- p.hash(0); anchor <- p.hash(1); f <- p.flag(2)
          _ <- slotOf(anchor)
          id <- subscribe(
            session,
            method,
            f,
            s"${wp.toHex} ${anchor.toHex}",
            Some(head => workPackageStatus(head.hash, wp, anchor).fold(e => failed(e.message), identity))
          )
        yield id
      case "subscribeSyncStatus" =>
        register(session, new RpcSubscription(newId(), method, false, _ => (syncStatus, syncStatus), method, everyRefresh = true))

      case other if other.startsWith("unsubscribe") && Subscribable.contains("subscribe" + other.drop(11)) =>
        for id <- p.number(0)
        yield
          val key = java.lang.Long.valueOf(id)
          val mine = Option(session.subscriptions.get(key)).exists(_.method == "subscribe" + other.drop(11))
          Json.fromBoolean(mine && session.subscriptions.remove(key) != null)

      case _ => Left(RpcError.methodNotFound)


  private def slotOf(h: Hash): Either[RpcError, Long] =
    chain.slotOf(h).orElse(Option.when(h == genesisHash)(0L)).toRight(RpcError.blockUnavailable(h))

  private def state(h: Hash): Either[RpcError, StateTrie] = chain.stateAt(h).toRight(RpcError.blockUnavailable(h))

  private def atState(h: Hash)(read: StateTrie => Json): Either[RpcError, Json] = state(h).flatMap(t => whole(h)(read(t)))

  private def whole[A](h: Hash)(read: => A): Either[RpcError, A] =
    try Right(read)
    catch case _: MissingTrieNodeException => Left(RpcError.blockUnavailable(h))

  private def raw(t: StateTrie, key: JamBytes): Json = t.read(key).map(v => blob(v.toArray)).getOrElse(Json.Null)

  private def statistics(t: StateTrie): Json = raw(t, StateKeys.simpleKey(StateKeys.ACTIVITY_STATISTICS))

  private def serviceData(t: StateTrie, id: Long): Json = raw(t, StateKey.computeServiceAccountKey(id))

  private def serviceValue(t: StateTrie, id: Long, key: Array[Byte]): Json =
    raw(t, StateKey.computeStorageStateKey(id, JamBytes(key)))

  private def preimageBytes(t: StateTrie, id: Long, hash: Hash): Option[Array[Byte]] =
    t.read(StateKey.computeServiceDataStateKey(id, 0xfffffffeL, JamBytes(hash.bytes.toArray))).map(_.toArray)

  private def servicePreimage(t: StateTrie, id: Long, hash: Hash): Json =
    preimageBytes(t, id, hash).map(blob).getOrElse(Json.Null)

  private def serviceRequest(t: StateTrie, id: Long, hash: Hash, length: Long): Json =
    t.read(StateKey.computePreimageInfoStateKey(id, length, JamBytes(hash.bytes.toArray))) match
      case None => Json.Null
      case Some(value) =>
        val bytes = value.toArray
        val count = if bytes.isEmpty then 0 else bytes(0) & 0xff
        Json.arr((0 until count).map(i => Json.fromLong(LittleEndian.get(bytes, 1 + 4 * i, 4)))*)

  private def listServices(t: StateTrie): Json =
    val ids = t.getKeyValues(JamBytes(Array(0xff.toByte)), 8).iterator.map(_._1.toArray).collect {
      case k if k.length == 31 && k(2) == 0 && k(4) == 0 && k(6) == 0 && (8 until 31).forall(k(_) == 0) =>
        (k(1) & 0xffL) | ((k(3) & 0xffL) << 8) | ((k(5) & 0xffL) << 16) | ((k(7) & 0xffL) << 24)
    }.toList.sorted
    Json.arr(ids.map(Json.fromLong)*)

  private def syncStatus: Json = Json.fromString(if syncing() then "InProgress" else "Completed")


  private def failed(why: String): Json = Json.obj("Failed" -> Json.fromString(why))

  private def workPackageStatus(header: Hash, wp: Hash, anchor: Hash): Either[RpcError, Json] =
    for _ <- slotOf(header); _ <- slotOf(anchor); status <- statusOnChain(header, wp, anchor) yield status

  private def cannotTell(why: String): RpcError = RpcError.other(s"the work-package's status cannot be told: $why")

  private def chainBetween(tip: Hash, from: Long, to: Long): Option[List[Hash]] =
    if to < from then Some(Nil)
    else
      chain.ancestorAtHeight(tip, to).flatMap { top =>
        var out = List(top)
        var cursor: Option[Hash] = Some(top)
        var n = to
        while n > from && cursor.isDefined do
          cursor = cursor.flatMap(chain.parentHash)
          cursor.foreach(h => out = h :: out)
          n -= 1
        Option.when(cursor.isDefined)(out)
      }

  private def statusOnChain(header: Hash, wp: Hash, anchor: Hash): Either[RpcError, Json] =
    (chain.heightOf(header), chain.heightOf(anchor)) match
      case (_, None)   => Left(RpcError.blockUnavailable(anchor))
      case (None, _)   => Left(RpcError.blockUnavailable(header))
      case (Some(headerHeight), Some(anchorHeight)) if anchorHeight > headerHeight =>
        Right(failed("Anchor is on a different fork"))
      case (Some(headerHeight), Some(anchorHeight)) =>
        chain.ancestorAtHeight(header, anchorHeight) match
          case None => Left(cannotTell(s"the chain of block ${header.toHex} is not held back to the anchor"))
          case Some(other) if other != anchor => Right(failed("Anchor is on a different fork"))
          case Some(_) =>
            val window = math.min(headerHeight, anchorHeight + config.maxBlockHistory)
            chainBetween(header, anchorHeight + 1, window)
              .toRight(cannotTell(s"the chain of block ${header.toHex} is not held back to the anchor"))
              .flatMap(findReport(_, wp))
              .flatMap {
                case Some((block, report)) => reportedStatus(header, headerHeight, block, report, wp)
                case None =>
                  val remaining = config.maxBlockHistory - (headerHeight - anchorHeight)
                  Right(
                    if remaining > 0 then Json.obj("Reportable" -> Json.obj("remaining_blocks" -> Json.fromLong(remaining)))
                    else failed("Anchor is too old")
                  )
              }

  private def findReport(blocks: List[Hash], wp: Hash): Either[RpcError, Option[(Hash, WorkReport)]] =
    blocks.iterator
      .map(h => chain.block(h).toRight(RpcError.blockUnavailable(h)).map(b => h -> b))
      .map(_.map((h, b) => b.extrinsic.guarantees.map(_.report).find(_.packageSpec.hash == wp).map(h -> _)))
      .collectFirst { case Left(e) => Left(e); case Right(Some(found)) => Right(Some(found)) }
      .getOrElse(Right(None))

  private def reportedStatus(
      header: Hash,
      headerHeight: Long,
      reportedIn: Hash,
      report: WorkReport,
      wp: Hash
  ): Either[RpcError, Json] =
    val core = report.coreIndex.toInt
    val fields = List(
      "reported_in" -> describe(reportedIn),
      "core" -> Json.fromInt(core),
      "report_hash" -> blob(Hashing.blake2b256(report.encode.toArray).bytes.toArray)
    )
    val reportedHeight = chain.heightOf(reportedIn).getOrElse(headerHeight)
    val reported = Json.obj("Reported" -> Json.obj(fields*))
    chainBetween(header, reportedHeight + 1, math.min(headerHeight, reportedHeight + MaxStatusWalk))
      .toRight(cannotTell(s"the chain of block ${header.toHex} is not held back to its report"))
      .flatMap { later =>
        val outcomes = later.iterator.map(h => h -> availability(h, core, wp))
        outcomes
          .collectFirst {
            case (h, Availability.MadeAvailable) =>
              Right(Json.obj("Ready" -> Json.obj((fields :+ ("ready_in" -> describe(h)))*)))
            case (_, Availability.NotPending) => Right(reported)
            case (h, Availability.Unknown) =>
              Left(cannotTell(s"the state before block ${h.toHex} is no longer held, so whether it made the report available is not known"))
          }
          .getOrElse(Right(reported))
      }

  private def describe(h: Hash): Json = descriptor(h, chain.slotOf(h).getOrElse(0L))

  private enum Availability:
    case MadeAvailable, StillPending, NotPending, Unknown

  private def availability(h: Hash, core: Int, wp: Hash): Availability =
    chain.block(h).flatMap(block => chain.stateViewAt(block.header.parent).map(block -> _)) match
      case None => Availability.Unknown
      case Some((block, prior)) =>
        try
          if !prior.cores.reports.lift(core).flatten.exists(_.report.packageSpec.hash == wp) then Availability.NotPending
          else if io.forge.jam.protocol.assurance.AssuranceTransition
              .assuredCores(block.extrinsic.assurances, config.coresCount, prior.validators.current.size)
              .contains(core)
          then Availability.MadeAvailable
          else Availability.StillPending
        catch case _: MissingTrieNodeException => Availability.Unknown

  private def workReport(hash: Hash): Option[WorkReport] =
    knownReport(hash).orElse {
      Iterator
        .iterate(Option(chain.best.hash))(_.flatMap(chain.parentHash))
        .takeWhile(_.isDefined)
        .take(MaxStatusWalk)
        .flatten
        .flatMap(h => chain.block(h).toList.flatMap(_.extrinsic.guarantees.map(_.report)))
        .find(r => Hashing.blake2b256(r.encode.toArray) == hash)
    }

  private def segmentsRootOf(wp: Hash): Option[Hash] =
    chain.stateView().beta.history.flatMap(_.reported).find(_.hash == wp).map(_.exportsRoot)

  private def segments(root: Option[Hash], indices: Seq[Int]): Either[RpcError, Json] =
    root
      .flatMap(fetchSegments(_, indices))
      .filter(_.size == indices.size)
      .map(found => Json.arr(found.map(blob)*))
      .toRight(RpcError.segmentUnavailable)

  private lazy val parameters: Json =
    val bytes = ConstantsBlob.build(config)
    require(bytes.length == ParameterLayout.map(_._2).sum, "the constants blob does not match JIP-2's parameters")
    var offset = 0
    val fields = ParameterLayout.map { (name, width) =>
      val value = LittleEndian.get(bytes, offset, width)
      offset += width
      name -> Json.fromLong(value)
    }
    Json.obj("V1" -> Json.obj(fields*))


  private def newId(): Long = ThreadLocalRandom.current().nextLong(1L, 1L << 53)

  private def register(session: RpcSession, s: RpcSubscription): Either[RpcError, Json] =
    if session.subscriptions.size >= MaxSubscriptions then
      Left(RpcError.other(s"at most $MaxSubscriptions subscriptions on a connection"))
    else
      session.subscriptions.put(java.lang.Long.valueOf(s.id), s)
      Right(Json.fromLong(s.id))

  private def inState(read: StateTrie => Json): Option[ChainHead => Json] =
    Some(head => chain.stateAtRoot(head.stateRoot).map(read).getOrElse(Json.Null))

  private def subscribe(
      session: RpcSession,
      method: String,
      finalized: Boolean,
      parameters: String,
      value: Option[ChainHead => Json]
  ): Either[RpcError, Json] =
    val current: ChainHead => (Json, Json) = value match
      case None =>
        head =>
          val d = descriptor(head.hash, head.slot)
          (d, d)
      case Some(read) =>
        head =>
          val v = read(head)
          (descriptor(head.hash, head.slot).deepMerge(Json.obj("value" -> v)), v)
    val key = s"$method $parameters ${if finalized then "finalized" else "best"}"
    register(session, new RpcSubscription(newId(), method, finalized, current, key))

  def refresh(sessions: Iterable[RpcSession]): Unit =
    val (best, finalized) = chain.heads
    val answers = scala.collection.mutable.HashMap.empty[(String, Hash), Option[(Json, Json)]]
    for session <- sessions; s <- session.subscriptions.values.asScala do
      val head = if s.finalized then finalized else best
      if s.everyRefresh || s.readAt != head.hash then
        val answer = answers.getOrElseUpdate(
          (s.key, head.hash),
          try
            computations.incrementAndGet()
            Some(s.current(head))
          catch case _: Exception => None // a state pruned between the head read and this one: the next refresh sends
        )
        answer.foreach { (result, watched) =>
          s.readAt = head.hash
          if watched != s.sent then
            s.sent = watched
            try
              session.send(
                Json.obj(
                  "jsonrpc" -> Json.fromString("2.0"),
                  "method" -> Json.fromString(s.method),
                  "params" -> Json.obj("subscription" -> Json.fromLong(s.id), "result" -> result)
                )
              )
            catch case _: Exception => ()
        }

object RpcApi:

  val MaxSubscriptions: Int = 64

  private val Subscribable = Set(
    "subscribeBestBlock",
    "subscribeFinalizedBlock",
    "subscribeStatistics",
    "subscribeStateValue",
    "subscribeServiceData",
    "subscribeServiceValue",
    "subscribeServicePreimage",
    "subscribeServiceRequest",
    "subscribeWorkPackageStatus",
    "subscribeSyncStatus"
  )

  private val Slow = Set("submitWorkPackage", "submitWorkPackageBundle", "fetchSegments", "fetchWorkPackageSegments")

  val Unsafe: Set[String] = Slow + "submitPreimage"

  def refusal(text: String, error: RpcError): Option[Json] =
    parse(text).toOption.flatMap(_.asObject) match
      case Some(request) => request("id").map(failure(_, error))
      case None          => Some(failure(Json.Null, error))

  val MaxStatusWalk: Int = 64

  val MaxSegmentsPerRequest: Int = 3072

  private val ParameterLayout: List[(String, Int)] = List(
    "deposit_per_item" -> 8,
    "deposit_per_byte" -> 8,
    "deposit_per_account" -> 8,
    "core_count" -> 2,
    "min_turnaround_period" -> 4,
    "epoch_period" -> 4,
    "max_accumulate_gas" -> 8,
    "max_is_authorized_gas" -> 8,
    "max_refine_gas" -> 8,
    "block_gas_limit" -> 8,
    "recent_block_count" -> 2,
    "max_work_items" -> 2,
    "max_dependencies" -> 2,
    "max_tickets_per_block" -> 2,
    "max_lookup_anchor_age" -> 4,
    "auth_window" -> 2,
    "slot_period_sec" -> 2,
    "auth_queue_len" -> 2,
    "rotation_period" -> 2,
    "max_extrinsics" -> 2,
    "availability_timeout" -> 2,
    "max_authorizer_code_size" -> 4,
    "max_input" -> 4,
    "max_service_code_size" -> 4,
    "max_imports" -> 4,
    "max_report_elective_data" -> 4,
    "transfer_memo_size" -> 4,
    "max_exports" -> 4,
    "epoch_tail_start" -> 4
  )

  def blob(bytes: Array[Byte]): Json = Json.fromString(Base64.getEncoder.encodeToString(bytes))

  def descriptor(hash: Hash, slot: Long): Json =
    Json.obj("header_hash" -> blob(hash.bytes.toArray), "slot" -> Json.fromLong(slot))

  private def success(id: Json, result: Json): Json =
    Json.obj("jsonrpc" -> Json.fromString("2.0"), "id" -> id, "result" -> result)

  private def failure(id: Json, e: RpcError): Json =
    val error = Json.obj("code" -> Json.fromInt(e.code), "message" -> Json.fromString(e.message))
    Json.obj(
      "jsonrpc" -> Json.fromString("2.0"),
      "id" -> id,
      "error" -> (if e.data.isNull then error else error.deepMerge(Json.obj("data" -> e.data)))
    )

  final class Params(values: Vector[Json]):
    private def at(i: Int): Either[RpcError, Json] =
      values.lift(i).toRight(RpcError.invalidParams(s"parameter ${i + 1} is missing"))

    def blob(i: Int): Either[RpcError, Array[Byte]] =
      at(i).flatMap(_.asString.toRight(RpcError.invalidParams(s"parameter ${i + 1} is not a Base64 string"))).flatMap { s =>
        try if s.length % 4 == 0 then Right(Base64.getDecoder.decode(s)) else throw new IllegalArgumentException
        catch case _: IllegalArgumentException => Left(RpcError.invalidParams(s"parameter ${i + 1} is not padded Base64"))
      }

    private def sized(i: Int, length: Int, what: String): Either[RpcError, Array[Byte]] =
      blob(i).filterOrElse(_.length == length, RpcError.invalidParams(s"parameter ${i + 1} is not a $what of $length bytes"))

    def hash(i: Int): Either[RpcError, Hash] = sized(i, 32, "hash").map(Hash(_))
    def stateKey(i: Int): Either[RpcError, JamBytes] = sized(i, 31, "state key").map(JamBytes(_))

    def number(i: Int): Either[RpcError, Long] =
      at(i).flatMap(_.asNumber.flatMap(_.toLong).filter(_ >= 0).toRight(RpcError.invalidParams(s"parameter ${i + 1} is not a number")))

    def blobs(i: Int): Either[RpcError, Seq[Array[Byte]]] =
      at(i).flatMap(_.asArray.toRight(RpcError.invalidParams(s"parameter ${i + 1} is not an Array"))).flatMap { items =>
        val inner = new Params(items)
        items.indices.foldLeft[Either[RpcError, Vector[Array[Byte]]]](Right(Vector.empty)) { (acc, j) =>
          acc.flatMap(done => inner.blob(j).map(done :+ _).left.map(_ => RpcError.invalidParams(s"parameter ${i + 1} holds something that is not padded Base64")))
        }
      }

    def indices(i: Int): Either[RpcError, Seq[Int]] =
      at(i).flatMap(_.asArray.toRight(RpcError.invalidParams(s"parameter ${i + 1} is not an Array"))).flatMap { items =>
        val numbers = items.flatMap(_.asNumber.flatMap(_.toInt).filter(n => n >= 0 && n < 65536))
        if numbers.size != items.size || items.size > RpcApi.MaxSegmentsPerRequest then
          Left(RpcError.invalidParams(s"parameter ${i + 1} is not a list of segment indices"))
        else Right(numbers)
      }

    def service(i: Int): Either[RpcError, Long] =
      number(i).filterOrElse(_ <= 0xffffffffL, RpcError.invalidParams(s"parameter ${i + 1} is not a service id"))

    def flag(i: Int): Either[RpcError, Boolean] =
      values.lift(i) match
        case None => Right(false)
        case Some(j) => j.asBoolean.toRight(RpcError.invalidParams(s"parameter ${i + 1} is not a boolean"))
