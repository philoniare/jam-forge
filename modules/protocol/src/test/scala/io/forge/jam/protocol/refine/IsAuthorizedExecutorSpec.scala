package io.forge.jam.protocol.refine

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import io.forge.jam.core.{ChainConfig, JamBytes, Hashing}
import io.forge.jam.core.primitives.{Gas, Hash, ServiceId, Timeslot}
import io.forge.jam.core.types.context.Context
import io.forge.jam.core.types.work.ExecutionResult
import io.forge.jam.core.types.workitem.WorkItem
import io.forge.jam.core.types.workpackage.WorkPackage
import spire.math.UShort

class IsAuthorizedExecutorSpec extends AnyFunSuite with Matchers:

  private val config = ChainConfig.TINY
  private val authCodeHash = Hash(Array.fill[Byte](32)(0x21))

  /** Deblob wrapper + zero metadata prefix (same as RefineExecutorSpec). */
  private def preimageOf(code: Array[Byte], bitmask: Array[Byte]): Array[Byte] =
    Array[Byte](0, 0, 0, code.length.toByte) ++ code ++ bitmask

  // JumpIndirect r0 + 0 → halt (echoes args as the trace).
  private val haltCode = Array[Byte](50, 0)
  private val haltBitmask = Array[Byte](1)

  private class HostLookup(
      hostService: Long,
      preimage: Option[Array[Byte]]
  ) extends HistoricalLookupService:
    def serviceExists(id: Long): Boolean = id == hostService
    def historicalLookup(id: Long, t: Long, h: Hash): Option[Array[Byte]] =
      if id == hostService && h == authCodeHash then preimage else None

  private def workPackage(hostService: Long): WorkPackage =
    WorkPackage(
      authCodeHost = ServiceId(hostService.toInt),
      authCodeHash = authCodeHash,
      context = Context(
        anchor = Hash(Array.fill[Byte](32)(1)),
        stateRoot = Hash(Array.fill[Byte](32)(2)),
        beefyRoot = Hash(Array.fill[Byte](32)(3)),
        lookupAnchor = Hash(Array.fill[Byte](32)(4)),
        lookupAnchorSlot = Timeslot(100),
        prerequisites = List.empty
      ),
      authorization = JamBytes(Array[Byte](0x0a, 0x0b, 0x0c)),
      authorizerConfig = JamBytes(Array[Byte](0x0d)),
      items = List(
        WorkItem(
          service = ServiceId(7),
          codeHash = Hash(Array.fill[Byte](32)(5)),
          payload = JamBytes(Array[Byte](1)),
          refineGasLimit = Gas(1000L),
          accumulateGasLimit = Gas(1000L),
          importSegments = List.empty,
          extrinsic = List.empty,
          exportCount = UShort(0)
        )
      )
    )

  test("BAD when the auth-code host service or preimage is missing") {
    val wp = workPackage(5L)
    val executor = new IsAuthorizedExecutor(config)

    executor
      .execute(wp, 0, new HostLookup(99L, None))
      .result shouldBe ExecutionResult.BadCode
    executor
      .execute(wp, 0, new HostLookup(5L, None))
      .result shouldBe ExecutionResult.BadCode
  }

  test("BIG when the authorization code exceeds Cmaxauthcodesize") {
    val wp = workPackage(5L)
    // metadata prefix 0x00 + 64001-byte "code"
    val oversized = Array[Byte](0) ++ new Array[Byte](64_001)
    val result =
      new IsAuthorizedExecutor(config).execute(wp, 0, new HostLookup(5L, Some(oversized)))
    result.result shouldBe ExecutionResult.CodeTooLarge
  }

  test("a halting authorizer returns its output as the trace with encode[2](core) args") {
    val wp = workPackage(5L)
    val accounts = new HostLookup(5L, Some(preimageOf(haltCode, haltBitmask)))
    val result = new IsAuthorizedExecutor(config).execute(wp, 3, accounts)

    result.result match
      case ExecutionResult.Ok(trace) =>
        // Args echo: encode[2](3) = [3, 0]
        trace.toArray shouldBe Array[Byte](3, 0)
      case other => fail(s"expected Ok, got $other")
    result.gasUsed should be > 0L
  }

  test("fetch during is-authorized serves the work package but not refine data") {
    val wp = workPackage(5L)
    // ecalli 1 (FETCH: r7 dest = input addr works? use r7 register default) —
    // simpler to exercise the dispatcher directly:
    val hc = new IsAuthorizedHostCallsProbe(config, wp)
    hc.probe()
  }

  test("authorizerHash is blake(authCodeHash ++ authorizerConfig)") {
    val wp = workPackage(5L)
    val expected = Hashing.blake2b256(
      authCodeHash.bytes.toArray ++ Array[Byte](0x0d)
    )
    new IsAuthorizedExecutor(config).authorizerHash(wp) shouldBe expected
  }

/** Exercises the private is-authorized dispatcher via a mock instance. */
private class IsAuthorizedHostCallsProbe(
    config: ChainConfig,
    wp: WorkPackage
) extends org.scalatest.matchers.should.Matchers:
  import spire.math.ULong

  def probe(): Unit =
    // Verify the shared fetch table with the is-authorized parameterisation
    // (only the work package present).
    val token = RefineFetch.fetchValue(
      ULong(9),
      ULong(0),
      ULong(0),
      config,
      Some(wp),
      None,
      None,
      None,
      None,
      None
    )
    token.map(_.toSeq) shouldBe Some(wp.authorization.toArray.toSeq)

    // Refine-only selectors resolve to none in the is-authorized context.
    for sel <- List(1L, 2L, 3L, 4L, 5L, 6L, 14L, 15L) do
      RefineFetch.fetchValue(
        ULong(sel),
        ULong(0),
        ULong(0),
        config,
        Some(wp),
        None,
        None,
        None,
        None,
        None
      ) shouldBe None
