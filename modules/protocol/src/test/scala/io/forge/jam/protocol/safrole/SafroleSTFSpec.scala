package io.forge.jam.protocol.safrole

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import io.forge.jam.core.ChainConfig
import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.{Hash, BandersnatchPublicKey, Ed25519PublicKey, BlsPublicKey}
import io.forge.jam.core.types.epoch.ValidatorKey
import io.forge.jam.core.types.tickets.{TicketMark, TicketEnvelope}
import io.forge.jam.crypto.BandersnatchVrf
import io.forge.jam.protocol.safrole.SafroleTypes.*
import spire.math.UByte

/**
 * Safrole STF behaviour not exercised by the stf/safrole vectors (which SafroleTest
 * runs in full): slot strictly below tau, offender nullification on the epoch
 * transition, and a losing ticket against a saturated accumulator.
 */
class SafroleSTFSpec extends AnyFunSuite with Matchers:

  private def hexToBytes(hex: String): Array[Byte] =
    val cleanHex = hex.stripPrefix("0x")
    cleanHex.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray

  private val tinyBanderKeys: List[BandersnatchPublicKey] = List(
    "0xff71c6c03ff88adb5ed52c9681de1629a54e702fc14729f6b50d2f0a76f185b3",
    "0xdee6d555b82024f1ccf8a1e37e60fa60fd40b1958c4bb3006af78647950e1b91",
    "0x9326edb21e5541717fde24ec085000b28709847b8aab1ac51f84e94b37ca1b66",
    "0x0746846d17469fb2f95ef365efcab9f4e22fa1feb53111c995376be8019981cc",
    "0x151e5c8fe2b9d8a606966a79edd2f9e5db47e83947ce368ccba53bf6ba20a40b",
    "0x2105650944fcd101621fd5bb3124c9fd191d114b7ad936c1d79d734f9f21392e"
  ).map(h => BandersnatchPublicKey(hexToBytes(h)))

  // Ring commitment (gamma_z) over the six keys above, in order.
  private val tinyFullRingRootHex =
    "af39b7de5fcfb9fb8a46b1645310529ce7d08af7301d9758249da4724ec698eb127f489b58e49ae9ab85027509116962a135fc4d97b66fbbed1d3df88cd7bf5cc6e5d7391d261a4b552246648defcb64ad440d61d69ec61b5473506a48d58e1992e630ae2b14e758ab0960e372172203f4c9a41777dadd529971d7ab9d23ab29fe0e9c85ec450505dde7f5ac038274cf"

  // A real ring-VRF ticket (attempt 0) and its verification inputs.
  private val ticketEntropyHex = "bb30a42c1e62f0afda5f0a4e8a562f7a13a24cea00ee81917b86b89e801314aa"
  private val ticketSignatureHex =
    "e5055ade89aba8daa558078c28b6f86468772f63bebe920d88ce3e189405c09660e430f3ea78a6d70aa849570e35de25a35414c1bf5df87f0ebcf495a9481ef289bfe8b2135788aa9687549f8a4a1d492323e817eb7ddd396d9d4d1f95da6b807822b436856a8a5c06f65f0aecb4a077944d0b5b308293c403a28a91c8d4474fe99615eebe49cd57973e101a5bc3982144d236031327aa95f58d16d92364df062a89bb767c63aef3cd2938e7af15cf9e3e60b19afd0d3d78c43ed0af8e9fd0108407aef70f2bba8bf2bd9316eb6c1cc923e133e00096f6ebbb58b124491d7093f271db465926809975343eaa02d30c108d6db123d2815dbfa6cbe74866ed9855eecbbab8e2011f84f71a2e360caeac3bcd64c4b46b11ca167e238cf5f0ccfe7f93ff1cab74815f48db1759417f39f81d8887490789027344561d32285bf92ca503bca1dcf8b013d5938e865ea933221f98910d47cc02f4a9a826c16b5893a6c792c4aee4b20c2df607796db538b5c6538623b88151a00aad17689fa94ee32123f1ec933cb4fdbc00e5452ba7f151b86359c8fed9878216e1fdac29c26a5a700c2994d49afce3daec4839a2ec63f1c36b47bef47014f9433005103d9d1bd5541d409ec0cfbeaf2dd92627a1fb702e9a0b509403f965c3be6a31aeb8ad97cbc725e359d8b8960a3a512af9af95c2712f26a5d4483b69001c956abfdc781148d73db38ee8ce579f5030bb28d98fa84e4c942327b6e58ee6ce1f0f1e71b4e4b7263b347140d7979bd4d1cda8532dc8ba12dbc31f7b154f0bda72fc486adba2737e6621633f98350f73040c9abf9530a09989476e6243cdcf397ac05a0ccb71eb6265af7258d8dc1ac2f66210686e7deeca4099bb7b2240d2dae18bcd739cf9fdcb0536745cdf6b30c3c29312511b8f71a035c2114ea47439261d30a556b66b84d1a4ba2cc64bdf2b19f862a363b9d91ee022ab7ffa61dedd23efb3c4effdbe955638956d6ee3998cf874449763818d23fbdb7297d47f82db65db6ebac8a6d06c0dbeb3163095c13fd27af4e31416c5561c7adc552bf02ad47d1c99757bc66c302eb2a851c706b097869faed86517a22fae96"

  /** Build a TINY validator key; `blsByte`/`edByte` let a test mark one key as an
    * offender with a non-zero BLS body so the zeroing is observable. */
  private def tinyValidator(bander: BandersnatchPublicKey, edByte: Int, blsByte: Int): ValidatorKey =
    ValidatorKey(
      bandersnatch = bander,
      ed25519 = Ed25519PublicKey(Array.fill(32)(edByte.toByte)),
      bls = BlsPublicKey(Array.fill(144)(blsByte.toByte)),
      metadata = JamBytes(Array.fill(ValidatorKey.MetadataSize)(edByte.toByte))
    )

  /** iota = six validators on the valid TINY ring; validator 0 is the offender,
    * with a non-zero (0xAB) BLS key. Returns (preStateAtEpochEnd, iota). */
  private def offenderEpochEndState(cfg: ChainConfig): (SafroleState, IndexedSeq[ValidatorKey]) =
    val iota = tinyBanderKeys.zipWithIndex.map { case (b, i) =>
      tinyValidator(b, edByte = i + 1, blsByte = if i == 0 then 0xAB else i + 1)
    }.toIndexedSeq
    val pre = SafroleState(
      tau = cfg.epochLength - 1,
      eta = List.fill(4)(Hash.zero),
      lambda = iota,
      kappa = iota,
      gammaK = iota,
      iota = iota,
      gammaA = List.empty,
      gammaS = TicketsOrKeys.Keys(List.fill(cfg.epochLength)(tinyBanderKeys.head)),
      gammaZ = JamBytes(hexToBytes(tinyFullRingRootHex)), // root over the full (un-nullified) ring
      postOffenders = List(iota.head.ed25519)
    )
    (pre, iota)

  test("a slot equal to or below tau returns BadSlot and leaves the state unchanged") {
    val cfg = ChainConfig.TINY
    val (pre, _) = offenderEpochEndState(cfg)

    for slot <- List(pre.tau, pre.tau - 1) do
      val input = SafroleInput(slot = slot, entropy = Hash.zero, extrinsic = List.empty)
      val (post, output) = SafroleTransition.stfInternal(input, pre, cfg)
      output shouldBe Left(SafroleErrorCode.BadSlot)
      post shouldBe pre
  }

  test("an offender's keys, including its full 144-byte BLS key, are nullified on the epoch transition and the ring root is regenerated") {
    assume(BandersnatchVrf.isAvailable, "Native library not available - skipping JNI test")
    val cfg = ChainConfig.TINY
    val (pre, iota) = offenderEpochEndState(cfg)

    // Pre-condition: the offender carries a non-zero, full-length BLS key.
    iota.head.bls.bytes.length shouldBe 144
    iota.head.bls.bytes.exists(_ != 0) shouldBe true

    // Crossing into the next epoch applies the Φ offender filter.
    val input = SafroleInput(slot = cfg.epochLength.toLong, entropy = Hash.zero, extrinsic = List.empty)
    val (post, output) = SafroleTransition.stfInternal(input, pre, cfg)

    output.isRight shouldBe true

    // Validator 0 (the offender) is fully nullified in the posterior γ_K.
    val nulled = post.gammaK.head
    nulled.bandersnatch.bytes.forall(_ == 0) shouldBe true
    nulled.ed25519.bytes.forall(_ == 0) shouldBe true
    nulled.metadata.toArray.forall(_ == 0) shouldBe true
    nulled.bls.bytes.length shouldBe 144
    nulled.bls.bytes.forall(_ == 0) shouldBe true

    // Non-offending validators are carried through unchanged.
    post.gammaK.tail.zip(iota.tail).foreach { case (p, original) =>
      p.bandersnatch.bytes shouldBe original.bandersnatch.bytes
      p.ed25519.bytes shouldBe original.ed25519.bytes
      p.bls.bytes shouldBe original.bls.bytes
    }

    post.gammaZ.length shouldBe SafroleTypes.BandersnatchRingCommitmentSize
    post.gammaZ.toArray.toSeq should not equal pre.gammaZ.toArray.toSeq
  }

  test("a losing ticket against a saturated accumulator rejects the block") {
    assume(BandersnatchVrf.isAvailable, "Native library not available - skipping JNI test")
    val cfg = ChainConfig.TINY

    val lowIds = (0 until cfg.epochLength).toList.map { i =>
      val b = new Array[Byte](32); b(31) = i.toByte
      TicketMark(JamBytes(b), UByte(0))
    }
    val validators = (0 until cfg.validatorCount).map(i => tinyValidator(tinyBanderKeys(i), i + 1, i + 1))

    val pre = SafroleState(
      tau = 0L,
      // eta(2) drives ticket verification and must match the vector's entropy.
      eta = List(Hash.zero, Hash.zero, Hash(hexToBytes(ticketEntropyHex)), Hash.zero),
      lambda = validators,
      kappa = validators,
      gammaK = validators,
      iota = validators,
      gammaA = lowIds,
      gammaS = TicketsOrKeys.Keys(List.fill(cfg.epochLength)(tinyBanderKeys.head)),
      gammaZ = JamBytes(hexToBytes(tinyFullRingRootHex)),
      postOffenders = List.empty
    )
    // slot 1 stays in epoch 0 (no rotation) and is before the ticket cutoff.
    val realTicket = TicketEnvelope(UByte(0), JamBytes(hexToBytes(ticketSignatureHex)))
    val input = SafroleInput(slot = 1L, entropy = Hash.zero, extrinsic = List(realTicket))

    val (post, output) = SafroleTransition.stfInternal(input, pre, cfg)

    output.isLeft shouldBe true
    output.left.toOption.get shouldBe SafroleErrorCode.Reserved
    post shouldBe pre
  }
