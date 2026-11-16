package io.forge.jam.node

import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}

import io.forge.jam.core.{constants, Hashing, JamBytes}
import io.forge.jam.core.primitives.{Ed25519Signature, Hash, ValidatorIndex}
import io.forge.jam.core.types.extrinsic.AssuranceExtrinsic
import io.forge.jam.crypto.Ed25519ZebraWrapper
import io.forge.jam.network.{Loopback, NodeIdentity, StreamKind}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** CE 141: an assurance is checked only when the peer that sent it is one of the validators. Finding the signer of
  * a stranger's message would cost up to one signature check per validator.
  */
class AssuranceGateSpec extends AnyFunSuite with Matchers:

  private val devKeys = (0 until 6).map(ValidatorKeySet.dev)

  /** Validator `by`'s assurance of an anchor, as the CE 141 message. */
  private def assurance(by: Int): Array[Byte] =
    val anchor = Hashing.blake2b256("anchor".getBytes)
    val bitfield = JamBytes(Array[Byte](1))
    val message = constants.JAM_AVAILABLE_BYTES ++ Hashing.blake2b256(anchor.bytes ++ bitfield.toArray).bytes
    val signature = Ed25519Signature(Ed25519ZebraWrapper.sign(devKeys(by).ed25519Secret, message))
    DistributionService.encodeAssurance(AssuranceExtrinsic(anchor, bitfield, ValidatorIndex(by), signature))

  /** Send `message` on CE 141 from a peer with key `senderKey`, and look at the pool once the stream has ended. */
  private def sendFrom(senderKey: Array[Byte], message: Array[Byte])(check: ExtrinsicPools => Unit): Unit =
    val pools = new ExtrinsicPools
    val egress = Executors.newSingleThreadExecutor()
    val service = new DistributionService(
      pools,
      coresCount = 2,
      egress,
      assurerKeys = () => devKeys.map(_.ed25519Public)
    )
    try
      val (sender, _) = Loopback.pair(
        senderKey,
        NodeIdentity.generate().publicKeyBytes,
        _ => None,
        kind => Option.when(kind == StreamKind.AssuranceDistribution)(service.assuranceHandler)
      )
      val stream = sender.openStream(StreamKind.AssuranceDistribution).get(10, TimeUnit.SECONDS)
      val ended = new CountDownLatch(1)
      stream.onClosed(() => ended.countDown())
      stream.send(message)
      ended.await(10, TimeUnit.SECONDS) shouldBe true
      check(pools)
    finally
      service.shutdown()
      egress.shutdownNow()

  test("a validator's own assurance is pooled") {
    sendFrom(devKeys(2).ed25519Public, assurance(by = 2))(_.assuranceCount shouldBe 1)
  }

  test("a validator may send another validator's assurance (a devnet node holding several keys)") {
    sendFrom(devKeys(1).ed25519Public, assurance(by = 2))(_.assuranceCount shouldBe 1)
  }

  test("an assurance from a peer that is not a validator is reset, valid or not") {
    sendFrom(NodeIdentity.generate().publicKeyBytes, assurance(by = 2))(_.assuranceCount shouldBe 0)
  }
