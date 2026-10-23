package io.forge.jam.crypto.benchmark

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.{BandersnatchPublicKey, Ed25519PublicKey, Ed25519Signature, Hash}
import io.forge.jam.crypto.{BandersnatchVrf, Ed25519, Ed25519ZebraWrapper, SigningContext}
import io.forge.jam.vrfs.BandersnatchWrapper
import spire.math.UByte

import java.util.Locale
import scala.util.Random

/**
 * Signature verification benchmark. Ed25519 single and batch (a full-config block's 1023
 * assurances), Bandersnatch IETF VRF (block seal and entropy), and Bandersnatch ring VRF (Safrole tickets) at ring
 * sizes 6 (tiny) and 1023 (full), plus the ring commitment built at each epoch change.
 *
 * Args: optional subset of `ed25519`, `ietf`, `ring`. Prints `RESULT name value unit` lines; single-thread figures
 * are the median of 5 runs.
 */
object SignatureVerifyBench:
  private val Validators = 1023
  private val Runs = 5

  private def result(name: String, value: Double, unit: String): Unit =
    println(f"RESULT $name $value%.3f $unit")

  private def timeMs[A](f: => A): (A, Double) =
    val t0 = System.nanoTime()
    val a = f
    (a, (System.nanoTime() - t0) / 1e6)

  private def median(xs: Seq[Double]): Double = xs.sorted.apply(xs.length / 2)

  private def parallelAll(n: Int)(f: Int => Boolean): Boolean =
    java.util.stream.IntStream.range(0, n).parallel().allMatch(i => f(i))

  private def ed25519(rng: Random): Unit =
    Ed25519ZebraWrapper.ensureLibraryLoaded()
    // An assurance signs `jam_available ++ H(anchor ++ bitfield)`: 45 bytes.
    val items = (0 until Validators).map { _ =>
      val secret = rng.nextBytes(32)
      val msg = "jam_available".getBytes("UTF-8") ++ rng.nextBytes(32)
      val pk = Ed25519PublicKey(Ed25519ZebraWrapper.publicFromSecret(secret))
      (pk, msg, Ed25519Signature(Ed25519ZebraWrapper.sign(secret, msg)))
    }
    def single(): Unit =
      items.foreach((pk, m, s) => if !Ed25519.verify(pk, m, s) then throw new IllegalStateException("bad sig"))
    val chunk = items.take(64)
    def batch64(): Unit = if !Ed25519.verifyAll(chunk) then throw new IllegalStateException("bad batch")
    def all(): Unit = if !Ed25519.verifyAll(items) then throw new IllegalStateException("bad batch")
    for _ <- 1 to 3 do { single(); batch64(); all() }
    result("ed25519_verify_single", median(Seq.fill(Runs)(timeMs(single())._2)) * 1000 / Validators, "us/sig")
    // 64 signatures are one native batch, verified on the calling thread.
    result("ed25519_verify_batch64", median(Seq.fill(Runs)(timeMs(batch64())._2)) * 1000 / 64, "us/sig")
    result("ed25519_verify_all_1023", median(Seq.fill(Runs)(timeMs(all())._2)), "ms")

  private def ietf(rng: Random): Unit =
    BandersnatchWrapper.ensureLibraryLoaded()
    val n = 64
    // A block seal: `jam_fallback_seal ++ eta_3` as VRF input, the unsealed header as aux data (~600 bytes).
    val cases = (0 until n).map { _ =>
      val secret = BandersnatchWrapper.secretFromSeed(rng.nextBytes(32))
      val input = SigningContext.fallbackSealInputData(rng.nextBytes(32))
      val aux = rng.nextBytes(600)
      (BandersnatchWrapper.publicFromSecret(secret), input, aux, secret)
    }
    def sign() = cases.map((_, in, aux, sk) => BandersnatchWrapper.ietfVrfSign(sk, in, aux))
    val sigs = sign()
    def verify(): Unit = cases.zip(sigs).foreach { case ((pk, in, aux, _), sig) =>
      if BandersnatchVrf.ietfVrfVerify(pk, in, aux, sig).isEmpty then throw new IllegalStateException("bad seal")
    }
    for _ <- 1 to 3 do { sign(); verify() }
    result("bandersnatch_ietf_sign", median(Seq.fill(Runs)(timeMs(sign())._2)) * 1000 / n, "us/sig")
    result("bandersnatch_ietf_verify", median(Seq.fill(Runs)(timeMs(verify())._2)) * 1000 / n, "us/sig")

  /** `tickets` proofs from distinct ring members, as a block's ticket extrinsic carries (K = 3 tiny, 16 full). */
  private def ring(rng: Random, ringSize: Int, tickets: Int, label: String): Unit =
    val secrets = Vector.fill(ringSize)(BandersnatchWrapper.secretFromSeed(rng.nextBytes(32)))
    val keys = secrets.map(s => BandersnatchPublicKey(BandersnatchWrapper.publicFromSecret(s))).toList
    val entropy = Hash(rng.nextBytes(32))
    // The first call also builds the ring context; time the commitment on a fresh key list after it.
    BandersnatchVrf.generateRingRoot(keys, ringSize)
    val rootMs = timeMs(BandersnatchVrf.generateRingRoot(keys.reverse, ringSize).get)._2
    result(s"bandersnatch_ring_commitment_$label", rootMs, "ms")
    val root = BandersnatchVrf.generateRingRoot(keys, ringSize).get
    val provers = (0 until tickets).map(i => (i * 7919) % ringSize)
    val (proofs, proveMs) = timeMs(provers.map { i =>
      BandersnatchVrf.createRingProof(secrets(i), keys, i, entropy, UByte(0)).get
    })
    result(s"bandersnatch_ring_prove_$label", proveMs / tickets, "ms/proof")
    def verifyOne(p: JamBytes): Boolean =
      BandersnatchVrf.verifyRingProof(p, root, entropy, UByte(0), ringSize).isDefined
    def serial(): Unit = if !proofs.forall(verifyOne) then throw new IllegalStateException("bad ticket")
    def parallel(): Unit = if !parallelAll(proofs.size)(i => verifyOne(proofs(i))) then
      throw new IllegalStateException("bad ticket")
    for _ <- 1 to 2 do { serial(); parallel() }
    result(s"bandersnatch_ring_verify_$label", median(Seq.fill(Runs)(timeMs(serial())._2)) / tickets, "ms/proof")
    result(
      s"bandersnatch_ring_verify_${tickets}_parallel_$label",
      median(Seq.fill(Runs)(timeMs(parallel())._2)),
      "ms"
    )

  def main(args: Array[String]): Unit =
    Locale.setDefault(Locale.US)
    org.slf4j.LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME) match
      case l: ch.qos.logback.classic.Logger => l.setLevel(ch.qos.logback.classic.Level.OFF)
      case _ => ()
    val only = args.toSet
    def enabled(name: String) = only.isEmpty || only.contains(name)
    println(s"SignatureVerifyBench cores=${Runtime.getRuntime.availableProcessors}")
    val rng = new Random(42)
    if enabled("ed25519") then ed25519(rng)
    if enabled("ietf") then ietf(rng)
    if enabled("ring") then
      ring(rng, ringSize = 6, tickets = 3, label = "tiny")
      ring(rng, ringSize = Validators, tickets = 16, label = "full")
