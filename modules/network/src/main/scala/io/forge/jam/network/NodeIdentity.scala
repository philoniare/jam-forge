package io.forge.jam.network

import java.math.BigInteger
import java.security.cert.X509Certificate
import java.security.{KeyPair, KeyPairGenerator, PrivateKey, SecureRandom, Security}
import java.util.Date

import org.bouncycastle.asn1.x500.X500Name
import org.bouncycastle.asn1.x509.{Extension, GeneralName, GeneralNames}
import org.bouncycastle.cert.jcajce.{JcaX509CertificateConverter, JcaX509v3CertificateBuilder}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.operator.jcajce.JcaContentSignerBuilder

final class NodeIdentity private (
    val keyPair: KeyPair,
    val certificate: X509Certificate
):
  def privateKey: PrivateKey = keyPair.getPrivate

  /** Raw 32-byte Ed25519 public key. */
  def publicKeyBytes: Array[Byte] =
    NodeIdentity.rawEd25519PublicKey(keyPair.getPublic.getEncoded)

  def altName: String = NodeIdentity.altName(publicKeyBytes)

object NodeIdentity:

  private val Base32Alphabet = "abcdefghijklmnopqrstuvwxyz234567"

  locally {
    if Security.getProvider(BouncyCastleProvider.PROVIDER_NAME) == null then
      Security.addProvider(new BouncyCastleProvider())
  }

  /** N(k): "e" followed by 52 base-32 digits of the key interpreted as a
    * little-endian integer, least-significant digit first.
    */
  def altName(publicKey: Array[Byte]): String =
    require(publicKey.length == 32, "Ed25519 public key must be 32 bytes")
    val sb = new StringBuilder(53)
    sb.append('e')
    // digit i = bits [5i, 5i+5) of the little-endian integer
    var i = 0
    while i < 52 do
      val bitPos = 5 * i
      val byteIdx = bitPos / 8
      val bitIdx = bitPos % 8
      val lo = publicKey(byteIdx) & 0xff
      val hi = if byteIdx + 1 < 32 then publicKey(byteIdx + 1) & 0xff else 0
      val digit = ((lo | (hi << 8)) >> bitIdx) & 0x1f
      sb.append(Base32Alphabet(digit))
      i += 1
    sb.toString

  /** Extract the raw 32-byte Ed25519 key from an X.509 SubjectPublicKeyInfo
    * encoding (the raw key is the trailing 32 bytes).
    */
  def rawEd25519PublicKey(spkiEncoded: Array[Byte]): Array[Byte] =
    java.util.Arrays.copyOfRange(
      spkiEncoded,
      spkiEncoded.length - 32,
      spkiEncoded.length
    )

  /** The peer's raw Ed25519 public key from its TLS certificate. */
  def peerPublicKey(cert: X509Certificate): Array[Byte] =
    rawEd25519PublicKey(cert.getPublicKey.getEncoded)

  /** Generate a fresh identity */
  def generate(): NodeIdentity =
    val kpg = KeyPairGenerator.getInstance("Ed25519")
    fromKeyPair(kpg.generateKeyPair())

  // RFC 8410 DER prefixes for Ed25519 key encodings.
  private val Pkcs8Prefix: Array[Byte] =
    Array(0x30, 0x2e, 0x02, 0x01, 0x00, 0x30, 0x05, 0x06, 0x03, 0x2b, 0x65,
      0x70, 0x04, 0x22, 0x04, 0x20).map(_.toByte)
  private val SpkiPrefix: Array[Byte] =
    Array(0x30, 0x2a, 0x30, 0x05, 0x06, 0x03, 0x2b, 0x65, 0x70, 0x03, 0x21,
      0x00).map(_.toByte)

  /** Deterministic identity from a 32-byte Ed25519 seed (the validator's
    * signing key doubles as its network identity per JAMNP). Encodes the key
    * pair through the JDK provider so the private key carries the v1 PKCS#8
    * form BoringSSL expects.
    */
  def fromSeed(seed: Array[Byte]): NodeIdentity =
    require(seed.length == 32, "Ed25519 seed must be 32 bytes")
    val pub = new org.bouncycastle.crypto.params.Ed25519PrivateKeyParameters(seed, 0)
      .generatePublicKey()
      .getEncoded
    val kf = java.security.KeyFactory.getInstance("Ed25519")
    val priv = kf.generatePrivate(
      new java.security.spec.PKCS8EncodedKeySpec(Pkcs8Prefix ++ seed)
    )
    val pubKey = kf.generatePublic(
      new java.security.spec.X509EncodedKeySpec(SpkiPrefix ++ pub)
    )
    fromKeyPair(new KeyPair(pubKey, priv))

  /** Build the JAMNP self-signed certificate for an existing key pair. */
  def fromKeyPair(keyPair: KeyPair): NodeIdentity =
    val pub = rawEd25519PublicKey(keyPair.getPublic.getEncoded)
    val name = altName(pub)
    val subject = new X500Name(s"CN=$name")
    val serial = new BigInteger(63, new SecureRandom())
    val now = System.currentTimeMillis()
    val notBefore = new Date(now - 24L * 3600 * 1000)
    val notAfter = new Date(now + 365L * 24 * 3600 * 1000)

    val builder = new JcaX509v3CertificateBuilder(
      subject,
      serial,
      notBefore,
      notAfter,
      subject,
      keyPair.getPublic
    )
    builder.addExtension(
      Extension.subjectAlternativeName,
      false,
      new GeneralNames(new GeneralName(GeneralName.dNSName, name))
    )

    val signer = new JcaContentSignerBuilder("Ed25519")
      .build(keyPair.getPrivate)
    val holder = builder.build(signer)
    val cert = new JcaX509CertificateConverter()
      .setProvider(BouncyCastleProvider.PROVIDER_NAME)
      .getCertificate(holder)

    new NodeIdentity(keyPair, cert)
