package io.forge.jam.crypto

/** BLS12-381 signatures via w3f-bls `TinyBLS381`: signatures in G1, the 144-byte
  * validator key `k_bls` is the double public key G1 ++ G2 of eprint 2022/1611, and a signature is the 112-byte
  * double signature (G1 point ++ DLEQ proof). Aggregates are plain 48-byte G1 sums checked against the summed G2
  * halves, which assumes possession of each key was proven (validator keys are registered on chain).
  */
object Bls:

  val PublicKeySize: Int = 144
  val SignatureSize: Int = 112
  val AggregateSize: Int = 48

  @volatile private var loaded = false

  private def ensureLoaded(): Unit =
    if !loaded then
      BlsWrapper.ensureLibraryLoaded()
      loaded = true

  def publicFromSeed(seed: Array[Byte]): Array[Byte] =
    ensureLoaded()
    Option(BlsWrapper.publicFromSeed(seed)).getOrElse(throw new IllegalStateException("BLS key derivation failed"))

  def sign(seed: Array[Byte], message: Array[Byte]): Array[Byte] =
    ensureLoaded()
    Option(BlsWrapper.sign(seed, message)).getOrElse(throw new IllegalStateException("BLS signing failed"))

  def verify(publicKey: Array[Byte], message: Array[Byte], signature: Array[Byte]): Boolean =
    publicKey.length == PublicKeySize && signature.length == SignatureSize && {
      ensureLoaded()
      BlsWrapper.verify(publicKey, message, signature)
    }

  def keyConsistent(publicKey: Array[Byte]): Boolean =
    publicKey.length == PublicKeySize && { ensureLoaded(); BlsWrapper.keyConsistent(publicKey) }

  def aggregate(signatures: Seq[Array[Byte]]): Option[Array[Byte]] =
    if signatures.isEmpty || signatures.exists(_.length != SignatureSize) then None
    else
      ensureLoaded()
      Option(BlsWrapper.aggregate(signatures.toArray.flatten))

  def verifyAggregate(publicKeys: Seq[Array[Byte]], message: Array[Byte], aggregate: Array[Byte]): Boolean =
    publicKeys.nonEmpty && publicKeys.forall(_.length == PublicKeySize) && aggregate.length == AggregateSize && {
      ensureLoaded()
      BlsWrapper.verifyAggregate(publicKeys.toArray.flatten, message, aggregate)
    }
