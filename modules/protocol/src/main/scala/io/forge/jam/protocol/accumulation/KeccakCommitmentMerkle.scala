package io.forge.jam.protocol.accumulation

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import org.bouncycastle.jcajce.provider.digest.Keccak

import java.nio.{ByteBuffer, ByteOrder}

private[accumulation] object KeccakCommitmentMerkle:

  /** Compute the Keccak Merkle root of service commitments.
    */
  def computeCommitmentRoot(commitments: Set[Commitment]): JamBytes =
    if commitments.isEmpty then return JamBytes(new Array[Byte](Hash.Size))

    // Sort by service index, then by hash for deterministic ordering
    val sortedCommitments =
      commitments.toList.sortBy(c => (c.serviceIndex, c.hash))
    val nodes = sortedCommitments.map { commitment =>
      val buffer = ByteBuffer.allocate(4 + Hash.Size).order(ByteOrder.LITTLE_ENDIAN)
      buffer.putInt(commitment.serviceIndex.toInt)
      buffer.put(commitment.hash.toArray)
      buffer.array()
    }

    // Binary Merkle tree with Keccak-256
    JamBytes(binaryMerklize(nodes))

  /** Well-balanced binary Merkle function.
    */
  private def binaryMerklize(leaves: List[Array[Byte]]): Array[Byte] =
    leaves match
      case Nil         => new Array[Byte](Hash.Size)
      case head :: Nil => keccak256(head)
      case _           =>
        binaryMerklizeHelper(leaves) match
          case MerklizeResult.Leaf(data) => keccak256(data)
          case MerklizeResult.Hash(hash) => hash

  /** Merkle result can be either a leaf (unhashed data) or a hash.
    */
  private enum MerklizeResult:
    case Leaf(data: Array[Byte])
    case Hash(hash: Array[Byte])

    def toByteArray: Array[Byte] = this match
      case Leaf(data) => data
      case Hash(hash) => hash

  /** Helper for well-balanced binary Merkle tree.
    */
  private def binaryMerklizeHelper(nodes: List[Array[Byte]]): MerklizeResult =
    nodes match
      case Nil         => MerklizeResult.Hash(new Array[Byte](Hash.Size))
      case head :: Nil => MerklizeResult.Leaf(head)
      case _           =>
        val mid = (nodes.size + 1) / 2 // roundup of half
        val left = nodes.take(mid)
        val right = nodes.drop(mid)
        val leftResult = binaryMerklizeHelper(left)
        val rightResult = binaryMerklizeHelper(right)
        // Hash with "node" prefix as per GP E.1.1
        MerklizeResult.Hash(
          keccakHashWithPrefix(
            "node".getBytes,
            leftResult.toByteArray,
            rightResult.toByteArray
          )
        )

  private def keccak256(data: Array[Byte]): Array[Byte] =
    val digest = new Keccak.Digest256()
    digest.update(data, 0, data.length)
    digest.digest()

  private def keccakHashWithPrefix(
      prefix: Array[Byte],
      left: Array[Byte],
      right: Array[Byte]
  ): Array[Byte] =
    val digest = new Keccak.Digest256()
    digest.update(prefix, 0, prefix.length)
    digest.update(left, 0, left.length)
    digest.update(right, 0, right.length)
    digest.digest()
