package io.forge.jam.core.trie

import io.forge.jam.core.{JamBytes, Hashing}
import io.forge.jam.core.primitives.Hash

enum TrieNodeType(val tag: Byte):
  case Branch       extends TrieNodeType(0)
  case EmbeddedLeaf extends TrieNodeType(1)
  case RegularLeaf  extends TrieNodeType(2)

object TrieNodeType:
  def fromTag(b: Byte): Option[TrieNodeType] = b match
    case 0 => Some(Branch)
    case 1 => Some(EmbeddedLeaf)
    case 2 => Some(RegularLeaf)
    case _ => None

final case class TrieNode(nodeType: TrieNodeType, left: JamBytes, right: JamBytes):
  require(left.length == 32, s"left must be 32 bytes, got ${left.length}")
  require(right.length == 32, s"right must be 32 bytes, got ${right.length}")

  lazy val hash: Hash = TrieNode.computeHash(nodeType, left, right)

  def encode: JamBytes =
    val out = new Array[Byte](65)
    out(0) = nodeType.tag
    System.arraycopy(left.toArray, 0, out, 1, 32)
    System.arraycopy(right.toArray, 0, out, 33, 32)
    JamBytes(out)

object TrieNode:
  val StorageBytes = 65

  def decode(data: JamBytes): TrieNode =
    require(data.length == StorageBytes, s"expected $StorageBytes bytes, got ${data.length}")
    val t = TrieNodeType.fromTag(data(0))
      .getOrElse(throw new IllegalArgumentException(s"unknown trie node type tag ${data(0)}"))
    val l = JamBytes(data.toArray.slice(1, 33))
    val r = JamBytes(data.toArray.slice(33, 65))
    TrieNode(t, l, r)

  def branch(left: Hash, right: Hash): TrieNode =
    TrieNode(TrieNodeType.Branch, JamBytes(left.bytes), JamBytes(right.bytes))

  def leaf(key: JamBytes, value: JamBytes): TrieNode =
    require(key.length == 31, s"key must be 31 bytes, got ${key.length}")
    if value.length <= 32 then
      val l = new Array[Byte](32)
      l(0) = value.length.toByte
      System.arraycopy(key.toArray, 0, l, 1, 31)
      val r = new Array[Byte](32)
      System.arraycopy(value.toArray, 0, r, 0, value.length)
      TrieNode(TrieNodeType.EmbeddedLeaf, JamBytes(l), JamBytes(r))
    else
      val l = new Array[Byte](32)
      l(0) = 0
      System.arraycopy(key.toArray, 0, l, 1, 31)
      TrieNode(TrieNodeType.RegularLeaf, JamBytes(l), JamBytes(Hashing.blake2b256(value).bytes))

  def leafKey(node: TrieNode): JamBytes =
    require(node.nodeType != TrieNodeType.Branch, "leafKey is only defined on leaf nodes")
    JamBytes(node.left.toArray.slice(1, 32))

  def embeddedValue(node: TrieNode): Option[JamBytes] =
    node.nodeType match
      case TrieNodeType.EmbeddedLeaf =>
        val len = node.left(0) & 0xff
        if len > 32 then None
        else Some(JamBytes(node.right.toArray.slice(0, len)))
      case _ => None

  val emptyHash: Hash = Hash.zero

  private def computeHash(t: TrieNodeType, left: JamBytes, right: JamBytes): Hash =
    val preimage = new Array[Byte](64)
    System.arraycopy(left.toArray, 0, preimage, 0, 32)
    System.arraycopy(right.toArray, 0, preimage, 32, 32)
    t match
      case TrieNodeType.Branch =>
        preimage(0) = (preimage(0) & 0x7f).toByte
      case TrieNodeType.EmbeddedLeaf =>
        val len = preimage(0) & 0x3f
        preimage(0) = (0x80 | len).toByte
      case TrieNodeType.RegularLeaf =>
        preimage(0) = 0xc0.toByte
    Hashing.blake2b256(JamBytes(preimage))
