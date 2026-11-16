package io.forge.jam.network

import java.net.InetSocketAddress
import java.security.{MessageDigest, SecureRandom}
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

import io.netty.buffer.ByteBuf
import io.netty.incubator.codec.quic.QuicTokenHandler

final class SignedQuicTokenHandler(clock: () => Long = () => System.currentTimeMillis()) extends QuicTokenHandler:
  import SignedQuicTokenHandler.*

  private val key =
    val bytes = new Array[Byte](32)
    new SecureRandom().nextBytes(bytes)
    new SecretKeySpec(bytes, "HmacSHA256")

  private def mac(address: InetSocketAddress, time: Long, dcid: Array[Byte]): Array[Byte] =
    val m = Mac.getInstance("HmacSHA256")
    m.init(key)
    m.update(address.getAddress.getAddress)
    m.update(java.nio.ByteBuffer.allocate(10).putShort(address.getPort.toShort).putLong(time).array())
    m.doFinal(dcid)

  override def writeToken(out: ByteBuf, dcid: ByteBuf, address: InetSocketAddress): Boolean =
    val id = new Array[Byte](dcid.readableBytes())
    dcid.getBytes(dcid.readerIndex(), id)
    val time = clock()
    out.writeBytes(mac(address, time, id)).writeLong(time).writeBytes(id)
    true

  override def validateToken(token: ByteBuf, address: InetSocketAddress): Int =
    val length = token.readableBytes()
    if length < HeaderBytes || length > HeaderBytes + MaxConnectionIdBytes then -1
    else
      val start = token.readerIndex()
      val claimed = new Array[Byte](MacBytes)
      token.getBytes(start, claimed)
      val time = token.getLong(start + MacBytes)
      val id = new Array[Byte](length - HeaderBytes)
      token.getBytes(start + HeaderBytes, id)
      val age = clock() - time
      if age < 0 || age > ValidMillis then -1
      else if !MessageDigest.isEqual(claimed, mac(address, time, id)) then -1
      else HeaderBytes

  override def maxTokenLength(): Int = HeaderBytes + MaxConnectionIdBytes

object SignedQuicTokenHandler:
  private val MacBytes = 32
  private val HeaderBytes = MacBytes + 8
  private val MaxConnectionIdBytes = 20

  val ValidMillis: Long = 30_000L
