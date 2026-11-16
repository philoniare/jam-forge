package io.forge.jam.network

import java.util.concurrent.CompletableFuture

import io.forge.jam.core.primitives.Ed25519PublicKey

/** One JAMNP-S stream as the protocol services see it: framed messages each way, a FIN, a reset. [[JamnpStream]] is
  * the QUIC one; a test can stand in another.
  */
trait MessageStream:
  def kind: Byte

  /** Register the message receiver; messages received before this call are delivered first, in order. */
  def onMessage(f: Array[Byte] => Unit): MessageStream

  /** Register a listener for the peer's FIN or reset; one that already happened fires it at once. */
  def onClosed(f: () => Unit): MessageStream
  def send(message: Array[Byte]): Unit

  /** FIN: no more messages from this side. */
  def finish(): Unit
  def close(): Unit

  /** Give the stream up in both directions: rejection or failure from its acceptor, cancellation from its opener. */
  def reset(): Unit
  def isOpen: Boolean
  def id: Long
  def pendingOutboundBytes: Long

/** A connection to one peer as the protocol services see it: who the peer is, and streams to it. */
trait PeerConnection:
  /** Whether this side dialed. */
  def initiator: Boolean
  def openedAtMillis: Long

  /** The peer's raw Ed25519 public key. */
  def peerKey: Array[Byte]
  def peerPublicKey: Ed25519PublicKey
  def negotiatedAlpn: String

  /** Count `bytes` received from the peer and not yet admitted against its budget; false when over it. */
  def holdInbound(bytes: Long): Boolean
  def releaseInbound(bytes: Long): Unit
  def heldInboundBytes: Long
  def pendingOutboundBytes: Long
  def droppedOutboundMessages: Long
  def openStream(kind: Byte): CompletableFuture[? <: MessageStream]
  def close(): Unit
  def isOpen: Boolean
  def openStreamCount: Int
  def remoteAddress: Option[java.net.SocketAddress]

/** Handles the streams of one kind that peers open, whatever carries them. */
trait PeerStreamHandler:
  def onStream(connection: PeerConnection, stream: MessageStream): Unit
