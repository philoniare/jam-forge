package io.forge.jam.pvm

import spire.math.UInt

/**
 * Memory access result with explicit failure modes.
 */
enum MemoryResult[+A]:
  case Success(value: A)
  case Segfault(address: UInt, pageAddress: UInt)
  case OutOfBounds(address: UInt)

  def map[B](f: A => B): MemoryResult[B] = this match
    case Success(v) => Success(f(v))
    case Segfault(a, p) => Segfault(a, p)
    case OutOfBounds(a) => OutOfBounds(a)

  def flatMap[B](f: A => MemoryResult[B]): MemoryResult[B] = this match
    case Success(v) => f(v)
    case Segfault(a, p) => Segfault(a, p)
    case OutOfBounds(a) => OutOfBounds(a)

  def isSuccess: Boolean = this.isInstanceOf[Success[?]]
  def isError: Boolean = !isSuccess

  def getOrElse[B >: A](default: => B): B = this match
    case Success(v) => v
    case _ => default

  def toOption: Option[A] = this match
    case Success(v) => Some(v)
    case _ => None

  def fold[B](onSuccess: A => B, onError: MemoryResult[Nothing] => B): B = this match
    case Success(v) => onSuccess(v)
    case err => onError(err.asInstanceOf[MemoryResult[Nothing]])

object MemoryResult:
  def success[A](a: A): MemoryResult[A] = Success(a)

/**
 * Interrupt kinds that can stop VM execution.
 */
enum InterruptKind:
  case Step
  case Ecalli(hostCallId: UInt)
  case Panic
  case Segfault(info: SegfaultInfo)
  case Finished
  case OutOfGas

/**
 * Segfault information for debugging and handling.
 */
final case class SegfaultInfo(
  pageAddress: UInt,
  pageSize: UInt
)
