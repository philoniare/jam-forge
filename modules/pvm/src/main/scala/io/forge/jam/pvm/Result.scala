package io.forge.jam.pvm

import spire.math.{UByte, UShort, UInt, ULong}

/**
 * Result type for potentially failing operations.
 * Using explicit error types instead of exceptions for purity and clarity.
 */
enum PvmResult[+A]:
  case Success(value: A)
  case OutOfGas
  case Panic
  case Segfault(pageAddress: UInt)
  case HostCall(hostCallId: UInt)
  case Finished

  def map[B](f: A => B): PvmResult[B] = this match
    case Success(v) => Success(f(v))
    case OutOfGas => OutOfGas
    case Panic => Panic
    case Segfault(addr) => Segfault(addr)
    case HostCall(id) => HostCall(id)
    case Finished => Finished

  def flatMap[B](f: A => PvmResult[B]): PvmResult[B] = this match
    case Success(v) => f(v)
    case OutOfGas => OutOfGas
    case Panic => Panic
    case Segfault(addr) => Segfault(addr)
    case HostCall(id) => HostCall(id)
    case Finished => Finished

  def isSuccess: Boolean = this.isInstanceOf[Success[?]]
  def isError: Boolean = !isSuccess

  def getOrElse[B >: A](default: => B): B = this match
    case Success(v) => v
    case _ => default

  def toOption: Option[A] = this match
    case Success(v) => Some(v)
    case _ => None

  def toEither: Either[PvmResult[Nothing], A] = this match
    case Success(v) => Right(v)
    case err => Left(err.asInstanceOf[PvmResult[Nothing]])

  def fold[B](onSuccess: A => B, onError: PvmResult[Nothing] => B): B = this match
    case Success(v) => onSuccess(v)
    case err => onError(err.asInstanceOf[PvmResult[Nothing]])

object PvmResult:
  def success[A](a: A): PvmResult[A] = Success(a)
  def unit: PvmResult[Unit] = Success(())

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

  def toEither: Either[String, A] = this match
    case Success(v) => Right(v)
    case Segfault(a, _) => Left(f"Segfault at address 0x${a.toLong}%08x")
    case OutOfBounds(a) => Left(f"Out of bounds access at 0x${a.toLong}%08x")

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

  def recover[B >: A](f: PartialFunction[MemoryResult[Nothing], B]): MemoryResult[B] = this match
    case Success(v) => Success(v)
    case err =>
      val errCast = err.asInstanceOf[MemoryResult[Nothing]]
      if f.isDefinedAt(errCast) then Success(f(errCast))
      else err.asInstanceOf[MemoryResult[B]]

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
