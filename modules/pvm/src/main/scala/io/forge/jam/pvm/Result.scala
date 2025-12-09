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
