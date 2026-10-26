package io.forge.jam.protocol

import scala.util.control.NoStackTrace

/** A host call's PANIC outcome. It is protocol control flow, not a bug, so it skips the stack trace. */
final class HostCallPanic(message: String) extends RuntimeException(message) with NoStackTrace
