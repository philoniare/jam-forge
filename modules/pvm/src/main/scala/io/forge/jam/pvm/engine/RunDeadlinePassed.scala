package io.forge.jam.pvm.engine

final class RunDeadlinePassed(val deadlineMillis: Long)
    extends RuntimeException(s"run past its deadline ($deadlineMillis ms since the epoch)", null, false, false)
