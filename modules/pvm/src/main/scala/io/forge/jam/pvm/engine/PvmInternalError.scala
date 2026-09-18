package io.forge.jam.pvm.engine

final case class PvmInternalError(cause: Throwable):
  def describe: String =
    s"${cause.getClass.getName}: ${Option(cause.getMessage).getOrElse("<no message>")}"
