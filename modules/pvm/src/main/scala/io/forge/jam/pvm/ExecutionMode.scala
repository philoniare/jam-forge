package io.forge.jam.pvm
import com.typesafe.scalalogging.StrictLogging

enum ExecutionMode:
  case Interpreted, Recompiled

object ExecutionMode extends StrictLogging:
  def fromProperty(value: Option[String]): ExecutionMode =
    value match
      case Some("recompiled") => Recompiled
      case Some(other) if other.nonEmpty =>
        logger.warn(s"ExecutionMode.fromProperty: unrecognized jam.pvm.executionMode value '$other' — falling back to Interpreted")
        Interpreted
      case _ => Interpreted

  val default: ExecutionMode =
    val resolved = fromProperty(Option(System.getProperty("jam.pvm.executionMode")))
    if resolved == Recompiled then
      logger.info("ExecutionMode.default resolved to Recompiled via -Djam.pvm.executionMode=recompiled")
    resolved
