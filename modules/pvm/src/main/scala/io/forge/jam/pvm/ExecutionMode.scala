package io.forge.jam.pvm
import com.typesafe.scalalogging.StrictLogging

import java.nio.file.Files

enum ExecutionMode:
  case Interpreted, Recompiled

object ExecutionMode extends StrictLogging:
  def fromProperty(value: Option[String]): ExecutionMode =
    value match
      case Some("recompiled") => Recompiled
      case Some("interpreted") => Interpreted
      case Some(other) if other.nonEmpty =>
        logger.warn(s"ExecutionMode.fromProperty: unrecognized jam.pvm.executionMode value '$other' — falling back to Interpreted")
        Interpreted
      case _ => Interpreted

  /** Whether this process can run guest code natively: the recompiler library is named by
    * `-Djam.pvm.recompiler.lib`, the file is there, and the host is one it emits code for.
    */
  def nativeAvailable: Boolean = nativeAvailable(PvmSettings.current)

  def nativeAvailable(settings: PvmSettings): Boolean =
    val arch = System.getProperty("os.arch", "").toLowerCase
    val os = System.getProperty("os.name", "").toLowerCase
    val hostSupported =
      (arch == "aarch64" || arch == "arm64" || arch == "amd64" || arch == "x86_64") &&
        (os.contains("mac") || os.contains("linux"))
    hostSupported && settings.recompilerLib.exists(Files.exists(_))

  /** `-Djam.pvm.executionMode=interpreted|recompiled` when given; otherwise recompiled wherever the library is
    * available, and the interpreter elsewhere.
    */
  val default: ExecutionMode =
    val settings = PvmSettings.current
    settings.executionMode match
      case Some(value) =>
        val resolved = fromProperty(Some(value))
        logger.info(s"ExecutionMode.default is $resolved (-Djam.pvm.executionMode=$value)")
        resolved
      case None =>
        val available = nativeAvailable(settings)
        val resolved = if available then Recompiled else Interpreted
        logger.info(s"ExecutionMode.default is $resolved (recompiler library ${if available then "found" else "not available"})")
        resolved
