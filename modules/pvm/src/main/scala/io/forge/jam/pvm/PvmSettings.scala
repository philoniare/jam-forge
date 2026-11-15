package io.forge.jam.pvm

import io.forge.jam.core.ProcessSettings

import java.nio.file.Path

final case class PvmSettings(
    executionMode: Option[String] = None,
    recompilerLib: Option[Path] = None,
    instanceMemoryBytes: Option[Long] = None
)

object PvmSettings:
  val ExecutionModeProperty = "jam.pvm.executionMode"
  val RecompilerLibProperty = "jam.pvm.recompiler.lib"
  val InstanceMemoryProperty = "jam.pvm.instanceMemoryBytes"

  val process: ProcessSettings[PvmSettings] = new ProcessSettings(lookup =>
    PvmSettings(
      executionMode = lookup(ExecutionModeProperty).filter(_.nonEmpty),
      recompilerLib = lookup(RecompilerLibProperty).map(Path.of(_)),
      instanceMemoryBytes = lookup(InstanceMemoryProperty).flatMap(_.toLongOption).filter(_ > 0)
    )
  )

  def current: PvmSettings = process.current
