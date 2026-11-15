package io.forge.jam.core

final class ProcessSettings[A](resolve: (String => Option[String]) => A):

  private var value: Option[A] = None

  def from(lookup: String => Option[String]): A = resolve(lookup)

  def fromSystemProperties: A = resolve(ProcessSettings.systemProperty)

  def install(settings: A): Unit = synchronized {
    if value.exists(_ != settings) then
      throw new IllegalStateException(s"process settings already in use: ${value.get}, cannot install $settings")
    value = Some(settings)
  }

  def current: A = synchronized {
    value match
      case Some(v) => v
      case None =>
        val v = fromSystemProperties
        value = Some(v)
        v
  }

object ProcessSettings:
  val systemProperty: String => Option[String] = name => Option(System.getProperty(name))
