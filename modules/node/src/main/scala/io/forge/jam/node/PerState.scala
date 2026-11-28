package io.forge.jam.node

final class PerState[K, A](compute: K => A):
  @volatile private var held: (K, A) | Null = null

  def apply(key: K): A =
    val h = held
    if h != null && PerState.enabled && h._1 == key then h._2
    else
      val value = compute(key)
      held = (key, value)
      value

object PerState:
  @volatile private[node] var enabled: Boolean = true
