package io.forge.jam.protocol.accumulation

import scala.collection.immutable.AbstractMap

trait AccountSource:
  def account(id: Long): Option[ServiceAccount]
  def ids: Iterable[Long]

final class LazyAccounts private (source: AccountSource, changes: Map[Long, Option[ServiceAccount]])
    extends AbstractMap[Long, ServiceAccount]:

  override def get(id: Long): Option[ServiceAccount] =
    changes.get(id) match
      case Some(change) => change
      case None         => source.account(id)

  override def contains(id: Long): Boolean = get(id).isDefined

  override def updated[V1 >: ServiceAccount](id: Long, value: V1): Map[Long, V1] =
    value match
      case account: ServiceAccount => new LazyAccounts(source, changes.updated(id, Some(account)))
      case _                       => iterator.toMap[Long, V1].updated(id, value)

  override def removed(id: Long): Map[Long, ServiceAccount] =
    new LazyAccounts(source, changes.updated(id, None))

  override def iterator: Iterator[(Long, ServiceAccount)] =
    val untouched = source.ids.iterator
      .filterNot(changes.contains)
      .flatMap(id => source.account(id).map(id -> _))
    untouched ++ written

  def written: Iterator[(Long, ServiceAccount)] =
    changes.iterator.collect { case (id, Some(account)) => id -> account }

  def removedIds: Iterator[Long] =
    changes.iterator.collect { case (id, None) => id }

object LazyAccounts:
  def apply(source: AccountSource): LazyAccounts = new LazyAccounts(source, Map.empty)
