package io.forge.jam.node

import io.forge.jam.db.BlockStore

trait RoleStore:
  def write(puts: Iterable[(String, Array[Byte])], deletes: Iterable[String] = Nil): Unit
  def scan(prefix: String, before: String = ""): List[(String, Array[Byte])]

object RoleStore:

  val none: RoleStore = new RoleStore:
    def write(puts: Iterable[(String, Array[Byte])], deletes: Iterable[String]): Unit = ()
    def scan(prefix: String, before: String): List[(String, Array[Byte])] = Nil

  def over(store: BlockStore): RoleStore = new RoleStore:
    def write(puts: Iterable[(String, Array[Byte])], deletes: Iterable[String]): Unit =
      store.writeMeta(puts, deletes)
    def scan(prefix: String, before: String): List[(String, Array[Byte])] = store.scanMeta(prefix, before)
