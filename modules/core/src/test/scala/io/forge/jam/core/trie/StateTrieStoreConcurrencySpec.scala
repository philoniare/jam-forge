package io.forge.jam.core.trie

import io.forge.jam.core.JamBytes
import io.forge.jam.core.primitives.Hash
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.{ConcurrentHashMap, CopyOnWriteArrayList, CountDownLatch}
import java.util.concurrent.atomic.AtomicBoolean
import scala.jdk.CollectionConverters.*

class StateTrieStoreConcurrencySpec extends AnyFlatSpec with Matchers:

  private final class ConcurrentBackend extends StateTrieBackend:
    private val nodes = new ConcurrentHashMap[Hash, TrieNode]()
    private val values = new ConcurrentHashMap[Hash, JamBytes]()
    def readNode(hash: Hash): Option[TrieNode] = Option(nodes.get(hash))
    def readRawValue(hash: Hash): Option[JamBytes] = Option(values.get(hash))
    def batchUpdate(ops: Seq[BackendOp]): Unit =
      ops.foreach {
        case BackendOp.WriteNode(node) => nodes.put(node.hash, node); ()
        case BackendOp.WriteRawValue(v) =>
          values.put(io.forge.jam.core.Hashing.blake2b256(v), v); ()
        case _: BackendOp.NodeRefDelta     => ()
        case _: BackendOp.RawValueRefDelta => ()
      }
    def gc(): Unit = ()
    def clear(): Unit = nodes.clear(); values.clear()

  private def key(i: Int): JamBytes =
    val a = new Array[Byte](31)
    a(0) = (i & 0xff).toByte
    a(1) = ((i >> 8) & 0xff).toByte
    JamBytes(a)

  private def value(i: Int): JamBytes = JamBytes(Array.fill[Byte](40)((i & 0xff).toByte))

  private val readerCount = 8
  private val idsPerReader = 800 // disjoint per-reader cache id range

  "StateTrieStore under a writer + N readers" should
    "never throw and never lose concurrently-cached entries" in {
      val backend = new ConcurrentBackend
      val store = new StateTrieStore(backend)
      store.bootstrap(Seq(key(0) -> value(0)))
      store.primeKnownServiceIds(Set(0L))

      val errors = new CopyOnWriteArrayList[Throwable]()
      val writerDone = new AtomicBoolean(false)
      val start = new CountDownLatch(1)

      val writerIters = 3000
      val writer = new Thread(() => {
        try {
          start.await()
          var i = 1
          while i <= writerIters do
            val trie = StateTrie.at(backend, store.currentRoot)
            trie.update(Seq(key(i) -> Some(value(i))))
            trie.save()
            store.markCommitted(trie.rootHash)
            store.addKnownServiceId(i.toLong)
            i += 1
        } catch {
          case t: Throwable => errors.add(t)
        } finally {
          writerDone.set(true)
        }
        ()
      })

      val readers = (0 until readerCount).map { r =>
        new Thread(() => {
          try {
            start.await()
            val base = 1000 + r * idsPerReader
            var k = 0
            while k < idsPerReader do
              val root = store.currentRoot // @volatile read of pinned
              // Guard-free backend-direct read — the stateView snapshot path.
              val trie = StateTrie.at(backend, root)
              trie.read(key(0))
              val id = (base + k).toLong
              if store.cachedServiceInfo(id).isEmpty then
                store.putCachedServiceInfo(id, value(k))
              store.cachedServiceIds
              k += 1
            // Keep snapshotting until the writer stops, to widen the race.
            while !writerDone.get() do
              val trie = StateTrie.at(backend, store.currentRoot)
              trie.read(key(0))
          } catch {
            case t: Throwable => errors.add(t)
          }
          ()
        })
      }

      writer.start()
      readers.foreach(_.start())
      start.countDown()
      writer.join()
      readers.foreach(_.join())

      withClue(errors.asScala.map(e => s"${e.getClass.getName}: ${e.getMessage}").mkString("\n")) {
        errors shouldBe empty
      }

      // Every id the readers wrote must survive concurrent puts (no lost
      // updates from a torn resize).
      val missing =
        (0 until readerCount).flatMap { r =>
          val base = 1000 + r * idsPerReader
          (0 until idsPerReader).map(base + _).filter(id => store.cachedServiceInfo(id.toLong).isEmpty)
        }
      withClue(s"lost ${missing.size} concurrently-cached ids, e.g. ${missing.take(8)}: ") {
        missing shouldBe empty
      }
    }
