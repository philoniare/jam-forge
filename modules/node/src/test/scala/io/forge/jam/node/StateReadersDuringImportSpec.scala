package io.forge.jam.node

import io.forge.jam.core.primitives.Hash
import io.forge.jam.protocol.state.TrieBackedJamState
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicBoolean

class StateReadersDuringImportSpec extends AnyFunSuite with Matchers with LoopbackFixture:

  private def services(view: TrieBackedJamState) =
    view.accumulation.serviceIds.toList.sorted.map(id => id -> view.accumulation.serviceInfo(id))

  test("readers of older and best states between and during imports do not change the imported state roots") {
    withNodes(2, "readers-during-import") { nodes =>
      val (a, b) = (nodes(0), nodes(1))
      val author = new BlockAuthor(a.chain, devKeys)
      val roots = new ConcurrentLinkedQueue[Hash]()
      roots.add(b.chain.best.stateRoot)

      val stop = new AtomicBoolean(false)
      val errors = new ConcurrentLinkedQueue[Throwable]()
      val background = new Thread(() =>
        try
          while !stop.get() do
            roots.forEach(root => services(b.chain.stateViewAtRoot(root)))
            services(b.chain.stateView())
        catch case t: Throwable => errors.add(t); ()
      )
      background.start()
      try
        for slot <- 1 to 12 do
          val block = author.tryAuthor(slot.toLong).getOrElse(fail(s"author block $slot"))
          val bytes = a.chain.encodeBlock(block)
          val onA = a.chain.importBlock(bytes).fold(e => fail(s"import on the author: $e"), identity)
          roots.forEach { root =>
            services(b.chain.stateViewAtRoot(root)) shouldBe services(a.chain.stateViewAtRoot(root))
          }
          services(b.chain.stateView()) shouldBe services(a.chain.stateViewAtRoot(b.chain.best.stateRoot))
          val onB = b.chain.importBlock(bytes).fold(e => fail(s"import at slot $slot: $e"), identity)
          withClue(s"slot $slot: ")(onB.stateRoot shouldBe onA.stateRoot)
          roots.add(onB.stateRoot)
      finally
        stop.set(true)
        background.join()
      withClue(errors.toArray.mkString("; "))(errors shouldBe empty)
      b.chain.best.stateRoot shouldBe a.chain.best.stateRoot
      services(b.chain.stateView()).size should be > 0
    }
  }
