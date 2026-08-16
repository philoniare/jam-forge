package io.forge.jam.conformance

import io.forge.jam.core.ChainConfig
import io.forge.jam.core.primitives.{Hash, Timeslot}
import io.forge.jam.protocol.traces.RawState
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StateStoreSpec extends AnyFlatSpec with Matchers:

  private def h(i: Int): Hash = Hash(Array.fill[Byte](32)(i.toByte))

  "StateStore.ancestryFor" should "reconstruct a chain deeper than the state-pruning window (BP-9)" in {
    val store = new StateStore(ChainConfig.TINY) // maxAncestryLength=24, maxExtraStates=4
    store.initialize(h(0), RawState.empty, List.empty, genesisParent = Some((h(0), Timeslot(0))))

    val n = 12 // > maxExtraStates(4), < maxAncestryLength(24)
    for i <- 1 to n do
      store.store(h(i), RawState.empty, isOriginal = true, parent = Some((h(i - 1), Timeslot(i))))

    store.size should be <= 6
    val hashes = store.ancestryFor(h(n)).map(_.headerHash)
    hashes should contain(h(n))
    hashes should contain(h(1))
    hashes.size should be >= n
  }

  it should "stay immune to fork pollution across sibling chains" in {
    val store = new StateStore(ChainConfig.TINY)
    store.initialize(h(0), RawState.empty, List.empty, genesisParent = Some((h(0), Timeslot(0))))
    // Two siblings off the same parent h(0): h(1) (original) and h(101) (fork mutation).
    store.store(h(1), RawState.empty, isOriginal = true, parent = Some((h(0), Timeslot(1))))
    store.store(h(101), RawState.empty, isOriginal = false, parent = Some((h(0), Timeslot(1))))

    // Each chain's ancestry contains only its own block + the shared parent — never the sibling.
    store.ancestryFor(h(1)).map(_.headerHash) should not contain h(101)
    store.ancestryFor(h(101)).map(_.headerHash) should not contain h(1)
  }

  it should "bound parentage to the lookup-anchor window in long sessions (BP-9)" in {
    val store = new StateStore(ChainConfig.TINY) // L = 24
    store.initialize(h(0), RawState.empty, List.empty, genesisParent = Some((h(0), Timeslot(0))))
    // Import far more than L blocks in one chain; parentage must not grow without bound.
    for i <- 1 to 60 do
      store.store(h(i), RawState.empty, isOriginal = true, parent = Some((h(i - 1), Timeslot(i))))
    // The reconstructable ancestry is capped at L (24), not 60.
    store.ancestryFor(h(60)).size should be <= store.maxAncestryLength
  }
