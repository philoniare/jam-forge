package io.forge.jam.safrole.stats

class StatStateTransition {
    fun transition(
        input: StatInput,
        preState: StatState
    ): Pair<StatState, StatOutput?> {
        val postState = preState.copy()
        return Pair(postState, null)
    }
}
