package io.forge.jam.core.encoding

import io.forge.jam.safrole.SafroleCase
import io.forge.jam.safrole.SafroleStateTransition
import kotlin.test.Test
import kotlin.test.assertEquals

class SafroleJsonTest {
    @Test
    fun testSafrole() {
        // Load JSON data from resources using the class loader
        val (inputCase) = TestFileLoader.loadTestData<SafroleCase>(
            "enact-epoch-change-with-no-tickets-1",
            ".scale"
        )

        // Run SFT function with pre_state and input
        val (postState, output) = SafroleStateTransition.transition(inputCase.input, inputCase.preState)

        // Compare the output with the output of STF
        println("Output=$output")
        println("Inputcase Output=${inputCase.output}")
        assertEquals(
            inputCase.output,
            output,
            "State transition outputs do not match"
        )


        // Compare the post_state with the post_state of STF
        assertEquals(
            inputCase.postState,
            postState,
            "State transition post_state do not match"
        )
        assertEquals(true, false, "")
    }
}
