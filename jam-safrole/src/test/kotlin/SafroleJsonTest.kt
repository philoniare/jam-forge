package io.forge.jam.core.encoding

import io.forge.jam.safrole.SafroleCase
import kotlin.test.Test

class SafroleJsonTest {
    @Test
    fun testSafrole() {
        // Load JSON data from resources using the class loader
        val inputCase = TestFileLoader.loadParseJsonData<SafroleCase>("enact-epoch-change-with-no-tickets-1")

        // Run SFT function with pre_state and input

        // Compare the output with the output of STF
//        assertContentEquals(
//            inputCase.postState,
//            inputCase.postState,
//            "Encoded bytes do not match expected output"
//        )
//
//        // Compare the post_state with the post_state of STF
//        assertContentEquals(
//            inputCase.output,
//            inputCase.output,
//            "Encoded bytes do not match expected output"
//        )
    }
}
