package io.forge.jam.core.encoding

import io.forge.jam.core.TicketEnvelope
import kotlin.test.Test
import kotlin.test.assertContentEquals

class TicketEnvelopeTest {

    private fun testEncodeTicket(configPath: String) {
        val (inputTickets, expectedOutputBytes) = TestFileLoader.loadTestDataFromTestVectors<List<TicketEnvelope>>(configPath, "tickets_extrinsic")

        val encodedTickets = inputTickets.map { ticket ->
            val extrinsic =
                TicketEnvelope(ticket.attempt, ticket.signature)
            extrinsic.encode()
        }

        val versionByte = byteArrayOf(0x03)
        val concatenatedEncodedTickets = versionByte + encodedTickets.reduce { acc, bytes -> acc + bytes }

        assertContentEquals(
            expectedOutputBytes,
            concatenatedEncodedTickets,
            "Encoded bytes do not match expected output"
        )
    }

    @Test
    fun testEncodeTicketTiny() {
        testEncodeTicket("codec/tiny")
    }

    @Test
    fun testEncodeTicketFull() {
        testEncodeTicket("codec/full")
    }
}
