package io.forge.jam.core.encoding

import io.forge.jam.core.TicketEnvelope
import kotlin.test.Test
import kotlin.test.assertContentEquals

class TicketEnvelopeTest {
    @Test
    fun testEncodeTicket() {
        // Load JSON data from resources using the class loader
        val (inputTickets, expectedOutputBytes) = TestFileLoader.loadTestDataFromTestVectors<List<TicketEnvelope>>("codec/tiny", "tickets_extrinsic")

        val encodedTickets = inputTickets.map { ticket ->
            val extrinsic =
                TicketEnvelope(ticket.attempt, ticket.signature)
            extrinsic.encode()
        }

        // Process each ticket
        val versionByte = byteArrayOf(0x03)
        val concatenatedEncodedTickets = versionByte + encodedTickets.reduce { acc, bytes -> acc + bytes }

        // Compare the concatenated encoded bytes with the expected output bytes
        assertContentEquals(
            expectedOutputBytes,
            concatenatedEncodedTickets,
            "Encoded bytes do not match expected output"
        )
    }
}


