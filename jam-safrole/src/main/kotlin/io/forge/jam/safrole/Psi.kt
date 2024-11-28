package io.forge.jam.safrole

import io.forge.jam.core.Encodable
import io.forge.jam.core.JamByteArrayList
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class Psi(
    // Good/Valid Reports
    @SerialName("good")
    val good: JamByteArrayList = JamByteArrayList(),

    // Bad/Invalid Reports
    @SerialName("bad")
    val bad: JamByteArrayList = JamByteArrayList(),

    // Wonky/Unknown Reports
    @SerialName("wonky")
    val wonky: JamByteArrayList = JamByteArrayList(),

    // Offending validators
    @SerialName("offenders")
    val offenders: JamByteArrayList = JamByteArrayList(),
) : Encodable {

    fun copy(): Psi = Psi(
        good = JamByteArrayList().apply { addAll(good) },
        bad = JamByteArrayList().apply { addAll(bad) },
        wonky = JamByteArrayList().apply { addAll(wonky) },
        offenders = JamByteArrayList().apply { addAll(offenders) }
    )

    override fun encode(): ByteArray {
        return good.encode() + bad.encode() + wonky.encode() + offenders.encode()
    }
}
