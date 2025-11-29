package io.forge.jam.safrole.preimage

import io.forge.jam.core.Encodable
import io.forge.jam.core.encodeList
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class AccountInfo(
    var preimages: List<PreimageHash>,
    @SerialName("lookup_meta")
    var lookupMeta: List<PreimageHistory>,
) : Encodable {
    override fun encode(): ByteArray {
        return encodeList(preimages) + encodeList(lookupMeta)
    }
}
