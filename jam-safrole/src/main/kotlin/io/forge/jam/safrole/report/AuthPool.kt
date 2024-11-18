package io.forge.jam.safrole.report

import kotlinx.serialization.Serializable

@Serializable
data class AuthPool(val auths: List<ByteArray>)
