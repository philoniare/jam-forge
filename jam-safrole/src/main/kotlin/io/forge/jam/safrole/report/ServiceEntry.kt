package io.forge.jam.safrole.report

import kotlinx.serialization.Serializable

@Serializable
data class ServiceEntry(
    val id: Long,
    val service: Service
)
