package io.forge.jam.core

import io.forge.jam.core.serializers.ByteArrayListSerializer
import kotlinx.serialization.Serializable

@Serializable(with = ByteArrayListSerializer::class)
class ByteArrayList : AbstractMutableList<ByteArray>() {
    private val items = mutableListOf<ByteArray>()

    override val size: Int get() = items.size

    override fun contains(element: ByteArray): Boolean {
        return items.any { it.contentEquals(element) }
    }

    override fun add(element: ByteArray): Boolean {
        return items.add(element.clone())
    }

    override fun add(index: Int, element: ByteArray) {
        items.add(index, element.clone())
    }

    override fun removeAt(index: Int): ByteArray {
        return items.removeAt(index)
    }

    override fun set(index: Int, element: ByteArray): ByteArray {
        return items.set(index, element.clone())
    }

    override fun get(index: Int): ByteArray {
        return items[index].clone()
    }

    override fun clear() {
        items.clear()
    }

    override fun indexOf(element: ByteArray): Int {
        return items.indexOfFirst { it.contentEquals(element) }
    }

    override fun lastIndexOf(element: ByteArray): Int {
        return items.indexOfLast { it.contentEquals(element) }
    }

    fun toList(): List<ByteArray> = items.map { it.clone() }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is ByteArrayList) return false
        if (size != other.size) return false
        return items.indices.all { items[it].contentEquals(other.items[it]) }
    }

    override fun hashCode(): Int {
        return items.fold(1) { acc, bytes -> 31 * acc + bytes.contentHashCode() }
    }
}
