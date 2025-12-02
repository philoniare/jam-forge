package io.forge.jam.core

import io.forge.jam.core.serializers.JamByteArrayListSerializer
import kotlinx.serialization.Serializable

@Serializable(with = JamByteArrayListSerializer::class)
class JamByteArrayList : AbstractMutableList<JamByteArray>(), Encodable, Cloneable {
    private val items = mutableListOf<JamByteArray>()

    companion object {
        fun fromBytes(data: ByteArray, offset: Int = 0, itemSize: Int = 32): Pair<JamByteArrayList, Int> {
            var currentOffset = offset
            val (length, lengthBytes) = decodeCompactInteger(data, currentOffset)
            currentOffset += lengthBytes
            val list = JamByteArrayList()
            for (i in 0 until length.toInt()) {
                list.add(JamByteArray(data.copyOfRange(currentOffset, currentOffset + itemSize)))
                currentOffset += itemSize
            }
            return Pair(list, currentOffset - offset)
        }
    }

    public override fun clone(): JamByteArrayList {
        val clone = JamByteArrayList()
        clone.items.addAll(items.map { it.clone() })
        return clone
    }

    override val size: Int get() = items.size

    override fun contains(element: JamByteArray): Boolean {
        return items.any { it.contentEquals(element) }
    }

    override fun add(element: JamByteArray): Boolean {
        return items.add(element.clone())
    }

    override fun add(index: Int, element: JamByteArray) {
        items.add(index, element.clone())
    }

    override fun removeAt(index: Int): JamByteArray {
        return items.removeAt(index)
    }

    override fun set(index: Int, element: JamByteArray): JamByteArray {
        return items.set(index, element.clone())
    }

    override fun get(index: Int): JamByteArray {
        return items[index].clone()
    }

    override fun clear() {
        items.clear()
    }

    override fun indexOf(element: JamByteArray): Int {
        return items.indexOfFirst { it.contentEquals(element) }
    }

    override fun lastIndexOf(element: JamByteArray): Int {
        return items.indexOfLast { it.contentEquals(element) }
    }

    fun toList(): List<JamByteArray> = items.map { it.clone() }
    override fun encode(): ByteArray {
        return encodeList(items)
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is JamByteArrayList) return false
        if (size != other.size) return false
        return items.indices.all { items[it].contentEquals(other.items[it]) }
    }

    override fun hashCode(): Int {
        return items.fold(1) { acc, item ->
            31 * acc + item.hashCode()
        }
    }

    override fun toString(): String {
        return buildString {
            append("JamByteArrayList(size=")
            append(size)
            append(", items=[")
            items.forEachIndexed { index, item ->
                if (index > 0) append(", ")
                append(item.toString())
            }
            append("])")
        }
    }
}
