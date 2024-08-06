package scalecodec

data class UnionValue<T>(val index: Int, val value: T) {
    init {
        require(index >= 0) { "Index cannot be negative number: $index" }
        require(index <= 255) { "Union can have max 255 values. Index: $index" }
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false
        other as UnionValue<*>
        if (index != other.index) return false
        if (value != other.value) return false
        return true
    }

    override fun hashCode(): Int {
        var result = index
        result = 31 * result + (value?.hashCode() ?: 0)
        return result
    }

    override fun toString(): String {
        return "UnionValue(index=$index, value=$value)"
    }
}
