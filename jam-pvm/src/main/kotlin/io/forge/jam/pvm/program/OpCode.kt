package io.forge.jam.pvm.program

/**
 * Represents the instruction opcodes for the virtual machine.
 */
@Suppress("EnumEntryName")
enum class Opcode(val value: UByte) {
    trap(0u),
    fallthrough(17u),
    jump_indirect(19u),
    load_imm(4u),
    load_u8(60u),
    load_i8(74u),
    load_u16(76u),
    load_i16(66u),
    load_u32(10u),
    load_i32(102u),
    load_u64(95u),
    store_u8(71u),
    store_u16(69u),
    store_u32(22u),
    store_u64(96u),
    load_imm_and_jump(6u),
    branch_eq_imm(7u),
    branch_not_eq_imm(15u),
    branch_less_unsigned_imm(44u),
    branch_less_signed_imm(32u),
    branch_greater_or_equal_unsigned_imm(52u),
    branch_greater_or_equal_signed_imm(45u),
    branch_less_or_equal_signed_imm(46u),
    branch_less_or_equal_unsigned_imm(59u),
    branch_greater_signed_imm(53u),
    branch_greater_unsigned_imm(50u),
    store_imm_indirect_u8(26u),
    store_imm_indirect_u16(54u),
    store_imm_indirect_u32(13u),
    store_imm_indirect_u64(93u),
    store_indirect_u8(16u),
    store_indirect_u16(29u),
    store_indirect_u32(3u),
    store_indirect_u64(90u),
    load_indirect_u8(11u),
    load_indirect_i8(21u),
    load_indirect_u16(37u),
    load_indirect_i16(33u),
    load_indirect_u32(1u),
    load_indirect_i32(99u),
    load_indirect_u64(91u),
    add_imm_32(2u),
    add_imm_64(104u),
    and_imm(18u),
    xor_imm(31u),
    or_imm(49u),
    mul_imm_32(35u),
    mul_imm_64(121u),
    set_less_than_unsigned_imm(27u),
    set_less_than_signed_imm(56u),
    shift_logical_left_imm_32(9u),
    shift_logical_left_imm_64(105u),
    shift_logical_right_imm_32(14u),
    shift_logical_right_imm_64(106u),
    shift_arithmetic_right_imm_32(25u),
    shift_arithmetic_right_imm_64(107u),
    negate_and_add_imm_32(40u),
    negate_and_add_imm_64(136u),
    set_greater_than_unsigned_imm(39u),
    set_greater_than_signed_imm(61u),
    shift_logical_right_imm_alt_32(72u),
    shift_logical_right_imm_alt_64(103u),
    shift_arithmetic_right_imm_alt_32(80u),
    shift_arithmetic_right_imm_alt_64(111u),
    shift_logical_left_imm_alt_32(75u),
    shift_logical_left_imm_alt_64(110u),
    cmov_if_zero_imm(85u),
    cmov_if_not_zero_imm(86u),
    branch_eq(24u),
    branch_not_eq(30u),
    branch_less_unsigned(47u),
    branch_less_signed(48u),
    branch_greater_or_equal_unsigned(41u),
    branch_greater_or_equal_signed(43u),
    add_32(8u),
    add_64(101u),
    sub_32(20u),
    sub_64(112u),
    and(23u),
    xor(28u),
    or(12u),
    mul_32(34u),
    mul_64(113u),
    mul_upper_signed_signed(67u),
    mul_upper_unsigned_unsigned(57u),
    mul_upper_signed_unsigned(81u),
    set_less_than_unsigned(36u),
    set_less_than_signed(58u),
    shift_logical_left_32(55u),
    shift_logical_left_64(100u),
    shift_logical_right_32(51u),
    shift_logical_right_64(108u),
    shift_arithmetic_right_32(77u),
    shift_arithmetic_right_64(109u),
    div_unsigned_32(68u),
    div_unsigned_64(114u),
    div_signed_32(64u),
    div_signed_64(115u),
    rem_unsigned_32(73u),
    rem_unsigned_64(116u),
    rem_signed_32(70u),
    rem_signed_64(117u),
    cmov_if_zero(83u),
    cmov_if_not_zero(84u),
    jump(5u),
    ecalli(78u),
    store_imm_u8(62u),
    store_imm_u16(79u),
    store_imm_u32(38u),
    store_imm_u64(98u),
    move_reg(82u),
    sbrk(87u),
    load_imm_and_jump_indirect(42u),
    load_imm_64(118u);


    companion object {
        /**
         * Lookup table mapping byte values to Opcode instances
         */
        private val lookupTable: Map<UByte, Opcode> = entries.associateBy { it.value }

        /**
         * Creates an Opcode from any byte value.
         *
         * @param byte The byte value to convert
         * @return The corresponding Opcode or null if the byte value is invalid
         */
        fun fromUByteAny(byte: UByte): Opcode? = lookupTable[byte]


    }

    override fun toString(): String = name

    fun canFallthrough(): Boolean = when (this) {
        trap,
        jump,
        jump_indirect,
        load_imm_and_jump,
        load_imm_and_jump_indirect -> false

        else -> true
    }

    fun startsNewBasicBlock(): Boolean = when (this) {
        trap,
        fallthrough,
        jump,
        jump_indirect,
        load_imm_and_jump,
        load_imm_and_jump_indirect,
        branch_eq,
        branch_eq_imm,
        branch_greater_or_equal_signed,
        branch_greater_or_equal_signed_imm,
        branch_greater_or_equal_unsigned,
        branch_greater_or_equal_unsigned_imm,
        branch_greater_signed_imm,
        branch_greater_unsigned_imm,
        branch_less_or_equal_signed_imm,
        branch_less_or_equal_unsigned_imm,
        branch_less_signed,
        branch_less_signed_imm,
        branch_less_unsigned,
        branch_less_unsigned_imm,
        branch_not_eq,
        branch_not_eq_imm -> true

        else -> false
    }
}
