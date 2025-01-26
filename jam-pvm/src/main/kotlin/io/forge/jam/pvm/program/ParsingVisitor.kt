package io.forge.jam.pvm.program

/**
 * Visitor interface for parsing virtual machine instructions.
 * Each method represents parsing a different type of instruction with its metadata and arguments.
 *
 * @param R The return type for all visitor methods
 */
interface ParsingVisitor<R> {
    // Zero-operand instructions
    fun panic(offset: UInt, argsLength: UInt): R
    fun fallthrough(offset: UInt, argsLength: UInt): R
    fun invalid(offset: UInt, argsLength: UInt): R

    // Jump and immediate operations
    fun jumpIndirect(offset: UInt, argsLength: UInt, reg: RawReg, imm: UInt): R
    fun loadImm(offset: UInt, argsLength: UInt, reg: RawReg, imm: UInt): R

    // Load operations
    fun loadU8(offset: UInt, argsLength: UInt, reg: RawReg, imm: UInt): R
    fun loadI8(offset: UInt, argsLength: UInt, reg: RawReg, imm: UInt): R
    fun loadU16(offset: UInt, argsLength: UInt, reg: RawReg, imm: UInt): R
    fun loadI16(offset: UInt, argsLength: UInt, reg: RawReg, imm: UInt): R
    fun loadU32(offset: UInt, argsLength: UInt, reg: RawReg, imm: UInt): R
    fun loadI32(offset: UInt, argsLength: UInt, reg: RawReg, imm: UInt): R
    fun loadU64(offset: UInt, argsLength: UInt, reg: RawReg, imm: UInt): R

    // Store operations
    fun storeU8(offset: UInt, argsLength: UInt, reg: RawReg, imm: UInt): R
    fun storeU16(offset: UInt, argsLength: UInt, reg: RawReg, imm: UInt): R
    fun storeU32(offset: UInt, argsLength: UInt, reg: RawReg, imm: UInt): R
    fun storeU64(offset: UInt, argsLength: UInt, reg: RawReg, imm: UInt): R

    // Branch operations with Imm
    fun loadImmAndJump(offset: UInt, argsLength: UInt, reg: RawReg, imm1: UInt, imm2: UInt): R
    fun branchEqImm(offset: UInt, argsLength: UInt, reg: RawReg, imm1: UInt, imm2: UInt): R
    fun branchNotEqImm(offset: UInt, argsLength: UInt, reg: RawReg, imm1: UInt, imm2: UInt): R
    fun branchLessUnsignedImm(offset: UInt, argsLength: UInt, reg: RawReg, imm1: UInt, imm2: UInt): R
    fun branchLessSignedImm(offset: UInt, argsLength: UInt, reg: RawReg, imm1: UInt, imm2: UInt): R
    fun branchGreaterOrEqualUnsignedImm(offset: UInt, argsLength: UInt, reg: RawReg, imm1: UInt, imm2: UInt): R
    fun branchGreaterOrEqualSignedImm(offset: UInt, argsLength: UInt, reg: RawReg, imm1: UInt, imm2: UInt): R
    fun branchLessOrEqualSignedImm(offset: UInt, argsLength: UInt, reg: RawReg, imm1: UInt, imm2: UInt): R
    fun branchLessOrEqualUnsignedImm(offset: UInt, argsLength: UInt, reg: RawReg, imm1: UInt, imm2: UInt): R
    fun branchGreaterSignedImm(offset: UInt, argsLength: UInt, reg: RawReg, imm1: UInt, imm2: UInt): R
    fun branchGreaterUnsignedImm(offset: UInt, argsLength: UInt, reg: RawReg, imm1: UInt, imm2: UInt): R

    // Store indirect operations
    fun storeImmIndirectU8(offset: UInt, argsLength: UInt, reg: RawReg, imm1: UInt, imm2: UInt): R
    fun storeImmIndirectU16(offset: UInt, argsLength: UInt, reg: RawReg, imm1: UInt, imm2: UInt): R
    fun storeImmIndirectU32(offset: UInt, argsLength: UInt, reg: RawReg, imm1: UInt, imm2: UInt): R
    fun storeImmIndirectU64(offset: UInt, argsLength: UInt, reg: RawReg, imm1: UInt, imm2: UInt): R

    // Store indirect with two registers
    fun storeIndirectU8(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun storeIndirectU16(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun storeIndirectU32(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun storeIndirectU64(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R

    // Load indirect operations
    fun loadIndirectU8(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun loadIndirectI8(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun loadIndirectU16(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun loadIndirectI16(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun loadIndirectI32(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun loadIndirectU32(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun loadIndirectU64(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R

    // Arithmetic operations with immediate
    fun addImm(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun add64Imm(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun andImm(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun and64Imm(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun xorImm(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun xor64Imm(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun orImm(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun or64Imm(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R

    // Multiplication operations with immediate
    fun mulImm(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun mul64Imm(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun mulUpperSignedSignedImm(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun mulUpperSignedSignedImm64(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun mulUpperUnsignedUnsignedImm(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun mulUpperUnsignedUnsignedImm64(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R

    // Comparison operations with immediate
    fun setLessThanUnsignedImm(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun setLessThanUnsigned64Imm(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun setLessThanSignedImm(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun setLessThanSigned64Imm(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R

    // Shift operations with immediate
    fun shiftLogicalLeftImm(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun shiftLogicalLeft64Imm(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun shiftLogicalRightImm(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun shiftLogicalRight64Imm(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun shiftArithmeticRightImm(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun shiftArithmeticRight64Imm(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R

    // Alternative arithmetic operations
    fun negateAndAddImm(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun setGreaterThanUnsignedImm(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun setGreaterThanUnsigned64Imm(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun setGreaterThanSignedImm(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun setGreaterThanSigned64Imm(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R

    // Alternative shift operations
    fun shiftLogicalRightImmAlt(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun shiftLogicalRight64ImmAlt(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun shiftArithmeticRightImmAlt(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun shiftArithmeticRight64ImmAlt(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun shiftLogicalLeftImmAlt(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun shiftLogicalLeft64ImmAlt(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R

    // Conditional move operations
    fun cmovIfZeroImm(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun cmovIfNotZeroImm(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R

    // Branch operations
    fun branchEq(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun branchNotEq(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun branchLessUnsigned(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun branchLessSigned(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun branchGreaterOrEqualUnsigned(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R
    fun branchGreaterOrEqualSigned(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm: UInt): R

    // Three-register arithmetic operations
    fun add(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun add64(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun sub(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun sub64(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun and(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun and64(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun xor(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun xor64(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun or(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun or64(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R

    // Multiplication operations
    fun mul(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun mul64(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun mulUpperSignedSigned(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun mulUpperSignedSigned64(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun mulUpperUnsignedUnsigned(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun mulUpperUnsignedUnsigned64(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun mulUpperSignedUnsigned(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun mulUpperSignedUnsigned64(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R

    // Comparison operations
    fun setLessThanUnsigned(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun setLessThanUnsigned64(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun setLessThanSigned(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun setLessThanSigned64(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R

    // Shift operations
    fun shiftLogicalLeft(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun shiftLogicalLeft64(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun shiftLogicalRight(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun shiftLogicalRight64(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun shiftArithmeticRight(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun shiftArithmeticRight64(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R

    // Division operations
    fun divUnsigned(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun divUnsigned64(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun divSigned(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun divSigned64(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R

    // Remainder operations
    fun remUnsigned(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun remUnsigned64(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun remSigned(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun remSigned64(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R

    // Conditional move operations with three registers
    fun cmovIfZero(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R
    fun cmovIfNotZero(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, reg3: RawReg): R

    // Jump and system call operations
    fun jump(offset: UInt, argsLength: UInt, imm: UInt): R
    fun ecalli(offset: UInt, argsLength: UInt, imm: UInt): R

    // Store immediate operations
    fun storeImmU8(offset: UInt, argsLength: UInt, imm1: UInt, imm2: UInt): R
    fun storeImmU16(offset: UInt, argsLength: UInt, imm1: UInt, imm2: UInt): R
    fun storeImmU32(offset: UInt, argsLength: UInt, imm1: UInt, imm2: UInt): R
    fun storeImmU64(offset: UInt, argsLength: UInt, imm1: UInt, imm2: UInt): R

    // Register move operations
    fun moveReg(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg): R
    fun sbrk(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg): R

    // Load immediate and jump indirect
    fun loadImmAndJumpIndirect(offset: UInt, argsLength: UInt, reg1: RawReg, reg2: RawReg, imm1: UInt, imm2: UInt): R
}
