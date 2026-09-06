mod aarch64;
mod execmem;

use execmem::ExecMem;

#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct RawInstr {
    pub opcode: u32,
    pub a: u32,
    pub b: u32,
    pub c: u32,
    pub imm: i64,
    pub imm2: i64,
}

pub const OP_PANIC: u32 = 0; // basic-block terminator -> PANIC exit
pub const OP_LOAD_IMM64: u32 = 20; // reg[a] = imm
pub const OP_JUMP: u32 = 40; // pc = imm (instruction index)
pub const OP_JUMP_INDIRECT: u32 = 50; // indirect: target = (reg[a]+imm) & 0xFFFFFFFF
pub const OP_STORE_INDIRECT_U8: u32 = 120;
pub const OP_STORE_INDIRECT_U16: u32 = 121;
pub const OP_STORE_INDIRECT_U32: u32 = 122;
pub const OP_STORE_INDIRECT_U64: u32 = 123;
pub const OP_LOAD_INDIRECT_U8: u32 = 124;
pub const OP_LOAD_INDIRECT_I8: u32 = 125;
pub const OP_LOAD_INDIRECT_U16: u32 = 126;
pub const OP_LOAD_INDIRECT_I16: u32 = 127;
pub const OP_LOAD_INDIRECT_U32: u32 = 128;
pub const OP_LOAD_INDIRECT_I32: u32 = 129;
pub const OP_LOAD_INDIRECT_U64: u32 = 130;
pub const OP_ADD_IMM64: u32 = 149; // reg[a] = reg[b] + imm      (wrapping)
pub const OP_BRANCH_EQ: u32 = 170; // if reg[a] == reg[b] then pc = imm
pub const OP_BRANCH_NE: u32 = 171; // if reg[a] != reg[b] then pc = imm
pub const OP_ADD64: u32 = 200; // reg[a] = reg[b] + reg[c]      (wrapping)
pub const OP_SUB64: u32 = 201; // reg[a] = reg[b] - reg[c]      (wrapping)
pub const OP_MUL64: u32 = 202; // reg[a] = reg[b] * reg[c]      (wrapping)

/// Indirect-jump sentinel: `JumpIndirect reg, offset` where `reg[a]+offset`
/// equals this value halts the program cleanly (EXIT_HALT). Mirrors the PVM
/// whole-program return address (2^32 - 2^16).
pub const DJUMP_HALT: u64 = 0xFFFF_0000;

// Exit codes returned by execute. Must match the Scala differential mapping.
pub const EXIT_HALT: u32 = 0; // clean exit (djump to the halt sentinel)
pub const EXIT_PANIC: u32 = 1; // trap, or djump to a non-jump-table target
pub const EXIT_OOG: u32 = 2;
pub const EXIT_FAULT: u32 = 3; // memory access out of the guest region

/// The recompiled block: owns its executable memory. The block's gas cost is
/// baked into the emitted code.
pub struct CompiledBlock {
    mem: ExecMem,
}

/// The abstract op the backend emitter consumes (decoupled from the FFI struct).
#[derive(Clone, Copy, Debug)]
pub enum Op {
    LoadImm64 { dst: u8, imm: u64 },
    AddImm64 { dst: u8, src: u8, imm: u64 },
    Add { dst: u8, src: u8, src2: u8 },
    Sub { dst: u8, src: u8, src2: u8 },
    Mul { dst: u8, src: u8, src2: u8 },
    /// reg[dst] = load `width` bytes at (reg[src]+imm)&0xFFFFFFFF, zero- or
    /// sign-extended per `signed`; fault if [addr, addr+width) is OOB.
    Load { dst: u8, src: u8, imm: u64, width: u8, signed: bool },
    /// store the low `width` bytes of reg[dst] at (reg[src]+imm)&0xFFFFFFFF;
    /// fault if OOB.
    Store { dst: u8, src: u8, imm: u64, width: u8 },
    /// Basic-block terminator: end execution with EXIT_PANIC.
    Trap,
    /// Unconditional jump to instruction index `target`.
    Jump { target: u32 },
    /// if reg[src] == reg[src2] then jump to `target`, else fall through.
    BranchEq { src: u8, src2: u8, target: u32 },
    /// if reg[src] != reg[src2] then jump to `target`, else fall through.
    BranchNe { src: u8, src2: u8, target: u32 },
    Djump { src: u8, imm: u64 },
}

impl Op {
    /// True if this op ends a basic block (control leaves sequentially here).
    pub fn is_terminator(&self) -> bool {
        matches!(
            self,
            Op::Trap | Op::Jump { .. } | Op::BranchEq { .. } | Op::BranchNe { .. } | Op::Djump { .. }
        )
    }
    /// The branch/jump target instruction index, if any.
    pub fn target(&self) -> Option<u32> {
        match self {
            Op::Jump { target } | Op::BranchEq { target, .. } | Op::BranchNe { target, .. } => Some(*target),
            _ => None,
        }
    }
}

/// A code-emitting backend for one host ISA. Registers are addressed as byte
/// offsets into the caller's `[u64; 13]` register file (skeleton ABI).
pub trait Backend {
    /// Emit machine code for a whole program: a sequence of ops decomposed into
    /// basic blocks, with per-block gas charged at each block's entry (returning
    /// `EXIT_OOG` when insufficient), static jumps/branches resolved to code
    /// offsets, `Trap` returning `EXIT_PANIC`, and memory faults `EXIT_FAULT`.
    /// The op index is the instruction "pc" that jumps/branches target.
    /// `jump_table` lists the instruction indices that are valid indirect
    /// (`djump`) targets; every entry is also treated as a block leader.
    fn emit_program(&self, ops: &[Op], jump_table: &[u32]) -> Vec<u8>;
}

/// Decode the FFI instruction array 1:1 into ops (targets are instruction
/// indices). Returns None on an unsupported opcode (caller deopts).
fn decode(instrs: &[RawInstr]) -> Option<Vec<Op>> {
    let mut ops = Vec::with_capacity(instrs.len());
    for ins in instrs {
        match ins.opcode {
            OP_PANIC => ops.push(Op::Trap),
            OP_LOAD_IMM64 => ops.push(Op::LoadImm64 { dst: ins.a as u8, imm: ins.imm as u64 }),
            OP_ADD_IMM64 => ops.push(Op::AddImm64 {
                dst: ins.a as u8,
                src: ins.b as u8,
                imm: ins.imm as u64,
            }),
            OP_ADD64 => ops.push(Op::Add { dst: ins.a as u8, src: ins.b as u8, src2: ins.c as u8 }),
            OP_SUB64 => ops.push(Op::Sub { dst: ins.a as u8, src: ins.b as u8, src2: ins.c as u8 }),
            OP_MUL64 => ops.push(Op::Mul { dst: ins.a as u8, src: ins.b as u8, src2: ins.c as u8 }),
            OP_LOAD_INDIRECT_U64 => ops.push(Op::Load { dst: ins.a as u8, src: ins.b as u8, imm: ins.imm as u64, width: 8, signed: false }),
            OP_LOAD_INDIRECT_U8 => ops.push(Op::Load { dst: ins.a as u8, src: ins.b as u8, imm: ins.imm as u64, width: 1, signed: false }),
            OP_LOAD_INDIRECT_U16 => ops.push(Op::Load { dst: ins.a as u8, src: ins.b as u8, imm: ins.imm as u64, width: 2, signed: false }),
            OP_LOAD_INDIRECT_U32 => ops.push(Op::Load { dst: ins.a as u8, src: ins.b as u8, imm: ins.imm as u64, width: 4, signed: false }),
            OP_LOAD_INDIRECT_I8 => ops.push(Op::Load { dst: ins.a as u8, src: ins.b as u8, imm: ins.imm as u64, width: 1, signed: true }),
            OP_LOAD_INDIRECT_I16 => ops.push(Op::Load { dst: ins.a as u8, src: ins.b as u8, imm: ins.imm as u64, width: 2, signed: true }),
            OP_LOAD_INDIRECT_I32 => ops.push(Op::Load { dst: ins.a as u8, src: ins.b as u8, imm: ins.imm as u64, width: 4, signed: true }),
            OP_STORE_INDIRECT_U64 => ops.push(Op::Store { dst: ins.a as u8, src: ins.b as u8, imm: ins.imm as u64, width: 8 }),
            OP_STORE_INDIRECT_U8 => ops.push(Op::Store { dst: ins.a as u8, src: ins.b as u8, imm: ins.imm as u64, width: 1 }),
            OP_STORE_INDIRECT_U16 => ops.push(Op::Store { dst: ins.a as u8, src: ins.b as u8, imm: ins.imm as u64, width: 2 }),
            OP_STORE_INDIRECT_U32 => ops.push(Op::Store { dst: ins.a as u8, src: ins.b as u8, imm: ins.imm as u64, width: 4 }),
            OP_JUMP => ops.push(Op::Jump { target: ins.imm as u32 }),
            OP_BRANCH_EQ => ops.push(Op::BranchEq { src: ins.a as u8, src2: ins.b as u8, target: ins.imm as u32 }),
            OP_BRANCH_NE => ops.push(Op::BranchNe { src: ins.a as u8, src2: ins.b as u8, target: ins.imm as u32 }),
            OP_JUMP_INDIRECT => ops.push(Op::Djump { src: ins.a as u8, imm: ins.imm as u64 }),
            _ => return None, // unsupported opcode: signal deopt to the caller
        }
    }
    Some(ops)
}

/// Compile a pre-decoded single-basic-block program. Returns a heap-owned
/// `CompiledBlock` pointer, or null if the program contains an unsupported
/// opcode (the caller must then deopt to the interpreter).
///
/// `jump_table`/`jt_n` list the valid indirect-jump (`djump`) target indices.
///
/// # Safety
/// `instrs` must point to `n` valid `RawInstr` values; `jump_table` to `jt_n`
/// u32 values (may be null when `jt_n == 0`).
#[no_mangle]
pub unsafe extern "C" fn pvm_compile(
    instrs: *const RawInstr,
    n: usize,
    jump_table: *const u32,
    jt_n: usize,
) -> *mut CompiledBlock {
    if instrs.is_null() {
        return std::ptr::null_mut();
    }
    let slice = std::slice::from_raw_parts(instrs, n);
    let ops = match decode(slice) {
        Some(x) => x,
        None => return std::ptr::null_mut(),
    };
    let jt: Vec<u32> = if jump_table.is_null() || jt_n == 0 {
        Vec::new()
    } else {
        std::slice::from_raw_parts(jump_table, jt_n)
            .iter()
            .copied()
            .filter(|&t| (t as usize) < n) // keep only in-range targets
            .collect()
    };
    let backend = aarch64::Aarch64Backend;
    let code = backend.emit_program(&ops, &jt);
    let mem = match ExecMem::from_code(&code) {
        Some(m) => m,
        None => return std::ptr::null_mut(),
    };
    Box::into_raw(Box::new(CompiledBlock { mem }))
}

/// Execute a compiled block over the caller's register file, gas cell, and
/// guest memory region.
///
/// `regs` points to 13 little-endian u64 PVM registers (read and written in
/// place). `gas` points to a single i64 the block decrements by its cost.
/// `mem` is the base of a zero-copy guest-memory region of `mem_len` bytes
/// (may be null with mem_len 0 for register-only programs). Returns an EXIT_*
/// code; EXIT_FAULT on an out-of-region access.
///
/// # Safety
/// `block` must be a live pointer from `pvm_compile`; `regs` must point to 13
/// u64s; `gas` to one i64; `mem` to `mem_len` bytes.
#[no_mangle]
pub unsafe extern "C" fn pvm_execute(
    block: *mut CompiledBlock,
    regs: *mut u64,
    gas: *mut i64,
    mem: *mut u8,
    mem_len: u64,
) -> u32 {
    if block.is_null() || regs.is_null() || gas.is_null() {
        return EXIT_PANIC;
    }
    let block = &*block;
    // Emitted code signature:
    //   extern "C" fn(*mut u64 /*x0 regs*/, *mut i64 /*x1 gas*/,
    //                 *mut u8 /*x2 mem*/, u64 /*x3 mem_len*/) -> u32
    let f: extern "C" fn(*mut u64, *mut i64, *mut u8, u64) -> u32 =
        std::mem::transmute(block.mem.as_ptr());
    f(regs, gas, mem, mem_len)
}

/// Free a compiled block (unmaps its executable memory).
///
/// # Safety
/// `block` must be a live pointer from `pvm_compile`, not used afterwards.
#[no_mangle]
pub unsafe extern "C" fn pvm_free(block: *mut CompiledBlock) {
    if !block.is_null() {
        drop(Box::from_raw(block));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run(instrs: &[RawInstr], regs: &mut [u64; 13], gas: &mut i64) -> u32 {
        run_full(instrs, regs, gas, &mut [], &[])
    }

    fn run_mem(instrs: &[RawInstr], regs: &mut [u64; 13], gas: &mut i64, mem: &mut [u8]) -> u32 {
        run_full(instrs, regs, gas, mem, &[])
    }

    fn run_full(
        instrs: &[RawInstr],
        regs: &mut [u64; 13],
        gas: &mut i64,
        mem: &mut [u8],
        jt: &[u32],
    ) -> u32 {
        unsafe {
            let blk = pvm_compile(instrs.as_ptr(), instrs.len(), jt.as_ptr(), jt.len());
            assert!(!blk.is_null(), "compile returned null");
            let ex = pvm_execute(
                blk,
                regs.as_mut_ptr(),
                gas as *mut i64,
                mem.as_mut_ptr(),
                mem.len() as u64,
            );
            pvm_free(blk);
            ex
        }
    }

    fn ri(opcode: u32, a: u32, b: u32, c: u32, imm: i64) -> RawInstr {
        RawInstr { opcode, a, b, c, imm, imm2: 0 }
    }

    #[test]
    fn arithmetic_block_halts_with_correct_regs_and_gas() {
        let prog = [
            ri(OP_LOAD_IMM64, 7, 0, 0, 100),
            ri(OP_ADD_IMM64, 8, 7, 0, 5),
            ri(OP_ADD64, 9, 7, 8, 0),
            ri(OP_SUB64, 10, 9, 7, 0),
            ri(OP_MUL64, 11, 8, 7, 0),
            ri(OP_PANIC, 0, 0, 0, 0),
        ];
        let mut regs = [0u64; 13];
        let mut gas = 1000i64;
        let exit = run(&prog, &mut regs, &mut gas);
        assert_eq!(exit, EXIT_PANIC);
        assert_eq!(regs[7], 100);
        assert_eq!(regs[8], 105);
        assert_eq!(regs[9], 205);
        assert_eq!(regs[10], 105);
        assert_eq!(regs[11], 105 * 100);
        assert_eq!(gas, 1000 - 6);
    }

    #[test]
    fn wrapping_is_64bit() {
        let prog = [
            ri(OP_LOAD_IMM64, 0, 0, 0, u64::MAX as i64),
            ri(OP_ADD_IMM64, 1, 0, 0, 3),
            ri(OP_PANIC, 0, 0, 0, 0),
        ];
        let mut regs = [0u64; 13];
        let mut gas = 100i64;
        run(&prog, &mut regs, &mut gas);
        assert_eq!(regs[1], 2);
    }

    #[test]
    fn load_store_roundtrip_within_bounds() {
        // r1 = 0xDEADBEEF; store r1 at mem[8]; load mem[8] into r2; trap
        let prog = [
            ri(OP_LOAD_IMM64, 1, 0, 0, 0xDEADBEEFu32 as i64),
            ri(OP_STORE_INDIRECT_U64, 1, 0, 0, 8), // mem[r0+8]=r1, r0=0
            ri(OP_LOAD_INDIRECT_U64, 2, 0, 0, 8),  // r2=mem[r0+8]
            ri(OP_PANIC, 0, 0, 0, 0),
        ];
        let mut regs = [0u64; 13];
        let mut gas = 100i64;
        let mut mem = [0u8; 32];
        let exit = run_mem(&prog, &mut regs, &mut gas, &mut mem);
        assert_eq!(exit, EXIT_PANIC);
        assert_eq!(regs[2], 0xDEADBEEF);
        // little-endian bytes at mem[8..16]
        assert_eq!(&mem[8..12], &[0xEF, 0xBE, 0xAD, 0xDE]);
    }

    #[test]
    fn out_of_bounds_load_faults() {
        // load at offset 40 into a 32-byte region -> fault
        let prog = [
            ri(OP_LOAD_INDIRECT_U64, 1, 0, 0, 40),
            ri(OP_PANIC, 0, 0, 0, 0),
        ];
        let mut regs = [0u64; 13];
        let mut gas = 100i64;
        let mut mem = [0u8; 32];
        let exit = run_mem(&prog, &mut regs, &mut gas, &mut mem);
        assert_eq!(exit, EXIT_FAULT);
    }

    #[test]
    fn boundary_load_last_valid_qword_ok() {
        // 32-byte region: offset 24 loads bytes [24,32) — the last valid qword.
        let prog = [
            ri(OP_LOAD_INDIRECT_U64, 1, 0, 0, 24),
            ri(OP_PANIC, 0, 0, 0, 0),
        ];
        let mut regs = [0u64; 13];
        let mut gas = 100i64;
        let mut mem = [0u8; 32];
        mem[24] = 0x7f;
        let exit = run_mem(&prog, &mut regs, &mut gas, &mut mem);
        assert_eq!(exit, EXIT_PANIC);
        assert_eq!(regs[1], 0x7f);
    }

    #[test]
    fn countdown_loop_runs_to_trap() {
        // r1=3; r2=1; r3=0; loop: r1-=r2; if r1!=r3 goto loop; trap
        let prog = [
            ri(OP_LOAD_IMM64, 1, 0, 0, 3), // 0  B0
            ri(OP_LOAD_IMM64, 2, 0, 0, 1), // 1  B0
            ri(OP_LOAD_IMM64, 3, 0, 0, 0), // 2  B0
            ri(OP_SUB64, 1, 1, 2, 0),      // 3  B1
            ri(OP_BRANCH_NE, 1, 3, 0, 3),  // 4  B1 -> 3
            ri(OP_PANIC, 0, 0, 0, 0),      // 5  B2
        ];
        let mut regs = [0u64; 13];
        let mut gas = 100i64;
        let exit = run(&prog, &mut regs, &mut gas);
        assert_eq!(exit, EXIT_PANIC);
        assert_eq!(regs[1], 0);
        // B0 cost 3, B1 cost 2 run 3x, B2 cost 1 => 3 + 6 + 1 = 10
        assert_eq!(gas, 90);
    }

    #[test]
    fn oog_mid_loop_freezes_state() {
        // Per-instruction gas (matches interpreter): charge 1 before each instr,
        // OOG before executing when gas<0, registers frozen at the prior instr.
        let prog = [
            ri(OP_LOAD_IMM64, 1, 0, 0, 3),
            ri(OP_LOAD_IMM64, 2, 0, 0, 1),
            ri(OP_LOAD_IMM64, 3, 0, 0, 0),
            ri(OP_SUB64, 1, 1, 2, 0),
            ri(OP_BRANCH_NE, 1, 3, 0, 3),
            ri(OP_PANIC, 0, 0, 0, 0),
        ];
        let mut regs = [0u64; 13];
        let mut gas = 6i64;
        // charges: LOAD,LOAD,LOAD,SUB(r1=2),BRANCH(taken),SUB(r1=1),BRANCH -> OOG
        let exit = run(&prog, &mut regs, &mut gas);
        assert_eq!(exit, EXIT_OOG);
        assert_eq!(regs[1], 1); // two SUBs applied; frozen before the 2nd branch
        assert_eq!(gas, -1);
    }

    #[test]
    fn unconditional_jump_skips() {
        // r1=5; jump over the overwrite; trap. r1 must stay 5.
        let prog = [
            ri(OP_LOAD_IMM64, 1, 0, 0, 5), // 0
            ri(OP_JUMP, 0, 0, 0, 3),       // 1 -> 3
            ri(OP_LOAD_IMM64, 1, 0, 0, 99),// 2 (skipped)
            ri(OP_PANIC, 0, 0, 0, 0),      // 3
        ];
        let mut regs = [0u64; 13];
        let mut gas = 100i64;
        let exit = run(&prog, &mut regs, &mut gas);
        assert_eq!(exit, EXIT_PANIC);
        assert_eq!(regs[1], 5);
    }

    #[test]
    fn subword_load_store_widths() {
        // Store 0x1122334455667788 as u64; read back narrow (u8/u16/u32) and
        // signed (i8) — all little-endian.
        let prog = [
            ri(OP_LOAD_IMM64, 1, 0, 0, 0x1122334455667788u64 as i64),
            ri(OP_STORE_INDIRECT_U64, 1, 0, 0, 0),
            ri(OP_LOAD_INDIRECT_U8, 2, 0, 0, 0),  // 0x88
            ri(OP_LOAD_INDIRECT_U16, 3, 0, 0, 0), // 0x7788
            ri(OP_LOAD_INDIRECT_U32, 4, 0, 0, 0), // 0x55667788
            ri(OP_LOAD_INDIRECT_I8, 5, 0, 0, 0),  // (i8)0x88 = -120
            ri(OP_PANIC, 0, 0, 0, 0),
        ];
        let mut regs = [0u64; 13];
        let mut gas = 100i64;
        let mut mem = [0u8; 8];
        let exit = run_mem(&prog, &mut regs, &mut gas, &mut mem);
        assert_eq!(exit, EXIT_PANIC);
        assert_eq!(regs[2], 0x88);
        assert_eq!(regs[3], 0x7788);
        assert_eq!(regs[4], 0x5566_7788);
        assert_eq!(regs[5], 0xFFFF_FFFF_FFFF_FF88); // sign-extended -120
    }

    #[test]
    fn narrow_store_truncates() {
        // store_u8 of a wide value writes only the low byte; rest of mem stays 0.
        let prog = [
            ri(OP_LOAD_IMM64, 1, 0, 0, 0xAABBCCDDu32 as i64),
            ri(OP_STORE_INDIRECT_U8, 1, 0, 0, 2),
            ri(OP_PANIC, 0, 0, 0, 0),
        ];
        let mut regs = [0u64; 13];
        let mut gas = 100i64;
        let mut mem = [0u8; 8];
        let exit = run_mem(&prog, &mut regs, &mut gas, &mut mem);
        assert_eq!(exit, EXIT_PANIC);
        assert_eq!(mem[2], 0xDD);
        assert_eq!(&mem[0..2], &[0, 0]);
        assert_eq!(&mem[3..8], &[0, 0, 0, 0, 0]);
    }

    #[test]
    fn subword_oob_faults() {
        // u32 load at offset 6 into an 8-byte region needs [6,10) -> fault.
        let prog = [
            ri(OP_LOAD_INDIRECT_U32, 1, 0, 0, 6),
            ri(OP_PANIC, 0, 0, 0, 0),
        ];
        let mut regs = [0u64; 13];
        let mut gas = 100i64;
        let mut mem = [0u8; 8];
        assert_eq!(run_mem(&prog, &mut regs, &mut gas, &mut mem), EXIT_FAULT);
    }

    #[test]
    fn djump_to_valid_target_jumps() {
        // r1 = 3 (a valid jump-table target index); JumpIndirect r1+0;
        // ...; block at 3 sets r2 = 7; trap. Jump table = {3}.
        let prog = [
            ri(OP_LOAD_IMM64, 1, 0, 0, 3),  // 0
            ri(OP_JUMP_INDIRECT, 1, 0, 0, 0), // 1 -> reg[1]+0 = 3
            ri(OP_LOAD_IMM64, 2, 0, 0, 99), // 2 (skipped)
            ri(OP_LOAD_IMM64, 2, 0, 0, 7),  // 3 (target)
            ri(OP_PANIC, 0, 0, 0, 0),       // 4
        ];
        let mut regs = [0u64; 13];
        let mut gas = 100i64;
        let exit = run_full(&prog, &mut regs, &mut gas, &mut [], &[3]);
        assert_eq!(exit, EXIT_PANIC); // ends at the trap
        assert_eq!(regs[2], 7);
    }

    #[test]
    fn djump_with_nonzero_offset_jumps() {
        // r1 = 1; JumpIndirect r1+2 -> target index 3 (same target as above,
        // reached via a nonzero imm offset instead of baking it into the reg).
        let prog = [
            ri(OP_LOAD_IMM64, 1, 0, 0, 1),  // 0
            ri(OP_JUMP_INDIRECT, 1, 0, 0, 2), // 1 -> reg[1]+2 = 3
            ri(OP_LOAD_IMM64, 2, 0, 0, 99), // 2 (skipped)
            ri(OP_LOAD_IMM64, 2, 0, 0, 7),  // 3 (target)
            ri(OP_PANIC, 0, 0, 0, 0),       // 4
        ];
        let mut regs = [0u64; 13];
        let mut gas = 100i64;
        let exit = run_full(&prog, &mut regs, &mut gas, &mut [], &[3]);
        assert_eq!(exit, EXIT_PANIC);
        assert_eq!(regs[2], 7);
    }

    #[test]
    fn djump_to_sentinel_halts() {
        let prog = [
            ri(OP_LOAD_IMM64, 1, 0, 0, DJUMP_HALT as i64),
            ri(OP_JUMP_INDIRECT, 1, 0, 0, 0),
            ri(OP_PANIC, 0, 0, 0, 0),
        ];
        let mut regs = [0u64; 13];
        let mut gas = 100i64;
        let exit = run_full(&prog, &mut regs, &mut gas, &mut [], &[]);
        assert_eq!(exit, EXIT_HALT); // clean exit, distinct from trap's PANIC
    }

    #[test]
    fn djump_to_untabled_target_panics() {
        // reg holds 2 but the jump table only allows {3} -> panic
        let prog = [
            ri(OP_LOAD_IMM64, 1, 0, 0, 2),
            ri(OP_JUMP_INDIRECT, 1, 0, 0, 0),
            ri(OP_PANIC, 0, 0, 0, 0),
        ];
        let mut regs = [0u64; 13];
        let mut gas = 100i64;
        let exit = run_full(&prog, &mut regs, &mut gas, &mut [], &[3]);
        assert_eq!(exit, EXIT_PANIC);
    }

    #[test]
    fn out_of_gas_before_first_instr_leaves_regs_untouched() {
        let prog = [
            ri(OP_LOAD_IMM64, 5, 0, 0, 999),
            ri(OP_PANIC, 0, 0, 0, 0),
        ];
        let mut regs = [7u64; 13];
        let mut gas = 0i64; // 0-1 < 0 before the first instruction executes
        let exit = run(&prog, &mut regs, &mut gas);
        assert_eq!(exit, EXIT_OOG);
        assert_eq!(regs[5], 7); // nothing executed
        assert_eq!(gas, -1);
    }

    #[test]
    fn out_of_gas_after_partial_execution() {
        let prog = [
            ri(OP_LOAD_IMM64, 5, 0, 0, 999),
            ri(OP_PANIC, 0, 0, 0, 0),
        ];
        let mut regs = [7u64; 13];
        let mut gas = 1i64; // LOAD executes (1->0), trap OOGs (0->-1)
        let exit = run(&prog, &mut regs, &mut gas);
        assert_eq!(exit, EXIT_OOG);
        assert_eq!(regs[5], 999); // partial execution before OOG
        assert_eq!(gas, -1);
    }
}
