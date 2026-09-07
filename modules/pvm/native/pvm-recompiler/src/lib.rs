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
    pub pc: u32,
    pub imm: i64,
    pub imm2: i64,
}

#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct Region {
    pub base: u32,
    pub len: u32,
    pub buf_offset: u32,
    /// 1 = ReadWrite, 0 = ReadOnly. Loads succeed against either; stores
    /// require this to be 1 (matches `PageAccess.isWritable`, PageMap.scala).
    pub writable: u32,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, Default)]
pub struct ExecOut {
    pub pc: u32,
    pub fault_page: u32,
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

/// The recompiled block: owns its executable memory
pub struct CompiledBlock {
    mem: ExecMem,
    instruction_count: u32,
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
    fn emit_program(&self, ops: &[Op], pcs: &[u32], jump_table: &[u32], code_len: u32) -> (Vec<u8>, u32);
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
    code_len: u32,
) -> *mut CompiledBlock {
    if instrs.is_null() {
        return std::ptr::null_mut();
    }
    let slice = std::slice::from_raw_parts(instrs, n);
    let ops = match decode(slice) {
        Some(x) => x,
        None => return std::ptr::null_mut(),
    };
    let pcs: Vec<u32> = slice.iter().map(|i| i.pc).collect();
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
    let (code, instruction_count) = backend.emit_program(&ops, &pcs, &jt, code_len);
    let mem = match ExecMem::from_code(&code) {
        Some(m) => m,
        None => return std::ptr::null_mut(),
    };
    Box::into_raw(Box::new(CompiledBlock { mem, instruction_count }))
}

/// Execute a compiled block over the caller's register file, gas cell, and
/// permission-aware guest memory
///
/// `regs` points to 13 little-endian u64 PVM registers (read and written in
/// place). `gas` points to a single i64 the block decrements by its cost.
/// `regions`/`n_regions` describe the guest memory map (see [`Region`]);
///
/// # Safety
/// `block` must be a live pointer from `pvm_compile`; `regs` must point to 13
/// u64s; `gas` to one i64; `regions` to `n_regions` valid `Region`s; `backing`
/// to `backing_len` bytes (every region's `[buf_offset, buf_offset+len)` must
/// lie within it); `out` to one `ExecOut`.
#[no_mangle]
pub unsafe extern "C" fn pvm_execute(
    block: *mut CompiledBlock,
    regs: *mut u64,
    gas: *mut i64,
    regions: *const Region,
    n_regions: u64,
    backing: *mut u8,
    page_shift: u32,
    entry_index: u32,
    out: *mut ExecOut,
) -> u32 {
    if block.is_null() || regs.is_null() || gas.is_null() || out.is_null() {
        return EXIT_PANIC;
    }
    let block = &*block;
    if entry_index >= block.instruction_count {
        *out = ExecOut { pc: 0, fault_page: 0 };
        return EXIT_PANIC;
    }
    let base = block.mem.as_ptr();
    let f: extern "C" fn(*mut u64, *mut i64, *const Region, u64, *mut u8, u32, *mut ExecOut, u32) -> u32 =
        std::mem::transmute(base);
    f(regs, gas, regions, n_regions, backing, page_shift, out, entry_index)
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

    fn run(instrs: &[RawInstr], regs: &mut [u64; 13], gas: &mut i64) -> (u32, ExecOut) {
        run_full(instrs, regs, gas, &[], &mut [], &[], 0)
    }

    fn run_mem(
        instrs: &[RawInstr],
        regs: &mut [u64; 13],
        gas: &mut i64,
        regions: &[Region],
        backing: &mut [u8],
    ) -> (u32, ExecOut) {
        run_full(instrs, regs, gas, regions, backing, &[], 0)
    }

    fn run_full(
        instrs: &[RawInstr],
        regs: &mut [u64; 13],
        gas: &mut i64,
        regions: &[Region],
        backing: &mut [u8],
        jt: &[u32],
        entry_index: u32,
    ) -> (u32, ExecOut) {
        let code_len = (instrs.len() as u32) * 4;
        run_full_with_code_len(instrs, regs, gas, regions, backing, jt, entry_index, code_len)
    }

    fn run_full_with_code_len(
        instrs: &[RawInstr],
        regs: &mut [u64; 13],
        gas: &mut i64,
        regions: &[Region],
        backing: &mut [u8],
        jt: &[u32],
        entry_index: u32,
        code_len: u32,
    ) -> (u32, ExecOut) {
        unsafe {
            let blk = pvm_compile(instrs.as_ptr(), instrs.len(), jt.as_ptr(), jt.len(), code_len);
            assert!(!blk.is_null(), "compile returned null");
            let mut out = ExecOut::default();
            let ex = pvm_execute(
                blk,
                regs.as_mut_ptr(),
                gas as *mut i64,
                regions.as_ptr(),
                regions.len() as u64,
                backing.as_mut_ptr(),
                12, // page_shift: 4096-byte pages in these tests
                entry_index,
                &mut out as *mut ExecOut,
            );
            pvm_free(blk);
            (ex, out)
        }
    }

    fn ri(opcode: u32, a: u32, b: u32, c: u32, imm: i64) -> RawInstr {
        RawInstr { opcode, a, b, c, pc: 0, imm, imm2: 0 }
    }

    fn with_pcs(mut prog: Vec<RawInstr>) -> Vec<RawInstr> {
        for (i, ins) in prog.iter_mut().enumerate() {
            ins.pc = (i as u32) * 4;
        }
        prog
    }

    /// A single RW region covering `[0, len)` backed 1:1 by `backing`.
    fn rw_region(len: u32) -> Region {
        Region { base: 0, len, buf_offset: 0, writable: 1 }
    }

    #[test]
    fn arithmetic_block_halts_with_correct_regs_and_gas() {
        let prog = with_pcs(vec![
            ri(OP_LOAD_IMM64, 7, 0, 0, 100),
            ri(OP_ADD_IMM64, 8, 7, 0, 5),
            ri(OP_ADD64, 9, 7, 8, 0),
            ri(OP_SUB64, 10, 9, 7, 0),
            ri(OP_MUL64, 11, 8, 7, 0),
            ri(OP_PANIC, 0, 0, 0, 0),
        ]);
        let mut regs = [0u64; 13];
        let mut gas = 1000i64;
        let (exit, out) = run(&prog, &mut regs, &mut gas);
        assert_eq!(exit, EXIT_PANIC);
        assert_eq!(regs[7], 100);
        assert_eq!(regs[8], 105);
        assert_eq!(regs[9], 205);
        assert_eq!(regs[10], 105);
        assert_eq!(regs[11], 105 * 100);
        assert_eq!(gas, 1000 - 6);
        // Final pc = the panicking instruction's own pc (interpreter-semantics.md
        // "Panic: PC = the panicking instruction's own pc").
        assert_eq!(out.pc, 5 * 4);
    }

    #[test]
    fn wrapping_is_64bit() {
        let prog = with_pcs(vec![
            ri(OP_LOAD_IMM64, 0, 0, 0, u64::MAX as i64),
            ri(OP_ADD_IMM64, 1, 0, 0, 3),
            ri(OP_PANIC, 0, 0, 0, 0),
        ]);
        let mut regs = [0u64; 13];
        let mut gas = 100i64;
        run(&prog, &mut regs, &mut gas);
        assert_eq!(regs[1], 2);
    }

    #[test]
    fn load_store_roundtrip_within_bounds() {
        // r1 = 0xDEADBEEF; store r1 at mem[8]; load mem[8] into r2; trap
        let prog = with_pcs(vec![
            ri(OP_LOAD_IMM64, 1, 0, 0, 0xDEADBEEFu32 as i64),
            ri(OP_STORE_INDIRECT_U64, 1, 0, 0, 8), // mem[r0+8]=r1, r0=0
            ri(OP_LOAD_INDIRECT_U64, 2, 0, 0, 8),  // r2=mem[r0+8]
            ri(OP_PANIC, 0, 0, 0, 0),
        ]);
        let mut regs = [0u64; 13];
        let mut gas = 100i64;
        let mut mem = [0u8; 32];
        let regions = [rw_region(32)];
        let (exit, out) = run_mem(&prog, &mut regs, &mut gas, &regions, &mut mem);
        assert_eq!(exit, EXIT_PANIC);
        assert_eq!(out.pc, 3 * 4);
        assert_eq!(regs[2], 0xDEADBEEF);
        // little-endian bytes at mem[8..16]
        assert_eq!(&mem[8..12], &[0xEF, 0xBE, 0xAD, 0xDE]);
    }

    #[test]
    fn out_of_bounds_load_faults() {
        let prog = with_pcs(vec![
            ri(OP_LOAD_INDIRECT_U64, 1, 0, 0, 40),
            ri(OP_PANIC, 0, 0, 0, 0),
        ]);
        let mut regs = [0u64; 13];
        regs[0] = 0x10000;
        let mut gas = 100i64;
        let mut mem = [0u8; 32];
        let regions = [Region { base: 0x10000, len: 32, buf_offset: 0, writable: 1 }];
        let (exit, out) = run_mem(&prog, &mut regs, &mut gas, &regions, &mut mem);
        assert_eq!(exit, EXIT_FAULT);
        assert_eq!(out.pc, 0);
        assert_eq!(out.fault_page, 0x10000);
    }

    #[test]
    fn boundary_load_last_valid_qword_ok() {
        // 32-byte region: offset 24 loads bytes [24,32) — the last valid qword.
        let prog = with_pcs(vec![
            ri(OP_LOAD_INDIRECT_U64, 1, 0, 0, 24),
            ri(OP_PANIC, 0, 0, 0, 0),
        ]);
        let mut regs = [0u64; 13];
        regs[0] = 0x10000;
        let mut gas = 100i64;
        let mut mem = [0u8; 32];
        mem[24] = 0x7f;
        let regions = [Region { base: 0x10000, len: 32, buf_offset: 0, writable: 1 }];
        let (exit, out) = run_mem(&prog, &mut regs, &mut gas, &regions, &mut mem);
        assert_eq!(exit, EXIT_PANIC);
        assert_eq!(out.pc, 4);
        assert_eq!(regs[1], 0x7f);
    }

    #[test]
    fn countdown_loop_runs_to_trap() {
        // r1=3; r2=1; r3=0; loop: r1-=r2; if r1!=r3 goto loop; trap
        let prog = with_pcs(vec![
            ri(OP_LOAD_IMM64, 1, 0, 0, 3), // 0  B0
            ri(OP_LOAD_IMM64, 2, 0, 0, 1), // 1  B0
            ri(OP_LOAD_IMM64, 3, 0, 0, 0), // 2  B0
            ri(OP_SUB64, 1, 1, 2, 0),      // 3  B1
            ri(OP_BRANCH_NE, 1, 3, 0, 3),  // 4  B1 -> 3
            ri(OP_PANIC, 0, 0, 0, 0),      // 5  B2
        ]);
        let mut regs = [0u64; 13];
        let mut gas = 100i64;
        let (exit, out) = run(&prog, &mut regs, &mut gas);
        assert_eq!(exit, EXIT_PANIC);
        assert_eq!(regs[1], 0);
        // B0 cost 3, B1 cost 2 run 3x, B2 cost 1 => 3 + 6 + 1 = 10
        assert_eq!(gas, 90);
        assert_eq!(out.pc, 5 * 4);
    }

    #[test]
    fn oog_mid_loop_freezes_state() {
        // Per-instruction gas (matches interpreter): charge 1 before each instr,
        // OOG before executing when gas<0, registers frozen at the prior instr.
        let prog = with_pcs(vec![
            ri(OP_LOAD_IMM64, 1, 0, 0, 3),
            ri(OP_LOAD_IMM64, 2, 0, 0, 1),
            ri(OP_LOAD_IMM64, 3, 0, 0, 0),
            ri(OP_SUB64, 1, 1, 2, 0),
            ri(OP_BRANCH_NE, 1, 3, 0, 3),
            ri(OP_PANIC, 0, 0, 0, 0),
        ]);
        let mut regs = [0u64; 13];
        let mut gas = 6i64;
        // charges: LOAD,LOAD,LOAD,SUB(r1=2),BRANCH(taken),SUB(r1=1),BRANCH -> OOG
        let (exit, out) = run(&prog, &mut regs, &mut gas);
        assert_eq!(exit, EXIT_OOG);
        assert_eq!(regs[1], 1); // two SUBs applied; frozen before the 2nd branch
        assert_eq!(gas, -1);
        // OOG PC = the instruction whose charge tipped gas < 0: the 2nd BRANCH_NE
        // at instruction index 4 (byte offset 16), NOT the target it never took.
        assert_eq!(out.pc, 4 * 4);
    }

    #[test]
    fn unconditional_jump_skips() {
        // r1=5; jump over the overwrite; trap. r1 must stay 5.
        let prog = with_pcs(vec![
            ri(OP_LOAD_IMM64, 1, 0, 0, 5), // 0
            ri(OP_JUMP, 0, 0, 0, 3),       // 1 -> 3
            ri(OP_LOAD_IMM64, 1, 0, 0, 99),// 2 (skipped)
            ri(OP_PANIC, 0, 0, 0, 0),      // 3
        ]);
        let mut regs = [0u64; 13];
        let mut gas = 100i64;
        let (exit, out) = run(&prog, &mut regs, &mut gas);
        assert_eq!(exit, EXIT_PANIC);
        assert_eq!(regs[1], 5);
        assert_eq!(out.pc, 3 * 4);
    }

    #[test]
    fn subword_load_store_widths() {
        // Store 0x1122334455667788 as u64; read back narrow (u8/u16/u32) and
        // signed (i8) — all little-endian.
        let prog = with_pcs(vec![
            ri(OP_LOAD_IMM64, 1, 0, 0, 0x1122334455667788u64 as i64),
            ri(OP_STORE_INDIRECT_U64, 1, 0, 0, 0),
            ri(OP_LOAD_INDIRECT_U8, 2, 0, 0, 0),  // 0x88
            ri(OP_LOAD_INDIRECT_U16, 3, 0, 0, 0), // 0x7788
            ri(OP_LOAD_INDIRECT_U32, 4, 0, 0, 0), // 0x55667788
            ri(OP_LOAD_INDIRECT_I8, 5, 0, 0, 0),  // (i8)0x88 = -120
            ri(OP_PANIC, 0, 0, 0, 0),
        ]);
        let mut regs = [0u64; 13];
        let mut gas = 100i64;
        let mut mem = [0u8; 8];
        let regions = [rw_region(8)];
        let (exit, _out) = run_mem(&prog, &mut regs, &mut gas, &regions, &mut mem);
        assert_eq!(exit, EXIT_PANIC);
        assert_eq!(regs[2], 0x88);
        assert_eq!(regs[3], 0x7788);
        assert_eq!(regs[4], 0x5566_7788);
        assert_eq!(regs[5], 0xFFFF_FFFF_FFFF_FF88); // sign-extended -120
    }

    #[test]
    fn narrow_store_truncates() {
        // store_u8 of a wide value writes only the low byte; rest of mem stays 0.
        let prog = with_pcs(vec![
            ri(OP_LOAD_IMM64, 1, 0, 0, 0xAABBCCDDu32 as i64),
            ri(OP_STORE_INDIRECT_U8, 1, 0, 0, 2),
            ri(OP_PANIC, 0, 0, 0, 0),
        ]);
        let mut regs = [0u64; 13];
        let mut gas = 100i64;
        let mut mem = [0u8; 8];
        let regions = [rw_region(8)];
        let (exit, _out) = run_mem(&prog, &mut regs, &mut gas, &regions, &mut mem);
        assert_eq!(exit, EXIT_PANIC);
        assert_eq!(mem[2], 0xDD);
        assert_eq!(&mem[0..2], &[0, 0]);
        assert_eq!(&mem[3..8], &[0, 0, 0, 0, 0]);
    }

    #[test]
    fn subword_oob_faults() {
        // u32 load at offset 6 into an 8-byte region needs [6,10) -> fault.
        let prog = with_pcs(vec![
            ri(OP_LOAD_INDIRECT_U32, 1, 0, 0, 6),
            ri(OP_PANIC, 0, 0, 0, 0),
        ]);
        let mut regs = [0u64; 13];
        regs[0] = 0x10000;
        let mut gas = 100i64;
        let mut mem = [0u8; 8];
        let regions = [Region { base: 0x10000, len: 8, buf_offset: 0, writable: 1 }];
        let (exit, _out) = run_mem(&prog, &mut regs, &mut gas, &regions, &mut mem);
        assert_eq!(exit, EXIT_FAULT);
    }

    #[test]
    fn djump_to_valid_target_jumps() {
        // r1 = 3 (a valid jump-table target index); JumpIndirect r1+0;
        // ...; block at 3 sets r2 = 7; trap. Jump table = {3}.
        let prog = with_pcs(vec![
            ri(OP_LOAD_IMM64, 1, 0, 0, 3),  // 0
            ri(OP_JUMP_INDIRECT, 1, 0, 0, 0), // 1 -> reg[1]+0 = 3
            ri(OP_LOAD_IMM64, 2, 0, 0, 99), // 2 (skipped)
            ri(OP_LOAD_IMM64, 2, 0, 0, 7),  // 3 (target)
            ri(OP_PANIC, 0, 0, 0, 0),       // 4
        ]);
        let mut regs = [0u64; 13];
        let mut gas = 100i64;
        let (exit, out) = run_full(&prog, &mut regs, &mut gas, &[], &mut [], &[3], 0);
        assert_eq!(exit, EXIT_PANIC); // ends at the trap
        assert_eq!(regs[2], 7);
        assert_eq!(out.pc, 4 * 4);
    }

    #[test]
    fn djump_with_nonzero_offset_jumps() {
        // r1 = 1; JumpIndirect r1+2 -> target index 3 (same target as above,
        // reached via a nonzero imm offset instead of baking it into the reg).
        let prog = with_pcs(vec![
            ri(OP_LOAD_IMM64, 1, 0, 0, 1),  // 0
            ri(OP_JUMP_INDIRECT, 1, 0, 0, 2), // 1 -> reg[1]+2 = 3
            ri(OP_LOAD_IMM64, 2, 0, 0, 99), // 2 (skipped)
            ri(OP_LOAD_IMM64, 2, 0, 0, 7),  // 3 (target)
            ri(OP_PANIC, 0, 0, 0, 0),       // 4
        ]);
        let mut regs = [0u64; 13];
        let mut gas = 100i64;
        let (exit, out) = run_full(&prog, &mut regs, &mut gas, &[], &mut [], &[3], 0);
        assert_eq!(exit, EXIT_PANIC);
        assert_eq!(regs[2], 7);
        assert_eq!(out.pc, 4 * 4);
    }

    #[test]
    fn djump_to_sentinel_halts() {
        let prog = with_pcs(vec![
            ri(OP_LOAD_IMM64, 1, 0, 0, DJUMP_HALT as i64),
            ri(OP_JUMP_INDIRECT, 1, 0, 0, 0),
            ri(OP_PANIC, 0, 0, 0, 0),
        ]);
        let mut regs = [0u64; 13];
        let mut gas = 100i64;
        let (exit, out) = run_full(&prog, &mut regs, &mut gas, &[], &mut [], &[], 0);
        assert_eq!(exit, EXIT_HALT); // clean exit, distinct from trap's PANIC
        // Halt PC = the pc of the djump instruction that hit the sentinel
        // (interpreter-semantics.md "Halt/Finished").
        assert_eq!(out.pc, 1 * 4);
    }

    #[test]
    fn djump_to_untabled_target_panics() {
        // reg holds 2 but the jump table only allows {3} -> panic
        let prog = with_pcs(vec![
            ri(OP_LOAD_IMM64, 1, 0, 0, 2),
            ri(OP_JUMP_INDIRECT, 1, 0, 0, 0),
            ri(OP_PANIC, 0, 0, 0, 0),
        ]);
        let mut regs = [0u64; 13];
        let mut gas = 100i64;
        let (exit, out) = run_full(&prog, &mut regs, &mut gas, &[], &mut [], &[3], 0);
        assert_eq!(exit, EXIT_PANIC);
        // Panic PC = the djump instruction's own pc, never a target.
        assert_eq!(out.pc, 1 * 4);
    }

    #[test]
    fn out_of_gas_before_first_instr_leaves_regs_untouched() {
        let prog = with_pcs(vec![
            ri(OP_LOAD_IMM64, 5, 0, 0, 999),
            ri(OP_PANIC, 0, 0, 0, 0),
        ]);
        let mut regs = [7u64; 13];
        let mut gas = 0i64; // 0-1 < 0 before the first instruction executes
        let (exit, out) = run(&prog, &mut regs, &mut gas);
        assert_eq!(exit, EXIT_OOG);
        assert_eq!(regs[5], 7); // nothing executed
        assert_eq!(gas, -1);
        assert_eq!(out.pc, 0);
    }

    #[test]
    fn out_of_gas_after_partial_execution() {
        let prog = with_pcs(vec![
            ri(OP_LOAD_IMM64, 5, 0, 0, 999),
            ri(OP_PANIC, 0, 0, 0, 0),
        ]);
        let mut regs = [7u64; 13];
        let mut gas = 1i64; // LOAD executes (1->0), trap OOGs (0->-1)
        let (exit, out) = run(&prog, &mut regs, &mut gas);
        assert_eq!(exit, EXIT_OOG);
        assert_eq!(regs[5], 999); // partial execution before OOG
        assert_eq!(gas, -1);
        assert_eq!(out.pc, 1 * 4);
    }

    #[test]
    fn store_to_read_only_region_faults_not_panics() {
        // Region is RO; a store must fault (Segfault), never panic.
        let prog = with_pcs(vec![
            ri(OP_LOAD_IMM64, 1, 0, 0, 42),
            ri(OP_STORE_INDIRECT_U64, 1, 0, 0, 0),
            ri(OP_PANIC, 0, 0, 0, 0),
        ]);
        let mut regs = [0u64; 13];
        let mut gas = 100i64;
        let mut mem = [0u8; 4096];
        let regions = [Region { base: 0x10000, len: 4096, buf_offset: 0, writable: 0 }];
        // r0 must point at the region base for the store's effective address
        // to land inside it: base = 0x10000.
        regs[0] = 0x10000;
        let (exit, out) = run_mem(&prog, &mut regs, &mut gas, &regions, &mut mem);
        assert_eq!(exit, EXIT_FAULT);
        assert_eq!(out.pc, 1 * 4); // the STORE instruction's own pc
        assert_eq!(out.fault_page, 0x10000); // page-aligned base of the RO page
        // Nothing was actually written (mem stays untouched at store's target).
        assert_eq!(&mem[0..8], &[0u8; 8]);
    }

    #[test]
    fn unmapped_gap_between_regions_faults_at_gap_page() {
        // Two regions with a gap: [0x10000,0x11000) and [0x20000,0x21000).
        // A load at 0x18000 (in the gap, page-aligned) must fault at 0x18000.
        let prog = with_pcs(vec![
            ri(OP_LOAD_INDIRECT_U64, 1, 0, 0, 0),
            ri(OP_PANIC, 0, 0, 0, 0),
        ]);
        let mut regs = [0u64; 13];
        regs[0] = 0x18000;
        let mut gas = 100i64;
        let mut mem = [0u8; 8192];
        let regions = [
            Region { base: 0x10000, len: 4096, buf_offset: 0, writable: 1 },
            Region { base: 0x20000, len: 4096, buf_offset: 4096, writable: 1 },
        ];
        let (exit, out) = run_mem(&prog, &mut regs, &mut gas, &regions, &mut mem);
        assert_eq!(exit, EXIT_FAULT);
        assert_eq!(out.pc, 0);
        assert_eq!(out.fault_page, 0x18000);
    }

    #[test]
    fn spanning_access_reports_first_failing_page_low_to_high() {
        let prog = with_pcs(vec![
            ri(OP_LOAD_INDIRECT_U64, 1, 0, 0, 0x0FFC),
            ri(OP_PANIC, 0, 0, 0, 0),
        ]);
        let mut regs = [0u64; 13];
        regs[0] = 0x10000;
        let mut gas = 100i64;
        let mut mem = [0u8; 4096];
        let regions = [Region { base: 0x10000, len: 4096, buf_offset: 0, writable: 1 }];
        let (exit, out) = run_mem(&prog, &mut regs, &mut gas, &regions, &mut mem);
        assert_eq!(exit, EXIT_FAULT);
        assert_eq!(out.fault_page, 0x11000);
    }

    #[test]
    fn fault_page_below_min_valid_address_escalates_to_panic() {
        let prog = with_pcs(vec![
            ri(OP_LOAD_INDIRECT_U64, 1, 0, 0, 0), // r0=0 -> access [0,8) unmapped, page 0
            ri(OP_PANIC, 0, 0, 0, 0),
        ]);
        let mut regs = [0u64; 13];
        regs[0] = 0;
        let mut gas = 100i64;
        let mut mem = [0u8; 4096];
        // Only a region far above address 0 is mapped, so [0,8) is unmapped.
        let regions = [Region { base: 0x10000, len: 4096, buf_offset: 0, writable: 1 }];
        let (exit, out) = run_mem(&prog, &mut regs, &mut gas, &regions, &mut mem);
        assert_eq!(exit, EXIT_PANIC);
        assert_eq!(out.pc, 0); // the load instruction's own pc, per panic rule
    }

    #[test]
    fn entry_index_starts_mid_program() {
        // Program: [0]=LOAD r1=99 (skipped), [1]=LOAD r2=7 (entry), [2]=PANIC.
        // Entering at index 1 must never execute instruction 0.
        let prog = with_pcs(vec![
            ri(OP_LOAD_IMM64, 1, 0, 0, 99),
            ri(OP_LOAD_IMM64, 2, 0, 0, 7),
            ri(OP_PANIC, 0, 0, 0, 0),
        ]);
        let mut regs = [0u64; 13];
        let mut gas = 100i64;
        let (exit, out) = run_full(&prog, &mut regs, &mut gas, &[], &mut [], &[], 1);
        assert_eq!(exit, EXIT_PANIC);
        assert_eq!(regs[1], 0); // instruction 0 never ran
        assert_eq!(regs[2], 7);
        assert_eq!(out.pc, 2 * 4);
        // Gas charged only for the 2 executed instructions (index 1, 2).
        assert_eq!(gas, 98);
    }

    #[test]
    fn entry_index_out_of_range_panics_without_executing() {
        let prog = with_pcs(vec![
            ri(OP_LOAD_IMM64, 5, 0, 0, 999),
            ri(OP_PANIC, 0, 0, 0, 0),
        ]);
        let mut regs = [7u64; 13];
        let mut gas = 100i64;
        let (exit, out) = run_full(&prog, &mut regs, &mut gas, &[], &mut [], &[], 99);
        assert_eq!(exit, EXIT_PANIC);
        assert_eq!(regs[5], 7); // nothing executed
        assert_eq!(gas, 100); // no gas charged either
        assert_eq!(out.pc, 0);
    }

    #[test]
    fn falls_off_end_of_code_with_no_terminator_panics_at_code_len() {
        let prog = with_pcs(vec![ri(OP_ADD64, 9, 7, 8, 0)]);
        let mut regs = [0u64; 13];
        regs[7] = 1;
        regs[8] = 2;
        let mut gas = 10_000i64;
        let code_len = 3u32;
        let (exit, out) = run_full_with_code_len(&prog, &mut regs, &mut gas, &[], &mut [], &[], 0, code_len);
        assert_eq!(exit, EXIT_PANIC);
        assert_eq!(regs[9], 3); // the arithmetic itself still ran correctly
        assert_eq!(out.pc, code_len);
        assert_eq!(gas, 10_000 - 2);
    }

    #[test]
    fn falls_off_end_of_code_when_gas_exhausted_reports_oog_not_panic() {
        let prog = with_pcs(vec![ri(OP_ADD64, 9, 7, 8, 0)]);
        let mut regs = [0u64; 13];
        regs[7] = 1;
        regs[8] = 2;
        let mut gas = 1i64; // Add64 charges 1->0; synthesized Panic charges 0->-1
        let code_len = 3u32;
        let (exit, out) = run_full_with_code_len(&prog, &mut regs, &mut gas, &[], &mut [], &[], 0, code_len);
        assert_eq!(exit, EXIT_OOG);
        assert_eq!(regs[9], 3); // Add64 still executed before the OOG
        assert_eq!(out.pc, code_len);
        assert_eq!(gas, -1);
    }

    #[test]
    fn entry_index_mid_program_falls_off_end_of_code_panics_at_code_len() {
        let prog = with_pcs(vec![
            ri(OP_LOAD_IMM64, 1, 0, 0, 99),
            ri(OP_ADD64, 9, 7, 8, 0),
        ]);
        let mut regs = [0u64; 13];
        regs[7] = 10;
        regs[8] = 20;
        let mut gas = 100i64;
        // code_len independent of the pc=index*4 convention, as above.
        let code_len = 7u32;
        let (exit, out) =
            run_full_with_code_len(&prog, &mut regs, &mut gas, &[], &mut [], &[], 1, code_len);
        assert_eq!(exit, EXIT_PANIC);
        assert_eq!(regs[1], 0); // instruction 0 never ran
        assert_eq!(regs[9], 30); // Add64 (the entry instruction) did run
        assert_eq!(out.pc, code_len);
        // Gas charged only for the 2 dispatched "instructions": the real
        // Add64 at entry, plus the synthesized trailing Panic.
        assert_eq!(gas, 100 - 2);
    }
}
