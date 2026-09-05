mod aarch64;
mod execmem;

use execmem::ExecMem;

#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct RawInstr {
    pub opcode: u32,
    pub dst: u32,
    pub src: u32,
    pub src2: u32,
    pub imm: u64,
}

pub const OP_TRAP: u32 = 0; // basic-block terminator -> PANIC exit
pub const OP_LOAD_IMM64: u32 = 1; // reg[dst] = imm
pub const OP_ADD_IMM64: u32 = 2; // reg[dst] = reg[src] + imm      (wrapping)
pub const OP_ADD: u32 = 3; // reg[dst] = reg[src] + reg[src2]      (wrapping)
pub const OP_SUB: u32 = 4; // reg[dst] = reg[src] - reg[src2]      (wrapping)
pub const OP_MUL: u32 = 5; // reg[dst] = reg[src] * reg[src2]      (wrapping)
pub const OP_LOAD_U64: u32 = 6; // reg[dst] = mem_u64[(reg[src]+imm) & 0xFFFFFFFF]
pub const OP_STORE_U64: u32 = 7; // mem_u64[(reg[src]+imm) & 0xFFFFFFFF] = reg[dst]
pub const OP_JUMP: u32 = 8; // pc = imm (instruction index)
pub const OP_BRANCH_EQ: u32 = 9; // if reg[src] == reg[src2] then pc = imm
pub const OP_BRANCH_NE: u32 = 10; // if reg[src] != reg[src2] then pc = imm
pub const OP_DJUMP: u32 = 11; // indirect: target = reg[src] (see DJUMP_HALT)
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
    /// reg[dst] = mem_u64[ (reg[src] + imm) & 0xFFFFFFFF ]; fault if OOB.
    LoadU64 { dst: u8, src: u8, imm: u64 },
    /// mem_u64[ (reg[src] + imm) & 0xFFFFFFFF ] = reg[dst]; fault if OOB.
    StoreU64 { dst: u8, src: u8, imm: u64 },
    /// Basic-block terminator: end execution with EXIT_PANIC.
    Trap,
    /// Unconditional jump to instruction index `target`.
    Jump { target: u32 },
    /// if reg[src] == reg[src2] then jump to `target`, else fall through.
    BranchEq { src: u8, src2: u8, target: u32 },
    /// if reg[src] != reg[src2] then jump to `target`, else fall through.
    BranchNe { src: u8, src2: u8, target: u32 },
    Djump { src: u8 },
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
            OP_TRAP => ops.push(Op::Trap),
            OP_LOAD_IMM64 => ops.push(Op::LoadImm64 { dst: ins.dst as u8, imm: ins.imm }),
            OP_ADD_IMM64 => ops.push(Op::AddImm64 {
                dst: ins.dst as u8,
                src: ins.src as u8,
                imm: ins.imm,
            }),
            OP_ADD => ops.push(Op::Add { dst: ins.dst as u8, src: ins.src as u8, src2: ins.src2 as u8 }),
            OP_SUB => ops.push(Op::Sub { dst: ins.dst as u8, src: ins.src as u8, src2: ins.src2 as u8 }),
            OP_MUL => ops.push(Op::Mul { dst: ins.dst as u8, src: ins.src as u8, src2: ins.src2 as u8 }),
            OP_LOAD_U64 => ops.push(Op::LoadU64 { dst: ins.dst as u8, src: ins.src as u8, imm: ins.imm }),
            OP_STORE_U64 => ops.push(Op::StoreU64 { dst: ins.dst as u8, src: ins.src as u8, imm: ins.imm }),
            OP_JUMP => ops.push(Op::Jump { target: ins.imm as u32 }),
            OP_BRANCH_EQ => ops.push(Op::BranchEq { src: ins.src as u8, src2: ins.src2 as u8, target: ins.imm as u32 }),
            OP_BRANCH_NE => ops.push(Op::BranchNe { src: ins.src as u8, src2: ins.src2 as u8, target: ins.imm as u32 }),
            OP_DJUMP => ops.push(Op::Djump { src: ins.src as u8 }),
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

    #[test]
    fn arithmetic_block_halts_with_correct_regs_and_gas() {
        let prog = [
            RawInstr { opcode: OP_LOAD_IMM64, dst: 7, src: 0, src2: 0, imm: 100 },
            RawInstr { opcode: OP_ADD_IMM64, dst: 8, src: 7, src2: 0, imm: 5 },
            RawInstr { opcode: OP_ADD, dst: 9, src: 7, src2: 8, imm: 0 },
            RawInstr { opcode: OP_SUB, dst: 10, src: 9, src2: 7, imm: 0 },
            RawInstr { opcode: OP_MUL, dst: 11, src: 8, src2: 7, imm: 0 },
            RawInstr { opcode: OP_TRAP, dst: 0, src: 0, src2: 0, imm: 0 },
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
            RawInstr { opcode: OP_LOAD_IMM64, dst: 0, src: 0, src2: 0, imm: u64::MAX },
            RawInstr { opcode: OP_ADD_IMM64, dst: 1, src: 0, src2: 0, imm: 3 },
            RawInstr { opcode: OP_TRAP, dst: 0, src: 0, src2: 0, imm: 0 },
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
            RawInstr { opcode: OP_LOAD_IMM64, dst: 1, src: 0, src2: 0, imm: 0xDEADBEEF },
            RawInstr { opcode: OP_STORE_U64, dst: 1, src: 0, src2: 0, imm: 8 }, // mem[r0+8]=r1, r0=0
            RawInstr { opcode: OP_LOAD_U64, dst: 2, src: 0, src2: 0, imm: 8 },  // r2=mem[r0+8]
            RawInstr { opcode: OP_TRAP, dst: 0, src: 0, src2: 0, imm: 0 },
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
            RawInstr { opcode: OP_LOAD_U64, dst: 1, src: 0, src2: 0, imm: 40 },
            RawInstr { opcode: OP_TRAP, dst: 0, src: 0, src2: 0, imm: 0 },
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
            RawInstr { opcode: OP_LOAD_U64, dst: 1, src: 0, src2: 0, imm: 24 },
            RawInstr { opcode: OP_TRAP, dst: 0, src: 0, src2: 0, imm: 0 },
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
            RawInstr { opcode: OP_LOAD_IMM64, dst: 1, src: 0, src2: 0, imm: 3 }, // 0  B0
            RawInstr { opcode: OP_LOAD_IMM64, dst: 2, src: 0, src2: 0, imm: 1 }, // 1  B0
            RawInstr { opcode: OP_LOAD_IMM64, dst: 3, src: 0, src2: 0, imm: 0 }, // 2  B0
            RawInstr { opcode: OP_SUB, dst: 1, src: 1, src2: 2, imm: 0 },        // 3  B1
            RawInstr { opcode: OP_BRANCH_NE, dst: 0, src: 1, src2: 3, imm: 3 },  // 4  B1 -> 3
            RawInstr { opcode: OP_TRAP, dst: 0, src: 0, src2: 0, imm: 0 },       // 5  B2
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
        // Same loop, gas only enough for B0 + one B1 pass.
        let prog = [
            RawInstr { opcode: OP_LOAD_IMM64, dst: 1, src: 0, src2: 0, imm: 3 },
            RawInstr { opcode: OP_LOAD_IMM64, dst: 2, src: 0, src2: 0, imm: 1 },
            RawInstr { opcode: OP_LOAD_IMM64, dst: 3, src: 0, src2: 0, imm: 0 },
            RawInstr { opcode: OP_SUB, dst: 1, src: 1, src2: 2, imm: 0 },
            RawInstr { opcode: OP_BRANCH_NE, dst: 0, src: 1, src2: 3, imm: 3 },
            RawInstr { opcode: OP_TRAP, dst: 0, src: 0, src2: 0, imm: 0 },
        ];
        let mut regs = [0u64; 13];
        let mut gas = 6i64; // B0(3) + B1(2) = 5 ok; next B1 entry 1-2<0 -> OOG
        let exit = run(&prog, &mut regs, &mut gas);
        assert_eq!(exit, EXIT_OOG);
        assert_eq!(regs[1], 2); // one decrement applied, then frozen
        assert_eq!(gas, -1);
    }

    #[test]
    fn unconditional_jump_skips() {
        // r1=5; jump over the overwrite; trap. r1 must stay 5.
        let prog = [
            RawInstr { opcode: OP_LOAD_IMM64, dst: 1, src: 0, src2: 0, imm: 5 }, // 0
            RawInstr { opcode: OP_JUMP, dst: 0, src: 0, src2: 0, imm: 3 },       // 1 -> 3
            RawInstr { opcode: OP_LOAD_IMM64, dst: 1, src: 0, src2: 0, imm: 99 },// 2 (skipped)
            RawInstr { opcode: OP_TRAP, dst: 0, src: 0, src2: 0, imm: 0 },       // 3
        ];
        let mut regs = [0u64; 13];
        let mut gas = 100i64;
        let exit = run(&prog, &mut regs, &mut gas);
        assert_eq!(exit, EXIT_PANIC);
        assert_eq!(regs[1], 5);
    }

    #[test]
    fn djump_to_valid_target_jumps() {
        // r1 = 3 (a valid jump-table target index); djump r1; ... ; block at 3
        // sets r2 = 7; trap. Jump table = {3}.
        let prog = [
            RawInstr { opcode: OP_LOAD_IMM64, dst: 1, src: 0, src2: 0, imm: 3 }, // 0
            RawInstr { opcode: OP_DJUMP, dst: 0, src: 1, src2: 0, imm: 0 },      // 1 -> reg[1]=3
            RawInstr { opcode: OP_LOAD_IMM64, dst: 2, src: 0, src2: 0, imm: 99 },// 2 (skipped)
            RawInstr { opcode: OP_LOAD_IMM64, dst: 2, src: 0, src2: 0, imm: 7 }, // 3 (target)
            RawInstr { opcode: OP_TRAP, dst: 0, src: 0, src2: 0, imm: 0 },       // 4
        ];
        let mut regs = [0u64; 13];
        let mut gas = 100i64;
        let exit = run_full(&prog, &mut regs, &mut gas, &mut [], &[3]);
        assert_eq!(exit, EXIT_PANIC); // ends at the trap
        assert_eq!(regs[2], 7);
    }

    #[test]
    fn djump_to_sentinel_halts() {
        let prog = [
            RawInstr { opcode: OP_LOAD_IMM64, dst: 1, src: 0, src2: 0, imm: DJUMP_HALT },
            RawInstr { opcode: OP_DJUMP, dst: 0, src: 1, src2: 0, imm: 0 },
            RawInstr { opcode: OP_TRAP, dst: 0, src: 0, src2: 0, imm: 0 },
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
            RawInstr { opcode: OP_LOAD_IMM64, dst: 1, src: 0, src2: 0, imm: 2 },
            RawInstr { opcode: OP_DJUMP, dst: 0, src: 1, src2: 0, imm: 0 },
            RawInstr { opcode: OP_TRAP, dst: 0, src: 0, src2: 0, imm: 0 },
        ];
        let mut regs = [0u64; 13];
        let mut gas = 100i64;
        let exit = run_full(&prog, &mut regs, &mut gas, &mut [], &[3]);
        assert_eq!(exit, EXIT_PANIC);
    }

    #[test]
    fn out_of_gas_leaves_regs_untouched() {
        let prog = [
            RawInstr { opcode: OP_LOAD_IMM64, dst: 5, src: 0, src2: 0, imm: 999 },
            RawInstr { opcode: OP_TRAP, dst: 0, src: 0, src2: 0, imm: 0 },
        ];
        let mut regs = [7u64; 13];
        let mut gas = 1i64;
        let exit = run(&prog, &mut regs, &mut gas);
        assert_eq!(exit, EXIT_OOG);
        assert_eq!(regs[5], 7);
        assert_eq!(gas, -1);
    }
}
