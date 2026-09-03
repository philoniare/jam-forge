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

// Exit codes returned by execute. Must match the Scala differential mapping.
pub const EXIT_HALT: u32 = 0;
pub const EXIT_PANIC: u32 = 1;
pub const EXIT_OOG: u32 = 2;

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
}

/// A code-emitting backend for one host ISA. Registers are addressed as byte
/// offsets into the caller's `[u64; 13]` register file (skeleton ABI).
pub trait Backend {
    /// Emit machine code for `ops` as one basic block terminated by a trap that
    /// returns `EXIT_PANIC`. `gas_cost` is subtracted from the caller's gas cell
    /// at block entry; if the result is negative the block returns `EXIT_OOG`
    /// without touching the register file. Returns the raw machine-code bytes.
    fn emit_block(&self, ops: &[Op], gas_cost: i64) -> Vec<u8>;
}

fn decode(instrs: &[RawInstr]) -> Option<(Vec<Op>, i64)> {
    let mut ops = Vec::with_capacity(instrs.len());
    for ins in instrs {
        match ins.opcode {
            OP_TRAP => break, // terminator; stop collecting body ops
            OP_LOAD_IMM64 => ops.push(Op::LoadImm64 { dst: ins.dst as u8, imm: ins.imm }),
            OP_ADD_IMM64 => ops.push(Op::AddImm64 {
                dst: ins.dst as u8,
                src: ins.src as u8,
                imm: ins.imm,
            }),
            OP_ADD => ops.push(Op::Add { dst: ins.dst as u8, src: ins.src as u8, src2: ins.src2 as u8 }),
            OP_SUB => ops.push(Op::Sub { dst: ins.dst as u8, src: ins.src as u8, src2: ins.src2 as u8 }),
            OP_MUL => ops.push(Op::Mul { dst: ins.dst as u8, src: ins.src as u8, src2: ins.src2 as u8 }),
            _ => return None, // unsupported opcode: signal deopt to the caller
        }
    }
    // Block cost = every instruction INCLUDING the terminator, matching the
    // interpreter which charges 1 gas per compiled instruction in the block.
    let gas_cost = instrs.len() as i64;
    Some((ops, gas_cost))
}

/// Compile a pre-decoded single-basic-block program. Returns a heap-owned
/// `CompiledBlock` pointer, or null if the program contains an unsupported
/// opcode (the caller must then deopt to the interpreter).
///
/// # Safety
/// `instrs` must point to `n` valid `RawInstr` values.
#[no_mangle]
pub unsafe extern "C" fn pvm_compile(instrs: *const RawInstr, n: usize) -> *mut CompiledBlock {
    if instrs.is_null() {
        return std::ptr::null_mut();
    }
    let slice = std::slice::from_raw_parts(instrs, n);
    let (ops, gas_cost) = match decode(slice) {
        Some(x) => x,
        None => return std::ptr::null_mut(),
    };
    let backend = aarch64::Aarch64Backend;
    let code = backend.emit_block(&ops, gas_cost);
    let mem = match ExecMem::from_code(&code) {
        Some(m) => m,
        None => return std::ptr::null_mut(),
    };
    Box::into_raw(Box::new(CompiledBlock { mem }))
}

/// Execute a compiled block over the caller's register file and gas cell.
///
/// `regs` points to 13 little-endian u64 PVM registers (read and written in
/// place). `gas` points to a single i64 the block decrements by its cost.
/// Returns an EXIT_* code.
///
/// # Safety
/// `block` must be a live pointer from `pvm_compile`; `regs` must point to 13
/// u64s; `gas` to one i64.
#[no_mangle]
pub unsafe extern "C" fn pvm_execute(block: *mut CompiledBlock, regs: *mut u64, gas: *mut i64) -> u32 {
    if block.is_null() || regs.is_null() || gas.is_null() {
        return EXIT_PANIC;
    }
    let block = &*block;
    // The emitted code has signature: extern "C" fn(*mut u64 /*regs*/, *mut i64 /*gas*/) -> u32
    let f: extern "C" fn(*mut u64, *mut i64) -> u32 = std::mem::transmute(block.mem.as_ptr());
    f(regs, gas)
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
        unsafe {
            let blk = pvm_compile(instrs.as_ptr(), instrs.len());
            assert!(!blk.is_null(), "compile returned null");
            let ex = pvm_execute(blk, regs.as_mut_ptr(), gas as *mut i64);
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
