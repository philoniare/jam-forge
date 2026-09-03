//! AArch64 single-pass emitter
use crate::{Backend, Op};

const X0: u32 = 0; // regs base
const X1: u32 = 1; // gas ptr
const X2: u32 = 2; // guest memory base
const X3: u32 = 3; // guest memory length (bytes)
const X8: u32 = 8; // scratch a / effective address
const X9: u32 = 9; // scratch b / address end
const X10: u32 = 10; // scratch c (store value)
const X11: u32 = 11; // gas value / mem access base
const XZR: u32 = 31;

pub struct Aarch64Backend;

struct Asm {
    words: Vec<u32>,
}

impl Asm {
    fn new() -> Self {
        Asm { words: Vec::new() }
    }
    fn push(&mut self, w: u32) {
        self.words.push(w);
    }
    fn len(&self) -> usize {
        self.words.len()
    }

    // LDR Xt, [Xn, #(imm12*8)]
    fn ldr(&mut self, rt: u32, rn: u32, imm12: u32) {
        self.push(0xF940_0000 | (imm12 << 10) | (rn << 5) | rt);
    }
    // STR Xt, [Xn, #(imm12*8)]
    fn str(&mut self, rt: u32, rn: u32, imm12: u32) {
        self.push(0xF900_0000 | (imm12 << 10) | (rn << 5) | rt);
    }
    // ADD Xd, Xn, Xm  (LSL #0)
    fn add(&mut self, rd: u32, rn: u32, rm: u32) {
        self.push(0x8B00_0000 | (rm << 16) | (rn << 5) | rd);
    }
    // SUB Xd, Xn, Xm
    fn sub(&mut self, rd: u32, rn: u32, rm: u32) {
        self.push(0xCB00_0000 | (rm << 16) | (rn << 5) | rd);
    }
    // MUL Xd, Xn, Xm  ==  MADD Xd, Xn, Xm, XZR
    fn mul(&mut self, rd: u32, rn: u32, rm: u32) {
        self.push(0x9B00_0000 | (rm << 16) | (XZR << 10) | (rn << 5) | rd);
    }
    // SUBS Xd, Xn, #imm12  (sets flags)
    fn subs_imm(&mut self, rd: u32, rn: u32, imm12: u32) {
        self.push(0xF100_0000 | ((imm12 & 0xFFF) << 10) | (rn << 5) | rd);
    }
    // ADD Xd, Xn, #imm12
    fn add_imm(&mut self, rd: u32, rn: u32, imm12: u32) {
        self.push(0x9100_0000 | ((imm12 & 0xFFF) << 10) | (rn << 5) | rd);
    }
    // CMP Xn, Xm  ==  SUBS XZR, Xn, Xm  (sets flags)
    fn cmp(&mut self, rn: u32, rm: u32) {
        self.push(0xEB00_0000 | (rm << 16) | (rn << 5) | XZR);
    }
    // MOV Wd, Wn  (zero-extends low 32 bits into Xd)  ==  ORR Wd, WZR, Wn
    fn uxtw(&mut self, rd: u32, rn: u32) {
        self.push(0x2A00_0000 | (rn << 16) | (XZR << 5) | rd);
    }
    // LDR Xt, [Xn]  (register-base, zero offset)
    fn ldr0(&mut self, rt: u32, rn: u32) {
        self.push(0xF940_0000 | (rn << 5) | rt);
    }
    // STR Xt, [Xn]  (register-base, zero offset)
    fn str0(&mut self, rt: u32, rn: u32) {
        self.push(0xF900_0000 | (rn << 5) | rt);
    }
    // MOVZ Wd, #imm16
    fn movz_w(&mut self, rd: u32, imm16: u32) {
        self.push(0x5280_0000 | ((imm16 & 0xFFFF) << 5) | rd);
    }
    // RET (x30)
    fn ret(&mut self) {
        self.push(0xD65F_03C0);
    }
    // B.cond with a forward target expressed in instruction indices; patched later.
    fn b_cond_placeholder(&mut self, cond: u32) -> usize {
        let idx = self.len();
        self.push(0x5400_0000 | cond); // imm19 = 0 for now
        idx
    }
    fn patch_b_cond(&mut self, at: usize, target: usize) {
        let rel = (target as i64) - (at as i64); // in instructions
        let imm19 = (rel as u32) & 0x7_FFFF;
        let cond = self.words[at] & 0xF;
        self.words[at] = 0x5400_0000 | (imm19 << 5) | cond;
    }

    // Materialise a full 64-bit immediate into `rd` (movz + 3 movk).
    fn mov_imm64(&mut self, rd: u32, imm: u64) {
        let h0 = (imm & 0xFFFF) as u32;
        let h1 = ((imm >> 16) & 0xFFFF) as u32;
        let h2 = ((imm >> 32) & 0xFFFF) as u32;
        let h3 = ((imm >> 48) & 0xFFFF) as u32;
        // MOVZ rd, #h0, LSL #0
        self.push(0xD280_0000 | (h0 << 5) | rd);
        // MOVK rd, #h1, LSL #16
        self.push(0xF280_0000 | (1 << 21) | (h1 << 5) | rd);
        // MOVK rd, #h2, LSL #32
        self.push(0xF280_0000 | (2 << 21) | (h2 << 5) | rd);
        // MOVK rd, #h3, LSL #48
        self.push(0xF280_0000 | (3 << 21) | (h3 << 5) | rd);
    }

    fn to_bytes(&self) -> Vec<u8> {
        let mut out = Vec::with_capacity(self.words.len() * 4);
        for w in &self.words {
            out.extend_from_slice(&w.to_le_bytes());
        }
        out
    }
}

const COND_HI: u32 = 0x8; // unsigned higher
const COND_LT: u32 = 0xB; // signed less-than

impl Backend for Aarch64Backend {
    fn emit_block(&self, ops: &[Op], gas_cost: i64) -> Vec<u8> {
        // Skeleton constraint: block gas cost must fit an unsigned 12-bit SUBS imm.
        assert!(gas_cost >= 0 && gas_cost < 4096, "skeleton block gas cost out of imm12 range");
        let mut a = Asm::new();
        let mut fault_branches: Vec<usize> = Vec::new();

        // --- gas: gas -= cost; if < 0 goto oog -------------------------------
        a.ldr(X11, X1, 0); // x11 = *gas
        a.subs_imm(X11, X11, gas_cost as u32); // x11 = x11 - cost, set flags
        a.str(X11, X1, 0); // *gas = x11
        let oog_branch = a.b_cond_placeholder(COND_LT); // B.LT oog (patched)

        // Emit a bounds-checked effective address into x8, and mem base+addr
        // into x11. Faults (B.HI to fault epilogue) if [addr, addr+8) escapes
        // the guest region. Address is masked to 32 bits (PVM address space).
        let addr_into = |a: &mut Asm, src: u8, imm: u64, fb: &mut Vec<usize>| {
            a.ldr(X8, X0, src as u32); // x8 = reg[src]
            a.mov_imm64(X9, imm); // x9 = imm
            a.add(X8, X8, X9); // x8 = reg[src] + imm (64-bit)
            a.uxtw(X8, X8); // x8 = addr & 0xFFFFFFFF
            a.add_imm(X9, X8, 8); // x9 = addr + 8 (no overflow: addr < 2^32)
            a.cmp(X9, X3); // compare addr+8 vs mem_len
            fb.push(a.b_cond_placeholder(COND_HI)); // B.HI fault
            a.add(X11, X2, X8); // x11 = mem_base + addr
        };

        // --- body ------------------------------------------------------------
        for op in ops {
            match *op {
                Op::LoadImm64 { dst, imm } => {
                    a.mov_imm64(X8, imm);
                    a.str(X8, X0, dst as u32);
                }
                Op::AddImm64 { dst, src, imm } => {
                    a.ldr(X8, X0, src as u32);
                    a.mov_imm64(X9, imm);
                    a.add(X8, X8, X9);
                    a.str(X8, X0, dst as u32);
                }
                Op::Add { dst, src, src2 } => {
                    a.ldr(X8, X0, src as u32);
                    a.ldr(X9, X0, src2 as u32);
                    a.add(X8, X8, X9);
                    a.str(X8, X0, dst as u32);
                }
                Op::Sub { dst, src, src2 } => {
                    a.ldr(X8, X0, src as u32);
                    a.ldr(X9, X0, src2 as u32);
                    a.sub(X8, X8, X9);
                    a.str(X8, X0, dst as u32);
                }
                Op::Mul { dst, src, src2 } => {
                    a.ldr(X8, X0, src as u32);
                    a.ldr(X9, X0, src2 as u32);
                    a.mul(X8, X8, X9);
                    a.str(X8, X0, dst as u32);
                }
                Op::LoadU64 { dst, src, imm } => {
                    addr_into(&mut a, src, imm, &mut fault_branches);
                    a.ldr0(X8, X11); // x8 = mem_u64[addr]
                    a.str(X8, X0, dst as u32); // reg[dst] = x8
                }
                Op::StoreU64 { dst, src, imm } => {
                    addr_into(&mut a, src, imm, &mut fault_branches);
                    a.ldr(X10, X0, dst as u32); // x10 = reg[dst]
                    a.str0(X10, X11); // mem_u64[addr] = x10
                }
            }
        }

        // --- halt epilogue: w0 = PANIC(1); ret -------------------------------
        a.movz_w(X0, 1);
        a.ret();

        // --- oog epilogue: w0 = OOG(2); ret ----------------------------------
        let oog_idx = a.len();
        a.movz_w(X0, 2);
        a.ret();

        // --- fault epilogue: w0 = FAULT(3); ret ------------------------------
        let fault_idx = a.len();
        a.movz_w(X0, 3);
        a.ret();

        a.patch_b_cond(oog_branch, oog_idx);
        for b in fault_branches {
            a.patch_b_cond(b, fault_idx);
        }
        a.to_bytes()
    }
}
