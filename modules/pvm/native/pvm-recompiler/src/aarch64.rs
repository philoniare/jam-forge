//! AArch64 single-pass emitter
use crate::{Backend, Op};

const X0: u32 = 0; // regs base
const X1: u32 = 1; // gas ptr
const X8: u32 = 8; // scratch a
const X9: u32 = 9; // scratch b
const X11: u32 = 11; // gas value
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

const COND_LT: u32 = 0xB;

impl Backend for Aarch64Backend {
    fn emit_block(&self, ops: &[Op], gas_cost: i64) -> Vec<u8> {
        // Skeleton constraint: block gas cost must fit an unsigned 12-bit SUBS imm.
        assert!(gas_cost >= 0 && gas_cost < 4096, "skeleton block gas cost out of imm12 range");
        let mut a = Asm::new();

        // --- gas: gas -= cost; if < 0 goto oog -------------------------------
        a.ldr(X11, X1, 0); // x11 = *gas
        a.subs_imm(X11, X11, gas_cost as u32); // x11 = x11 - cost, set flags
        a.str(X11, X1, 0); // *gas = x11
        let branch_at = a.b_cond_placeholder(COND_LT); // B.LT oog (patched)

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
            }
        }

        // --- halt epilogue: w0 = PANIC(1); ret -------------------------------
        a.movz_w(X0, 1);
        a.ret();

        // --- oog epilogue: w0 = OOG(2); ret ----------------------------------
        let oog_idx = a.len();
        a.movz_w(X0, 2);
        a.ret();

        a.patch_b_cond(branch_at, oog_idx);
        a.to_bytes()
    }
}
