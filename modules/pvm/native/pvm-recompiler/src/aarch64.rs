//! AArch64 single-pass emitter
use crate::{Backend, Op, DJUMP_HALT};

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
    // CMP Xn, #imm12  ==  SUBS XZR, Xn, #imm12  (sets flags)
    fn cmp_imm(&mut self, rn: u32, imm12: u32) {
        self.push(0xF100_0000 | ((imm12 & 0xFFF) << 10) | (rn << 5) | XZR);
    }
    // MOV Wd, Wn  (zero-extends low 32 bits into Xd)  ==  ORR Wd, WZR, Wn
    fn uxtw(&mut self, rd: u32, rn: u32) {
        self.push(0x2A00_0000 | (rn << 16) | (XZR << 5) | rd);
    }
    // Register-base, zero-offset loads/stores. Ws written by the byte/half/word
    // variants zero the upper 32 bits of the X register; LDRS* sign-extend to 64.
    fn ldr0(&mut self, rt: u32, rn: u32) {
        self.push(0xF940_0000 | (rn << 5) | rt); // LDR Xt (64)
    }
    fn str0(&mut self, rt: u32, rn: u32) {
        self.push(0xF900_0000 | (rn << 5) | rt); // STR Xt (64)
    }
    fn ldrb(&mut self, rt: u32, rn: u32) {
        self.push(0x3940_0000 | (rn << 5) | rt); // LDRB Wt (u8 -> zext)
    }
    fn ldrh(&mut self, rt: u32, rn: u32) {
        self.push(0x7940_0000 | (rn << 5) | rt); // LDRH Wt (u16 -> zext)
    }
    fn ldrw(&mut self, rt: u32, rn: u32) {
        self.push(0xB940_0000 | (rn << 5) | rt); // LDR Wt (u32 -> zext)
    }
    fn ldrsb(&mut self, rt: u32, rn: u32) {
        self.push(0x3980_0000 | (rn << 5) | rt); // LDRSB Xt (i8 -> sext64)
    }
    fn ldrsh(&mut self, rt: u32, rn: u32) {
        self.push(0x7980_0000 | (rn << 5) | rt); // LDRSH Xt (i16 -> sext64)
    }
    fn ldrsw(&mut self, rt: u32, rn: u32) {
        self.push(0xB980_0000 | (rn << 5) | rt); // LDRSW Xt (i32 -> sext64)
    }
    fn strb(&mut self, rt: u32, rn: u32) {
        self.push(0x3900_0000 | (rn << 5) | rt); // STRB Wt (low 8)
    }
    fn strh(&mut self, rt: u32, rn: u32) {
        self.push(0x7900_0000 | (rn << 5) | rt); // STRH Wt (low 16)
    }
    fn strw(&mut self, rt: u32, rn: u32) {
        self.push(0xB900_0000 | (rn << 5) | rt); // STR Wt (low 32)
    }
    fn emit_load(&mut self, rt: u32, rn: u32, width: u8, signed: bool) {
        match (width, signed) {
            (1, false) => self.ldrb(rt, rn),
            (2, false) => self.ldrh(rt, rn),
            (4, false) => self.ldrw(rt, rn),
            (1, true) => self.ldrsb(rt, rn),
            (2, true) => self.ldrsh(rt, rn),
            (4, true) => self.ldrsw(rt, rn),
            (8, _) => self.ldr0(rt, rn),
            _ => panic!("unsupported load width {width}"),
        }
    }
    fn emit_store(&mut self, rt: u32, rn: u32, width: u8) {
        match width {
            1 => self.strb(rt, rn),
            2 => self.strh(rt, rn),
            4 => self.strw(rt, rn),
            8 => self.str0(rt, rn),
            _ => panic!("unsupported store width {width}"),
        }
    }
    // MOVZ Wd, #imm16
    fn movz_w(&mut self, rd: u32, imm16: u32) {
        self.push(0x5280_0000 | ((imm16 & 0xFFFF) << 5) | rd);
    }
    // RET (x30)
    fn ret(&mut self) {
        self.push(0xD65F_03C0);
    }
    // B.cond placeholder (target patched later, in instruction words).
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
    // B (unconditional) placeholder; imm26 patched later (in instruction words).
    fn b_placeholder(&mut self) -> usize {
        let idx = self.len();
        self.push(0x1400_0000);
        idx
    }
    fn patch_b(&mut self, at: usize, target: usize) {
        let rel = (target as i64) - (at as i64);
        let imm26 = (rel as u32) & 0x3FF_FFFF;
        self.words[at] = 0x1400_0000 | imm26;
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

const COND_EQ: u32 = 0x0; // equal
const COND_NE: u32 = 0x1; // not equal
const COND_HI: u32 = 0x8; // unsigned higher
const COND_LT: u32 = 0xB; // signed less-than

struct Blocks {
    block_of: Vec<usize>,
    block_lo: Vec<usize>,
    block_hi: Vec<usize>,
}

fn analyze_blocks(ops: &[Op], jump_table: &[u32]) -> Blocks {
    let n = ops.len();
    // Leaders: op 0, every branch/jump target, every indirect (djump) target,
    // and the op after any terminator.
    let mut is_leader = vec![false; n + 1];
    if n > 0 {
        is_leader[0] = true;
    }
    for &t in jump_table {
        if (t as usize) < n {
            is_leader[t as usize] = true;
        }
    }
    for (i, op) in ops.iter().enumerate() {
        if let Some(t) = op.target() {
            if (t as usize) < n {
                is_leader[t as usize] = true;
            }
        }
        if op.is_terminator() && i + 1 < n {
            is_leader[i + 1] = true;
        }
    }
    let mut block_of = vec![0usize; n];
    let mut block_lo = Vec::new();
    let mut block_hi = Vec::new();
    let mut b = 0usize;
    let mut i = 0usize;
    while i < n {
        let lo = i;
        block_lo.push(lo);
        // extend until the next leader
        i += 1;
        while i < n && !is_leader[i] {
            i += 1;
        }
        block_hi.push(i);
        for pc in lo..i {
            block_of[pc] = b;
        }
        b += 1;
    }
    Blocks { block_of, block_lo, block_hi }
}

impl Backend for Aarch64Backend {
    fn emit_program(&self, ops: &[Op], jump_table: &[u32]) -> Vec<u8> {
        let mut a = Asm::new();
        if ops.is_empty() {
            // empty program: immediate PANIC (nothing to run)
            a.movz_w(X0, 1);
            a.ret();
            return a.to_bytes();
        }
        let blocks = analyze_blocks(ops, jump_table);
        let nblocks = blocks.block_lo.len();

        let mut block_start_asm = vec![0usize; nblocks];
        let mut oog_branches: Vec<usize> = Vec::new();
        let mut fault_branches: Vec<usize> = Vec::new();
        let mut halt_branches: Vec<usize> = Vec::new();
        // (asm index of branch, target block, is_conditional)
        let mut cf_branches: Vec<(usize, usize, bool)> = Vec::new();

        // Emit a bounds-checked effective address into x8 and mem base+addr into
        // x11; B.HI to the fault epilogue if [addr, addr+width) escapes the
        // region. Address masked to 32 bits (PVM address space).
        let addr_into = |a: &mut Asm, src: u8, imm: u64, width: u8, fb: &mut Vec<usize>| {
            a.ldr(X8, X0, src as u32);
            a.mov_imm64(X9, imm);
            a.add(X8, X8, X9);
            a.uxtw(X8, X8);
            a.add_imm(X9, X8, width as u32);
            a.cmp(X9, X3);
            fb.push(a.b_cond_placeholder(COND_HI));
            a.add(X11, X2, X8);
        };

        for b in 0..nblocks {
            block_start_asm[b] = a.len();
            let lo = blocks.block_lo[b];
            let hi = blocks.block_hi[b];
            let cost = (hi - lo) as u32;

            // --- block-entry gas: gas -= cost; if < 0 goto oog ---------------
            assert!(cost < 4096, "skeleton block gas cost out of imm12 range");
            a.ldr(X11, X1, 0);
            a.subs_imm(X11, X11, cost);
            a.str(X11, X1, 0);
            oog_branches.push(a.b_cond_placeholder(COND_LT));

            // --- block body --------------------------------------------------
            for pc in lo..hi {
                match ops[pc] {
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
                    Op::Load { dst, src, imm, width, signed } => {
                        addr_into(&mut a, src, imm, width, &mut fault_branches);
                        a.emit_load(X8, X11, width, signed);
                        a.str(X8, X0, dst as u32);
                    }
                    Op::Store { dst, src, imm, width } => {
                        addr_into(&mut a, src, imm, width, &mut fault_branches);
                        a.ldr(X10, X0, dst as u32);
                        a.emit_store(X10, X11, width);
                    }
                    Op::Trap => {
                        a.movz_w(X0, 1); // EXIT_PANIC
                        a.ret();
                    }
                    Op::Jump { target } => {
                        let idx = a.b_placeholder();
                        cf_branches.push((idx, blocks.block_of[target as usize], false));
                    }
                    Op::BranchEq { src, src2, target } => {
                        a.ldr(X8, X0, src as u32);
                        a.ldr(X9, X0, src2 as u32);
                        a.cmp(X8, X9);
                        let idx = a.b_cond_placeholder(COND_EQ);
                        cf_branches.push((idx, blocks.block_of[target as usize], true));
                        // not-taken: fall through to the next block (emitted next)
                    }
                    Op::BranchNe { src, src2, target } => {
                        a.ldr(X8, X0, src as u32);
                        a.ldr(X9, X0, src2 as u32);
                        a.cmp(X8, X9);
                        let idx = a.b_cond_placeholder(COND_NE);
                        cf_branches.push((idx, blocks.block_of[target as usize], true));
                    }
                    Op::Djump { src } => {
                        a.ldr(X8, X0, src as u32);
                        a.mov_imm64(X9, DJUMP_HALT);
                        a.cmp(X8, X9);
                        halt_branches.push(a.b_cond_placeholder(COND_EQ));
                        for &t in jump_table {
                            let ti = t as usize;
                            if ti >= ops.len() {
                                continue;
                            }
                            assert!(t < 4096, "skeleton djump target index out of imm12 range");
                            a.cmp_imm(X8, t);
                            let idx = a.b_cond_placeholder(COND_EQ);
                            cf_branches.push((idx, blocks.block_of[ti], true));
                        }
                        // no jump-table match -> panic
                        a.movz_w(X0, 1);
                        a.ret();
                    }
                }
            }
            // Blocks ending without a terminator fall through to the next block,
            // which is emitted immediately after — no branch needed.
        }

        // --- epilogues -------------------------------------------------------
        let oog_idx = a.len();
        a.movz_w(X0, 2); // EXIT_OOG
        a.ret();
        let fault_idx = a.len();
        a.movz_w(X0, 3); // EXIT_FAULT
        a.ret();
        let halt_idx = a.len();
        a.movz_w(X0, 0); // EXIT_HALT
        a.ret();

        // --- patch ------------------------------------------------------------
        for b in oog_branches {
            a.patch_b_cond(b, oog_idx);
        }
        for b in fault_branches {
            a.patch_b_cond(b, fault_idx);
        }
        for b in halt_branches {
            a.patch_b_cond(b, halt_idx);
        }
        for (at, target_block, is_cond) in cf_branches {
            let tgt = block_start_asm[target_block];
            if is_cond {
                a.patch_b_cond(at, tgt);
            } else {
                a.patch_b(at, tgt);
            }
        }
        a.to_bytes()
    }
}
