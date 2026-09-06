//! AArch64 single-pass emitter
use crate::{Backend, Op, DJUMP_HALT, EXIT_FAULT, EXIT_HALT, EXIT_OOG, EXIT_PANIC};
use dynasmrt::{dynasm, DynasmApi, DynasmLabelApi};

type Asm = dynasmrt::aarch64::Assembler;

pub struct Aarch64Backend;

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

fn mov_imm64(a: &mut Asm, rd: u8, imm: u64) {
    let h0 = (imm & 0xFFFF) as u32;
    let h1 = ((imm >> 16) & 0xFFFF) as u32;
    let h2 = ((imm >> 32) & 0xFFFF) as u32;
    let h3 = ((imm >> 48) & 0xFFFF) as u32;
    dynasm!(a
        ; .arch aarch64
        ; movz X(rd), #h0
        ; movk X(rd), #h1, lsl #16
        ; movk X(rd), #h2, lsl #32
        ; movk X(rd), #h3, lsl #48
    );
}

fn addr_into(a: &mut Asm, src: u8, imm: u64, width: u8, fault_label: dynasmrt::DynamicLabel) {
    let src = src as u32;
    dynasm!(a
        ; .arch aarch64
        ; ldr x8, [x0, #src * 8]
    );
    mov_imm64(a, 9, imm);
    let width = width as u32;
    dynasm!(a
        ; .arch aarch64
        ; add x8, x8, x9
        ; mov w8, w8              // zero-extend low 32 bits into x8 (UXTW)
        ; add x9, x8, #width
        ; cmp x9, x3
        ; b.hi =>fault_label
        ; add x12, x2, x8
    );
}

/// Emit a bounds-checked load of `width` bytes at [x12] into x8, zero- or
/// sign-extended per `signed`.
fn emit_load(a: &mut Asm, width: u8, signed: bool) {
    match (width, signed) {
        (1, false) => dynasm!(a; .arch aarch64; ldrb w8, [x12]),
        (2, false) => dynasm!(a; .arch aarch64; ldrh w8, [x12]),
        (4, false) => dynasm!(a; .arch aarch64; ldr w8, [x12]),
        (1, true) => dynasm!(a; .arch aarch64; ldrsb x8, [x12]),
        (2, true) => dynasm!(a; .arch aarch64; ldrsh x8, [x12]),
        (4, true) => dynasm!(a; .arch aarch64; ldrsw x8, [x12]),
        (8, _) => dynasm!(a; .arch aarch64; ldr x8, [x12]),
        _ => panic!("unsupported load width {width}"),
    }
}

/// Emit a store of the low `width` bytes of x10 at [x12].
fn emit_store(a: &mut Asm, width: u8) {
    match width {
        1 => dynasm!(a; .arch aarch64; strb w10, [x12]),
        2 => dynasm!(a; .arch aarch64; strh w10, [x12]),
        4 => dynasm!(a; .arch aarch64; str w10, [x12]),
        8 => dynasm!(a; .arch aarch64; str x10, [x12]),
        _ => panic!("unsupported store width {width}"),
    }
}

impl Backend for Aarch64Backend {
    fn emit_program(&self, ops: &[Op], jump_table: &[u32]) -> Vec<u8> {
        let mut a = Asm::new().expect("dynasm assembler alloc");

        if ops.is_empty() {
            // empty program: immediate PANIC (nothing to run)
            dynasm!(a
                ; .arch aarch64
                ; movz w0, #EXIT_PANIC as u32
                ; ret
            );
            let buf = a.finalize().expect("finalize");
            return buf.to_vec();
        }

        let blocks = analyze_blocks(ops, jump_table);
        let nblocks = blocks.block_lo.len();

        // One dynamic label per basic block (branch/jump target resolution),
        // plus the shared OOG/FAULT/HALT epilogues.
        let block_labels: Vec<dynasmrt::DynamicLabel> = (0..nblocks).map(|_| a.new_dynamic_label()).collect();
        let oog_label = a.new_dynamic_label();
        let fault_label = a.new_dynamic_label();
        let halt_label = a.new_dynamic_label();

        dynasm!(a
            ; .arch aarch64
            ; ldr x11, [x1]   // x11 = *gas (live throughout)
        );

        for b in 0..nblocks {
            dynasm!(a
                ; .arch aarch64
                ; =>block_labels[b]
            );
            let lo = blocks.block_lo[b];
            let hi = blocks.block_hi[b];

            // --- block body (per-instruction gas) ----------------------------
            for pc in lo..hi {
                // charge 1 gas for this instruction; OOG (gas<0) before executing
                dynasm!(a
                    ; .arch aarch64
                    ; subs x11, x11, #1
                    ; b.lt =>oog_label
                );
                match ops[pc] {
                    Op::LoadImm64 { dst, imm } => {
                        let dst = dst as u32;
                        mov_imm64(&mut a, 8, imm);
                        dynasm!(a; .arch aarch64; str x8, [x0, #dst * 8]);
                    }
                    Op::AddImm64 { dst, src, imm } => {
                        let (dst, src) = (dst as u32, src as u32);
                        dynasm!(a; .arch aarch64; ldr x8, [x0, #src * 8]);
                        mov_imm64(&mut a, 9, imm);
                        dynasm!(a
                            ; .arch aarch64
                            ; add x8, x8, x9
                            ; str x8, [x0, #dst * 8]
                        );
                    }
                    Op::Add { dst, src, src2 } => {
                        let (dst, src, src2) = (dst as u32, src as u32, src2 as u32);
                        dynasm!(a
                            ; .arch aarch64
                            ; ldr x8, [x0, #src * 8]
                            ; ldr x9, [x0, #src2 * 8]
                            ; add x8, x8, x9
                            ; str x8, [x0, #dst * 8]
                        );
                    }
                    Op::Sub { dst, src, src2 } => {
                        let (dst, src, src2) = (dst as u32, src as u32, src2 as u32);
                        dynasm!(a
                            ; .arch aarch64
                            ; ldr x8, [x0, #src * 8]
                            ; ldr x9, [x0, #src2 * 8]
                            ; sub x8, x8, x9
                            ; str x8, [x0, #dst * 8]
                        );
                    }
                    Op::Mul { dst, src, src2 } => {
                        let (dst, src, src2) = (dst as u32, src as u32, src2 as u32);
                        dynasm!(a
                            ; .arch aarch64
                            ; ldr x8, [x0, #src * 8]
                            ; ldr x9, [x0, #src2 * 8]
                            ; mul x8, x8, x9
                            ; str x8, [x0, #dst * 8]
                        );
                    }
                    Op::Load { dst, src, imm, width, signed } => {
                        addr_into(&mut a, src, imm, width, fault_label);
                        emit_load(&mut a, width, signed);
                        let dst = dst as u32;
                        dynasm!(a; .arch aarch64; str x8, [x0, #dst * 8]);
                    }
                    Op::Store { dst, src, imm, width } => {
                        addr_into(&mut a, src, imm, width, fault_label);
                        let dst = dst as u32;
                        dynasm!(a; .arch aarch64; ldr x10, [x0, #dst * 8]);
                        emit_store(&mut a, width);
                    }
                    Op::Trap => {
                        dynasm!(a
                            ; .arch aarch64
                            ; str x11, [x1]     // flush gas
                            ; movz w0, #EXIT_PANIC as u32
                            ; ret
                        );
                    }
                    Op::Jump { target } => {
                        let tgt = block_labels[blocks.block_of[target as usize]];
                        dynasm!(a; .arch aarch64; b =>tgt);
                    }
                    Op::BranchEq { src, src2, target } => {
                        let (src, src2) = (src as u32, src2 as u32);
                        let tgt = block_labels[blocks.block_of[target as usize]];
                        dynasm!(a
                            ; .arch aarch64
                            ; ldr x8, [x0, #src * 8]
                            ; ldr x9, [x0, #src2 * 8]
                            ; cmp x8, x9
                            ; b.eq =>tgt
                        );
                        // not-taken: fall through to the next block (emitted next)
                    }
                    Op::BranchNe { src, src2, target } => {
                        let (src, src2) = (src as u32, src2 as u32);
                        let tgt = block_labels[blocks.block_of[target as usize]];
                        dynasm!(a
                            ; .arch aarch64
                            ; ldr x8, [x0, #src * 8]
                            ; ldr x9, [x0, #src2 * 8]
                            ; cmp x8, x9
                            ; b.ne =>tgt
                        );
                    }
                    Op::Djump { src } => {
                        let src = src as u32;
                        dynasm!(a; .arch aarch64; ldr x8, [x0, #src * 8]);
                        mov_imm64(&mut a, 9, DJUMP_HALT);
                        dynasm!(a
                            ; .arch aarch64
                            ; cmp x8, x9
                            ; b.eq =>halt_label
                        );
                        for &t in jump_table {
                            let ti = t as usize;
                            if ti >= ops.len() {
                                continue;
                            }
                            assert!(t < 4096, "skeleton djump target index out of imm12 range");
                            let tgt = block_labels[blocks.block_of[ti]];
                            dynasm!(a
                                ; .arch aarch64
                                ; cmp x8, #t
                                ; b.eq =>tgt
                            );
                        }
                        // no jump-table match -> panic
                        dynasm!(a
                            ; .arch aarch64
                            ; str x11, [x1]     // flush gas
                            ; movz w0, #EXIT_PANIC as u32
                            ; ret
                        );
                    }
                }
            }
            // Blocks ending without a terminator fall through to the next block,
            // which is emitted immediately after — no branch needed.
        }

        // --- epilogues (each flushes the live gas register back to *gas) ------
        dynasm!(a
            ; .arch aarch64
            ; =>oog_label
            ; str x11, [x1]     // gas is now -1 (the failing decrement)
            ; movz w0, #EXIT_OOG as u32
            ; ret
            ; =>fault_label
            ; str x11, [x1]
            ; movz w0, #EXIT_FAULT as u32
            ; ret
            ; =>halt_label
            ; str x11, [x1]
            ; movz w0, #EXIT_HALT as u32
            ; ret
        );

        let buf = a.finalize().expect("finalize: unresolved labels or relocation range overflow");
        buf.to_vec()
    }
}
