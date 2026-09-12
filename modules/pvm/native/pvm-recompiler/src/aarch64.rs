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

fn mov_imm32(a: &mut Asm, rd: u8, imm: u32) {
    let h0 = imm & 0xFFFF;
    let h1 = (imm >> 16) & 0xFFFF;
    dynasm!(a
        ; .arch aarch64
        ; movz W(rd), #h0
        ; movk W(rd), #h1, lsl #16
    );
}

fn emit_bounds_check_subroutine(a: &mut Asm, entry: dynasmrt::DynamicLabel) {
    // Loop over regions: x16 = region index, x17 = regions*[i] pointer.
    dynasm!(a
        ; .arch aarch64
        ; =>entry
        ; mov x16, #0
    );
    let loop_top = a.new_dynamic_label();
    let loop_next = a.new_dynamic_label();
    let found = a.new_dynamic_label();
    let scan_done = a.new_dynamic_label();
    dynasm!(a
        ; .arch aarch64
        ; =>loop_top
        ; cmp x16, x3
        ; b.hs =>scan_done  // exhausted region table -> not found
        // x17 = &regions[x16]  (Region is 16 bytes: base,len,buf_offset,writable)
        ; lsl x17, x16, #4
        ; add x17, x2, x17
        ; ldr w8, [x17]        // region.base
        ; ldr w9, [x17, #4]    // region.len
        // region_end = base + len (both u32, widen to 64-bit for safe compare)
        ; add x9, x8, x9       // x9 = region.base + region.len (region_end), zero-extended
        ; mov w8, w8            // zero-extend base into x8
        ; mov w9, w9            // zero-extend region_end into x9
        // containment: addr >= base && addr + width <= region_end
        ; add x10, x13, x14     // x10 = addr + width
        ; cmp x13, x8
        ; b.lo =>loop_next       // addr < base -> not this region
        ; cmp x10, x9
        ; b.hi =>loop_next       // addr+width > region_end -> not this region
        // permission: if store (x15==1), region must be writable
        ; ldr w9, [x17, #12]    // region.writable
        ; cmp x15, #0
        ; b.eq =>found           // load: containment is enough
        ; cmp w9, #0
        ; b.ne =>found           // store into a writable region: ok
        // store into a read-only region: contained but not permitted -> falls
        // through to "not found" (RO-store must fault, exactly like an
        // unmapped region — the interpreter's isWritable check is the same
        // shape as isReadable, just against the writable bitset)
        ; =>loop_next
        ; add x16, x16, #1
        ; b =>loop_top
        ; =>found
        // x8 = backing + region.buf_offset + (addr - region.base)
        ; ldr w9, [x17, #8]     // region.buf_offset
        ; sub x10, x13, x8      // addr - region.base   (x8 still holds region.base here)
        ; add x8, x4, x9
        ; add x8, x8, x10
        ; mov w9, #1
        ; ret
        ; =>scan_done
    );

    // ---- Not found in any (permission-appropriate) region: compute the
    // first-failing-page fault address per the low-to-high scan rule. ----
    // addr_page = (addr >>> page_shift) << page_shift
    dynasm!(a
        ; .arch aarch64
        ; lsr x10, x13, x5
        ; lsl x10, x10, x5      // x10 = addr_page
    );
    // Does ANY (permission-appropriate) region contain byte `addr` itself
    // (not the whole span — just checking whether addr's own page is the
    // failing one, or whether it's the far end of the span that fails)?
    // Re-scan: x16 = 0 again.
    let probe_top = a.new_dynamic_label();
    let probe_next = a.new_dynamic_label();
    let addr_page_ok = a.new_dynamic_label();
    let probe_done = a.new_dynamic_label();
    dynasm!(a
        ; .arch aarch64
        ; mov x16, #0
        ; =>probe_top
        ; cmp x16, x3
        ; b.hs =>probe_done
        ; lsl x17, x16, #4
        ; add x17, x2, x17
        ; ldr w8, [x17]
        ; ldr w9, [x17, #4]
        ; add x9, x8, x9
        ; mov w8, w8
        ; mov w9, w9
        // addr itself in [base, region_end)?
        ; cmp x13, x8
        ; b.lo =>probe_next
        ; cmp x13, x9
        ; b.hs =>probe_next
        ; ldr w9, [x17, #12]
        ; cmp x15, #0
        ; b.eq =>addr_page_ok    // load: containment of addr is enough
        ; cmp w9, #0
        ; b.ne =>addr_page_ok
        ; =>probe_next
        ; add x16, x16, #1
        ; b =>probe_top
        ; =>probe_done
    );
    // addr's own page fails (no permitted region contains byte `addr`):
    // fault_page = addr_page. Also compute the < MinValidAddress escalation.
    let compute_escalation = a.new_dynamic_label();
    dynasm!(a
        ; .arch aarch64
        ; mov w9, #0             // w9 = "not found" sentinel (success flag)
        ; b =>compute_escalation
        ; =>addr_page_ok
        // addr's own page/permission is fine; the span must cross into an
        // unmapped/unpermitted page at the far end -> fault_page =
        // align_down(addr+width-1).
        ; add x10, x13, x14
        ; sub x10, x10, #1       // x10 = addr+width-1
        ; lsr x10, x10, x5
        ; lsl x10, x10, x5       // x10 = align_down(addr+width-1)
        ; mov w9, #0
        ; =>compute_escalation
    );
    // x10 = fault_page. Escalate to PANIC if fault_page < MinValidAddress
    // (0x10000), mirroring InterpreterCore.segfault's escalation exactly.
    let no_escalate = a.new_dynamic_label();
    dynasm!(a; .arch aarch64; mov w16, #0);
    mov_imm32(a, 17, 0x10000); // Abi.MinValidAddress
    dynasm!(a
        ; .arch aarch64
        ; cmp w10, w17
        ; b.hs =>no_escalate
        ; mov w16, #1           // escalate: fault_page < MinValidAddress
        ; =>no_escalate
        ; ret
    );
}

/// Emit a bounds-checked load of `width` bytes at [x8] into x8 (result),
/// zero- or sign-extended per `signed`. Overwrites x8 with the loaded value.
fn emit_load(a: &mut Asm, width: u8, signed: bool) {
    match (width, signed) {
        (1, false) => dynasm!(a; .arch aarch64; ldrb w8, [x8]),
        (2, false) => dynasm!(a; .arch aarch64; ldrh w8, [x8]),
        (4, false) => dynasm!(a; .arch aarch64; ldr w8, [x8]),
        (1, true) => dynasm!(a; .arch aarch64; ldrsb x8, [x8]),
        (2, true) => dynasm!(a; .arch aarch64; ldrsh x8, [x8]),
        (4, true) => dynasm!(a; .arch aarch64; ldrsw x8, [x8]),
        (8, _) => dynasm!(a; .arch aarch64; ldr x8, [x8]),
        _ => panic!("unsupported load width {width}"),
    }
}

/// Emit a store of the low `width` bytes of x10 at [x8].
fn emit_store(a: &mut Asm, width: u8) {
    match width {
        1 => dynasm!(a; .arch aarch64; strb w10, [x8]),
        2 => dynasm!(a; .arch aarch64; strh w10, [x8]),
        4 => dynasm!(a; .arch aarch64; str w10, [x8]),
        8 => dynasm!(a; .arch aarch64; str x10, [x8]),
        _ => panic!("unsupported store width {width}"),
    }
}

fn emit_b_cond(a: &mut Asm, cond: CmpCond, target: dynasmrt::DynamicLabel) {
    match cond {
        CmpCond::LtU => dynasm!(a; .arch aarch64; b.lo =>target),
        CmpCond::LeU => dynasm!(a; .arch aarch64; b.ls =>target),
        CmpCond::GeU => dynasm!(a; .arch aarch64; b.hs =>target),
        CmpCond::GtU => dynasm!(a; .arch aarch64; b.hi =>target),
        CmpCond::LtS => dynasm!(a; .arch aarch64; b.lt =>target),
        CmpCond::LeS => dynasm!(a; .arch aarch64; b.le =>target),
        CmpCond::GeS => dynasm!(a; .arch aarch64; b.ge =>target),
        CmpCond::GtS => dynasm!(a; .arch aarch64; b.gt =>target),
    }
}

fn emit_cset(a: &mut Asm, rd: u8, cond: CmpCond) {
    match cond {
        CmpCond::LtU => dynasm!(a; .arch aarch64; cset X(rd), lo),
        CmpCond::LeU => dynasm!(a; .arch aarch64; cset X(rd), ls),
        CmpCond::GeU => dynasm!(a; .arch aarch64; cset X(rd), hs),
        CmpCond::GtU => dynasm!(a; .arch aarch64; cset X(rd), hi),
        CmpCond::LtS => dynasm!(a; .arch aarch64; cset X(rd), lt),
        CmpCond::LeS => dynasm!(a; .arch aarch64; cset X(rd), le),
        CmpCond::GeS => dynasm!(a; .arch aarch64; cset X(rd), ge),
        CmpCond::GtS => dynasm!(a; .arch aarch64; cset X(rd), gt),
    }
}

fn emit_popcount64(a: &mut Asm) {
    mov_imm64(a, 9, 0x5555_5555_5555_5555);
    dynasm!(a
        ; .arch aarch64
        ; and x10, x8, x9        // x10 = x8 & m1
        ; lsr x8, x8, #1
        ; and x8, x8, x9         // x8 = (x8 >> 1) & m1
        ; add x8, x8, x10        // x8 = pairwise 2-bit counts
    );
    mov_imm64(a, 9, 0x3333_3333_3333_3333);
    dynasm!(a
        ; .arch aarch64
        ; and x10, x8, x9        // x10 = x8 & m2
        ; lsr x8, x8, #2
        ; and x8, x8, x9         // x8 = (x8 >> 2) & m2
        ; add x8, x8, x10        // x8 = nibble 4-bit counts
    );
    mov_imm64(a, 9, 0x0f0f_0f0f_0f0f_0f0f);
    dynasm!(a
        ; .arch aarch64
        ; add x8, x8, x8, lsr #4 // x8 = byte-pair sums (each nibble <= 8, no overflow)
        ; and x8, x8, x9         // x8 = per-byte popcount (0..8) in each byte lane
    );
    mov_imm64(a, 9, 0x0101_0101_0101_0101);
    dynasm!(a
        ; .arch aarch64
        ; mul x8, x8, x9         // x8 = sum of all 8 byte lanes folds into the top byte
        ; lsr x8, x8, #56        // x8 = final population count (0..64)
    );
}

fn emit_alu(a: &mut Asm, kind: AluKind, width: Width) {
    match (width, kind) {
        (Width::W32, AluKind::Add) => dynasm!(a; .arch aarch64; add w8, w8, w9),
        (Width::W32, AluKind::Sub) => dynasm!(a; .arch aarch64; sub w8, w8, w9),
        (Width::W32, AluKind::Mul) => dynasm!(a; .arch aarch64; mul w8, w8, w9),
        (Width::W32, AluKind::And) => dynasm!(a; .arch aarch64; and w8, w8, w9),
        (Width::W32, AluKind::Or) => dynasm!(a; .arch aarch64; orr w8, w8, w9),
        (Width::W32, AluKind::Xor) => dynasm!(a; .arch aarch64; eor w8, w8, w9),
        (Width::W64, AluKind::Add) => dynasm!(a; .arch aarch64; add x8, x8, x9),
        (Width::W64, AluKind::Sub) => dynasm!(a; .arch aarch64; sub x8, x8, x9),
        (Width::W64, AluKind::Mul) => dynasm!(a; .arch aarch64; mul x8, x8, x9),
        (Width::W64, AluKind::And) => dynasm!(a; .arch aarch64; and x8, x8, x9),
        (Width::W64, AluKind::Or) => dynasm!(a; .arch aarch64; orr x8, x8, x9),
        (Width::W64, AluKind::Xor) => dynasm!(a; .arch aarch64; eor x8, x8, x9),
    }
}

enum ShiftAmount {
    Imm(u32),
    Reg(u8),
}

fn emit_shift_rotate(a: &mut Asm, kind: ShiftKind, width: Width, amount: ShiftAmount) {
    match (width, kind, amount) {
        // ---- 64-bit, immediate amount ----
        (Width::W64, ShiftKind::Shl, ShiftAmount::Imm(n)) => dynasm!(a; .arch aarch64; lsl x8, x8, #n),
        (Width::W64, ShiftKind::ShrLogical, ShiftAmount::Imm(n)) => dynasm!(a; .arch aarch64; lsr x8, x8, #n),
        (Width::W64, ShiftKind::ShrArith, ShiftAmount::Imm(n)) => dynasm!(a; .arch aarch64; asr x8, x8, #n),
        (Width::W64, ShiftKind::RotateRight, ShiftAmount::Imm(n)) => {
            if n == 0 { /* ROR #0 is not encodable; a zero rotate is a no-op */ }
            else { dynasm!(a; .arch aarch64; ror x8, x8, #n) }
        }
        (Width::W64, ShiftKind::RotateLeft, ShiftAmount::Imm(n)) => {
            // No native ROL; rotl(x, n) == ror(x, 64-n) for 1<=n<=63; n==0 is a no-op.
            let rn = (64 - n) % 64;
            if rn == 0 { /* no-op */ } else { dynasm!(a; .arch aarch64; ror x8, x8, #rn) }
        }
        // ---- 64-bit, register amount (already masked & 63 by the caller) ----
        (Width::W64, ShiftKind::Shl, ShiftAmount::Reg(rm)) => dynasm!(a; .arch aarch64; lslv x8, x8, X(rm)),
        (Width::W64, ShiftKind::ShrLogical, ShiftAmount::Reg(rm)) => dynasm!(a; .arch aarch64; lsrv x8, x8, X(rm)),
        (Width::W64, ShiftKind::ShrArith, ShiftAmount::Reg(rm)) => dynasm!(a; .arch aarch64; asrv x8, x8, X(rm)),
        (Width::W64, ShiftKind::RotateRight, ShiftAmount::Reg(rm)) => dynasm!(a; .arch aarch64; rorv x8, x8, X(rm)),
        (Width::W64, ShiftKind::RotateLeft, ShiftAmount::Reg(rm)) => {
            dynasm!(a
                ; .arch aarch64
                ; mov x13, #64
                ; sub x13, x13, X(rm)
                ; and x13, x13, #63
                ; rorv x8, x8, x13
            );
        }
        // ---- 32-bit, immediate amount (operates on w8, low 32 bits) ----
        (Width::W32, ShiftKind::Shl, ShiftAmount::Imm(n)) => dynasm!(a; .arch aarch64; lsl w8, w8, #n),
        (Width::W32, ShiftKind::ShrLogical, ShiftAmount::Imm(n)) => dynasm!(a; .arch aarch64; lsr w8, w8, #n),
        (Width::W32, ShiftKind::ShrArith, ShiftAmount::Imm(n)) => dynasm!(a; .arch aarch64; asr w8, w8, #n),
        (Width::W32, ShiftKind::RotateRight, ShiftAmount::Imm(n)) => {
            if n == 0 { /* no-op */ } else { dynasm!(a; .arch aarch64; ror w8, w8, #n) }
        }
        (Width::W32, ShiftKind::RotateLeft, ShiftAmount::Imm(n)) => {
            let rn = (32 - n) % 32;
            if rn == 0 { /* no-op */ } else { dynasm!(a; .arch aarch64; ror w8, w8, #rn) }
        }
        // ---- 32-bit, register amount (already masked & 31 by the caller) ----
        (Width::W32, ShiftKind::Shl, ShiftAmount::Reg(rm)) => dynasm!(a; .arch aarch64; lslv w8, w8, W(rm)),
        (Width::W32, ShiftKind::ShrLogical, ShiftAmount::Reg(rm)) => dynasm!(a; .arch aarch64; lsrv w8, w8, W(rm)),
        (Width::W32, ShiftKind::ShrArith, ShiftAmount::Reg(rm)) => dynasm!(a; .arch aarch64; asrv w8, w8, W(rm)),
        (Width::W32, ShiftKind::RotateRight, ShiftAmount::Reg(rm)) => dynasm!(a; .arch aarch64; rorv w8, w8, W(rm)),
        (Width::W32, ShiftKind::RotateLeft, ShiftAmount::Reg(rm)) => {
            // Same `rm` == the caller's masked-amount scratch (w9) hazard as
            // the 64-bit arm above — compute into w13 first.
            dynasm!(a
                ; .arch aarch64
                ; mov w13, #32
                ; sub w13, w13, W(rm)
                ; and w13, w13, #31
                ; rorv w8, w8, w13
            );
        }
    }
}

impl Backend for Aarch64Backend {
    fn emit_program(
        &self,
        ops: &[Op],
        pcs: &[u32],
        jump_table: &[u32],
        jump_table_ptr: *const u32,
        code_len: u32,
    ) -> (Vec<u8>, u32) {
        let mut a = Asm::new().expect("dynasm assembler alloc");

        if ops.is_empty() {
            // empty program: immediate PANIC (nothing to run)
            dynasm!(a
                ; .arch aarch64
                ; mov w9, #0
                ; str w9, [x6]        // out->pc = 0
                ; movz w0, #EXIT_PANIC as u32
                ; ret
            );
            let buf = a.finalize().expect("finalize");
            return (buf.to_vec(), 0);
        }

        let blocks = analyze_blocks(ops, jump_table);
        let nblocks = blocks.block_lo.len();

        // One dynamic label per basic block (branch/jump target resolution)
        let block_labels: Vec<dynasmrt::DynamicLabel> = (0..nblocks).map(|_| a.new_dynamic_label()).collect();
        let instr_labels: Vec<dynasmrt::DynamicLabel> = (0..ops.len()).map(|_| a.new_dynamic_label()).collect();
        let oog_label = a.new_dynamic_label();
        let fault_label = a.new_dynamic_label();
        let panic_label = a.new_dynamic_label();
        let halt_label = a.new_dynamic_label();
        let bounds_check_label = a.new_dynamic_label();
        let dispatch_by_index_label = a.new_dynamic_label();
        let end_of_code_label = a.new_dynamic_label();

        dynasm!(a
            ; .arch aarch64
            ; ldr x11, [x1]   // x11 = *gas (live throughout)
            ; mov x19, x30    // save the entry LR before any `bl` clobbers x30 (see LR NOTE)
        );
        assert!(ops.len() < 4096, "skeleton entry-index dispatch chain out of imm12 range");
        for i in 0..ops.len() {
            dynasm!(a
                ; .arch aarch64
                ; cmp w7, #i as u32
                ; b.eq =>instr_labels[i]
            );
        }
        dynasm!(a; .arch aarch64; b =>panic_label);
        dynasm!(a; .arch aarch64; =>dispatch_by_index_label);
        assert!(ops.len() < 4096, "skeleton dispatch-by-index chain out of imm12 range");
        for i in 0..ops.len() {
            dynasm!(a
                ; .arch aarch64
                ; cmp w7, #i as u32
                ; b.eq =>instr_labels[i]
            );
        }
        dynasm!(a; .arch aarch64; b =>panic_label);

        for b in 0..nblocks {
            dynasm!(a
                ; .arch aarch64
                ; =>block_labels[b]
            );
            let lo = blocks.block_lo[b];
            let hi = blocks.block_hi[b];

            // --- block body (per-instruction gas + pc) -----------------------
            for pc in lo..hi {
                dynasm!(a
                    ; .arch aarch64
                    ; =>instr_labels[pc]
                );
                mov_imm32(&mut a, 12, pcs[pc]);
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
                        let dst = dst as u32;
                        let srcr = src as u32;
                        mov_imm64(&mut a, 9, imm);
                        dynasm!(a
                            ; .arch aarch64
                            ; ldr x13, [x0, #srcr * 8]
                            ; add x13, x13, x9
                            ; mov w13, w13          // mask address to 32 bits (UXTW)
                            ; mov x14, #width as u64
                            ; mov x15, #0            // load
                            ; bl =>bounds_check_label
                            ; cbz w9, =>fault_label
                        );
                        emit_load(&mut a, width, signed);
                        dynasm!(a; .arch aarch64; str x8, [x0, #dst * 8]);
                    }
                    Op::Store { dst, src, imm, width } => {
                        let dst = dst as u32;
                        let srcr = src as u32;
                        mov_imm64(&mut a, 9, imm);
                        dynasm!(a
                            ; .arch aarch64
                            ; ldr x13, [x0, #srcr * 8]
                            ; add x13, x13, x9
                            ; mov w13, w13
                            ; mov x14, #width as u64
                            ; mov x15, #1            // store
                            ; bl =>bounds_check_label
                            ; cbz w9, =>fault_label
                            ; ldr x10, [x0, #dst * 8]
                        );
                        emit_store(&mut a, width);
                    }
                    Op::Trap => {
                        dynasm!(a
                            ; .arch aarch64
                            ; b =>panic_label
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
                    Op::Djump { src, imm } => {
                        let src = src as u32;
                        dynasm!(a; .arch aarch64; ldr x8, [x0, #src * 8]);
                        mov_imm64(&mut a, 9, imm);
                        dynasm!(a
                            ; .arch aarch64
                            ; add x8, x8, x9
                            ; mov w8, w8      // mask to 32 bits (UXTW), addr now in x8/w8
                        );
                        mov_imm64(&mut a, 9, DJUMP_HALT);
                        dynasm!(a
                            ; .arch aarch64
                            ; cmp x8, x9
                            ; b.eq =>halt_label
                            // invalid if addr == 0
                            ; cbz w8, =>panic_label
                            ; and w16, w8, #1
                            ; cbnz w16, =>panic_label
                        );
                        // idx = addr/2 - 1 (addr already known even and nonzero)
                        dynasm!(a
                            ; .arch aarch64
                            ; lsr w9, w8, #1
                            ; sub w9, w9, #1
                        );
                        mov_imm32(&mut a, 10, jump_table.len() as u32);
                        dynasm!(a
                            ; .arch aarch64
                            ; cmp w9, w10
                            ; b.hs =>panic_label   // idx >= table.len() (unsigned) -> panic
                        );
                        mov_imm64(&mut a, 13, jump_table_ptr as u64);
                        dynasm!(a
                            ; .arch aarch64
                            ; lsl x14, x9, #2     // byte offset = idx * 4
                            ; add x13, x13, x14
                            ; ldr w7, [x13]        // w7 = jump_table[idx] (target instruction index, or u32::MAX)
                            ; mov w9, #0xFFFF
                            ; movk w9, #0xFFFF, lsl #16   // w9 = u32::MAX sentinel
                            ; cmp w7, w9
                            ; b.eq =>panic_label
                            ; bl =>dispatch_by_index_label
                        );
                    }
                    Op::Fallthrough => {
                        if pc + 1 < ops.len() {
                            let tgt = block_labels[blocks.block_of[pc + 1]];
                            dynasm!(a; .arch aarch64; b =>tgt);
                        } else {
                            dynasm!(a; .arch aarch64; b =>end_of_code_label);
                        }
                    }
                    Op::LoadImm32 { dst, imm } => {
                        let dst = dst as u32;
                        dynasm!(a
                            ; .arch aarch64
                            ; movz w8, #(imm & 0xFFFF) as u32
                            ; movk w8, #((imm >> 16) & 0xFFFF) as u32, lsl #16
                            ; sxtw x8, w8
                            ; str x8, [x0, #dst * 8]
                        );
                    }
                    Op::LoadAbs64 { dst, imm } => {
                        let dst = dst as u32;
                        mov_imm64(&mut a, 13, imm);
                        dynasm!(a
                            ; .arch aarch64
                            ; mov w13, w13          // mask address to 32 bits (UXTW)
                            ; mov x14, #8            // width
                            ; mov x15, #0            // load
                            ; bl =>bounds_check_label
                            ; cbz w9, =>fault_label
                        );
                        emit_load(&mut a, 8, false);
                        dynasm!(a; .arch aarch64; str x8, [x0, #dst * 8]);
                    }
                    Op::StoreAbs64 { src, imm } => {
                        let src = src as u32;
                        mov_imm64(&mut a, 13, imm);
                        dynasm!(a
                            ; .arch aarch64
                            ; mov w13, w13
                            ; mov x14, #8
                            ; mov x15, #1            // store
                            ; bl =>bounds_check_label
                            ; cbz w9, =>fault_label
                            ; ldr x10, [x0, #src * 8]
                        );
                        emit_store(&mut a, 8);
                    }
                    Op::MoveReg { dst, src } => {
                        // reg[dst] = reg[src] (raw 64-bit copy, no sign extension).
                        let (dst, src) = (dst as u32, src as u32);
                        dynasm!(a
                            ; .arch aarch64
                            ; ldr x8, [x0, #src * 8]
                            ; str x8, [x0, #dst * 8]
                        );
                    }
                    Op::AddImm32 { dst, src, imm } => {
                        let (dst, src) = (dst as u32, src as u32);
                        dynasm!(a
                            ; .arch aarch64
                            ; ldr w8, [x0, #src * 8]
                            ; movz w9, #(imm & 0xFFFF) as u32
                            ; movk w9, #((imm >> 16) & 0xFFFF) as u32, lsl #16
                            ; add w8, w8, w9
                            ; sxtw x8, w8
                            ; str x8, [x0, #dst * 8]
                        );
                    }
                    Op::Shl64Imm { dst, src, imm } => {
                        // reg[dst] = reg[src] << (imm & 63).
                        let (dst, src) = (dst as u32, src as u32);
                        let shift = (imm & 63) as u32;
                        dynasm!(a
                            ; .arch aarch64
                            ; ldr x8, [x0, #src * 8]
                            ; lsl x8, x8, #shift
                            ; str x8, [x0, #dst * 8]
                        );
                    }
                    Op::And { dst, src, src2 } => {
                        let (dst, src, src2) = (dst as u32, src as u32, src2 as u32);
                        dynasm!(a
                            ; .arch aarch64
                            ; ldr x8, [x0, #src * 8]
                            ; ldr x9, [x0, #src2 * 8]
                            ; and x8, x8, x9
                            ; str x8, [x0, #dst * 8]
                        );
                    }
                    Op::Or { dst, src, src2 } => {
                        let (dst, src, src2) = (dst as u32, src as u32, src2 as u32);
                        dynasm!(a
                            ; .arch aarch64
                            ; ldr x8, [x0, #src * 8]
                            ; ldr x9, [x0, #src2 * 8]
                            ; orr x8, x8, x9
                            ; str x8, [x0, #dst * 8]
                        );
                    }
                    Op::CmovIfNotZero { dst, src, src2 } => {
                        // if reg[src2] != 0 then reg[dst] = reg[src] (else unchanged).
                        let (dst, src, src2) = (dst as u32, src as u32, src2 as u32);
                        let skip = a.new_dynamic_label();
                        dynasm!(a
                            ; .arch aarch64
                            ; ldr x9, [x0, #src2 * 8]
                            ; cbz x9, =>skip
                            ; ldr x8, [x0, #src * 8]
                            ; str x8, [x0, #dst * 8]
                            ; =>skip
                        );
                    }
                    Op::BranchEqImm { src, imm, target } => {
                        let src = src as u32;
                        let tgt = block_labels[blocks.block_of[target as usize]];
                        dynasm!(a; .arch aarch64; ldr x8, [x0, #src * 8]);
                        mov_imm64(&mut a, 9, imm);
                        dynasm!(a
                            ; .arch aarch64
                            ; cmp x8, x9
                            ; b.eq =>tgt
                        );
                        // not-taken: fall through to the next block (emitted next)
                    }
                    Op::BranchNeImm { src, imm, target } => {
                        let src = src as u32;
                        let tgt = block_labels[blocks.block_of[target as usize]];
                        dynasm!(a; .arch aarch64; ldr x8, [x0, #src * 8]);
                        mov_imm64(&mut a, 9, imm);
                        dynasm!(a
                            ; .arch aarch64
                            ; cmp x8, x9
                            ; b.ne =>tgt
                        );
                    }
                    Op::BranchCmpImm { src, imm, target, cond } => {
                        let src = src as u32;
                        let tgt = block_labels[blocks.block_of[target as usize]];
                        dynasm!(a; .arch aarch64; ldr x8, [x0, #src * 8]);
                        mov_imm64(&mut a, 9, imm);
                        dynasm!(a; .arch aarch64; cmp x8, x9);
                        emit_b_cond(&mut a, cond, tgt);
                    }
                    Op::BranchCmp { src, src2, target, cond } => {
                        let (src, src2) = (src as u32, src2 as u32);
                        let tgt = block_labels[blocks.block_of[target as usize]];
                        dynasm!(a
                            ; .arch aarch64
                            ; ldr x8, [x0, #src * 8]
                            ; ldr x9, [x0, #src2 * 8]
                            ; cmp x8, x9
                        );
                        emit_b_cond(&mut a, cond, tgt);
                    }
                    Op::SetCmpImm { dst, src, imm, cond } => {
                        let (dst, src) = (dst as u32, src as u32);
                        dynasm!(a; .arch aarch64; ldr x8, [x0, #src * 8]);
                        mov_imm64(&mut a, 9, imm);
                        dynasm!(a; .arch aarch64; cmp x8, x9);
                        emit_cset(&mut a, 8, cond);
                        dynasm!(a; .arch aarch64; str x8, [x0, #dst * 8]);
                    }
                    Op::SetCmp { dst, src, src2, cond } => {
                        let (dst, src, src2) = (dst as u32, src as u32, src2 as u32);
                        dynasm!(a
                            ; .arch aarch64
                            ; ldr x8, [x0, #src * 8]
                            ; ldr x9, [x0, #src2 * 8]
                            ; cmp x8, x9
                        );
                        emit_cset(&mut a, 8, cond);
                        dynasm!(a; .arch aarch64; str x8, [x0, #dst * 8]);
                    }
                    Op::ShiftRotateImm { dst, src, imm, kind, width } => {
                        let (dst, src) = (dst as u32, src as u32);
                        let mask: u64 = match width { Width::W32 => 31, Width::W64 => 63 };
                        let n = (imm & mask) as u32;
                        match width {
                            Width::W32 => dynasm!(a; .arch aarch64; ldr w8, [x0, #src * 8]),
                            Width::W64 => dynasm!(a; .arch aarch64; ldr x8, [x0, #src * 8]),
                        }
                        emit_shift_rotate(&mut a, kind, width, ShiftAmount::Imm(n));
                        match width {
                            Width::W32 => dynasm!(a; .arch aarch64; sxtw x8, w8; str x8, [x0, #dst * 8]),
                            Width::W64 => dynasm!(a; .arch aarch64; str x8, [x0, #dst * 8]),
                        }
                    }
                    Op::ShiftRotateImmAlt { dst, src, imm, kind, width } => {
                        let (dst, src) = (dst as u32, src as u32);
                        let mask: u32 = match width { Width::W32 => 31, Width::W64 => 63 };
                        let mask64: u64 = mask as u64;
                        match width {
                            Width::W32 => {
                                mov_imm32(&mut a, 8, imm as u32);
                                dynasm!(a
                                    ; .arch aarch64
                                    ; ldr w9, [x0, #src * 8]
                                    ; and w9, w9, #mask
                                );
                            }
                            Width::W64 => {
                                mov_imm64(&mut a, 8, imm);
                                dynasm!(a
                                    ; .arch aarch64
                                    ; ldr x9, [x0, #src * 8]
                                    ; and x9, x9, #mask64
                                );
                            }
                        }
                        emit_shift_rotate(&mut a, kind, width, ShiftAmount::Reg(9));
                        match width {
                            Width::W32 => dynasm!(a; .arch aarch64; sxtw x8, w8; str x8, [x0, #dst * 8]),
                            Width::W64 => dynasm!(a; .arch aarch64; str x8, [x0, #dst * 8]),
                        }
                    }
                    Op::ShiftRotateReg { dst, src, src2, kind, width } => {
                        let (dst, src, src2) = (dst as u32, src as u32, src2 as u32);
                        let mask: u32 = match width { Width::W32 => 31, Width::W64 => 63 };
                        let mask64: u64 = mask as u64;
                        match width {
                            Width::W32 => dynasm!(a
                                ; .arch aarch64
                                ; ldr w8, [x0, #src * 8]
                                ; ldr w9, [x0, #src2 * 8]
                                ; and w9, w9, #mask
                            ),
                            Width::W64 => dynasm!(a
                                ; .arch aarch64
                                ; ldr x8, [x0, #src * 8]
                                ; ldr x9, [x0, #src2 * 8]
                                ; and x9, x9, #mask64
                            ),
                        }
                        emit_shift_rotate(&mut a, kind, width, ShiftAmount::Reg(9));
                        match width {
                            Width::W32 => dynasm!(a; .arch aarch64; sxtw x8, w8; str x8, [x0, #dst * 8]),
                            Width::W64 => dynasm!(a; .arch aarch64; str x8, [x0, #dst * 8]),
                        }
                    }
                    Op::AluReg { dst, src, src2, kind, width } => {
                        let (dst, src, src2) = (dst as u32, src as u32, src2 as u32);
                        match width {
                            Width::W32 => dynasm!(a
                                ; .arch aarch64
                                ; ldr w8, [x0, #src * 8]
                                ; ldr w9, [x0, #src2 * 8]
                            ),
                            Width::W64 => dynasm!(a
                                ; .arch aarch64
                                ; ldr x8, [x0, #src * 8]
                                ; ldr x9, [x0, #src2 * 8]
                            ),
                        }
                        emit_alu(&mut a, kind, width);
                        match width {
                            Width::W32 => dynasm!(a; .arch aarch64; sxtw x8, w8; str x8, [x0, #dst * 8]),
                            Width::W64 => dynasm!(a; .arch aarch64; str x8, [x0, #dst * 8]),
                        }
                    }
                    Op::AluImm { dst, src, imm, kind, width } => {
                        let (dst, src) = (dst as u32, src as u32);
                        match width {
                            Width::W32 => {
                                dynasm!(a; .arch aarch64; ldr w8, [x0, #src * 8]);
                                mov_imm32(&mut a, 9, imm as u32);
                            }
                            Width::W64 => {
                                dynasm!(a; .arch aarch64; ldr x8, [x0, #src * 8]);
                                mov_imm64(&mut a, 9, imm);
                            }
                        }
                        emit_alu(&mut a, kind, width);
                        match width {
                            Width::W32 => dynasm!(a; .arch aarch64; sxtw x8, w8; str x8, [x0, #dst * 8]),
                            Width::W64 => dynasm!(a; .arch aarch64; str x8, [x0, #dst * 8]),
                        }
                    }
                    Op::NegateAndAddImm { dst, src, imm, width } => {
                        let (dst, src) = (dst as u32, src as u32);
                        match width {
                            Width::W32 => {
                                dynasm!(a; .arch aarch64; ldr w8, [x0, #src * 8]);
                                mov_imm32(&mut a, 9, imm as u32);
                                // x9(imm) - x8(src): operand order matters.
                                dynasm!(a; .arch aarch64; sub w8, w9, w8; sxtw x8, w8);
                            }
                            Width::W64 => {
                                dynasm!(a; .arch aarch64; ldr x8, [x0, #src * 8]);
                                mov_imm64(&mut a, 9, imm);
                                dynasm!(a; .arch aarch64; sub x8, x9, x8);
                            }
                        }
                        dynasm!(a; .arch aarch64; str x8, [x0, #dst * 8]);
                    }
                    Op::CountSetBits { dst, src, width } => {
                        let (dst, src) = (dst as u32, src as u32);
                        match width {
                            Width::W32 => dynasm!(a; .arch aarch64; ldr w8, [x0, #src * 8]),
                            Width::W64 => dynasm!(a; .arch aarch64; ldr x8, [x0, #src * 8]),
                        }
                        emit_popcount64(&mut a);
                        match width {
                            Width::W32 => dynasm!(a; .arch aarch64; sxtw x8, w8; str x8, [x0, #dst * 8]),
                            Width::W64 => dynasm!(a; .arch aarch64; str x8, [x0, #dst * 8]),
                        }
                    }
                    Op::CountLeadingZeroBits { dst, src, width } => {
                        let (dst, src) = (dst as u32, src as u32);
                        match width {
                            Width::W32 => dynasm!(a
                                ; .arch aarch64
                                ; ldr w8, [x0, #src * 8]
                                ; clz w8, w8
                                ; sxtw x8, w8
                            ),
                            Width::W64 => dynasm!(a
                                ; .arch aarch64
                                ; ldr x8, [x0, #src * 8]
                                ; clz x8, x8
                            ),
                        }
                        dynasm!(a; .arch aarch64; str x8, [x0, #dst * 8]);
                    }
                    Op::CountTrailingZeroBits { dst, src, width } => {
                        let (dst, src) = (dst as u32, src as u32);
                        match width {
                            Width::W32 => dynasm!(a
                                ; .arch aarch64
                                ; ldr w8, [x0, #src * 8]
                                ; rbit w8, w8
                                ; clz w8, w8
                                ; sxtw x8, w8
                            ),
                            Width::W64 => dynasm!(a
                                ; .arch aarch64
                                ; ldr x8, [x0, #src * 8]
                                ; rbit x8, x8
                                ; clz x8, x8
                            ),
                        }
                        dynasm!(a; .arch aarch64; str x8, [x0, #dst * 8]);
                    }
                    Op::SignExtend8 { dst, src } => {
                        let (dst, src) = (dst as u32, src as u32);
                        dynasm!(a
                            ; .arch aarch64
                            ; ldr w8, [x0, #src * 8]
                            ; sxtb x8, w8
                            ; str x8, [x0, #dst * 8]
                        );
                    }
                    Op::SignExtend16 { dst, src } => {
                        let (dst, src) = (dst as u32, src as u32);
                        dynasm!(a
                            ; .arch aarch64
                            ; ldr w8, [x0, #src * 8]
                            ; sxth x8, w8
                            ; str x8, [x0, #dst * 8]
                        );
                    }
                    Op::ZeroExtend16 { dst, src } => {
                        let (dst, src) = (dst as u32, src as u32);
                        dynasm!(a
                            ; .arch aarch64
                            ; ldr w8, [x0, #src * 8]
                            ; uxth w8, w8
                            ; str x8, [x0, #dst * 8]
                        );
                    }
                    Op::ReverseByte { dst, src } => {
                        let (dst, src) = (dst as u32, src as u32);
                        dynasm!(a
                            ; .arch aarch64
                            ; ldr x8, [x0, #src * 8]
                            ; rev x8, x8
                            ; str x8, [x0, #dst * 8]
                        );
                    }
                    Op::InvertedLogical { dst, src, src2, kind } => {
                        let (dst, src, src2) = (dst as u32, src as u32, src2 as u32);
                        dynasm!(a
                            ; .arch aarch64
                            ; ldr x8, [x0, #src * 8]
                            ; ldr x9, [x0, #src2 * 8]
                        );
                        match kind {
                            InvLogicalKind::AndNot => dynasm!(a; .arch aarch64; bic x8, x8, x9),
                            InvLogicalKind::OrNot => dynasm!(a; .arch aarch64; orn x8, x8, x9),
                            InvLogicalKind::Xnor => dynasm!(a; .arch aarch64; eon x8, x8, x9),
                        }
                        dynasm!(a; .arch aarch64; str x8, [x0, #dst * 8]);
                    }
                    Op::MinMax { dst, src, src2, kind } => {
                        let (dst, src, src2) = (dst as u32, src as u32, src2 as u32);
                        dynasm!(a
                            ; .arch aarch64
                            ; ldr x8, [x0, #src * 8]
                            ; ldr x9, [x0, #src2 * 8]
                            ; cmp x8, x9
                        );
                        match kind {
                            MinMaxKind::MaxSigned => dynasm!(a; .arch aarch64; csel x8, x8, x9, gt),
                            MinMaxKind::MaxUnsigned => dynasm!(a; .arch aarch64; csel x8, x8, x9, hi),
                            MinMaxKind::MinSigned => dynasm!(a; .arch aarch64; csel x8, x8, x9, lt),
                            MinMaxKind::MinUnsigned => dynasm!(a; .arch aarch64; csel x8, x8, x9, lo),
                        }
                        dynasm!(a; .arch aarch64; str x8, [x0, #dst * 8]);
                    }
                    Op::CmovIfZero { dst, src, src2 } => {
                        let (dst, src, src2) = (dst as u32, src as u32, src2 as u32);
                        let skip = a.new_dynamic_label();
                        dynasm!(a
                            ; .arch aarch64
                            ; ldr x9, [x0, #src2 * 8]
                            ; cbnz x9, =>skip
                            ; ldr x8, [x0, #src * 8]
                            ; str x8, [x0, #dst * 8]
                            ; =>skip
                        );
                    }
                    Op::CmovImm { dst, src, imm, zero_taken } => {
                        let (dst, src) = (dst as u32, src as u32);
                        let skip = a.new_dynamic_label();
                        dynasm!(a; .arch aarch64; ldr x9, [x0, #src * 8]);
                        if zero_taken {
                            dynasm!(a; .arch aarch64; cbnz x9, =>skip);
                        } else {
                            dynasm!(a; .arch aarch64; cbz x9, =>skip);
                        }
                        mov_imm64(&mut a, 8, imm);
                        dynasm!(a
                            ; .arch aarch64
                            ; str x8, [x0, #dst * 8]
                            ; =>skip
                        );
                    }
                }
            }
            let last_op_is_terminator = ops[hi - 1].is_terminator();
            let is_last_block = b + 1 == nblocks;
            if !last_op_is_terminator && is_last_block {
                dynasm!(a; .arch aarch64; b =>end_of_code_label);
            }
        }

        let fault_escalate_label = a.new_dynamic_label();
        dynasm!(a
            ; .arch aarch64
            ; =>end_of_code_label
        );
        mov_imm32(&mut a, 12, code_len);
        dynasm!(a
            ; .arch aarch64
            ; subs x11, x11, #1
            ; b.lt =>oog_label
            ; b =>panic_label
            ; =>oog_label
            ; str w12, [x6]      // out->pc = current instruction's pc
            ; str x11, [x1]      // gas is now -1 (the failing decrement)
            ; mov x30, x19
            ; movz w0, #EXIT_OOG as u32
            ; ret
            ; =>panic_label
            ; str w12, [x6]
            ; str x11, [x1]
            ; mov x30, x19
            ; movz w0, #EXIT_PANIC as u32
            ; ret
            ; =>fault_label
            ; str w12, [x6]
            ; str x11, [x1]
            ; mov x30, x19
            ; cbnz x16, =>fault_escalate_label
            ; str w10, [x6, #4]  // out->fault_page = computed fault page
            ; movz w0, #EXIT_FAULT as u32
            ; ret
            ; =>fault_escalate_label
            ; movz w0, #EXIT_PANIC as u32
            ; ret
            ; =>halt_label
            ; str w12, [x6]
            ; str x11, [x1]
            ; mov x30, x19
            ; movz w0, #EXIT_HALT as u32
            ; ret
        );

        emit_bounds_check_subroutine(&mut a, bounds_check_label);

        let buf = a.finalize().expect("finalize: unresolved labels or relocation range overflow");
        let code = buf.to_vec();
        (code, ops.len() as u32)
    }
}
