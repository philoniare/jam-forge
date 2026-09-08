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
