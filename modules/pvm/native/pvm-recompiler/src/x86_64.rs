//! x86-64 (System V) single-pass emitter. Same program shape, exits, gas accounting and memory-fault rules as the
//! AArch64 emitter ([`crate::aarch64`]), which is the reference for every op's semantics.
//!
//! Entry (System V AMD64): `f(regs, gas, regions, n_regions, backing, page_shift, out, entry_index, host_fn,
//! host_ctx)`: six arguments in rdi, rsi, rdx, rcx, r8, r9d and four on the stack.
//!
//! State kept for the whole run lives in callee-saved registers, so a host upcall needs no save/restore:
//!
//! | register | holds                                   | AArch64 counterpart |
//! |----------|-----------------------------------------|---------------------|
//! | rbx      | guest register file (`*mut [u64; 13]`)   | x0                  |
//! | r12      | gas, live                               | x11                 |
//! | r13d     | pc of the current instruction           | w12                 |
//! | r14      | page permission table (see below)       |                     |
//! | rbp      | backing buffer                          | x4                  |
//!
//! Frame, after six pushes and `sub rsp, 40` (rsp is then 16-byte aligned, as a call needs):
//! `[rsp]` gas cell, `[rsp+8]` page shift, `[rsp+16]` out, `[rsp+24]` host_fn, `[rsp+32]` host_ctx, `[rsp+40]`
//! region table, `[rsp+48]` region count.
//!
//! Memory accesses take a fast path through the page permission table (the eleventh argument): one byte per
//! 4 KiB guest page, 0 = take the slow path, 1 = readable, 2 = writable. A non-zero entry promises that the whole
//! page lies in a region of that permission whose bytes are at `backing + guest address`; the caller sets entries
//! only when it lays guest memory out that way. Anything else (an unmapped page, an access that straddles a page,
//! a caller with a packed backing buffer and an all-zero table) falls back to the region scan, which also works
//! out the fault.
//!
//! Scratch: rax (value, host address), rcx (second operand, shift count, bounds-check success flag), rdx (store
//! value, fault page), rsi (guest address), rdi (access width), r8 (1 = store), r9 (region index, escalate flag),
//! r10 (region pointer), r11 (dispatch index).
//!
//! The emitted code must have the same length whatever `entry_offsets_ptr` is: the program is emitted twice, once
//! to learn the entry offsets and once with the pointer to them. Every pointer is therefore loaded with a 10-byte
//! `mov r64, imm64`, and every jump to a label is rel32.
use crate::aarch64::analyze_blocks;
use crate::{
    AluKind, Backend, CmpCond, InvLogicalKind, MinMaxKind, MulUpperKind, Op, ShiftKind, Width, DJUMP_HALT,
    EXIT_FAULT, EXIT_HALT, EXIT_OOG, EXIT_PANIC, HOST_CONTINUE, HOST_OOG, HOST_OOG_NEXT, HOST_PANIC,
};
use dynasmrt::{dynasm, DynasmApi, DynasmLabelApi};

type Asm = dynasmrt::x64::Assembler;
type Label = dynasmrt::DynamicLabel;

pub struct X86_64Backend;

// Frame slots (from rsp, in the body of the function).
const GAS_CELL: i32 = 0;
const PAGE_SHIFT: i32 = 8;
const OUT: i32 = 16;
const HOST_FN: i32 = 24;
const HOST_CTX: i32 = 32;
const REGIONS: i32 = 40;
const N_REGIONS: i32 = 48;
const LOCALS: i32 = 56;
// Six callee-saved pushes and the return address lie between the locals and the stack arguments.
const STACK_ARGS: i32 = LOCALS + 6 * 8 + 8;

/// Byte offset of guest register `r` in the register file.
fn reg(r: u8) -> i32 {
    (r as i32) * 8
}

/// Load a 64-bit constant into host register `r` (0 = rax, 1 = rcx, 2 = rdx, 6 = rsi) with the shortest encoding:
/// most constants fit 32 bits, and a ten-byte `mov r64, imm64` for each one triples the code's size.
fn mov_imm(a: &mut Asm, r: u8, imm: u64) {
    if imm <= u32::MAX as u64 {
        dynasm!(a; .arch x64; mov Rd(r), DWORD imm as u32 as i32); // zero-extends
    } else if imm as i64 >= i32::MIN as i64 && (imm as i64) < 0 {
        dynasm!(a; .arch x64; mov Rq(r), DWORD imm as i64 as i32); // sign-extends
    } else {
        dynasm!(a; .arch x64; mov Rq(r), QWORD imm as i64);
    }
}

/// rax = sign_extend32(imm).
fn mov_rax_sext32(a: &mut Asm, imm: u32) {
    dynasm!(a; .arch x64;; mov_imm(a, 0, (imm as i32 as i64) as u64));
}

/// The bounds check. In: rsi = guest address (32-bit), rdi = width, r8 = 1 for a store. Out, found: ecx = 1 and
/// rax = host address. Out, not found: ecx = 0, edx = the faulting page address, r9d = 1 when that page is below
/// MinValidAddress (the fault is then a panic). Clobbers rax, rcx, rdx, r9, r10. Called with `call`, so the frame
/// slots are 8 bytes further from rsp inside it.
fn emit_bounds_check_subroutine(a: &mut Asm, entry: Label) {
    let loop_top = a.new_dynamic_label();
    let loop_next = a.new_dynamic_label();
    let found = a.new_dynamic_label();
    let scan_done = a.new_dynamic_label();
    dynasm!(a
        ; .arch x64
        ; =>entry
        ; xor r9d, r9d
        ; =>loop_top
        ; cmp r9, QWORD [rsp + N_REGIONS + 8]
        ; jae =>scan_done               // region table exhausted: not found
        ; mov r10, r9
        ; shl r10, 4                    // Region is 16 bytes: base, len, buf_offset, writable
        ; add r10, QWORD [rsp + REGIONS + 8]
        ; mov eax, DWORD [r10]          // region.base
        ; mov ecx, DWORD [r10 + 4]      // region.len
        ; add rcx, rax
        ; mov ecx, ecx                  // region_end as u32, as the AArch64 emitter has it
        ; lea rdx, [rsi + rdi]          // addr + width
        ; cmp rsi, rax
        ; jb =>loop_next                // addr < base
        ; cmp rdx, rcx
        ; ja =>loop_next                // addr + width > region_end
        ; mov ecx, DWORD [r10 + 12]     // region.writable
        ; test r8, r8
        ; jz =>found                    // load: containment is enough
        ; test ecx, ecx
        ; jnz =>found                   // store into a writable region
        // a store into a read-only region faults like an unmapped one
        ; =>loop_next
        ; inc r9
        ; jmp =>loop_top
        ; =>found
        ; mov ecx, DWORD [r10 + 8]      // region.buf_offset
        ; mov rdx, rsi
        ; sub rdx, rax                  // addr - region.base
        ; lea rax, [rbp + rcx]
        ; add rax, rdx                  // backing + buf_offset + (addr - base)
        ; mov ecx, 1
        ; ret
        ; =>scan_done
    );

    // Not found: the faulting page is addr's own page unless some permitted region holds byte `addr`, in which
    // case the span runs off the far end and the page of its last byte is the one that fails.
    let probe_top = a.new_dynamic_label();
    let probe_next = a.new_dynamic_label();
    let addr_page_ok = a.new_dynamic_label();
    let probe_done = a.new_dynamic_label();
    let compute_escalation = a.new_dynamic_label();
    let no_escalate = a.new_dynamic_label();
    dynasm!(a
        ; .arch x64
        ; mov rdx, rsi
        ; mov ecx, DWORD [rsp + PAGE_SHIFT + 8]
        ; shr rdx, cl
        ; shl rdx, cl                   // rdx = addr's page
        ; xor r9d, r9d
        ; =>probe_top
        ; cmp r9, QWORD [rsp + N_REGIONS + 8]
        ; jae =>probe_done
        ; mov r10, r9
        ; shl r10, 4
        ; add r10, QWORD [rsp + REGIONS + 8]
        ; mov eax, DWORD [r10]
        ; mov ecx, DWORD [r10 + 4]
        ; add rcx, rax
        ; mov ecx, ecx
        ; cmp rsi, rax
        ; jb =>probe_next
        ; cmp rsi, rcx
        ; jae =>probe_next
        ; mov ecx, DWORD [r10 + 12]
        ; test r8, r8
        ; jz =>addr_page_ok
        ; test ecx, ecx
        ; jnz =>addr_page_ok
        ; =>probe_next
        ; inc r9
        ; jmp =>probe_top
        ; =>probe_done
        ; xor ecx, ecx
        ; jmp =>compute_escalation
        ; =>addr_page_ok
        ; lea rdx, [rsi + rdi - 1]      // last byte of the span
        ; mov ecx, DWORD [rsp + PAGE_SHIFT + 8]
        ; shr rdx, cl
        ; shl rdx, cl
        ; xor ecx, ecx
        ; =>compute_escalation
        ; xor r9d, r9d
        ; cmp edx, DWORD 0x10000        // Abi.MinValidAddress
        ; jae =>no_escalate
        ; mov r9d, 1
        ; =>no_escalate
        ; ret
    );
}

/// rax = the `width` bytes at host address [rax], zero- or sign-extended.
fn emit_load(a: &mut Asm, width: u8, signed: bool) {
    match (width, signed) {
        (1, false) => dynasm!(a; .arch x64; movzx eax, BYTE [rax]),
        (2, false) => dynasm!(a; .arch x64; movzx eax, WORD [rax]),
        (4, false) => dynasm!(a; .arch x64; mov eax, DWORD [rax]),
        (1, true) => dynasm!(a; .arch x64; movsx rax, BYTE [rax]),
        (2, true) => dynasm!(a; .arch x64; movsx rax, WORD [rax]),
        (4, true) => dynasm!(a; .arch x64; movsxd rax, DWORD [rax]),
        (8, _) => dynasm!(a; .arch x64; mov rax, QWORD [rax]),
        _ => panic!("unsupported load width {width}"),
    }
}

/// Store the low `width` bytes of rdx at host address [rax].
fn emit_store(a: &mut Asm, width: u8) {
    match width {
        1 => dynasm!(a; .arch x64; mov BYTE [rax], dl),
        2 => dynasm!(a; .arch x64; mov WORD [rax], dx),
        4 => dynasm!(a; .arch x64; mov DWORD [rax], edx),
        8 => dynasm!(a; .arch x64; mov QWORD [rax], rdx),
        _ => panic!("unsupported store width {width}"),
    }
}

/// With rsi = guest address (any upper bits): mask it to 32 bits, bounds-check `width` bytes for a load or store,
/// and leave the host address in rax; an access that fails goes to `fault`.
fn emit_checked_access(a: &mut Asm, width: u8, store: bool, bounds_check: Label, fault: Label) {
    let slow = a.new_dynamic_label();
    let done = a.new_dynamic_label();
    let need: i8 = if store { 2 } else { 1 };
    dynasm!(a
        ; .arch x64
        ; mov esi, esi
        ; mov eax, esi
        ; shr eax, 12
        ; cmp BYTE [r14 + rax], BYTE need
        ; jb =>slow
    );
    if width > 1 {
        // The whole access must lie in the page that was checked.
        dynasm!(a
            ; .arch x64
            ; mov eax, esi
            ; and eax, DWORD 0xfff
            ; cmp eax, DWORD 0x1000 - width as i32
            ; ja =>slow
        );
    }
    dynasm!(a
        ; .arch x64
        ; lea rax, [rbp + rsi]
        ; jmp =>done
        ; =>slow
        ; mov edi, DWORD width as i32
        ; mov r8d, DWORD store as i32
        ; call =>bounds_check
        ; test ecx, ecx
        ; jz =>fault
        ; =>done
    );
}

/// Jump to `target` when `cond` holds for the preceding `cmp left, right`.
fn emit_jcc(a: &mut Asm, cond: CmpCond, target: Label) {
    match cond {
        CmpCond::LtU => dynasm!(a; .arch x64; jb =>target),
        CmpCond::LeU => dynasm!(a; .arch x64; jbe =>target),
        CmpCond::GeU => dynasm!(a; .arch x64; jae =>target),
        CmpCond::GtU => dynasm!(a; .arch x64; ja =>target),
        CmpCond::LtS => dynasm!(a; .arch x64; jl =>target),
        CmpCond::LeS => dynasm!(a; .arch x64; jle =>target),
        CmpCond::GeS => dynasm!(a; .arch x64; jge =>target),
        CmpCond::GtS => dynasm!(a; .arch x64; jg =>target),
    }
}

/// rax = 1 when `cond` holds for the preceding `cmp`, else 0.
fn emit_setcc_rax(a: &mut Asm, cond: CmpCond) {
    match cond {
        CmpCond::LtU => dynasm!(a; .arch x64; setb al),
        CmpCond::LeU => dynasm!(a; .arch x64; setbe al),
        CmpCond::GeU => dynasm!(a; .arch x64; setae al),
        CmpCond::GtU => dynasm!(a; .arch x64; seta al),
        CmpCond::LtS => dynasm!(a; .arch x64; setl al),
        CmpCond::LeS => dynasm!(a; .arch x64; setle al),
        CmpCond::GeS => dynasm!(a; .arch x64; setge al),
        CmpCond::GtS => dynasm!(a; .arch x64; setg al),
    }
    dynasm!(a; .arch x64; movzx eax, al);
}

/// rax = popcount(rax), by the same SWAR steps as the AArch64 emitter (no POPCNT requirement). Clobbers rcx, rdx.
fn emit_popcount64(a: &mut Asm) {
    dynasm!(a
        ; .arch x64
        ; mov rcx, QWORD 0x5555_5555_5555_5555u64 as i64
        ; mov rdx, rax
        ; and rdx, rcx
        ; shr rax, 1
        ; and rax, rcx
        ; add rax, rdx
        ; mov rcx, QWORD 0x3333_3333_3333_3333u64 as i64
        ; mov rdx, rax
        ; and rdx, rcx
        ; shr rax, 2
        ; and rax, rcx
        ; add rax, rdx
        ; mov rcx, QWORD 0x0f0f_0f0f_0f0f_0f0fu64 as i64
        ; mov rdx, rax
        ; shr rdx, 4
        ; add rax, rdx
        ; and rax, rcx
        ; mov rcx, QWORD 0x0101_0101_0101_0101u64 as i64
        ; imul rax, rcx
        ; shr rax, 56
    );
}

/// rax = rax `kind` rcx, in 32 or 64 bits (a 32-bit result is in eax, not yet sign-extended).
fn emit_alu(a: &mut Asm, kind: AluKind, width: Width) {
    match (width, kind) {
        (Width::W32, AluKind::Add) => dynasm!(a; .arch x64; add eax, ecx),
        (Width::W32, AluKind::Sub) => dynasm!(a; .arch x64; sub eax, ecx),
        (Width::W32, AluKind::Mul) => dynasm!(a; .arch x64; imul eax, ecx),
        (Width::W32, AluKind::And) => dynasm!(a; .arch x64; and eax, ecx),
        (Width::W32, AluKind::Or) => dynasm!(a; .arch x64; or eax, ecx),
        (Width::W32, AluKind::Xor) => dynasm!(a; .arch x64; xor eax, ecx),
        (Width::W64, AluKind::Add) => dynasm!(a; .arch x64; add rax, rcx),
        (Width::W64, AluKind::Sub) => dynasm!(a; .arch x64; sub rax, rcx),
        (Width::W64, AluKind::Mul) => dynasm!(a; .arch x64; imul rax, rcx),
        (Width::W64, AluKind::And) => dynasm!(a; .arch x64; and rax, rcx),
        (Width::W64, AluKind::Or) => dynasm!(a; .arch x64; or rax, rcx),
        (Width::W64, AluKind::Xor) => dynasm!(a; .arch x64; xor rax, rcx),
    }
}

/// rax (eax) = reg[src], rcx (ecx) = reg[src2].
fn emit_load_operands(a: &mut Asm, src: u8, src2: u8, width: Width) {
    match width {
        Width::W32 => dynasm!(a
            ; .arch x64
            ; mov eax, DWORD [rbx + reg(src)]
            ; mov ecx, DWORD [rbx + reg(src2)]
        ),
        Width::W64 => dynasm!(a
            ; .arch x64
            ; mov rax, QWORD [rbx + reg(src)]
            ; mov rcx, QWORD [rbx + reg(src2)]
        ),
    }
}

/// reg[dst] = rax, sign-extending eax first for a 32-bit result.
fn emit_store_result(a: &mut Asm, dst: u8, width: Width) {
    match width {
        Width::W32 => dynasm!(a; .arch x64; movsxd rax, eax; mov QWORD [rbx + reg(dst)], rax),
        Width::W64 => dynasm!(a; .arch x64; mov QWORD [rbx + reg(dst)], rax),
    }
}

/// rax = rax / rcx. Division by zero gives all ones; signed MIN / -1 gives MIN (the dividend).
fn emit_div(a: &mut Asm, width: Width, signed: bool) {
    let zero = a.new_dynamic_label();
    let done = a.new_dynamic_label();
    let divide = a.new_dynamic_label();
    match width {
        Width::W32 => {
            dynasm!(a; .arch x64; test ecx, ecx; jz =>zero);
            if signed {
                dynasm!(a
                    ; .arch x64
                    ; cmp eax, DWORD i32::MIN
                    ; jne =>divide
                    ; cmp ecx, DWORD -1
                    ; je =>done               // MIN / -1: eax already holds MIN
                    ; =>divide
                    ; cdq
                    ; idiv ecx
                    ; jmp =>done
                );
            } else {
                dynasm!(a; .arch x64; xor edx, edx; div ecx; jmp =>done);
            }
            dynasm!(a; .arch x64; =>zero; mov eax, DWORD -1; =>done);
        }
        Width::W64 => {
            dynasm!(a; .arch x64; test rcx, rcx; jz =>zero);
            if signed {
                dynasm!(a
                    ; .arch x64
                    ; mov rdx, QWORD i64::MIN
                    ; cmp rax, rdx
                    ; jne =>divide
                    ; cmp rcx, DWORD -1
                    ; je =>done               // MIN / -1: rax already holds MIN
                    ; =>divide
                    ; cqo
                    ; idiv rcx
                    ; jmp =>done
                );
            } else {
                dynasm!(a; .arch x64; xor edx, edx; div rcx; jmp =>done);
            }
            dynasm!(a; .arch x64; =>zero; mov rax, QWORD -1; =>done);
        }
    }
}

/// rax = rax % rcx. A zero divisor leaves the dividend; signed MIN % -1 gives 0.
fn emit_rem(a: &mut Asm, width: Width, signed: bool) {
    let done = a.new_dynamic_label();
    let divide = a.new_dynamic_label();
    match width {
        Width::W32 => {
            dynasm!(a; .arch x64; test ecx, ecx; jz =>done);
            if signed {
                dynasm!(a
                    ; .arch x64
                    ; cmp eax, DWORD i32::MIN
                    ; jne =>divide
                    ; cmp ecx, DWORD -1
                    ; jne =>divide
                    ; xor eax, eax            // MIN % -1 = 0
                    ; jmp =>done
                    ; =>divide
                    ; cdq
                    ; idiv ecx
                    ; mov eax, edx
                );
            } else {
                dynasm!(a; .arch x64; xor edx, edx; div ecx; mov eax, edx);
            }
            dynasm!(a; .arch x64; =>done);
        }
        Width::W64 => {
            dynasm!(a; .arch x64; test rcx, rcx; jz =>done);
            if signed {
                dynasm!(a
                    ; .arch x64
                    ; mov rdx, QWORD i64::MIN
                    ; cmp rax, rdx
                    ; jne =>divide
                    ; cmp rcx, DWORD -1
                    ; jne =>divide
                    ; xor eax, eax            // MIN % -1 = 0
                    ; jmp =>done
                    ; =>divide
                    ; cqo
                    ; idiv rcx
                    ; mov rax, rdx
                );
            } else {
                dynasm!(a; .arch x64; xor edx, edx; div rcx; mov rax, rdx);
            }
            dynasm!(a; .arch x64; =>done);
        }
    }
}

enum ShiftAmount {
    Imm(u32),
    /// The count is in cl, already masked to the width by the caller.
    Cl,
}

/// rax (eax) = rax (eax) shifted or rotated by `amount`.
fn emit_shift_rotate(a: &mut Asm, kind: ShiftKind, width: Width, amount: ShiftAmount) {
    match (width, kind, amount) {
        (Width::W64, ShiftKind::Shl, ShiftAmount::Imm(n)) => dynasm!(a; .arch x64; shl rax, n as i8),
        (Width::W64, ShiftKind::ShrLogical, ShiftAmount::Imm(n)) => dynasm!(a; .arch x64; shr rax, n as i8),
        (Width::W64, ShiftKind::ShrArith, ShiftAmount::Imm(n)) => dynasm!(a; .arch x64; sar rax, n as i8),
        (Width::W64, ShiftKind::RotateRight, ShiftAmount::Imm(n)) => dynasm!(a; .arch x64; ror rax, n as i8),
        (Width::W64, ShiftKind::RotateLeft, ShiftAmount::Imm(n)) => dynasm!(a; .arch x64; rol rax, n as i8),
        (Width::W64, ShiftKind::Shl, ShiftAmount::Cl) => dynasm!(a; .arch x64; shl rax, cl),
        (Width::W64, ShiftKind::ShrLogical, ShiftAmount::Cl) => dynasm!(a; .arch x64; shr rax, cl),
        (Width::W64, ShiftKind::ShrArith, ShiftAmount::Cl) => dynasm!(a; .arch x64; sar rax, cl),
        (Width::W64, ShiftKind::RotateRight, ShiftAmount::Cl) => dynasm!(a; .arch x64; ror rax, cl),
        (Width::W64, ShiftKind::RotateLeft, ShiftAmount::Cl) => dynasm!(a; .arch x64; rol rax, cl),
        (Width::W32, ShiftKind::Shl, ShiftAmount::Imm(n)) => dynasm!(a; .arch x64; shl eax, n as i8),
        (Width::W32, ShiftKind::ShrLogical, ShiftAmount::Imm(n)) => dynasm!(a; .arch x64; shr eax, n as i8),
        (Width::W32, ShiftKind::ShrArith, ShiftAmount::Imm(n)) => dynasm!(a; .arch x64; sar eax, n as i8),
        (Width::W32, ShiftKind::RotateRight, ShiftAmount::Imm(n)) => dynasm!(a; .arch x64; ror eax, n as i8),
        (Width::W32, ShiftKind::RotateLeft, ShiftAmount::Imm(n)) => dynasm!(a; .arch x64; rol eax, n as i8),
        (Width::W32, ShiftKind::Shl, ShiftAmount::Cl) => dynasm!(a; .arch x64; shl eax, cl),
        (Width::W32, ShiftKind::ShrLogical, ShiftAmount::Cl) => dynasm!(a; .arch x64; shr eax, cl),
        (Width::W32, ShiftKind::ShrArith, ShiftAmount::Cl) => dynasm!(a; .arch x64; sar eax, cl),
        (Width::W32, ShiftKind::RotateRight, ShiftAmount::Cl) => dynasm!(a; .arch x64; ror eax, cl),
        (Width::W32, ShiftKind::RotateLeft, ShiftAmount::Cl) => dynasm!(a; .arch x64; rol eax, cl),
    }
}

/// Set the pc to `pc` and charge the block's gas: out of gas (leaving the cost uncharged) when gas < cost.
fn emit_block_gas_charge(a: &mut Asm, cost: i64, pc: u32, oog: Label) {
    dynasm!(a; .arch x64; mov r13d, DWORD pc as i32);
    if cost <= 0 {
        return;
    }
    dynasm!(a
        ; .arch x64
        ;; mov_imm(a, 1, (cost) as u64)
        ; cmp r12, rcx
        ; jl =>oog
        ; sub r12, rcx
    );
}

/// r12 = *gas (after an upcall, which may have charged gas).
fn emit_reload_gas(a: &mut Asm) {
    dynasm!(a; .arch x64; mov rcx, QWORD [rsp + GAS_CELL]; mov r12, QWORD [rcx]);
}

/// Call `host_fn(host_ctx, host_id, pc)` and act on its status.
fn emit_host_call(a: &mut Asm, host_id: u64, panic: Label, oog: Label, next_pc: u32) {
    let not_panic = a.new_dynamic_label();
    let not_oog = a.new_dynamic_label();
    let not_oog_next = a.new_dynamic_label();
    let is_continue = a.new_dynamic_label();
    dynasm!(a
        ; .arch x64
        ; mov rax, QWORD [rsp + GAS_CELL]
        ; mov QWORD [rax], r12           // the handler reads and charges gas through the cell
        ; mov rdi, QWORD [rsp + HOST_CTX]
        ;; mov_imm(a, 6, (host_id as i64) as u64)
        ; mov edx, r13d
        ; call QWORD [rsp + HOST_FN]
        ; cmp rax, DWORD HOST_PANIC as i32
        ; jne =>not_panic
    );
    emit_reload_gas(a);
    dynasm!(a; .arch x64; jmp =>panic; =>not_panic; cmp rax, DWORD HOST_OOG as i32; jne =>not_oog);
    emit_reload_gas(a);
    dynasm!(a; .arch x64; jmp =>oog; =>not_oog; cmp rax, DWORD HOST_OOG_NEXT as i32; jne =>not_oog_next);
    emit_reload_gas(a);
    dynasm!(a
        ; .arch x64
        ; mov r13d, DWORD next_pc as i32
        ; jmp =>oog
        ; =>not_oog_next
        ; cmp rax, DWORD HOST_CONTINUE as i32
        ; je =>is_continue
    );
    emit_reload_gas(a);
    dynasm!(a; .arch x64; jmp =>panic; =>is_continue);
    emit_reload_gas(a);
}

/// With eax = the 32-bit jump address: halt on the halt sentinel, panic on an address that is zero, odd, outside
/// the jump table or mapped to no instruction, else continue at the instruction the table names.
#[allow(clippy::too_many_arguments)]
fn emit_indirect_jump(
    a: &mut Asm,
    jump_table_len: u32,
    jump_table_ptr: *const u32,
    halt: Label,
    panic: Label,
    dispatch_by_index: Label,
) {
    dynasm!(a
        ; .arch x64
        ; mov rcx, QWORD DJUMP_HALT as i64
        ; cmp rax, rcx
        ; je =>halt
        ; test eax, eax
        ; jz =>panic
        ; test eax, DWORD 1
        ; jnz =>panic
        ; mov ecx, eax
        ; shr ecx, 1
        ; sub ecx, 1                     // idx = addr / 2 - 1
        ; cmp ecx, DWORD jump_table_len as i32
        ; jae =>panic
        ; mov rsi, QWORD jump_table_ptr as i64
        ; mov r11d, DWORD [rsi + rcx * 4]
        ; cmp r11d, DWORD -1             // u32::MAX: no instruction starts there
        ; je =>panic
        ; jmp =>dispatch_by_index
    );
}

/// Store pc and gas to their cells, restore the caller's registers and return `exit`.
fn emit_exit(a: &mut Asm, exit: u32) {
    dynasm!(a
        ; .arch x64
        ; mov rax, QWORD [rsp + OUT]
        ; mov DWORD [rax], r13d          // out->pc
        ; mov rax, QWORD [rsp + GAS_CELL]
        ; mov QWORD [rax], r12
        ; add rsp, LOCALS
        ; pop r15
        ; pop r14
        ; pop r13
        ; pop r12
        ; pop rbp
        ; pop rbx
        ; mov eax, DWORD exit as i32
        ; ret
    );
}

impl Backend for X86_64Backend {
    fn emit_program(
        &self,
        ops: &[Op],
        pcs: &[u32],
        block_gas: &[i64],
        jump_table: &[u32],
        jump_table_ptr: *const u32,
        entry_offsets_ptr: *const u32,
        code_len: u32,
    ) -> Option<(Vec<u8>, u32, Vec<u32>)> {
        let mut a = Asm::new().expect("dynasm assembler alloc");

        if ops.is_empty() {
            // Empty program: an immediate PANIC at pc 0. `out` is the seventh argument, the first on the stack.
            dynasm!(a
                ; .arch x64
                ; mov rax, QWORD [rsp + 8]
                ; mov DWORD [rax], 0
                ; mov eax, DWORD EXIT_PANIC as i32
                ; ret
            );
            a.commit().ok()?;
            let buf = a.finalize().ok()?;
            return Some((buf.to_vec(), 0, Vec::new()));
        }

        let blocks = analyze_blocks(ops, jump_table);
        let nblocks = blocks.block_lo.len();
        let block_labels: Vec<Label> = (0..nblocks).map(|_| a.new_dynamic_label()).collect();
        let instr_labels: Vec<Label> = (0..ops.len()).map(|_| a.new_dynamic_label()).collect();
        let oog_label = a.new_dynamic_label();
        let fault_label = a.new_dynamic_label();
        let panic_label = a.new_dynamic_label();
        let halt_label = a.new_dynamic_label();
        let bounds_check_label = a.new_dynamic_label();
        let func_start_label = a.new_dynamic_label();
        let dispatch_by_index_label = a.new_dynamic_label();
        let end_of_code_label = a.new_dynamic_label();
        let entry_offsets_len = ops.len() as u32;

        dynasm!(a
            ; .arch x64
            ; =>func_start_label
            ; push rbx
            ; push rbp
            ; push r12
            ; push r13
            ; push r14
            ; push r15
            ; sub rsp, LOCALS
            ; mov rbx, rdi                          // guest registers
            ; mov QWORD [rsp + GAS_CELL], rsi
            ; mov r12, QWORD [rsi]                  // gas, live from here on
            ; mov QWORD [rsp + REGIONS], rdx
            ; mov QWORD [rsp + N_REGIONS], rcx
            ; mov r14, QWORD [rsp + STACK_ARGS + 32] // page permissions
            ; mov rbp, r8                           // backing
            ; mov DWORD [rsp + PAGE_SHIFT], r9d
            ; mov rax, QWORD [rsp + STACK_ARGS]
            ; mov QWORD [rsp + OUT], rax
            ; mov r11d, DWORD [rsp + STACK_ARGS + 8] // entry index
            ; mov rax, QWORD [rsp + STACK_ARGS + 16]
            ; mov QWORD [rsp + HOST_FN], rax
            ; mov rax, QWORD [rsp + STACK_ARGS + 24]
            ; mov QWORD [rsp + HOST_CTX], rax
            ; xor r13d, r13d
            // Continue at the instruction whose index is in r11d: the entry point, and every indirect jump.
            ; =>dispatch_by_index_label
            ; cmp r11d, DWORD entry_offsets_len as i32
            ; jae =>panic_label
            ; mov rsi, QWORD entry_offsets_ptr as i64
            ; mov ecx, DWORD [rsi + r11 * 4]        // bytes from the function start
            ; lea rax, [=>func_start_label]
            ; add rax, rcx
            ; jmp rax
        );

        let mut entry_offsets: Vec<u32> = vec![0u32; ops.len()];
        for b in 0..nblocks {
            dynasm!(a; .arch x64; =>block_labels[b]);
            let lo = blocks.block_lo[b];
            let hi = blocks.block_hi[b];
            entry_offsets[lo] = a.offset().0 as u32;
            emit_block_gas_charge(&mut a, block_gas[lo], pcs[lo], oog_label);

            for pc in lo..hi {
                let this_pc = pcs[pc] as i32;
                dynasm!(a; .arch x64; =>instr_labels[pc]; mov r13d, DWORD this_pc);
                match ops[pc] {
                    Op::LoadImm64 { dst, imm } => {
                        dynasm!(a; .arch x64;; mov_imm(&mut a, 0, (imm as i64) as u64); mov QWORD [rbx + reg(dst)], rax);
                    }
                    Op::AddImm64 { dst, src, imm } => {
                        dynasm!(a
                            ; .arch x64
                            ; mov rax, QWORD [rbx + reg(src)]
                            ;; mov_imm(&mut a, 1, (imm as i64) as u64)
                            ; add rax, rcx
                            ; mov QWORD [rbx + reg(dst)], rax
                        );
                    }
                    Op::Add { dst, src, src2 } => {
                        emit_load_operands(&mut a, src, src2, Width::W64);
                        dynasm!(a; .arch x64; add rax, rcx; mov QWORD [rbx + reg(dst)], rax);
                    }
                    Op::Sub { dst, src, src2 } => {
                        emit_load_operands(&mut a, src, src2, Width::W64);
                        dynasm!(a; .arch x64; sub rax, rcx; mov QWORD [rbx + reg(dst)], rax);
                    }
                    Op::Mul { dst, src, src2 } => {
                        emit_load_operands(&mut a, src, src2, Width::W64);
                        dynasm!(a; .arch x64; imul rax, rcx; mov QWORD [rbx + reg(dst)], rax);
                    }
                    Op::Load { dst, src, imm, width, signed } => {
                        dynasm!(a
                            ; .arch x64
                            ; mov rsi, QWORD [rbx + reg(src)]
                            ;; mov_imm(&mut a, 1, (imm as i64) as u64)
                            ; add rsi, rcx
                        );
                        emit_checked_access(&mut a, width, false, bounds_check_label, fault_label);
                        emit_load(&mut a, width, signed);
                        dynasm!(a; .arch x64; mov QWORD [rbx + reg(dst)], rax);
                    }
                    Op::Store { dst, src, imm, width } => {
                        dynasm!(a
                            ; .arch x64
                            ; mov rsi, QWORD [rbx + reg(src)]
                            ;; mov_imm(&mut a, 1, (imm as i64) as u64)
                            ; add rsi, rcx
                        );
                        emit_checked_access(&mut a, width, true, bounds_check_label, fault_label);
                        dynasm!(a; .arch x64; mov rdx, QWORD [rbx + reg(dst)]);
                        emit_store(&mut a, width);
                    }
                    Op::Trap => dynasm!(a; .arch x64; jmp =>panic_label),
                    Op::Ecalli { host_id } => {
                        let next_pc = if pc + 1 < ops.len() { pcs[pc + 1] } else { code_len };
                        emit_host_call(&mut a, host_id, panic_label, oog_label, next_pc);
                    }
                    Op::Unlikely => {}
                    Op::Fallthrough => {
                        if pc + 1 < ops.len() {
                            let tgt = block_labels[blocks.block_of[pc + 1]];
                            dynasm!(a; .arch x64; jmp =>tgt);
                        } else {
                            dynasm!(a; .arch x64; jmp =>end_of_code_label);
                        }
                    }
                    Op::Jump { target } => {
                        let tgt = block_labels[blocks.block_of[target as usize]];
                        dynasm!(a; .arch x64; jmp =>tgt);
                    }
                    Op::BranchEq { src, src2, target } => {
                        let tgt = block_labels[blocks.block_of[target as usize]];
                        emit_load_operands(&mut a, src, src2, Width::W64);
                        dynasm!(a; .arch x64; cmp rax, rcx; je =>tgt);
                    }
                    Op::BranchNe { src, src2, target } => {
                        let tgt = block_labels[blocks.block_of[target as usize]];
                        emit_load_operands(&mut a, src, src2, Width::W64);
                        dynasm!(a; .arch x64; cmp rax, rcx; jne =>tgt);
                    }
                    Op::BranchEqImm { src, imm, target } => {
                        let tgt = block_labels[blocks.block_of[target as usize]];
                        dynasm!(a
                            ; .arch x64
                            ; mov rax, QWORD [rbx + reg(src)]
                            ;; mov_imm(&mut a, 1, (imm as i64) as u64)
                            ; cmp rax, rcx
                            ; je =>tgt
                        );
                    }
                    Op::BranchNeImm { src, imm, target } => {
                        let tgt = block_labels[blocks.block_of[target as usize]];
                        dynasm!(a
                            ; .arch x64
                            ; mov rax, QWORD [rbx + reg(src)]
                            ;; mov_imm(&mut a, 1, (imm as i64) as u64)
                            ; cmp rax, rcx
                            ; jne =>tgt
                        );
                    }
                    Op::BranchCmpImm { src, imm, target, cond } => {
                        let tgt = block_labels[blocks.block_of[target as usize]];
                        dynasm!(a
                            ; .arch x64
                            ; mov rax, QWORD [rbx + reg(src)]
                            ;; mov_imm(&mut a, 1, (imm as i64) as u64)
                            ; cmp rax, rcx
                        );
                        emit_jcc(&mut a, cond, tgt);
                    }
                    Op::BranchCmp { src, src2, target, cond } => {
                        let tgt = block_labels[blocks.block_of[target as usize]];
                        emit_load_operands(&mut a, src, src2, Width::W64);
                        dynasm!(a; .arch x64; cmp rax, rcx);
                        emit_jcc(&mut a, cond, tgt);
                    }
                    Op::Djump { src, imm } => {
                        dynasm!(a
                            ; .arch x64
                            ; mov rax, QWORD [rbx + reg(src)]
                            ;; mov_imm(&mut a, 1, (imm as i64) as u64)
                            ; add rax, rcx
                            ; mov eax, eax                // the address is 32 bits
                        );
                        emit_indirect_jump(
                            &mut a,
                            jump_table.len() as u32,
                            jump_table_ptr,
                            halt_label,
                            panic_label,
                            dispatch_by_index_label,
                        );
                    }
                    Op::LoadImm32 { dst, imm } => {
                        mov_rax_sext32(&mut a, imm);
                        dynasm!(a; .arch x64; mov QWORD [rbx + reg(dst)], rax);
                    }
                    Op::LoadAbs64 { dst, imm } => {
                        dynasm!(a; .arch x64;; mov_imm(&mut a, 6, (imm as i64) as u64));
                        emit_checked_access(&mut a, 8, false, bounds_check_label, fault_label);
                        emit_load(&mut a, 8, false);
                        dynasm!(a; .arch x64; mov QWORD [rbx + reg(dst)], rax);
                    }
                    Op::StoreAbs64 { src, imm } => {
                        dynasm!(a; .arch x64;; mov_imm(&mut a, 6, (imm as i64) as u64));
                        emit_checked_access(&mut a, 8, true, bounds_check_label, fault_label);
                        dynasm!(a; .arch x64; mov rdx, QWORD [rbx + reg(src)]);
                        emit_store(&mut a, 8);
                    }
                    Op::MoveReg { dst, src } => {
                        dynasm!(a
                            ; .arch x64
                            ; mov rax, QWORD [rbx + reg(src)]
                            ; mov QWORD [rbx + reg(dst)], rax
                        );
                    }
                    Op::AddImm32 { dst, src, imm } => {
                        dynasm!(a
                            ; .arch x64
                            ; mov eax, DWORD [rbx + reg(src)]
                            ; add eax, DWORD imm as i32
                        );
                        emit_store_result(&mut a, dst, Width::W32);
                    }
                    Op::Shl64Imm { dst, src, imm } => {
                        let shift = (imm & 63) as i8;
                        dynasm!(a
                            ; .arch x64
                            ; mov rax, QWORD [rbx + reg(src)]
                            ; shl rax, shift
                            ; mov QWORD [rbx + reg(dst)], rax
                        );
                    }
                    Op::And { dst, src, src2 } => {
                        emit_load_operands(&mut a, src, src2, Width::W64);
                        dynasm!(a; .arch x64; and rax, rcx; mov QWORD [rbx + reg(dst)], rax);
                    }
                    Op::Or { dst, src, src2 } => {
                        emit_load_operands(&mut a, src, src2, Width::W64);
                        dynasm!(a; .arch x64; or rax, rcx; mov QWORD [rbx + reg(dst)], rax);
                    }
                    Op::CmovIfNotZero { dst, src, src2 } => {
                        let skip = a.new_dynamic_label();
                        dynasm!(a
                            ; .arch x64
                            ; mov rcx, QWORD [rbx + reg(src2)]
                            ; test rcx, rcx
                            ; jz =>skip
                            ; mov rax, QWORD [rbx + reg(src)]
                            ; mov QWORD [rbx + reg(dst)], rax
                            ; =>skip
                        );
                    }
                    Op::CmovIfZero { dst, src, src2 } => {
                        let skip = a.new_dynamic_label();
                        dynasm!(a
                            ; .arch x64
                            ; mov rcx, QWORD [rbx + reg(src2)]
                            ; test rcx, rcx
                            ; jnz =>skip
                            ; mov rax, QWORD [rbx + reg(src)]
                            ; mov QWORD [rbx + reg(dst)], rax
                            ; =>skip
                        );
                    }
                    Op::CmovImm { dst, src, imm, zero_taken } => {
                        let skip = a.new_dynamic_label();
                        dynasm!(a; .arch x64; mov rcx, QWORD [rbx + reg(src)]; test rcx, rcx);
                        if zero_taken {
                            dynasm!(a; .arch x64; jnz =>skip);
                        } else {
                            dynasm!(a; .arch x64; jz =>skip);
                        }
                        dynasm!(a
                            ; .arch x64
                            ;; mov_imm(&mut a, 0, (imm as i64) as u64)
                            ; mov QWORD [rbx + reg(dst)], rax
                            ; =>skip
                        );
                    }
                    Op::SetCmpImm { dst, src, imm, cond } => {
                        dynasm!(a
                            ; .arch x64
                            ; mov rax, QWORD [rbx + reg(src)]
                            ;; mov_imm(&mut a, 1, (imm as i64) as u64)
                            ; cmp rax, rcx
                        );
                        emit_setcc_rax(&mut a, cond);
                        dynasm!(a; .arch x64; mov QWORD [rbx + reg(dst)], rax);
                    }
                    Op::SetCmp { dst, src, src2, cond } => {
                        emit_load_operands(&mut a, src, src2, Width::W64);
                        dynasm!(a; .arch x64; cmp rax, rcx);
                        emit_setcc_rax(&mut a, cond);
                        dynasm!(a; .arch x64; mov QWORD [rbx + reg(dst)], rax);
                    }
                    Op::ShiftRotateImm { dst, src, imm, kind, width } => {
                        let mask: u64 = match width { Width::W32 => 31, Width::W64 => 63 };
                        let n = (imm & mask) as u32;
                        match width {
                            Width::W32 => dynasm!(a; .arch x64; mov eax, DWORD [rbx + reg(src)]),
                            Width::W64 => dynasm!(a; .arch x64; mov rax, QWORD [rbx + reg(src)]),
                        }
                        emit_shift_rotate(&mut a, kind, width, ShiftAmount::Imm(n));
                        emit_store_result(&mut a, dst, width);
                    }
                    Op::ShiftRotateImmAlt { dst, src, imm, kind, width } => {
                        // The immediate is shifted by the register.
                        match width {
                            Width::W32 => dynasm!(a
                                ; .arch x64
                                ; mov eax, DWORD imm as u32 as i32
                                ; mov ecx, DWORD [rbx + reg(src)]
                                ; and ecx, DWORD 31
                            ),
                            Width::W64 => dynasm!(a
                                ; .arch x64
                                ;; mov_imm(&mut a, 0, (imm as i64) as u64)
                                ; mov rcx, QWORD [rbx + reg(src)]
                                ; and ecx, DWORD 63
                            ),
                        }
                        emit_shift_rotate(&mut a, kind, width, ShiftAmount::Cl);
                        emit_store_result(&mut a, dst, width);
                    }
                    Op::ShiftRotateReg { dst, src, src2, kind, width } => {
                        emit_load_operands(&mut a, src, src2, width);
                        match width {
                            Width::W32 => dynasm!(a; .arch x64; and ecx, DWORD 31),
                            Width::W64 => dynasm!(a; .arch x64; and ecx, DWORD 63),
                        }
                        emit_shift_rotate(&mut a, kind, width, ShiftAmount::Cl);
                        emit_store_result(&mut a, dst, width);
                    }
                    Op::AluReg { dst, src, src2, kind, width } => {
                        emit_load_operands(&mut a, src, src2, width);
                        emit_alu(&mut a, kind, width);
                        emit_store_result(&mut a, dst, width);
                    }
                    Op::AluImm { dst, src, imm, kind, width } => {
                        match width {
                            Width::W32 => dynasm!(a
                                ; .arch x64
                                ; mov eax, DWORD [rbx + reg(src)]
                                ; mov ecx, DWORD imm as u32 as i32
                            ),
                            Width::W64 => dynasm!(a
                                ; .arch x64
                                ; mov rax, QWORD [rbx + reg(src)]
                                ;; mov_imm(&mut a, 1, (imm as i64) as u64)
                            ),
                        }
                        emit_alu(&mut a, kind, width);
                        emit_store_result(&mut a, dst, width);
                    }
                    Op::NegateAndAddImm { dst, src, imm, width } => {
                        // imm minus reg: the operand order matters.
                        match width {
                            Width::W32 => dynasm!(a
                                ; .arch x64
                                ; mov ecx, DWORD [rbx + reg(src)]
                                ; mov eax, DWORD imm as u32 as i32
                                ; sub eax, ecx
                            ),
                            Width::W64 => dynasm!(a
                                ; .arch x64
                                ; mov rcx, QWORD [rbx + reg(src)]
                                ;; mov_imm(&mut a, 0, (imm as i64) as u64)
                                ; sub rax, rcx
                            ),
                        }
                        emit_store_result(&mut a, dst, width);
                    }
                    Op::CountSetBits { dst, src, width } => {
                        match width {
                            Width::W32 => dynasm!(a; .arch x64; mov eax, DWORD [rbx + reg(src)]),
                            Width::W64 => dynasm!(a; .arch x64; mov rax, QWORD [rbx + reg(src)]),
                        }
                        emit_popcount64(&mut a);
                        emit_store_result(&mut a, dst, width);
                    }
                    Op::CountLeadingZeroBits { dst, src, width } => {
                        // BSR, not LZCNT: no CPU feature needed, and zero is handled apart (BSR leaves it undefined).
                        let done = a.new_dynamic_label();
                        match width {
                            Width::W32 => dynasm!(a
                                ; .arch x64
                                ; mov ecx, DWORD [rbx + reg(src)]
                                ; mov eax, DWORD 32
                                ; test ecx, ecx
                                ; jz =>done
                                ; bsr ecx, ecx
                                ; mov eax, DWORD 31
                                ; sub eax, ecx
                                ; =>done
                            ),
                            Width::W64 => dynasm!(a
                                ; .arch x64
                                ; mov rcx, QWORD [rbx + reg(src)]
                                ; mov eax, DWORD 64
                                ; test rcx, rcx
                                ; jz =>done
                                ; bsr rcx, rcx
                                ; mov eax, DWORD 63
                                ; sub eax, ecx
                                ; =>done
                            ),
                        }
                        dynasm!(a; .arch x64; mov QWORD [rbx + reg(dst)], rax);
                    }
                    Op::CountTrailingZeroBits { dst, src, width } => {
                        let done = a.new_dynamic_label();
                        match width {
                            Width::W32 => dynasm!(a
                                ; .arch x64
                                ; mov ecx, DWORD [rbx + reg(src)]
                                ; mov eax, DWORD 32
                                ; test ecx, ecx
                                ; jz =>done
                                ; bsf eax, ecx
                                ; =>done
                            ),
                            Width::W64 => dynasm!(a
                                ; .arch x64
                                ; mov rcx, QWORD [rbx + reg(src)]
                                ; mov eax, DWORD 64
                                ; test rcx, rcx
                                ; jz =>done
                                ; bsf rax, rcx
                                ; =>done
                            ),
                        }
                        dynasm!(a; .arch x64; mov QWORD [rbx + reg(dst)], rax);
                    }
                    Op::SignExtend8 { dst, src } => {
                        dynasm!(a
                            ; .arch x64
                            ; movsx rax, BYTE [rbx + reg(src)]
                            ; mov QWORD [rbx + reg(dst)], rax
                        );
                    }
                    Op::SignExtend16 { dst, src } => {
                        dynasm!(a
                            ; .arch x64
                            ; movsx rax, WORD [rbx + reg(src)]
                            ; mov QWORD [rbx + reg(dst)], rax
                        );
                    }
                    Op::ZeroExtend16 { dst, src } => {
                        dynasm!(a
                            ; .arch x64
                            ; movzx eax, WORD [rbx + reg(src)]
                            ; mov QWORD [rbx + reg(dst)], rax
                        );
                    }
                    Op::ReverseByte { dst, src } => {
                        dynasm!(a
                            ; .arch x64
                            ; mov rax, QWORD [rbx + reg(src)]
                            ; bswap rax
                            ; mov QWORD [rbx + reg(dst)], rax
                        );
                    }
                    Op::InvertedLogical { dst, src, src2, kind } => {
                        emit_load_operands(&mut a, src, src2, Width::W64);
                        match kind {
                            InvLogicalKind::AndNot => dynasm!(a; .arch x64; not rcx; and rax, rcx),
                            InvLogicalKind::OrNot => dynasm!(a; .arch x64; not rcx; or rax, rcx),
                            InvLogicalKind::Xnor => dynasm!(a; .arch x64; xor rax, rcx; not rax),
                        }
                        dynasm!(a; .arch x64; mov QWORD [rbx + reg(dst)], rax);
                    }
                    Op::MinMax { dst, src, src2, kind } => {
                        emit_load_operands(&mut a, src, src2, Width::W64);
                        dynasm!(a; .arch x64; cmp rax, rcx);
                        // rax stays when it is the wanted one; otherwise rcx replaces it.
                        match kind {
                            MinMaxKind::MaxSigned => dynasm!(a; .arch x64; cmovle rax, rcx),
                            MinMaxKind::MaxUnsigned => dynasm!(a; .arch x64; cmovbe rax, rcx),
                            MinMaxKind::MinSigned => dynasm!(a; .arch x64; cmovge rax, rcx),
                            MinMaxKind::MinUnsigned => dynasm!(a; .arch x64; cmovae rax, rcx),
                        }
                        dynasm!(a; .arch x64; mov QWORD [rbx + reg(dst)], rax);
                    }
                    Op::Div { dst, src, src2, width, signed } => {
                        emit_load_operands(&mut a, src, src2, width);
                        emit_div(&mut a, width, signed);
                        emit_store_result(&mut a, dst, width);
                    }
                    Op::Rem { dst, src, src2, width, signed } => {
                        emit_load_operands(&mut a, src, src2, width);
                        emit_rem(&mut a, width, signed);
                        emit_store_result(&mut a, dst, width);
                    }
                    Op::MulUpper { dst, src, src2, kind } => {
                        emit_load_operands(&mut a, src, src2, Width::W64);
                        match kind {
                            MulUpperKind::SignedSigned => dynasm!(a; .arch x64; imul rcx; mov rax, rdx),
                            MulUpperKind::UnsignedUnsigned => dynasm!(a; .arch x64; mul rcx; mov rax, rdx),
                            MulUpperKind::SignedUnsigned => {
                                // high(a_signed * b_unsigned) = umulh(a, b) - (a < 0 ? b : 0)
                                let skip = a.new_dynamic_label();
                                dynasm!(a
                                    ; .arch x64
                                    ; mov rsi, rax
                                    ; mul rcx
                                    ; test rsi, rsi
                                    ; jns =>skip
                                    ; sub rdx, rcx
                                    ; =>skip
                                    ; mov rax, rdx
                                );
                            }
                        }
                        dynasm!(a; .arch x64; mov QWORD [rbx + reg(dst)], rax);
                    }
                    Op::LoadImmAndJump { dst, imm, target } => {
                        let tgt = block_labels[blocks.block_of[target as usize]];
                        mov_rax_sext32(&mut a, imm);
                        dynasm!(a; .arch x64; mov QWORD [rbx + reg(dst)], rax; jmp =>tgt);
                    }
                    Op::LoadImmAndJumpIndirect { dst, base, imm, offset } => {
                        // The address is taken from reg[base] before reg[dst] is written (they may be the same).
                        dynasm!(a
                            ; .arch x64
                            ; mov rax, QWORD [rbx + reg(base)]
                            ;; mov_imm(&mut a, 1, (offset as i64) as u64)
                            ; add rax, rcx
                            ; mov eax, eax
                            ;; mov_imm(&mut a, 1, (imm as i32 as i64) as u64)
                            ; mov QWORD [rbx + reg(dst)], rcx
                        );
                        emit_indirect_jump(
                            &mut a,
                            jump_table.len() as u32,
                            jump_table_ptr,
                            halt_label,
                            panic_label,
                            dispatch_by_index_label,
                        );
                    }
                    Op::LoadAbs { dst, imm, width, signed } => {
                        dynasm!(a; .arch x64;; mov_imm(&mut a, 6, (imm as i64) as u64));
                        emit_checked_access(&mut a, width, false, bounds_check_label, fault_label);
                        emit_load(&mut a, width, signed);
                        dynasm!(a; .arch x64; mov QWORD [rbx + reg(dst)], rax);
                    }
                    Op::StoreAbs { src, imm, width } => {
                        dynasm!(a; .arch x64;; mov_imm(&mut a, 6, (imm as i64) as u64));
                        emit_checked_access(&mut a, width, true, bounds_check_label, fault_label);
                        dynasm!(a; .arch x64; mov rdx, QWORD [rbx + reg(src)]);
                        emit_store(&mut a, width);
                    }
                    Op::StoreImmAbs { imm_addr, value, width } => {
                        dynasm!(a; .arch x64;; mov_imm(&mut a, 6, (imm_addr as i64) as u64));
                        emit_checked_access(&mut a, width, true, bounds_check_label, fault_label);
                        dynasm!(a; .arch x64;; mov_imm(&mut a, 2, (value as i64) as u64));
                        emit_store(&mut a, width);
                    }
                    Op::StoreImmIndirect { base, offset, value, width } => {
                        dynasm!(a
                            ; .arch x64
                            ; mov rsi, QWORD [rbx + reg(base)]
                            ;; mov_imm(&mut a, 1, (offset as i64) as u64)
                            ; add rsi, rcx
                        );
                        emit_checked_access(&mut a, width, true, bounds_check_label, fault_label);
                        dynasm!(a; .arch x64;; mov_imm(&mut a, 2, (value as i64) as u64));
                        emit_store(&mut a, width);
                    }
                }
            }
            if !ops[hi - 1].is_terminator() {
                if b + 1 == nblocks {
                    dynasm!(a; .arch x64; jmp =>end_of_code_label);
                } else {
                    dynasm!(a; .arch x64; jmp =>instr_labels[blocks.block_lo[b + 1]]);
                }
            }
        }

        // Entry stubs for instructions inside a block: charge the cost from there, then join the block's code.
        for b in 0..nblocks {
            let lo = blocks.block_lo[b];
            let hi = blocks.block_hi[b];
            for pc in (lo + 1)..hi {
                entry_offsets[pc] = a.offset().0 as u32;
                emit_block_gas_charge(&mut a, block_gas[pc], pcs[pc], oog_label);
                dynasm!(a; .arch x64; jmp =>instr_labels[pc]);
            }
        }

        // Running off the end of the code is a panic at pc = code length.
        dynasm!(a
            ; .arch x64
            ; =>end_of_code_label
            ; mov r13d, DWORD code_len as i32
            ; jmp =>panic_label
            ; =>oog_label
        );
        emit_exit(&mut a, EXIT_OOG);
        dynasm!(a; .arch x64; =>panic_label);
        emit_exit(&mut a, EXIT_PANIC);
        dynasm!(a; .arch x64; =>halt_label);
        emit_exit(&mut a, EXIT_HALT);

        // A memory fault: edx = faulting page, r9d = 1 when it escalates to a panic (page below MinValidAddress).
        let fault_escalate_label = a.new_dynamic_label();
        dynasm!(a
            ; .arch x64
            ; =>fault_label
            ; test r9d, r9d
            ; jnz =>fault_escalate_label
            ; mov rax, QWORD [rsp + OUT]
            ; mov DWORD [rax + 4], edx       // out->fault_page
        );
        emit_exit(&mut a, EXIT_FAULT);
        dynasm!(a; .arch x64; =>fault_escalate_label);
        emit_exit(&mut a, EXIT_PANIC);

        emit_bounds_check_subroutine(&mut a, bounds_check_label);

        a.commit().ok()?;
        let buf = a.finalize().ok()?;
        Some((buf.to_vec(), ops.len() as u32, entry_offsets))
    }
}
