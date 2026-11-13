//! x86-64 (System V) single-pass emitter. Same program shape, exits, gas accounting and memory-fault rules as the
//! AArch64 emitter ([`crate::aarch64`]), which is the reference for every op's semantics.
//!
//! Entry (System V AMD64): `f(regs, gas, regions, n_regions, backing, page_shift, out, entry_index, host_fn,
//! host_ctx)`: six arguments in rdi, rsi, rdx, rcx, r8, r9d and four on the stack.
//!
//! | register                               | holds                                              |
//! |----------------------------------------|----------------------------------------------------|
//! | rbx                                    | guest register file (`*mut [u64; 13]`)              |
//! | r12                                    | gas, live                                          |
//! | rbp                                    | backing buffer (guest address 0 when laid out flat) |
//! | rdi, r8, r9, r10, r11, r13, r14, r15   | eight guest registers (see [`RegMap`])              |
//! | rax, rcx, rdx, rsi                     | scratch                                            |
//!
//! Frame, after six pushes and `sub rsp, 104` (rsp is then 16-byte aligned, as a call needs): `[rsp]` gas cell,
//! `+8` page shift, `+16` out, `+24` host_fn, `+32` host_ctx, `+40` region table, `+48` region count, `+56` page
//! permission table, `+64` pc of the last instruction that could exit, `+68` fault escalation flag, `+72..104`
//! registers put aside by a slow memory access.
//!
//! Memory accesses take a fast path through the page permission table (the eleventh argument), one byte of
//! flags per 4 KiB guest page:
//!
//! | bit | promises                                                                                  |
//! |-----|-------------------------------------------------------------------------------------------|
//! | 1   | the whole page lies in a readable region whose bytes are at `backing + guest address`       |
//! | 2   | bit 1 holds for this page and for the page before it                                       |
//! | 4   | as bit 1, for a writable region                                                            |
//! | 8   | bit 4 holds for this page and for the page before it                                       |
//!
//! An access looks up the page of its last byte. With the "and the page before" bit set it cannot touch a closed
//! page whether it straddles or not, and goes ahead; that is every access but those to a region's first page.
//! The caller sets bits only when it lays guest memory out that way. Anything else (a region's first page, an
//! unmapped page, a caller with a packed backing buffer and an all-zero table) is settled out of line, in the
//! end by the region scan, which also works out the fault.
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
const PERMS: i32 = 56;
const PC: i32 = 64;
const ESCALATE: i32 = 68;
// Where a slow memory access keeps the mapped registers the region scan uses as scratch.
const SAVED: i32 = 72;
const LOCALS: i32 = 104;
// Six callee-saved pushes and the return address lie between the locals and the stack arguments.
const STACK_ARGS: i32 = LOCALS + 6 * 8 + 8;

/// Byte offset of guest register `r` in the register file.
fn reg(r: u8) -> i32 {
    (r as i32) * 8
}

/// Which host register holds each guest register for the whole run, if any (dynasm register numbers; NONE = it
/// stays in the register file). Eight fit beside the scratch and state registers, and they go to the guest
/// registers compiled code uses most: the stack pointer, the two saved registers and the first five argument
/// registers.
/// The register file is only right at the edges: loaded on entry and after a host call, written back before a
/// host call and on every exit.
pub(crate) struct RegMap([u8; 13]);

const NONE: u8 = 0xff;
const RDI: u8 = 7;

impl RegMap {
    fn new() -> RegMap {
        let mut map = [NONE; 13];
        // guest sp, s0, s1, a0..a4 -> r15, r14, r13, rdi, r8, r9, r10, r11
        for (guest, host) in [(1u8, 15u8), (5, 14), (6, 13), (7, RDI), (8, 8), (9, 9), (10, 10), (11, 11)] {
            map[guest as usize] = host;
        }
        RegMap(map)
    }

    fn host(&self, guest: u8) -> Option<u8> {
        let h = self.0[guest as usize];
        (h != NONE).then_some(h)
    }

    /// Load every mapped guest register from the register file.
    fn emit_load_all(&self, a: &mut Asm) {
        for g in 0..13u8 {
            if let Some(h) = self.host(g) {
                dynasm!(a; .arch x64; mov Rq(h), QWORD [rbx + reg(g)]);
            }
        }
    }

    /// Write every mapped guest register to the register file.
    fn emit_store_all(&self, a: &mut Asm) {
        for g in 0..13u8 {
            if let Some(h) = self.host(g) {
                dynasm!(a; .arch x64; mov QWORD [rbx + reg(g)], Rq(h));
            }
        }
    }
}

/// Scratch register `s` (dynasm number) = guest register `g`.
fn ld(a: &mut Asm, m: &RegMap, s: u8, g: u8) {
    match m.host(g) {
        Some(h) => dynasm!(a; .arch x64; mov Rq(s), Rq(h)),
        None => dynasm!(a; .arch x64; mov Rq(s), QWORD [rbx + reg(g)]),
    }
}

/// The low 32 bits of scratch register `s` = the low 32 bits of guest register `g` (the upper 32 are zeroed).
fn ld32(a: &mut Asm, m: &RegMap, s: u8, g: u8) {
    match m.host(g) {
        Some(h) => dynasm!(a; .arch x64; mov Rd(s), Rd(h)),
        None => dynasm!(a; .arch x64; mov Rd(s), DWORD [rbx + reg(g)]),
    }
}

/// Guest register `g` = scratch register `s`.
fn st(a: &mut Asm, m: &RegMap, g: u8, s: u8) {
    match m.host(g) {
        Some(h) => dynasm!(a; .arch x64; mov Rq(h), Rq(s)),
        None => dynasm!(a; .arch x64; mov QWORD [rbx + reg(g)], Rq(s)),
    }
}

/// Code that rarely runs, kept out of the instruction stream so the stream stays dense: a guest program is far
/// larger than the caches, and how much of it fits decides how fast it runs. Paths are recorded while the
/// stream is emitted and written after it.
pub(crate) struct ColdCode {
    /// One thunk per access width (1, 2, 4, 8) and kind (load, store).
    thunks: [[Label; 2]; 4],
    paths: std::cell::RefCell<Vec<ColdPath>>,
}

enum ColdPath {
    /// A memory access whose page (of the last byte) is not open together with the one before it. With
    /// rcx = the permission table and rax = that page, as the stream left them: when the page itself is open
    /// and the access does not straddle, rejoin at `ok`; otherwise scan the regions, then fault, or do the
    /// access at the address the scan gives and rejoin at `done`.
    Access { entry: Label, ok: Label, done: Label, width: u8, tail: AccessTail, fault: Label },
    /// A block that could not pay: undo the charge, record the block's pc, and leave out of gas.
    OutOfGas { entry: Label, cost: i64, pc: u32, oog: Label },
}

impl ColdCode {
    fn thunk(&self, width: u8, store: bool) -> Label {
        self.thunks[width.trailing_zeros() as usize][store as usize]
    }

    fn emit(&self, a: &mut Asm, m: &RegMap) {
        for path in self.paths.borrow().iter() {
            match *path {
                ColdPath::Access { entry, ok, done, width, tail, fault } => {
                    let scan = a.new_dynamic_label();
                    let store = tail.is_store();
                    let alone: i8 = if store { PERM_WRITE } else { PERM_READ };
                    dynasm!(a; .arch x64; =>entry; test BYTE [rcx + rax], BYTE alone; jz =>scan);
                    if width > 1 {
                        dynasm!(a
                            ; .arch x64
                            ; mov eax, esi
                            ; and eax, DWORD 0xfff
                            ; cmp eax, DWORD 0x1000 - width as i32
                            ; ja =>scan
                        );
                    }
                    dynasm!(a
                        ; .arch x64
                        ; jmp =>ok
                        ; =>scan
                        ; call =>self.thunk(width, store)
                        ; test ecx, ecx
                        ; jz =>fault
                    );
                    match tail {
                        AccessTail::Load { dst, signed } => {
                            emit_load(a, width, signed);
                            st(a, m, dst, RAX);
                        }
                        AccessTail::StoreReg { src } => {
                            ld(a, m, RDX, src);
                            emit_store(a, width);
                        }
                        AccessTail::StoreImm { value } => {
                            mov_imm(a, RDX, value);
                            emit_store(a, width);
                        }
                    }
                    dynasm!(a; .arch x64; jmp =>done);
                }
                ColdPath::OutOfGas { entry, cost, pc, oog } => dynasm!(a
                    ; .arch x64
                    ; =>entry
                    ;; mov_imm(a, 1, cost as u64)
                    ; add r12, rcx
                    ; mov DWORD [rsp + 64], DWORD pc as i32
                    ; jmp =>oog
                ),
            }
        }
    }
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
/// MinValidAddress (the fault is then a panic). Clobbers rax, rcx, rdx, r9, r10. Called from a slow-path thunk,
/// itself called from the access: the frame slots are 16 bytes further from rsp inside it.
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
        ; cmp r9, QWORD [rsp + N_REGIONS + 16]
        ; jae =>scan_done               // region table exhausted: not found
        ; mov r10, r9
        ; shl r10, 4                    // Region is 16 bytes: base, len, buf_offset, writable
        ; add r10, QWORD [rsp + REGIONS + 16]
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
        ; mov ecx, DWORD [rsp + PAGE_SHIFT + 16]
        ; shr rdx, cl
        ; shl rdx, cl                   // rdx = addr's page
        ; xor r9d, r9d
        ; =>probe_top
        ; cmp r9, QWORD [rsp + N_REGIONS + 16]
        ; jae =>probe_done
        ; mov r10, r9
        ; shl r10, 4
        ; add r10, QWORD [rsp + REGIONS + 16]
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
        ; mov ecx, DWORD [rsp + PAGE_SHIFT + 16]
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

const RAX: u8 = 0;
const RCX: u8 = 1;
const RDX: u8 = 2;
const RSI: u8 = 6;

/// A host register holding guest register `g`: its own when it is mapped, else `scratch`, loaded here.
fn src_reg(a: &mut Asm, m: &RegMap, g: u8, scratch: u8) -> u8 {
    match m.host(g) {
        Some(h) => h,
        None => {
            dynasm!(a; .arch x64; mov Rq(scratch), QWORD [rbx + reg(g)]);
            scratch
        }
    }
}

/// The host register to compute guest register `g`'s new value in: its own when it is mapped, else `scratch`,
/// which [`commit`] then writes to the register file.
fn dst_reg(m: &RegMap, g: u8, scratch: u8) -> u8 {
    m.host(g).unwrap_or(scratch)
}

/// Finish a value computed in the register [`dst_reg`] gave for guest register `g`.
fn commit(a: &mut Asm, m: &RegMap, g: u8, r: u8) {
    if m.host(g).is_none() {
        dynasm!(a; .arch x64; mov QWORD [rbx + reg(g)], Rq(r));
    }
}

fn fits_i32(v: i64) -> bool {
    v >= i32::MIN as i64 && v <= i32::MAX as i64
}

/// esi = (reg[base] + imm) mod 2^32: the guest address of an access.
fn emit_addr(a: &mut Asm, m: &RegMap, base: u8, imm: u64) {
    let b = src_reg(a, m, base, RSI);
    let disp = imm as u32 as i32;
    if disp == 0 {
        dynasm!(a; .arch x64; mov esi, Rd(b));
    } else {
        dynasm!(a; .arch x64; lea esi, [Rq(b) + disp]);
    }
}

#[derive(Clone, Copy)]
enum Bin {
    Add,
    Sub,
    Mul,
    And,
    Or,
}

/// reg[dst] = reg[s1] `kind` reg[s2], in 64 bits, working in the mapped registers where there are any.
fn emit_bin(a: &mut Asm, m: &RegMap, kind: Bin, dst: u8, s1: u8, s2: u8) {
    let d = dst_reg(m, dst, RAX);
    let x = src_reg(a, m, s1, RAX);
    let y = src_reg(a, m, s2, RCX);
    let op = |a: &mut Asm, to: u8, from: u8| match kind {
        Bin::Add => dynasm!(a; .arch x64; add Rq(to), Rq(from)),
        Bin::Sub => dynasm!(a; .arch x64; sub Rq(to), Rq(from)),
        Bin::Mul => dynasm!(a; .arch x64; imul Rq(to), Rq(from)),
        Bin::And => dynasm!(a; .arch x64; and Rq(to), Rq(from)),
        Bin::Or => dynasm!(a; .arch x64; or Rq(to), Rq(from)),
    };
    if d == y && d != x {
        // The destination is the second operand: it cannot be overwritten with the first one before use.
        match kind {
            Bin::Sub => {
                dynasm!(a; .arch x64; mov rax, Rq(x); sub rax, Rq(y); mov Rq(d), rax);
            }
            _ => op(a, d, x), // commutative
        }
    } else {
        if d != x {
            dynasm!(a; .arch x64; mov Rq(d), Rq(x));
        }
        op(a, d, y);
    }
    commit(a, m, dst, d);
}

/// Compare reg[s1] with reg[s2], for a following conditional jump.
fn emit_cmp_regs(a: &mut Asm, m: &RegMap, s1: u8, s2: u8) {
    let x = src_reg(a, m, s1, RAX);
    let y = src_reg(a, m, s2, RCX);
    dynasm!(a; .arch x64; cmp Rq(x), Rq(y));
}

/// Compare reg[src] with the constant `imm`, for a following conditional jump.
fn emit_cmp_imm(a: &mut Asm, m: &RegMap, src: u8, imm: u64) {
    let x = src_reg(a, m, src, RAX);
    let v = imm as i64;
    if fits_i32(v) {
        dynasm!(a; .arch x64; cmp Rq(x), DWORD v as i32);
    } else {
        mov_imm(a, RCX, imm);
        dynasm!(a; .arch x64; cmp Rq(x), rcx);
    }
}

// Page permission bits (see the module doc).
const PERM_READ: i8 = 1;
const PERM_READ_WITH_PREVIOUS: i8 = 2;
const PERM_WRITE: i8 = 4;
const PERM_WRITE_WITH_PREVIOUS: i8 = 8;

/// What an access does once its bytes are known to be accessible.
#[derive(Clone, Copy)]
enum AccessTail {
    /// reg[dst] = the bytes, zero- or sign-extended.
    Load { dst: u8, signed: bool },
    /// The bytes = the low end of reg[src].
    StoreReg { src: u8 },
    /// The bytes = the low end of `value`.
    StoreImm { value: u64 },
}

impl AccessTail {
    fn is_store(&self) -> bool {
        !matches!(self, AccessTail::Load { .. })
    }
}

/// A `width`-byte access at the guest address in esi.
///
/// In the stream: look up the page of the access's last byte, and when that page and the one before it are
/// both open (so the access cannot reach a closed page, straddling or not) do the access at `backing + address`.
/// Everything else is in the cold path this records: a page whose predecessor is closed, an unmapped page, a
/// caller that does not lay memory out flat.
fn emit_access(a: &mut Asm, m: &RegMap, cold: &ColdCode, width: u8, tail: AccessTail, fault: Label) {
    let entry = a.new_dynamic_label();
    let ok = a.new_dynamic_label();
    let done = a.new_dynamic_label();
    let store = tail.is_store();
    match width {
        1 => dynasm!(a; .arch x64; mov eax, esi),
        2 => dynasm!(a; .arch x64; lea eax, [rsi + 1]),
        4 => dynasm!(a; .arch x64; lea eax, [rsi + 3]),
        8 => dynasm!(a; .arch x64; lea eax, [rsi + 7]),
        _ => panic!("unsupported access width {width}"),
    }
    let both: i8 = if store { PERM_WRITE_WITH_PREVIOUS } else { PERM_READ_WITH_PREVIOUS };
    dynasm!(a
        ; .arch x64
        ; shr eax, 12
        ; mov rcx, QWORD [rsp + 56]         // PERMS, written out so the short encoding is used
        ; test BYTE [rcx + rax], BYTE both
        ; jz =>entry
        ; =>ok
    );
    match tail {
        AccessTail::Load { dst, signed } => {
            let d = dst_reg(m, dst, RAX);
            match (width, signed) {
                (1, false) => dynasm!(a; .arch x64; movzx Rd(d), BYTE [rbp + rsi]),
                (2, false) => dynasm!(a; .arch x64; movzx Rd(d), WORD [rbp + rsi]),
                (4, false) => dynasm!(a; .arch x64; mov Rd(d), DWORD [rbp + rsi]),
                (1, true) => dynasm!(a; .arch x64; movsx Rq(d), BYTE [rbp + rsi]),
                (2, true) => dynasm!(a; .arch x64; movsx Rq(d), WORD [rbp + rsi]),
                (4, true) => dynasm!(a; .arch x64; movsxd Rq(d), DWORD [rbp + rsi]),
                _ => dynasm!(a; .arch x64; mov Rq(d), QWORD [rbp + rsi]),
            }
            commit(a, m, dst, d);
        }
        AccessTail::StoreReg { src } => match width {
            4 => {
                let v = src_reg(a, m, src, RDX);
                dynasm!(a; .arch x64; mov DWORD [rbp + rsi], Rd(v));
            }
            8 => {
                let v = src_reg(a, m, src, RDX);
                dynasm!(a; .arch x64; mov QWORD [rbp + rsi], Rq(v));
            }
            // Through rdx: its low byte and word have plain encodings.
            1 => dynasm!(a; .arch x64;; ld(a, m, RDX, src); mov BYTE [rbp + rsi], dl),
            _ => dynasm!(a; .arch x64;; ld(a, m, RDX, src); mov WORD [rbp + rsi], dx),
        },
        AccessTail::StoreImm { value } => {
            mov_imm(a, RDX, value);
            match width {
                1 => dynasm!(a; .arch x64; mov BYTE [rbp + rsi], dl),
                2 => dynasm!(a; .arch x64; mov WORD [rbp + rsi], dx),
                4 => dynasm!(a; .arch x64; mov DWORD [rbp + rsi], edx),
                _ => dynasm!(a; .arch x64; mov QWORD [rbp + rsi], rdx),
            }
        }
    }
    dynasm!(a; .arch x64; =>done);
    cold.paths.borrow_mut().push(ColdPath::Access { entry, ok, done, width, tail, fault });
}

/// The thunk a slow access calls: run the region scan for `width` and `store` with the mapped registers it
/// uses as scratch (rdi, r8, r9, r10) put aside, and leave its "escalate to panic" answer in the frame. Returns
/// the scan's ecx, rax and edx.
fn emit_slow_thunk(a: &mut Asm, entry: Label, width: u8, store: bool, bounds_check: Label) {
    dynasm!(a
        ; .arch x64
        ; =>entry
        ; mov QWORD [rsp + SAVED + 8], rdi
        ; mov QWORD [rsp + SAVED + 16], r8
        ; mov QWORD [rsp + SAVED + 24], r9
        ; mov QWORD [rsp + SAVED + 32], r10
        ; mov edi, DWORD width as i32
        ; mov r8d, DWORD store as i32
        ; call =>bounds_check
        ; mov DWORD [rsp + ESCALATE + 8], r9d
        ; mov rdi, QWORD [rsp + SAVED + 8]
        ; mov r8, QWORD [rsp + SAVED + 16]
        ; mov r9, QWORD [rsp + SAVED + 24]
        ; mov r10, QWORD [rsp + SAVED + 32]
        ; ret
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
fn emit_load_operands(a: &mut Asm, m: &RegMap, src: u8, src2: u8, width: Width) {
    match width {
        Width::W32 => dynasm!(a
            ; .arch x64
            ;; ld32(a, m, 0, src)
            ;; ld32(a, m, 1, src2)
        ),
        Width::W64 => dynasm!(a
            ; .arch x64
            ;; ld(a, m, 0, src)
            ;; ld(a, m, 1, src2)
        ),
    }
}

/// reg[dst] = rax, sign-extending eax first for a 32-bit result.
fn emit_store_result(a: &mut Asm, m: &RegMap, dst: u8, width: Width) {
    match width {
        Width::W32 => dynasm!(a; .arch x64; movsxd rax, eax;; st(a, m, dst, 0)),
        Width::W64 => dynasm!(a; .arch x64;; st(a, m, dst, 0)),
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

/// Charge the block's gas. A block that cannot pay leaves out of gas with the cost uncharged and the pc at `pc`
/// (its first instruction): that path is cold.
fn emit_block_gas_charge(a: &mut Asm, cold: &ColdCode, cost: i64, pc: u32, oog: Label) {
    if cost <= 0 {
        return;
    }
    let entry = a.new_dynamic_label();
    if cost <= i8::MAX as i64 {
        dynasm!(a; .arch x64; sub r12, BYTE cost as i8);
    } else if cost <= i32::MAX as i64 {
        dynasm!(a; .arch x64; sub r12, DWORD cost as i32);
    } else {
        dynasm!(a; .arch x64;; mov_imm(a, 1, cost as u64); sub r12, rcx);
    }
    // After the subtraction "less" is exactly gas < cost, as a compare would have had it.
    dynasm!(a; .arch x64; jl =>entry);
    cold.paths.borrow_mut().push(ColdPath::OutOfGas { entry, cost, pc, oog });
}

/// r12 = *gas (after an upcall, which may have charged gas).
fn emit_reload_gas(a: &mut Asm) {
    dynasm!(a; .arch x64; mov rcx, QWORD [rsp + GAS_CELL]; mov r12, QWORD [rcx]);
}

/// Call `host_fn(host_ctx, host_id, pc)` and act on its status.
fn emit_host_call(a: &mut Asm, m: &RegMap, host_id: u64, panic: Label, oog: Label, next_pc: u32) {
    let not_panic = a.new_dynamic_label();
    let not_oog = a.new_dynamic_label();
    let not_oog_next = a.new_dynamic_label();
    let is_continue = a.new_dynamic_label();
    dynasm!(a
        ; .arch x64
        ;; m.emit_store_all(a)
        ; mov rax, QWORD [rsp + GAS_CELL]
        ; mov QWORD [rax], r12           // the handler reads and charges gas through the cell
        ; mov rdi, QWORD [rsp + HOST_CTX]
        ;; mov_imm(a, 6, (host_id as i64) as u64)
        ; mov edx, DWORD [rsp + PC]
        ; call QWORD [rsp + HOST_FN]
        // The handler worked on the register file: take every mapped register back from it. rax is not one.
        ;; m.emit_load_all(a)
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
        ; mov DWORD [rsp + PC], DWORD next_pc as i32
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
    entry_offsets_len: u32,
    entry_offsets_ptr: *const u32,
    func_start: Label,
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
        ; mov edx, DWORD [rsi + rcx * 4]
        ; cmp edx, DWORD -1             // u32::MAX: no instruction starts there
        ; je =>panic
        // The jump itself is emitted here, not shared: the branch predictor keeps targets per jump site, and a
        // function's return goes to few places while all returns together go everywhere.
        ; cmp edx, DWORD entry_offsets_len as i32
        ; jae =>panic
        ; mov rsi, QWORD entry_offsets_ptr as i64
        ; mov ecx, DWORD [rsi + rdx * 4]
        ; lea rax, [=>func_start]
        ; add rax, rcx
        ; jmp rax
    );
}

/// Write the mapped registers back, store pc and gas to their cells, restore the caller's registers and return `exit`.
fn emit_exit(a: &mut Asm, m: &RegMap, exit: u32) {
    m.emit_store_all(a);
    dynasm!(a
        ; .arch x64
        ; mov rax, QWORD [rsp + OUT]
        ; mov ecx, DWORD [rsp + PC]
        ; mov DWORD [rax], ecx           // out->pc
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
        let cold = ColdCode {
            thunks: std::array::from_fn(|_| std::array::from_fn(|_| a.new_dynamic_label())),
            paths: std::cell::RefCell::new(Vec::new()),
        };
        let m = RegMap::new();
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
            ; mov rax, QWORD [rsp + STACK_ARGS + 32]
            ; mov QWORD [rsp + PERMS], rax          // page permissions
            ; mov rbp, r8                           // backing
            ; mov DWORD [rsp + PAGE_SHIFT], r9d
            ; mov rax, QWORD [rsp + STACK_ARGS]
            ; mov QWORD [rsp + OUT], rax
            ; mov edx, DWORD [rsp + STACK_ARGS + 8] // entry index
            ; mov rax, QWORD [rsp + STACK_ARGS + 16]
            ; mov QWORD [rsp + HOST_FN], rax
            ; mov rax, QWORD [rsp + STACK_ARGS + 24]
            ; mov QWORD [rsp + HOST_CTX], rax
            ; mov DWORD [rsp + PC], DWORD 0
            ;; m.emit_load_all(&mut a)
            // Continue at the instruction whose index is in edx: the entry point, and every indirect jump.
            ; =>dispatch_by_index_label
            ; cmp edx, DWORD entry_offsets_len as i32
            ; jae =>panic_label
            ; mov rsi, QWORD entry_offsets_ptr as i64
            ; mov ecx, DWORD [rsi + rdx * 4]        // bytes from the function start
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
            emit_block_gas_charge(&mut a, &cold, block_gas[lo], pcs[lo], oog_label);

            for pc in lo..hi {
                let this_pc = pcs[pc] as i32;
                dynasm!(a; .arch x64; =>instr_labels[pc]);
                // The pc is only read on an exit, so only an instruction that can leave the code records it.
                if ops[pc].may_exit() {
                    dynasm!(a; .arch x64; mov DWORD [rsp + 64], DWORD this_pc); // PC, short encoding
                }
                match ops[pc] {
                    Op::LoadImm64 { dst, imm } => {
                        let d = dst_reg(&m, dst, RAX);
                        mov_imm(&mut a, d, imm);
                        commit(&mut a, &m, dst, d);
                    }
                    Op::AddImm64 { dst, src, imm } => {
                        let s = src_reg(&mut a, &m, src, RAX);
                        let d = dst_reg(&m, dst, RAX);
                        let v = imm as i64;
                        if d == s && v >= i8::MIN as i64 && v <= i8::MAX as i64 {
                            dynasm!(a; .arch x64; add Rq(d), BYTE v as i8);
                        } else if fits_i32(v) {
                            dynasm!(a; .arch x64; lea Rq(d), [Rq(s) + v as i32]);
                        } else {
                            mov_imm(&mut a, RCX, imm);
                            if d != s {
                                dynasm!(a; .arch x64; mov Rq(d), Rq(s));
                            }
                            dynasm!(a; .arch x64; add Rq(d), rcx);
                        }
                        commit(&mut a, &m, dst, d);
                    }
                    Op::Add { dst, src, src2 } => emit_bin(&mut a, &m, Bin::Add, dst, src, src2),
                    Op::Sub { dst, src, src2 } => emit_bin(&mut a, &m, Bin::Sub, dst, src, src2),
                    Op::Mul { dst, src, src2 } => emit_bin(&mut a, &m, Bin::Mul, dst, src, src2),
                    Op::Load { dst, src, imm, width, signed } => {
                        emit_addr(&mut a, &m, src, imm);
                        emit_access(&mut a, &m, &cold, width, AccessTail::Load { dst, signed }, fault_label);
                    }
                    Op::Store { dst, src, imm, width } => {
                        emit_addr(&mut a, &m, src, imm);
                        emit_access(&mut a, &m, &cold, width, AccessTail::StoreReg { src: dst }, fault_label);
                    }
                    Op::Trap => dynasm!(a; .arch x64; jmp =>panic_label),
                    Op::Ecalli { host_id } => {
                        let next_pc = if pc + 1 < ops.len() { pcs[pc + 1] } else { code_len };
                        emit_host_call(&mut a, &m, host_id, panic_label, oog_label, next_pc);
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
                        emit_cmp_regs(&mut a, &m, src, src2);
                        dynasm!(a; .arch x64; je =>tgt);
                    }
                    Op::BranchNe { src, src2, target } => {
                        let tgt = block_labels[blocks.block_of[target as usize]];
                        emit_cmp_regs(&mut a, &m, src, src2);
                        dynasm!(a; .arch x64; jne =>tgt);
                    }
                    Op::BranchEqImm { src, imm, target } => {
                        let tgt = block_labels[blocks.block_of[target as usize]];
                        emit_cmp_imm(&mut a, &m, src, imm);
                        dynasm!(a; .arch x64; je =>tgt);
                    }
                    Op::BranchNeImm { src, imm, target } => {
                        let tgt = block_labels[blocks.block_of[target as usize]];
                        emit_cmp_imm(&mut a, &m, src, imm);
                        dynasm!(a; .arch x64; jne =>tgt);
                    }
                    Op::BranchCmpImm { src, imm, target, cond } => {
                        let tgt = block_labels[blocks.block_of[target as usize]];
                        emit_cmp_imm(&mut a, &m, src, imm);
                        emit_jcc(&mut a, cond, tgt);
                    }
                    Op::BranchCmp { src, src2, target, cond } => {
                        let tgt = block_labels[blocks.block_of[target as usize]];
                        emit_cmp_regs(&mut a, &m, src, src2);
                        emit_jcc(&mut a, cond, tgt);
                    }
                    Op::Djump { src, imm } => {
                        dynasm!(a
                            ; .arch x64
                            ;; ld(&mut a, &m, 0, src)
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
                            entry_offsets_len,
                            entry_offsets_ptr,
                            func_start_label,
                        );
                    }
                    Op::LoadImm32 { dst, imm } => {
                        let d = dst_reg(&m, dst, RAX);
                        mov_imm(&mut a, d, imm as i32 as i64 as u64);
                        commit(&mut a, &m, dst, d);
                    }
                    Op::LoadAbs64 { dst, imm } => {
                        dynasm!(a; .arch x64; mov esi, DWORD imm as u32 as i32);
                        emit_access(&mut a, &m, &cold, 8, AccessTail::Load { dst, signed: false }, fault_label);
                    }
                    Op::StoreAbs64 { src, imm } => {
                        dynasm!(a; .arch x64; mov esi, DWORD imm as u32 as i32);
                        emit_access(&mut a, &m, &cold, 8, AccessTail::StoreReg { src }, fault_label);
                    }
                    Op::MoveReg { dst, src } => {
                        let s = src_reg(&mut a, &m, src, RAX);
                        st(&mut a, &m, dst, s);
                    }
                    Op::AddImm32 { dst, src, imm } => {
                        let s = src_reg(&mut a, &m, src, RAX);
                        let d = dst_reg(&m, dst, RAX);
                        dynasm!(a; .arch x64; lea Rd(d), [Rq(s) + imm as i32]; movsxd Rq(d), Rd(d));
                        commit(&mut a, &m, dst, d);
                    }
                    Op::Shl64Imm { dst, src, imm } => {
                        let shift = (imm & 63) as i8;
                        let s = src_reg(&mut a, &m, src, RAX);
                        let d = dst_reg(&m, dst, RAX);
                        if d != s {
                            dynasm!(a; .arch x64; mov Rq(d), Rq(s));
                        }
                        dynasm!(a; .arch x64; shl Rq(d), shift);
                        commit(&mut a, &m, dst, d);
                    }
                    Op::And { dst, src, src2 } => emit_bin(&mut a, &m, Bin::And, dst, src, src2),
                    Op::Or { dst, src, src2 } => emit_bin(&mut a, &m, Bin::Or, dst, src, src2),
                    Op::CmovIfNotZero { dst, src, src2 } => {
                        let skip = a.new_dynamic_label();
                        dynasm!(a
                            ; .arch x64
                            ;; ld(&mut a, &m, 1, src2)
                            ; test rcx, rcx
                            ; jz =>skip
                            ;; ld(&mut a, &m, 0, src)
                            ;; st(&mut a, &m, dst, 0)
                            ; =>skip
                        );
                    }
                    Op::CmovIfZero { dst, src, src2 } => {
                        let skip = a.new_dynamic_label();
                        dynasm!(a
                            ; .arch x64
                            ;; ld(&mut a, &m, 1, src2)
                            ; test rcx, rcx
                            ; jnz =>skip
                            ;; ld(&mut a, &m, 0, src)
                            ;; st(&mut a, &m, dst, 0)
                            ; =>skip
                        );
                    }
                    Op::CmovImm { dst, src, imm, zero_taken } => {
                        let skip = a.new_dynamic_label();
                        dynasm!(a; .arch x64;; ld(&mut a, &m, 1, src); test rcx, rcx);
                        if zero_taken {
                            dynasm!(a; .arch x64; jnz =>skip);
                        } else {
                            dynasm!(a; .arch x64; jz =>skip);
                        }
                        dynasm!(a
                            ; .arch x64
                            ;; mov_imm(&mut a, 0, (imm as i64) as u64)
                            ;; st(&mut a, &m, dst, 0)
                            ; =>skip
                        );
                    }
                    Op::SetCmpImm { dst, src, imm, cond } => {
                        dynasm!(a
                            ; .arch x64
                            ;; ld(&mut a, &m, 0, src)
                            ;; mov_imm(&mut a, 1, (imm as i64) as u64)
                            ; cmp rax, rcx
                        );
                        emit_setcc_rax(&mut a, cond);
                        dynasm!(a; .arch x64;; st(&mut a, &m, dst, 0));
                    }
                    Op::SetCmp { dst, src, src2, cond } => {
                        emit_load_operands(&mut a, &m, src, src2, Width::W64);
                        dynasm!(a; .arch x64; cmp rax, rcx);
                        emit_setcc_rax(&mut a, cond);
                        dynasm!(a; .arch x64;; st(&mut a, &m, dst, 0));
                    }
                    Op::ShiftRotateImm { dst, src, imm, kind, width } => {
                        let mask: u64 = match width { Width::W32 => 31, Width::W64 => 63 };
                        let n = (imm & mask) as u32;
                        match width {
                            Width::W32 => dynasm!(a; .arch x64;; ld32(&mut a, &m, 0, src)),
                            Width::W64 => dynasm!(a; .arch x64;; ld(&mut a, &m, 0, src)),
                        }
                        emit_shift_rotate(&mut a, kind, width, ShiftAmount::Imm(n));
                        emit_store_result(&mut a, &m, dst, width);
                    }
                    Op::ShiftRotateImmAlt { dst, src, imm, kind, width } => {
                        // The immediate is shifted by the register.
                        match width {
                            Width::W32 => dynasm!(a
                                ; .arch x64
                                ; mov eax, DWORD imm as u32 as i32
                                ;; ld32(&mut a, &m, 1, src)
                                ; and ecx, DWORD 31
                            ),
                            Width::W64 => dynasm!(a
                                ; .arch x64
                                ;; mov_imm(&mut a, 0, (imm as i64) as u64)
                                ;; ld(&mut a, &m, 1, src)
                                ; and ecx, DWORD 63
                            ),
                        }
                        emit_shift_rotate(&mut a, kind, width, ShiftAmount::Cl);
                        emit_store_result(&mut a, &m, dst, width);
                    }
                    Op::ShiftRotateReg { dst, src, src2, kind, width } => {
                        emit_load_operands(&mut a, &m, src, src2, width);
                        match width {
                            Width::W32 => dynasm!(a; .arch x64; and ecx, DWORD 31),
                            Width::W64 => dynasm!(a; .arch x64; and ecx, DWORD 63),
                        }
                        emit_shift_rotate(&mut a, kind, width, ShiftAmount::Cl);
                        emit_store_result(&mut a, &m, dst, width);
                    }
                    Op::AluReg { dst, src, src2, kind, width } => {
                        emit_load_operands(&mut a, &m, src, src2, width);
                        emit_alu(&mut a, kind, width);
                        emit_store_result(&mut a, &m, dst, width);
                    }
                    Op::AluImm { dst, src, imm, kind, width } => {
                        match width {
                            Width::W32 => dynasm!(a
                                ; .arch x64
                                ;; ld32(&mut a, &m, 0, src)
                                ; mov ecx, DWORD imm as u32 as i32
                            ),
                            Width::W64 => dynasm!(a
                                ; .arch x64
                                ;; ld(&mut a, &m, 0, src)
                                ;; mov_imm(&mut a, 1, (imm as i64) as u64)
                            ),
                        }
                        emit_alu(&mut a, kind, width);
                        emit_store_result(&mut a, &m, dst, width);
                    }
                    Op::NegateAndAddImm { dst, src, imm, width } => {
                        // imm minus reg: the operand order matters.
                        match width {
                            Width::W32 => dynasm!(a
                                ; .arch x64
                                ;; ld32(&mut a, &m, 1, src)
                                ; mov eax, DWORD imm as u32 as i32
                                ; sub eax, ecx
                            ),
                            Width::W64 => dynasm!(a
                                ; .arch x64
                                ;; ld(&mut a, &m, 1, src)
                                ;; mov_imm(&mut a, 0, (imm as i64) as u64)
                                ; sub rax, rcx
                            ),
                        }
                        emit_store_result(&mut a, &m, dst, width);
                    }
                    Op::CountSetBits { dst, src, width } => {
                        match width {
                            Width::W32 => dynasm!(a; .arch x64;; ld32(&mut a, &m, 0, src)),
                            Width::W64 => dynasm!(a; .arch x64;; ld(&mut a, &m, 0, src)),
                        }
                        emit_popcount64(&mut a);
                        emit_store_result(&mut a, &m, dst, width);
                    }
                    Op::CountLeadingZeroBits { dst, src, width } => {
                        // BSR, not LZCNT: no CPU feature needed, and zero is handled apart (BSR leaves it undefined).
                        let done = a.new_dynamic_label();
                        match width {
                            Width::W32 => dynasm!(a
                                ; .arch x64
                                ;; ld32(&mut a, &m, 1, src)
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
                                ;; ld(&mut a, &m, 1, src)
                                ; mov eax, DWORD 64
                                ; test rcx, rcx
                                ; jz =>done
                                ; bsr rcx, rcx
                                ; mov eax, DWORD 63
                                ; sub eax, ecx
                                ; =>done
                            ),
                        }
                        dynasm!(a; .arch x64;; st(&mut a, &m, dst, 0));
                    }
                    Op::CountTrailingZeroBits { dst, src, width } => {
                        let done = a.new_dynamic_label();
                        match width {
                            Width::W32 => dynasm!(a
                                ; .arch x64
                                ;; ld32(&mut a, &m, 1, src)
                                ; mov eax, DWORD 32
                                ; test ecx, ecx
                                ; jz =>done
                                ; bsf eax, ecx
                                ; =>done
                            ),
                            Width::W64 => dynasm!(a
                                ; .arch x64
                                ;; ld(&mut a, &m, 1, src)
                                ; mov eax, DWORD 64
                                ; test rcx, rcx
                                ; jz =>done
                                ; bsf rax, rcx
                                ; =>done
                            ),
                        }
                        dynasm!(a; .arch x64;; st(&mut a, &m, dst, 0));
                    }
                    Op::SignExtend8 { dst, src } => {
                        dynasm!(a
                            ; .arch x64
                            ;; ld(&mut a, &m, 0, src)
                            ; movsx rax, al
                            ;; st(&mut a, &m, dst, 0)
                        );
                    }
                    Op::SignExtend16 { dst, src } => {
                        dynasm!(a
                            ; .arch x64
                            ;; ld(&mut a, &m, 0, src)
                            ; movsx rax, ax
                            ;; st(&mut a, &m, dst, 0)
                        );
                    }
                    Op::ZeroExtend16 { dst, src } => {
                        dynasm!(a
                            ; .arch x64
                            ;; ld(&mut a, &m, 0, src)
                            ; movzx eax, ax
                            ;; st(&mut a, &m, dst, 0)
                        );
                    }
                    Op::ReverseByte { dst, src } => {
                        dynasm!(a
                            ; .arch x64
                            ;; ld(&mut a, &m, 0, src)
                            ; bswap rax
                            ;; st(&mut a, &m, dst, 0)
                        );
                    }
                    Op::InvertedLogical { dst, src, src2, kind } => {
                        emit_load_operands(&mut a, &m, src, src2, Width::W64);
                        match kind {
                            InvLogicalKind::AndNot => dynasm!(a; .arch x64; not rcx; and rax, rcx),
                            InvLogicalKind::OrNot => dynasm!(a; .arch x64; not rcx; or rax, rcx),
                            InvLogicalKind::Xnor => dynasm!(a; .arch x64; xor rax, rcx; not rax),
                        }
                        dynasm!(a; .arch x64;; st(&mut a, &m, dst, 0));
                    }
                    Op::MinMax { dst, src, src2, kind } => {
                        emit_load_operands(&mut a, &m, src, src2, Width::W64);
                        dynasm!(a; .arch x64; cmp rax, rcx);
                        // rax stays when it is the wanted one; otherwise rcx replaces it.
                        match kind {
                            MinMaxKind::MaxSigned => dynasm!(a; .arch x64; cmovle rax, rcx),
                            MinMaxKind::MaxUnsigned => dynasm!(a; .arch x64; cmovbe rax, rcx),
                            MinMaxKind::MinSigned => dynasm!(a; .arch x64; cmovge rax, rcx),
                            MinMaxKind::MinUnsigned => dynasm!(a; .arch x64; cmovae rax, rcx),
                        }
                        dynasm!(a; .arch x64;; st(&mut a, &m, dst, 0));
                    }
                    Op::Div { dst, src, src2, width, signed } => {
                        emit_load_operands(&mut a, &m, src, src2, width);
                        emit_div(&mut a, width, signed);
                        emit_store_result(&mut a, &m, dst, width);
                    }
                    Op::Rem { dst, src, src2, width, signed } => {
                        emit_load_operands(&mut a, &m, src, src2, width);
                        emit_rem(&mut a, width, signed);
                        emit_store_result(&mut a, &m, dst, width);
                    }
                    Op::MulUpper { dst, src, src2, kind } => {
                        emit_load_operands(&mut a, &m, src, src2, Width::W64);
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
                        dynasm!(a; .arch x64;; st(&mut a, &m, dst, 0));
                    }
                    Op::LoadImmAndJump { dst, imm, target } => {
                        let tgt = block_labels[blocks.block_of[target as usize]];
                        let d = dst_reg(&m, dst, RAX);
                        mov_imm(&mut a, d, imm as i32 as i64 as u64);
                        commit(&mut a, &m, dst, d);
                        dynasm!(a; .arch x64; jmp =>tgt);
                    }
                    Op::LoadImmAndJumpIndirect { dst, base, imm, offset } => {
                        // The address is taken from reg[base] before reg[dst] is written (they may be the same).
                        dynasm!(a
                            ; .arch x64
                            ;; ld(&mut a, &m, 0, base)
                            ;; mov_imm(&mut a, 1, (offset as i64) as u64)
                            ; add rax, rcx
                            ; mov eax, eax
                            ;; mov_imm(&mut a, 1, (imm as i32 as i64) as u64)
                            ;; st(&mut a, &m, dst, 1)
                        );
                        emit_indirect_jump(
                            &mut a,
                            jump_table.len() as u32,
                            jump_table_ptr,
                            halt_label,
                            panic_label,
                            entry_offsets_len,
                            entry_offsets_ptr,
                            func_start_label,
                        );
                    }
                    Op::LoadAbs { dst, imm, width, signed } => {
                        dynasm!(a; .arch x64; mov esi, DWORD imm as u32 as i32);
                        emit_access(&mut a, &m, &cold, width, AccessTail::Load { dst, signed }, fault_label);
                    }
                    Op::StoreAbs { src, imm, width } => {
                        dynasm!(a; .arch x64; mov esi, DWORD imm as u32 as i32);
                        emit_access(&mut a, &m, &cold, width, AccessTail::StoreReg { src }, fault_label);
                    }
                    Op::StoreImmAbs { imm_addr, value, width } => {
                        dynasm!(a; .arch x64; mov esi, DWORD imm_addr as u32 as i32);
                        emit_access(&mut a, &m, &cold, width, AccessTail::StoreImm { value }, fault_label);
                    }
                    Op::StoreImmIndirect { base, offset, value, width } => {
                        emit_addr(&mut a, &m, base, offset);
                        emit_access(&mut a, &m, &cold, width, AccessTail::StoreImm { value }, fault_label);
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
                emit_block_gas_charge(&mut a, &cold, block_gas[pc], pcs[pc], oog_label);
                dynasm!(a; .arch x64; jmp =>instr_labels[pc]);
            }
        }

        cold.emit(&mut a, &m);

        // Running off the end of the code is a panic at pc = code length.
        dynasm!(a
            ; .arch x64
            ; =>end_of_code_label
            ; mov DWORD [rsp + PC], DWORD code_len as i32
            ; jmp =>panic_label
            ; =>oog_label
        );
        emit_exit(&mut a, &m, EXIT_OOG);
        dynasm!(a; .arch x64; =>panic_label);
        emit_exit(&mut a, &m, EXIT_PANIC);
        dynasm!(a; .arch x64; =>halt_label);
        emit_exit(&mut a, &m, EXIT_HALT);

        // A memory fault: edx = faulting page; the frame's escalate slot is 1 when it becomes a panic (page below
        // MinValidAddress).
        let fault_escalate_label = a.new_dynamic_label();
        dynasm!(a
            ; .arch x64
            ; =>fault_label
            ; cmp DWORD [rsp + ESCALATE], DWORD 0
            ; jne =>fault_escalate_label
            ; mov rax, QWORD [rsp + OUT]
            ; mov DWORD [rax + 4], edx       // out->fault_page
        );
        emit_exit(&mut a, &m, EXIT_FAULT);
        dynasm!(a; .arch x64; =>fault_escalate_label);
        emit_exit(&mut a, &m, EXIT_PANIC);

        emit_bounds_check_subroutine(&mut a, bounds_check_label);
        for (i, width) in [1u8, 2, 4, 8].into_iter().enumerate() {
            for store in [false, true] {
                emit_slow_thunk(&mut a, cold.thunks[i][store as usize], width, store, bounds_check_label);
            }
        }

        a.commit().ok()?;
        let buf = a.finalize().ok()?;
        Some((buf.to_vec(), ops.len() as u32, entry_offsets))
    }
}
