
use super::{ri, with_pcs, xorshift64};
use crate::*;
use std::ffi::c_void;

const GUEST_SPACE: u64 = 1 << 32;
const PERM_PAGE_SHIFT: u32 = 12;
const PERM_READ: u8 = 1;
const PERM_READ_WITH_PREVIOUS: u8 = 2;
const PERM_WRITE: u8 = 4;
const PERM_WRITE_WITH_PREVIOUS: u8 = 8;

const PAGE: u64 = 1 << PERM_PAGE_SHIFT;
const MIN_VALID_ADDRESS: u32 = 0x10000;
const PAGE_SHIFT: u32 = 12;

#[derive(Clone, Copy, Debug)]
struct Rgn {
    base: u32,
    len: u32,
    writable: bool,
    room: u32,
}

const fn ro(base: u32, len: u32) -> Rgn {
    Rgn { base, len, writable: false, room: 0 }
}

const fn rw(base: u32, len: u32) -> Rgn {
    Rgn { base, len, writable: true, room: 0 }
}

impl Rgn {
    fn end(&self) -> u64 {
        self.base as u64 + self.len as u64
    }
}

fn pattern(addr: u32) -> u8 {
    (addr.wrapping_mul(0x9E37_79B1) >> 24) as u8
}

fn initial(regions: &[Rgn]) -> Vec<Vec<u8>> {
    regions.iter().map(|r| (0..r.len).map(|i| pattern(r.base.wrapping_add(i))).collect()).collect()
}

const GUARD: usize = 1 << 16;

struct FlatContext {
    reservation: *mut c_void,
    guest: *mut u8,
    perms: Vec<u8>,
    regions: Vec<Region>,
    in_use: bool,
}

impl FlatContext {
    fn new() -> FlatContext {
        assert_eq!((GUEST_SPACE >> PERM_PAGE_SHIFT) as usize, PAGE_PERMS_LEN);
        let total = GUEST_SPACE as usize + 2 * GUARD;
        unsafe {
            let reservation = libc::mmap(
                std::ptr::null_mut(),
                total,
                libc::PROT_NONE,
                libc::MAP_PRIVATE | libc::MAP_ANON | libc::MAP_NORESERVE,
                -1,
                0,
            );
            assert!(reservation != libc::MAP_FAILED, "mmap of the 4 GiB guest address space failed");
            let guest = (reservation as *mut u8).add(GUARD);
            let rc = libc::mprotect(guest as *mut c_void, GUEST_SPACE as usize, libc::PROT_READ | libc::PROT_WRITE);
            assert_eq!(rc, 0, "mprotect of the 4 GiB guest address space failed");
            FlatContext { reservation, guest, perms: vec![0u8; PAGE_PERMS_LEN], regions: Vec::new(), in_use: false }
        }
    }

    fn open_pages(&mut self, base: u64, len: u64, writable: bool) {
        let first = ((base + PAGE - 1) >> PERM_PAGE_SHIFT) as usize;
        let end = ((base + len) >> PERM_PAGE_SHIFT) as usize;
        if end <= first {
            return;
        }
        let alone = if writable { PERM_READ | PERM_WRITE } else { PERM_READ };
        let both =
            alone | if writable { PERM_READ_WITH_PREVIOUS | PERM_WRITE_WITH_PREVIOUS } else { PERM_READ_WITH_PREVIOUS };
        self.perms[first] = alone;
        if end > first + 1 {
            self.perms[first + 1..end].fill(both);
        }
    }

    fn link_first_page(&mut self, base: u64, len: u64, writable: bool) {
        let first = ((base + PAGE - 1) >> PERM_PAGE_SHIFT) as usize;
        let end = ((base + len) >> PERM_PAGE_SHIFT) as usize;
        if end <= first || first == 0 {
            return;
        }
        let previous = self.perms[first - 1];
        let mut own = self.perms[first];
        if previous & PERM_READ != 0 {
            own |= PERM_READ_WITH_PREVIOUS;
        }
        if writable && previous & PERM_WRITE != 0 {
            own |= PERM_WRITE_WITH_PREVIOUS;
        }
        self.perms[first] = own;
    }

    fn close_pages(&mut self, base: u64, len: u64) {
        let first = ((base + PAGE - 1) >> PERM_PAGE_SHIFT) as usize;
        let end = ((base + len) >> PERM_PAGE_SHIFT) as usize;
        if end > first {
            self.perms[first..end].fill(0);
        }
    }

    fn begin(&mut self, regions: &[Rgn], contents: &[Vec<u8>]) {
        assert!(!self.in_use, "a context runs one execution at a time");
        assert_eq!(regions.len(), contents.len());
        self.regions.clear();
        for (r, bytes) in regions.iter().zip(contents) {
            let (base, len) = (r.base as u64, r.len as u64);
            assert!(
                base + len <= GUEST_SPACE && bytes.len() as u64 <= len,
                "region {r:?} does not lie in the 32-bit guest address space, or its contents are longer"
            );
            self.regions.push(Region { base: r.base, len: r.len, buf_offset: r.base, writable: r.writable as u32 });
            if len > 0 {
                unsafe { std::ptr::copy_nonoverlapping(bytes.as_ptr(), self.guest.add(base as usize), bytes.len()) };
                self.open_pages(base, len, r.writable);
            }
        }
        for r in regions {
            self.link_first_page(r.base as u64, r.len as u64, r.writable);
        }
        self.in_use = true;
    }

    fn region_grown(&mut self, index: usize, new_len: u64) {
        let base = self.regions[index].base as u64;
        let writable = self.regions[index].writable != 0;
        self.regions[index].len = new_len as u32;
        self.open_pages(base, new_len, writable);
        self.link_first_page(base, new_len, writable);
    }

    fn finish(&mut self) {
        for i in 0..self.regions.len() {
            let (base, len) = (self.regions[i].base as u64, self.regions[i].len as u64);
            if len > 0 {
                unsafe { std::ptr::write_bytes(self.guest.add(base as usize), 0, len as usize) };
                self.close_pages(base, len);
            }
        }
        self.in_use = false;
    }

    fn region_bytes(&self) -> Vec<Vec<u8>> {
        self.regions
            .iter()
            .map(|r| unsafe { std::slice::from_raw_parts(self.guest.add(r.base as usize), r.len as usize) }.to_vec())
            .collect()
    }

    fn assert_nothing_written_outside_regions(&self, watch: &[u32]) {
        let mut spans: Vec<(u64, u64)> =
            self.regions.iter().filter(|r| r.len > 0).map(|r| (r.base as u64, r.base as u64 + r.len as u64)).collect();
        spans.sort_unstable();
        let mut probes = vec![(0, 64), (GUEST_SPACE - 64, GUEST_SPACE)];
        for &(lo, hi) in &spans {
            probes.push((lo.saturating_sub(PAGE), lo));
            probes.push((hi, (hi + PAGE).min(GUEST_SPACE)));
        }
        for &addr in watch {
            let addr = addr as u64;
            probes.push((addr.saturating_sub(64), (addr + 64).min(GUEST_SPACE)));
        }
        for (lo, hi) in probes {
            let mut at = lo;
            for &(region_lo, region_hi) in &spans {
                if at >= hi {
                    break;
                }
                if region_hi <= at {
                    continue;
                }
                if region_lo > at {
                    self.assert_zero(at, region_lo.min(hi));
                }
                at = region_hi;
            }
            if at < hi {
                self.assert_zero(at, hi);
            }
        }
    }

    fn assert_zero(&self, lo: u64, hi: u64) {
        let bytes = unsafe { std::slice::from_raw_parts(self.guest.add(lo as usize), (hi - lo) as usize) };
        if let Some(i) = bytes.iter().position(|&b| b != 0) {
            panic!(
                "the flat run wrote outside its regions: guest byte {:#010x} is {:#04x} (regions {:x?})",
                lo + i as u64,
                bytes[i],
                self.regions
            );
        }
    }

    fn assert_table_empty(&self) {
        if let Some(page) = self.perms.iter().position(|&b| b != 0) {
            panic!("page {page:#x} is still marked {:#x} after the run was finished", self.perms[page]);
        }
    }
}

impl Drop for FlatContext {
    fn drop(&mut self) {
        unsafe { libc::munmap(self.reservation, GUEST_SPACE as usize + 2 * GUARD) };
    }
}

const CANARY: u8 = 0xC5;
const CANARY_LEN: usize = 64;

fn pack(regions: &[Rgn], contents: &[Vec<u8>]) -> (Vec<Region>, Vec<u8>) {
    let mut table = Vec::with_capacity(regions.len());
    let mut backing = vec![CANARY; CANARY_LEN];
    for (r, bytes) in regions.iter().zip(contents) {
        table.push(Region { base: r.base, len: r.len, buf_offset: backing.len() as u32, writable: r.writable as u32 });
        backing.extend_from_slice(bytes);
        backing.resize(backing.len() + (r.len as usize - bytes.len()) + r.room as usize, 0);
        backing.resize(backing.len() + CANARY_LEN, CANARY);
    }
    (table, backing)
}

fn unpack(regions: &[Rgn], table: &[Region], backing: &[u8]) -> Vec<Vec<u8>> {
    let mut mem = Vec::with_capacity(regions.len());
    let mut at = 0usize;
    let expect = |lo: usize, hi: usize, byte: u8, what: &str| {
        if let Some(i) = backing[lo..hi].iter().position(|&b| b != byte) {
            panic!(
                "the region-scan run wrote outside its regions: {what} byte {} of the packed backing is {:#04x}",
                lo + i,
                backing[lo + i]
            );
        }
    };
    for (r, live) in regions.iter().zip(table) {
        let offset = live.buf_offset as usize;
        expect(at, offset, CANARY, "canary");
        let live_end = offset + live.len as usize;
        let room_end = offset + r.len as usize + r.room as usize;
        assert!(live_end <= room_end, "region {r:?} grew past its room");
        mem.push(backing[offset..live_end].to_vec());
        expect(live_end, room_end, 0, "unreached room");
        at = room_end;
    }
    expect(at, backing.len(), CANARY, "canary");
    mem
}

struct Block(*mut CompiledBlock);

impl Block {
    fn compile(prog: &[RawInstr]) -> Block {
        let prog = with_block_gas(prog, &[]);
        let block = unsafe { pvm_compile(prog.as_ptr(), prog.len(), std::ptr::null(), 0, prog.len() as u32 * 4) };
        assert!(!block.is_null(), "compile returned null");
        Block(block)
    }
}

impl Drop for Block {
    fn drop(&mut self) {
        unsafe { pvm_free(self.0) };
    }
}

#[derive(Clone, Copy, Debug)]
struct Grow {
    index: usize,
    new_len: u32,
}

struct HostState {
    grow: Option<Grow>,
    flat: *mut FlatContext,
    table: *mut Region,
    calls: u32,
}

extern "C" fn host_call(ctx: *mut c_void, _host_call_id: u64, _pc: u32) -> i64 {
    let state = unsafe { &mut *(ctx as *mut HostState) };
    state.calls += 1;
    let Some(grow) = state.grow else { return HOST_PANIC };
    unsafe {
        if state.flat.is_null() {
            (*state.table.add(grow.index)).len = grow.new_len;
        } else {
            (*state.flat).region_grown(grow.index, grow.new_len as u64);
        }
    }
    HOST_CONTINUE
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Outcome {
    exit: u32,
    pc: u32,
    fault_page: u32,
    gas: i64,
    regs: [u64; 13],
    mem: Vec<Vec<u8>>,
}

fn exit_name(exit: u32) -> &'static str {
    match exit {
        EXIT_HALT => "HALT",
        EXIT_PANIC => "PANIC",
        EXIT_OOG => "OOG",
        EXIT_FAULT => "FAULT",
        _ => "?",
    }
}

impl Outcome {
    fn summary(&self) -> String {
        match self.exit {
            EXIT_FAULT => format!("FAULT at page {:#010x}, pc {}", self.fault_page, self.pc),
            exit => format!("{}, pc {}", exit_name(exit), self.pc),
        }
    }

    fn difference(&self, other: &Outcome) -> Option<String> {
        let mut d = Vec::new();
        if (self.exit, self.pc, self.fault_page) != (other.exit, other.pc, other.fault_page) {
            d.push(format!("[{}] vs [{}]", self.summary(), other.summary()));
        }
        if self.gas != other.gas {
            d.push(format!("gas {} vs {}", self.gas, other.gas));
        }
        for i in 0..13 {
            if self.regs[i] != other.regs[i] {
                d.push(format!("r{i} {:#x} vs {:#x}", self.regs[i], other.regs[i]));
            }
        }
        if self.mem.len() != other.mem.len() {
            d.push(format!("{} regions vs {}", self.mem.len(), other.mem.len()));
        }
        for (i, (a, b)) in self.mem.iter().zip(&other.mem).enumerate() {
            if a.len() != b.len() {
                d.push(format!("region {i} is {} bytes vs {}", a.len(), b.len()));
            } else if let Some(at) = a.iter().zip(b).position(|(x, y)| x != y) {
                let count = a.iter().zip(b).filter(|(x, y)| x != y).count();
                d.push(format!(
                    "region {i} differs in {count} bytes, first +{at:#x}: {:#04x} vs {:#04x}",
                    a[at], b[at]
                ));
            }
        }
        if d.is_empty() {
            None
        } else {
            Some(d.join("; "))
        }
    }
}

fn run_scan(
    block: &Block,
    regions: &[Rgn],
    contents: &[Vec<u8>],
    mut regs: [u64; 13],
    mut gas: i64,
    grow: Option<Grow>,
) -> Outcome {
    let (mut table, mut backing) = pack(regions, contents);
    let table_ptr = table.as_mut_ptr();
    let mut host = HostState { grow, flat: std::ptr::null_mut(), table: table_ptr, calls: 0 };
    let mut out = ExecOut::default();
    let exit = unsafe {
        pvm_execute(
            block.0,
            regs.as_mut_ptr(),
            &mut gas,
            table_ptr,
            regions.len() as u64,
            backing.as_mut_ptr(),
            PAGE_SHIFT,
            0,
            &mut out,
            Some(host_call),
            &mut host as *mut HostState as *mut c_void,
        )
    };
    assert_eq!(host.calls, grow.is_some() as u32, "host calls made on the region-scan path");
    let mem = unpack(regions, &table, &backing);
    Outcome { exit, pc: out.pc, fault_page: out.fault_page, gas, regs, mem }
}

fn run_flat(
    ctx: &mut FlatContext,
    block: &Block,
    regions: &[Rgn],
    contents: &[Vec<u8>],
    mut regs: [u64; 13],
    mut gas: i64,
    grow: Option<Grow>,
    watch: &[u32],
) -> Outcome {
    ctx.begin(regions, contents);
    let ctx_ptr: *mut FlatContext = ctx;
    let mut host = HostState { grow, flat: ctx_ptr, table: std::ptr::null_mut(), calls: 0 };
    let mut out = ExecOut::default();
    let exit = unsafe {
        pvm_execute_flat(
            block.0,
            regs.as_mut_ptr(),
            &mut gas,
            (*ctx_ptr).regions.as_ptr(),
            regions.len() as u64,
            (*ctx_ptr).guest,
            PAGE_SHIFT,
            0,
            &mut out,
            Some(host_call),
            &mut host as *mut HostState as *mut c_void,
            (*ctx_ptr).perms.as_ptr(),
        )
    };
    assert_eq!(host.calls, grow.is_some() as u32, "host calls made on the flat path");
    ctx.assert_nothing_written_outside_regions(watch);
    let mem = ctx.region_bytes();
    ctx.finish();
    Outcome { exit, pc: out.pc, fault_page: out.fault_page, gas, regs, mem }
}

#[derive(Clone, Copy, Debug)]
enum Kind {
    Load { signed: bool },
    Store,
    StoreImm { value: u64 },
}

impl Kind {
    fn is_store(&self) -> bool {
        !matches!(self, Kind::Load { .. })
    }
}

#[derive(Clone, Copy, Debug)]
enum Place {
    Indirect { base: u8, offset: i64 },
    Absolute { imm: i64 },
}

#[derive(Clone, Copy, Debug)]
struct Step {
    kind: Kind,
    width: u8,
    place: Place,
    reg: u8,
}

impl Step {
    fn address(&self, regs: &[u64; 13]) -> u32 {
        match self.place {
            Place::Indirect { base, offset } => regs[base as usize].wrapping_add(offset as u64) as u32,
            Place::Absolute { imm } => imm as u32,
        }
    }

    fn instr(&self) -> RawInstr {
        let (reg, width) = (self.reg as u32, self.width);
        let by_width = |ops: [u32; 4]| ops[width.trailing_zeros() as usize];
        match (self.kind, self.place) {
            (Kind::Load { signed }, Place::Indirect { base, offset }) => {
                let op = match (width, signed) {
                    (1, false) => OP_LOAD_INDIRECT_U8,
                    (1, true) => OP_LOAD_INDIRECT_I8,
                    (2, false) => OP_LOAD_INDIRECT_U16,
                    (2, true) => OP_LOAD_INDIRECT_I16,
                    (4, false) => OP_LOAD_INDIRECT_U32,
                    (4, true) => OP_LOAD_INDIRECT_I32,
                    _ => OP_LOAD_INDIRECT_U64,
                };
                ri(op, reg, base as u32, 0, offset)
            }
            (Kind::Load { signed }, Place::Absolute { imm }) => {
                let op = match (width, signed) {
                    (1, false) => OP_LOAD_U8,
                    (1, true) => OP_LOAD_I8,
                    (2, false) => OP_LOAD_U16,
                    (2, true) => OP_LOAD_I16,
                    (4, false) => OP_LOAD_U32,
                    (4, true) => OP_LOAD_I32,
                    _ => OP_LOAD_U64,
                };
                ri(op, reg, 0, 0, imm)
            }
            (Kind::Store, Place::Indirect { base, offset }) => ri(
                by_width([OP_STORE_INDIRECT_U8, OP_STORE_INDIRECT_U16, OP_STORE_INDIRECT_U32, OP_STORE_INDIRECT_U64]),
                reg,
                base as u32,
                0,
                offset,
            ),
            (Kind::Store, Place::Absolute { imm }) => {
                ri(by_width([OP_STORE_U8, OP_STORE_U16, OP_STORE_U32, OP_STORE_U64]), reg, 0, 0, imm)
            }
            (Kind::StoreImm { value }, Place::Indirect { base, offset }) => RawInstr {
                imm2: value as i64,
                ..ri(
                    by_width([
                        OP_STORE_IMM_INDIRECT_U8,
                        OP_STORE_IMM_INDIRECT_U16,
                        OP_STORE_IMM_INDIRECT_U32,
                        OP_STORE_IMM_INDIRECT_U64,
                    ]),
                    base as u32,
                    0,
                    0,
                    offset,
                )
            },
            (Kind::StoreImm { value }, Place::Absolute { imm }) => RawInstr {
                imm2: value as i64,
                ..ri(by_width([OP_STORE_IMM_U8, OP_STORE_IMM_U16, OP_STORE_IMM_U32, OP_STORE_IMM_U64]), 0, 0, 0, imm)
            },
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Expect {
    Ok,
    Fault(u32),
    Panic,
}

fn fault_at(addr: u64) -> Expect {
    let page = (addr & !(PAGE - 1)) as u32;
    if page < MIN_VALID_ADDRESS {
        Expect::Panic
    } else {
        Expect::Fault(page)
    }
}

struct Model {
    regions: Vec<Rgn>,
    mem: Vec<Vec<u8>>,
}

impl Model {
    fn new(regions: &[Rgn], contents: &[Vec<u8>]) -> Model {
        Model { regions: regions.to_vec(), mem: contents.to_vec() }
    }

    fn holder(&self, addr: u64, store: bool) -> Option<usize> {
        self.regions.iter().position(|r| addr >= r.base as u64 && addr < r.end() && (!store || r.writable))
    }

    fn verdict(&self, addr: u32, width: u8, store: bool) -> Expect {
        let (lo, hi) = (addr as u64, addr as u64 + width as u64);
        if self.regions.iter().any(|r| lo >= r.base as u64 && hi <= r.end() && (!store || r.writable)) {
            return Expect::Ok;
        }
        fault_at(if self.holder(lo, store).is_some() { hi - 1 } else { lo })
    }

    fn byte(&mut self, addr: u64) -> &mut u8 {
        let i = self.holder(addr, false).unwrap_or_else(|| panic!("no region holds guest byte {addr:#x}"));
        let offset = (addr - self.regions[i].base as u64) as usize;
        &mut self.mem[i][offset]
    }

    fn apply(&mut self, step: &Step, regs: &mut [u64; 13]) {
        let addr = step.address(regs) as u64;
        let width = step.width as u64;
        match step.kind {
            Kind::Load { signed } => {
                let mut value = 0u64;
                for i in 0..width {
                    value |= (*self.byte(addr + i) as u64) << (8 * i);
                }
                if signed && width < 8 {
                    let shift = 64 - 8 * width;
                    value = (((value << shift) as i64) >> shift) as u64;
                }
                regs[step.reg as usize] = value;
            }
            Kind::Store | Kind::StoreImm { .. } => {
                let value = match step.kind {
                    Kind::StoreImm { value } => value,
                    _ => regs[step.reg as usize],
                };
                for i in 0..width {
                    *self.byte(addr + i) = (value >> (8 * i)) as u8;
                }
            }
        }
    }
}

const GAS: i64 = 1000;
const HALT_REG: u8 = 0;

fn seed_regs() -> [u64; 13] {
    let mut regs = [0u64; 13];
    for (i, r) in regs.iter_mut().enumerate() {
        *r = 0xF1E2_D3C4_B5A6_9788u64 + 0x0101_0101_0101_0101 * i as u64;
    }
    regs[HALT_REG as usize] = DJUMP_HALT;
    regs
}

fn program(lead: &[RawInstr], steps: &[Step]) -> Vec<RawInstr> {
    let mut prog = lead.to_vec();
    prog.extend(steps.iter().map(Step::instr));
    prog.push(ri(OP_JUMP_INDIRECT, HALT_REG as u32, 0, 0, 0));
    with_pcs(prog)
}

fn expected(
    model: &mut Model,
    lead: usize,
    steps: &[Step],
    verdicts: Option<&[Expect]>,
    mut regs: [u64; 13],
) -> Outcome {
    let gas = GAS - (lead + steps.len() + 1) as i64;
    for (i, step) in steps.iter().enumerate() {
        let pc = ((lead + i) * 4) as u32;
        let verdict = match verdicts {
            Some(v) => v[i],
            None => model.verdict(step.address(&regs), step.width, step.kind.is_store()),
        };
        match verdict {
            Expect::Ok => model.apply(step, &mut regs),
            Expect::Fault(page) => {
                return Outcome { exit: EXIT_FAULT, pc, fault_page: page, gas, regs, mem: model.mem.clone() }
            }
            Expect::Panic => return Outcome { exit: EXIT_PANIC, pc, fault_page: 0, gas, regs, mem: model.mem.clone() },
        }
    }
    Outcome { exit: EXIT_HALT, pc: ((lead + steps.len()) * 4) as u32, fault_page: 0, gas, regs, mem: model.mem.clone() }
}

const WIDTHS: [u8; 4] = [1, 2, 4, 8];
const IMM_VALUE: u64 = 0x8899_AABB_CCDD_EEFF;

fn kinds(width: u8) -> Vec<Kind> {
    let mut kinds = vec![Kind::Load { signed: false }];
    if width < 8 {
        kinds.push(Kind::Load { signed: true });
    }
    kinds.push(Kind::Store);
    kinds.push(Kind::StoreImm { value: IMM_VALUE });
    kinds
}

const REG_PAIRS: [(u8, u8); 5] = [(2, 3), (7, 8), (9, 9), (1, 12), (12, 5)];

#[derive(Clone, Copy, Debug)]
enum Via {
    Reg { base: u64, offset: i64 },
    Abs { imm: i64 },
}

const OK: Expect = Expect::Ok;
const PANIC: Expect = Expect::Panic;

const fn fault(page: u32) -> Expect {
    Expect::Fault(page)
}

const fn reg(base: u64, offset: i64) -> Via {
    Via::Reg { base, offset }
}

const fn abs(imm: i64) -> Via {
    Via::Abs { imm }
}

impl Via {
    fn step(self, kind: Kind, width: u8, turn: usize) -> (Step, [u64; 13]) {
        let (base_reg, reg) = REG_PAIRS[turn % REG_PAIRS.len()];
        let mut regs = seed_regs();
        let place = match self {
            Via::Reg { base, offset } => {
                regs[base_reg as usize] = base;
                Place::Indirect { base: base_reg, offset }
            }
            Via::Abs { imm } => Place::Absolute { imm },
        };
        (Step { kind, width, place, reg }, regs)
    }
}

struct Harness {
    regions: Vec<Rgn>,
    contents: Vec<Vec<u8>>,
    ctx: FlatContext,
    runs: usize,
}

impl Harness {
    fn new(regions: &[Rgn]) -> Harness {
        Harness { regions: regions.to_vec(), contents: initial(regions), ctx: FlatContext::new(), runs: 0 }
    }

    fn run_program(
        &mut self,
        prog: &[RawInstr],
        regs: [u64; 13],
        gas: i64,
        grow: Option<Grow>,
        watch: &[u32],
    ) -> (Outcome, Outcome) {
        let block = Block::compile(prog);
        let scan = run_scan(&block, &self.regions, &self.contents, regs, gas, grow);
        let flat = run_flat(&mut self.ctx, &block, &self.regions, &self.contents, regs, gas, grow, watch);
        self.runs += 1;
        (scan, flat)
    }

    fn run(&mut self, lead: &[RawInstr], steps: &[Step], regs: [u64; 13], grow: Option<Grow>) -> (Outcome, Outcome) {
        let watch: Vec<u32> = steps.iter().map(|s| s.address(&regs)).collect();
        self.run_program(&program(lead, steps), regs, GAS, grow, &watch)
    }

    fn check(&mut self, steps: &[Step], verdicts: Option<&[Expect]>, regs: [u64; 13]) -> Outcome {
        let (scan, flat) = self.run(&[], steps, regs, None);
        let what = || {
            let addrs: Vec<String> = steps.iter().map(|s| format!("{:#010x}", s.address(&regs))).collect();
            format!("{steps:x?} (addresses {addrs:?}) over {:x?}", self.regions)
        };
        if let Some(d) = scan.difference(&flat) {
            panic!("the two paths disagree: region scan vs page table: {d}\n  running {}", what());
        }
        let want = expected(&mut Model::new(&self.regions, &self.contents), 0, steps, verdicts, regs);
        if let Some(d) = flat.difference(&want) {
            panic!("both paths did the same, but not what was expected: got vs expected: {d}\n  running {}", what());
        }
        flat
    }

    fn access(&mut self, via: Via, width: u8, load: Expect, store: Expect) {
        for kind in kinds(width) {
            let (step, regs) = via.step(kind, width, self.runs);
            let expect = if kind.is_store() { store } else { load };
            self.check(&[step], Some(&[expect]), regs);
        }
    }

    fn at(&mut self, addr: u32, width: u8, load: Expect, store: Expect) {
        let a = addr as u64;
        self.access(reg(a, 0), width, load, store);
        self.access(reg(a.wrapping_sub(0x7F0) & 0xFFFF_FFFF, 0x7F0), width, load, store);
        self.access(reg((a + 0x1234) & 0xFFFF_FFFF, -0x1234), width, load, store);
        self.access(abs(a as i64), width, load, store);
        self.access(abs(addr as i32 as i64), width, load, store);
    }
}

const A: Rgn = ro(0x0001_0000, 0x3000); // [0x00010000, 0x00013000)
const B: Rgn = rw(0x0002_0000, 0x4000); // [0x00020000, 0x00024000)
const C: Rgn = rw(0x0003_0800, 0x1C00); // [0x00030800, 0x00032400): page 0x30 in part, 0x31 whole, 0x32 in part
const D: Rgn = rw(0x7FFF_E000, 0x5000); // [0x7fffe000, 0x80003000)
const E: Rgn = ro(0xC000_0000, 0x2000); // [0xc0000000, 0xc0002000)
const MAIN: [Rgn; 5] = [A, B, C, D, E];

fn store_into(r: &Rgn, addr: u32) -> Expect {
    if r.writable {
        Expect::Ok
    } else {
        fault_at(addr as u64)
    }
}

#[test]
fn page_table_is_built_as_the_java_context_builds_it() {
    const R: u8 = PERM_READ;
    const RP: u8 = PERM_READ_WITH_PREVIOUS;
    const W: u8 = PERM_WRITE;
    const WP: u8 = PERM_WRITE_WITH_PREVIOUS;
    let regions = [
        rw(0x0000_0000, 0x2000), // pages 0 and 1
        ro(0x0000_2000, 0x1000), // page 2, straight after a writable page
        rw(0x0001_0800, 0x2000), // [0x10800, 0x12800): only page 0x11 lies wholly inside
        ro(0x0002_0000, 0x3000), // pages 0x20..=0x22
        rw(0x0002_3000, 0x2000), // pages 0x23 and 0x24, straight after read-only pages
        rw(0x0002_5000, 0x1000), // page 0x25, straight after writable pages
        rw(0x0003_0000, 0x0800), // less than a page: nothing to open
        rw(0xFFFF_E000, 0x2000), // the last two pages of the address space
    ];
    let contents = initial(&regions);
    let mut ctx = FlatContext::new();
    ctx.begin(&regions, &contents);

    let expected: [(usize, u8); 12] = [
        (0x0, R | W),
        (0x1, R | W | RP | WP),
        (0x2, R | RP),
        (0x11, R | W),
        (0x20, R),
        (0x21, R | RP),
        (0x22, R | RP),
        (0x23, R | W | RP),
        (0x24, R | W | RP | WP),
        (0x25, R | W | RP | WP),
        (0xFFFFE, R | W),
        (0xFFFFF, R | W | RP | WP),
    ];
    for (page, bits) in expected {
        assert_eq!(ctx.perms[page], bits, "page {page:#x}");
    }
    assert_eq!(ctx.perms.iter().filter(|&&b| b != 0).count(), expected.len(), "no other page is open");

    for (r, live) in regions.iter().zip(&ctx.regions) {
        assert_eq!((live.base, live.len, live.buf_offset, live.writable), (r.base, r.len, r.base, r.writable as u32));
    }
    assert_eq!(ctx.region_bytes(), contents);

    ctx.finish();
    ctx.assert_table_empty();
    ctx.assert_nothing_written_outside_regions(&[]);
    assert!(ctx.region_bytes().iter().all(|bytes| bytes.iter().all(|&b| b == 0)), "finish zeroes the regions");
}

#[test]
fn accesses_inside_one_page() {
    let mut h = Harness::new(&MAIN);
    for width in WIDTHS {
        let w = width as u32;
        for addr in [0x2_0010, 0x2_0FF0, 0x2_1000, 0x2_1800, 0x2_2000 - w, 0x2_3000, 0x2_3FF0] {
            h.at(addr, width, OK, OK);
        }
        for addr in [0x1_0010, 0x1_1000, 0x1_2000 - w, 0x1_2FF0] {
            h.at(addr, width, OK, fault(addr & !0xFFF));
        }
        for addr in [0x3_0900, 0x3_1000 - w, 0x3_1000, 0x3_1FF8, 0x3_2000, 0x3_2300] {
            h.at(addr, width, OK, OK);
        }
        for addr in [0x7FFF_E100, 0x7FFF_F800, 0x8000_0000 - w, 0x8000_0000, 0x8000_2FF0] {
            h.at(addr, width, OK, OK);
        }
        for addr in [0xC000_0040, 0xC000_1FF0] {
            h.at(addr, width, OK, fault(addr & !0xFFF));
        }
    }
}

#[test]
fn first_and_last_bytes_of_each_region() {
    let mut h = Harness::new(&MAIN);
    for r in MAIN {
        let (base, end) = (r.base, r.end() as u32);
        for width in WIDTHS {
            let w = width as u32;
            h.at(base, width, OK, store_into(&r, base));
            h.at(end - w, width, OK, store_into(&r, end - w));
            let past = end - w + 1;
            let store = if r.writable { fault_at(end as u64) } else { fault_at(past as u64) };
            h.at(past, width, fault_at(end as u64), store);
            h.at(base - 1, width, fault_at(base as u64 - 1), fault_at(base as u64 - 1));
        }
    }
}

#[test]
fn accesses_straddling_two_pages_of_one_region() {
    let mut h = Harness::new(&MAIN);
    let boundaries = [
        (A, 0x1_1000),
        (A, 0x1_2000),
        (B, 0x2_1000), // out of the region's first page
        (B, 0x2_3000),
        (C, 0x3_1000), // out of a page that is only partly inside the region
        (C, 0x3_2000), // into one
        (D, 0x7FFF_F000),
        (D, 0x8000_0000), // bit 31 changes within the access
        (D, 0x8000_2000),
        (E, 0xC000_1000),
    ];
    for (r, boundary) in boundaries {
        for width in [2u8, 4, 8] {
            for before in 1..width as u32 {
                let addr = boundary - before;
                h.at(addr, width, OK, store_into(&r, addr));
            }
        }
    }
}

#[test]
fn accesses_straddling_the_edge_of_a_region() {
    let mut h = Harness::new(&MAIN);
    for r in MAIN {
        let (base, end) = (r.base as u64, r.end());
        for width in [2u8, 4, 8] {
            for outside in 1..width as u64 {
                let addr = end - (width as u64 - outside);
                let store = if r.writable { fault_at(end) } else { fault_at(addr) };
                h.at(addr as u32, width, fault_at(end), store);
                let addr = base - outside;
                h.at(addr as u32, width, fault_at(addr), fault_at(addr));
            }
        }
    }
    assert_eq!(fault_at(A.base as u64 - 1), PANIC);
    assert_eq!(fault_at(A.end()), fault(0x1_3000));
    assert_eq!(fault_at(C.base as u64 - 1), fault(0x3_0000));
    assert_eq!(fault_at(C.end()), fault(0x3_2000));
    assert_eq!(fault_at(D.end()), fault(0x8000_3000));
}

#[test]
fn stores_to_read_only_regions_fault_and_leave_them_alone() {
    let mut h = Harness::new(&MAIN);
    for r in [A, E] {
        for width in WIDTHS {
            for addr in (r.base..r.end() as u32 - 8).step_by(0x3F1) {
                h.at(addr, width, OK, fault(addr & !0xFFF));
            }
        }
    }
}

#[test]
fn accesses_between_regions_fault_on_their_own_page() {
    let mut h = Harness::new(&MAIN);
    let gaps = [
        0x0001_3000u32,
        0x0001_8000,
        0x0001_FFF8,
        0x0002_4000,
        0x0003_0000,
        0x0003_07F8,
        0x0003_2400,
        0x0004_0000,
        0x7000_0000,
        0x7FFF_DFF8,
        0x8000_3000,
        0xA000_0FFC,
        0xBFFF_FFF8,
        0xC000_2000,
        0xE000_0000,
    ];
    for addr in gaps {
        for width in WIDTHS {
            h.at(addr, width, fault(addr & !0xFFF), fault(addr & !0xFFF));
        }
    }
}

#[test]
fn faults_below_64k_are_panics() {
    let mut h = Harness::new(&MAIN);
    for addr in [0u32, 1, 0xFFC, 0x1000, 0x8000, 0xFFF8] {
        for width in WIDTHS {
            h.at(addr, width, PANIC, PANIC);
        }
    }
    for width in WIDTHS {
        h.at(0x1_0000 - width as u32, width, PANIC, PANIC);
        h.at(0x1_0000, width, OK, fault(0x1_0000));
    }
}

#[test]
fn regions_below_64k_are_served_and_only_faults_there_panic() {
    let low = [rw(0x0000_0000, 0x2000), ro(0x0000_4000, 0x1000), rw(0x0000_F000, 0x2000)];
    let mut h = Harness::new(&low);
    for width in WIDTHS {
        let w = width as u32;
        for addr in [0, 0x10, 0x1000 - w, 0x1000, 0x2000 - w, 0xF000, 0x1_0000 - w, 0x1_0000, 0x1_1000 - w] {
            h.at(addr, width, OK, OK);
        }
        h.at(0x4000, width, OK, PANIC); // a store to a read-only page below the line
        h.at(0x2000, width, PANIC, PANIC); // unmapped
        h.at(0x1_1000, width, fault(0x1_1000), fault(0x1_1000)); // unmapped, above the line
    }
    for width in [2u8, 4, 8] {
        let w = width as u32;
        h.at(0x1000 - w / 2, width, OK, OK); // pages 0 and 1
        h.at(0x1_0000 - w / 2, width, OK, OK); // across the 64 KiB line inside one region
        h.at(0x2000 - w / 2, width, PANIC, PANIC); // off the end of the first region
        h.at(0x1_1000 - w / 2, width, fault(0x1_1000), fault(0x1_1000));
    }
}

#[test]
fn top_of_the_address_space_unmapped() {
    let mut h = Harness::new(&MAIN);
    let top = fault(0xFFFF_F000);
    for (addr, width) in [(0xFFFF_FFF8u32, 8u8), (0xFFFF_FFFC, 4), (0xFFFF_FFFE, 2), (0xFFFF_FFFF, 1), (0xFFFF_FFF8, 1)]
    {
        h.at(addr, width, top, top);
    }
    for (addr, width) in [(0xFFFF_FFFCu32, 8u8), (0xFFFF_FFFD, 8), (0xFFFF_FFFD, 4), (0xFFFF_FFFF, 2), (0xFFFF_FFF9, 8)]
    {
        h.at(addr, width, top, top);
    }
}

#[test]
fn top_of_the_address_space_mapped_up_to_its_last_byte() {
    let mut regions = MAIN.to_vec();
    regions.push(rw(0xFFFF_E000, 0x1FFF)); // [0xffffe000, 0xffffffff)
    let mut h = Harness::new(&regions);
    for (addr, width) in [(0xFFFF_FFF7u32, 8u8), (0xFFFF_FFFB, 4), (0xFFFF_FFFD, 2), (0xFFFF_FFFE, 1)] {
        h.at(addr, width, OK, OK);
    }
    for width in [2u8, 4, 8] {
        h.at(0xFFFF_F000 - width as u32 / 2, width, OK, OK); // into the last page
    }
    let top = fault(0xFFFF_F000);
    for (addr, width) in [(0xFFFF_FFF8u32, 8u8), (0xFFFF_FFFC, 4), (0xFFFF_FFFE, 2), (0xFFFF_FFFF, 1)] {
        h.at(addr, width, top, top);
    }
    for (addr, width) in [(0xFFFF_FFFCu32, 8u8), (0xFFFF_FFFD, 8), (0xFFFF_FFFD, 4), (0xFFFF_FFF9, 8)] {
        h.at(addr, width, PANIC, PANIC);
    }
    for width in [2u8, 4, 8] {
        h.at(0xFFFF_FFFF, width, top, top);
    }
}

#[test]
fn wrapping_accesses_never_take_the_inline_path() {
    let regions = [rw(0x0000_0000, 0x2000), rw(0x0002_0000, 0x1000), rw(0xFFFF_E000, 0x2000)];
    let contents = initial(&regions);
    let mut h = Harness::new(&regions);
    for (addr, width) in [
        (0xFFFF_FFFCu32, 8u8),
        (0xFFFF_FFFD, 8),
        (0xFFFF_FFFD, 4),
        (0xFFFF_FFFF, 2),
        (0xFFFF_FFF9, 8),
        (0xFFFF_FFFF, 8),
    ] {
        let a = addr as u64;
        for kind in kinds(width) {
            for via in [reg(a, 0), reg(a + 0x10, -0x10), abs(a as i64)] {
                let (step, regs) = via.step(kind, width, h.runs);
                let (scan, flat) = h.run(&[], &[step], regs, None);
                if let Some(d) = scan.difference(&flat) {
                    panic!("the two paths disagree on {step:x?} at {addr:#x}: region scan vs page table: {d}");
                }
                assert_ne!(flat.exit, EXIT_HALT, "{step:x?} at {addr:#x} wraps past 2^32 and must not succeed");
                assert_eq!(flat.pc, 0);
                assert_eq!(flat.regs, regs, "{step:x?} at {addr:#x}");
                assert_eq!(flat.mem, contents, "{step:x?} at {addr:#x}");
            }
        }
    }
}

#[test]
fn addresses_with_bit_31_set() {
    let mut h = Harness::new(&MAIN);
    for width in WIDTHS {
        let w = width as i64;
        h.access(reg(0x7FFF_F000, 0x1800), width, OK, OK);
        h.access(reg(0x4000_0000, 0x4000_1000), width, OK, OK);
        h.access(reg(0x7FFF_FFFF, 1), width, OK, OK);
        h.access(reg(0x8000_0000, 0x3000 - w), width, OK, OK);
        h.access(reg(0x8000_0000, 0x3000 - w + 1), width, fault(0x8000_3000), fault(0x8000_3000));
        h.access(reg(0x1000, 0x8000_0000), width, OK, OK);
        h.access(reg(0x1000, 0x8000_0000u32 as i32 as i64), width, OK, OK);
        h.access(reg(0x10, 0xC000_0000), width, OK, fault(0xC000_0000));
        h.access(reg(0x10, 0xC000_0000u32 as i32 as i64), width, OK, fault(0xC000_0000));
        h.access(reg(0x8000_1000, -8), width, OK, OK);
        h.access(reg(0x8000_0000, -0x1000), width, OK, OK);
        h.access(reg(0x8000_0000, -1), width, OK, OK);
        h.access(reg(0xC000_2000, -w), width, OK, fault(0xC000_1000));
        h.access(reg(0xC000_0000, -1), width, fault(0xBFFF_F000), fault(0xBFFF_F000));
        h.access(reg(0xFFFF_FFFF_8000_1000, 0), width, OK, OK);
        h.access(reg(0xFFFF_FFFF_C000_0FF8, 0), width, OK, fault(0xC000_0000));
        h.access(reg(0xFFFF_FFFF_8000_3000, -w), width, OK, OK);
        for addr in [0x8000_0000u32, 0x8000_1000, 0x8000_3000 - width as u32] {
            h.access(abs(addr as i64), width, OK, OK);
            h.access(abs(addr as i32 as i64), width, OK, OK);
        }
        h.access(abs(0xC000_0FF8), width, OK, fault(0xC000_0000));
        h.access(abs(0xC000_0FF8u32 as i32 as i64), width, OK, fault(0xC000_0000));
        h.access(abs(0x9000_0000), width, fault(0x9000_0000), fault(0x9000_0000));
        h.access(abs(0x9000_0000u32 as i32 as i64), width, fault(0x9000_0000), fault(0x9000_0000));
    }
}

#[test]
fn base_register_above_4gib_is_taken_mod_2_32() {
    let mut h = Harness::new(&MAIN);
    for width in WIDTHS {
        h.access(reg(0x0000_0001_0002_1000, 0), width, OK, OK);
        h.access(reg(0xFFFF_FFFF_0002_1000, 8), width, OK, OK);
        h.access(reg(0xDEAD_BEEF_0002_0FFC, 0), width, OK, OK);
        h.access(reg(0x8000_0000_0002_3FF8, 0), width, OK, OK);
        h.access(reg(0x0000_0123_0001_0FFC, 0), width, OK, fault(0x1_0000));
        h.access(reg(0x0000_0009_8000_0010, -0x20), width, OK, OK);
        h.access(reg(0xFFFF_FFF0, 0x2_1010), width, OK, OK);
        h.access(reg(0xFFFF_FFFF_FFFF_FFF0, 0x2_1010), width, OK, OK);
        h.access(reg(0xFFFF_FFFF, 0x7FFF_FFFF), width, OK, OK); // 0x7ffffffe, in D
        h.access(reg(0xFFFF_FFFF_FFFF_FFFF, 0x7FFF_FFFF), width, OK, OK);

        h.access(reg(0x10, -0x20), width, fault(0xFFFF_F000), fault(0xFFFF_F000));
        h.access(reg(0x0000_0007_0001_8000, 0), width, fault(0x1_8000), fault(0x1_8000));
        h.access(reg(0x0000_0001_0000_0008, 0), width, PANIC, PANIC);
        h.access(reg(0xFFFF_FFFF_FFFF_FFFF, 1), width, PANIC, PANIC);
    }
}

const SCATTERED: [Rgn; 8] = [
    ro(0x0001_0000, 0x2000),
    rw(0x0001_2800, 0x0800), // half a page, half a page after the region before it
    rw(0x0002_0000, 0x3000),
    ro(0x0002_3400, 0x1A00), // [0x23400, 0x24e00): starts 0x400 after the region before it
    rw(0x0002_6000, 0x1000),
    rw(0x7FFF_F000, 0x2000),
    ro(0xBFFF_F800, 0x1000),
    rw(0xFFFF_C000, 0x3000), // [0xffffc000, 0xfffff000): the last page stays unmapped
];

fn interesting_address(state: &mut u64, regions: &[Rgn]) -> u32 {
    let r = regions[(xorshift64(state) % regions.len() as u64) as usize];
    let jitter = (xorshift64(state) % 24) as i64 - 12;
    let within = xorshift64(state) % r.len as u64;
    let addr = match xorshift64(state) % 16 {
        0..=3 => r.base as i64 + jitter,
        4..=7 => r.end() as i64 + jitter,
        8..=11 => ((r.base as u64 + within) & !(PAGE - 1)) as i64 + jitter,
        12..=13 => (r.base as u64 + within) as i64,
        14 => [0i64, 0x1_0000, 0x8000_0000, 0x1_0000_0000][(xorshift64(state) % 4) as usize] + jitter,
        _ => xorshift64(state) as u32 as i64,
    };
    addr as u32
}

fn random_via(state: &mut u64, addr: u32) -> Via {
    match xorshift64(state) % 4 {
        0 => Via::Abs { imm: addr as i64 },
        1 => Via::Abs { imm: addr as i32 as i64 },
        _ => {
            let offset = match xorshift64(state) % 4 {
                0 => 0,
                1 => (xorshift64(state) % 0x1000) as i64,
                2 => -((xorshift64(state) % 0x1000) as i64),
                _ => xorshift64(state) as i32 as i64,
            };
            let high = if xorshift64(state) % 2 == 0 { 0 } else { xorshift64(state) << 32 };
            Via::Reg { base: high | addr.wrapping_sub(offset as u32) as u64, offset }
        }
    }
}

fn random_kind(state: &mut u64, width: u8) -> Kind {
    match xorshift64(state) % 5 {
        0 | 1 => Kind::Load { signed: width < 8 && xorshift64(state) % 2 == 0 },
        2 | 3 => Kind::Store,
        _ => Kind::StoreImm { value: xorshift64(state) },
    }
}

#[test]
fn random_accesses_agree_on_both_paths_and_with_the_rule() {
    let mut h = Harness::new(&SCATTERED);
    let model = Model::new(&h.regions, &h.contents);
    let mut state = 0x5EED_F1A7_0000_0001u64;
    let (mut oks, mut faults, mut panics) = (0, 0, 0);
    for _ in 0..800 {
        let width = WIDTHS[(xorshift64(&mut state) % 4) as usize];
        let addr = interesting_address(&mut state, &SCATTERED);
        let kind = random_kind(&mut state, width);
        let turn = xorshift64(&mut state) as usize;
        let (step, regs) = random_via(&mut state, addr).step(kind, width, turn);
        assert_eq!(step.address(&regs), addr);
        let verdict = model.verdict(addr, width, kind.is_store());
        match verdict {
            Expect::Ok => oks += 1,
            Expect::Fault(_) => faults += 1,
            Expect::Panic => panics += 1,
        }
        h.check(&[step], Some(&[verdict]), regs);
    }
    assert!(oks >= 200 && faults >= 200 && panics >= 10, "{oks} ok, {faults} faults, {panics} panics");
    h.ctx.assert_table_empty();
}

#[test]
fn random_programs_of_several_accesses_agree_on_both_paths_and_with_the_rule() {
    const BASES: [u8; 4] = [1, 2, 7, 9];
    const VALUES: [u8; 6] = [3, 5, 8, 10, 11, 12];
    let mut h = Harness::new(&SCATTERED);
    let mut state = 0x5EED_F1A7_0000_0002u64;
    let (mut halted, mut stopped, mut accesses) = (0, 0, 0);
    for _ in 0..200 {
        let mut regs = seed_regs();
        for &b in &BASES {
            let r = SCATTERED[(xorshift64(&mut state) % SCATTERED.len() as u64) as usize];
            let high = if xorshift64(&mut state) % 4 == 0 { xorshift64(&mut state) << 32 } else { 0 };
            regs[b as usize] = high | (r.base as u64 + xorshift64(&mut state) % r.len as u64);
        }
        let model = Model::new(&h.regions, &h.contents);
        let steps: Vec<Step> = (0..2 + xorshift64(&mut state) % 11)
            .map(|_| {
                let width = WIDTHS[(xorshift64(&mut state) % 4) as usize];
                let base = BASES[(xorshift64(&mut state) % 4) as usize];
                let reg = VALUES[(xorshift64(&mut state) % 6) as usize];
                let mut step;
                let mut tries = 0;
                loop {
                    let kind = random_kind(&mut state, width);
                    let place = match xorshift64(&mut state) % 8 {
                        0 => Place::Absolute { imm: interesting_address(&mut state, &SCATTERED) as i64 },
                        1 => Place::Indirect { base, offset: (xorshift64(&mut state) % 0x2000) as i64 - 0x1000 },
                        _ => Place::Indirect { base, offset: (xorshift64(&mut state) % 64) as i64 - 32 },
                    };
                    step = Step { kind, width, place, reg };
                    tries += 1;
                    let verdict = model.verdict(step.address(&regs), width, kind.is_store());
                    if verdict == Expect::Ok || tries == 2 {
                        break;
                    }
                }
                step
            })
            .collect();
        let outcome = h.check(&steps, None, regs);
        accesses += outcome.pc as usize / 4;
        if outcome.exit == EXIT_HALT {
            halted += 1;
        } else {
            stopped += 1;
        }
    }
    assert!(
        halted >= 40 && stopped >= 40 && accesses >= 600,
        "{halted} halted, {stopped} stopped, {accesses} accesses done"
    );
    h.ctx.assert_table_empty();
}

#[test]
fn a_region_grown_in_a_host_call_is_reachable_on_both_paths() {
    let heap = Rgn { room: 0x4000, ..rw(0x0004_0000, 0x1800) };
    let regions = [ro(0x0001_0000, 0x1000), heap, rw(0x0006_0000, 0x1000)];
    let grow = Grow { index: 1, new_len: 0x4800 };
    let ecalli = [ri(OP_ECALLI, 0, 0, 0, 7)];
    let mut h = Harness::new(&regions);

    let mut grown = regions;
    grown[1].len = grow.new_len;
    let mut grown_contents = h.contents.clone();
    grown_contents[1].resize(grow.new_len as usize, 0);

    let step = |kind, width, addr: u32, reg| Step {
        kind,
        width,
        place: Place::Indirect { base: 2, offset: addr as i64 - 0x4_0000 },
        reg,
    };
    let mut regs = seed_regs();
    regs[2] = 0x4_0000;
    let all = [
        step(Kind::Store, 8, 0x4_17FC, 3), // across the old end of the region
        step(Kind::Load { signed: false }, 8, 0x4_17FC, 4),
        step(Kind::Store, 8, 0x4_1FFC, 5), // two pages that were both closed
        step(Kind::Load { signed: false }, 8, 0x4_1FFC, 6),
        step(Kind::StoreImm { value: IMM_VALUE }, 4, 0x4_3FFE, 0), // from the last whole page into the partial one
        step(Kind::Load { signed: true }, 4, 0x4_3FFE, 7),
        step(Kind::Store, 8, 0x4_47F8, 8), // the last eight bytes
        step(Kind::Load { signed: false }, 1, 0x4_47FF, 9),
        step(Kind::Load { signed: true }, 2, 0x4_0FFF, 10), // the pages open from the start still are
        step(Kind::Load { signed: false }, 1, 0x4_4800, 11), // one byte past the new end
    ];

    for (steps, exit) in [(&all[..all.len() - 1], EXIT_HALT), (&all[..], EXIT_FAULT)] {
        let (scan, flat) = h.run(&ecalli, steps, regs, Some(grow));
        if let Some(d) = scan.difference(&flat) {
            panic!("the two paths disagree after the heap grew: region scan vs page table: {d}");
        }
        let want = expected(&mut Model::new(&grown, &grown_contents), 1, steps, None, regs);
        if let Some(d) = flat.difference(&want) {
            panic!("after the heap grew: got vs expected: {d}");
        }
        assert_eq!(flat.exit, exit);
        assert_eq!(flat.mem[1].len(), grow.new_len as usize);
    }
    assert_eq!(fault_at(0x4_4800), fault(0x4_4000));

    let (scan, flat) = h.run(&[], &all, regs, None);
    assert_eq!(scan.difference(&flat), None);
    let want = expected(&mut Model::new(&regions, &h.contents), 0, &all, None, regs);
    assert_eq!(flat.difference(&want), None);
    assert_eq!((flat.exit, flat.fault_page, flat.pc), (EXIT_FAULT, 0x4_1000, 0));
    h.ctx.assert_table_empty();
}

#[test]
fn a_copy_loop_across_pages_agrees_on_both_paths() {
    let prog = with_pcs(vec![
        ri(OP_LOAD_INDIRECT_U64, 4, 1, 0, 0),
        ri(OP_STORE_INDIRECT_U64, 4, 2, 0, 0),
        ri(OP_ADD_IMM64, 1, 1, 0, 13),
        ri(OP_ADD_IMM64, 2, 2, 0, 13),
        ri(OP_ADD_IMM64, 3, 3, 0, -1),
        ri(OP_BRANCH_NE_IMM, 3, 0, 0, 0), // r3 != 0: back to instruction 0
        ri(OP_JUMP_INDIRECT, HALT_REG as u32, 0, 0, 0),
    ]);
    let load =
        Step { kind: Kind::Load { signed: false }, width: 8, place: Place::Indirect { base: 1, offset: 0 }, reg: 4 };
    let store = Step { kind: Kind::Store, width: 8, place: Place::Indirect { base: 2, offset: 0 }, reg: 4 };
    let mut h = Harness::new(&MAIN);

    let runs = [
        (900u64, 10_000i64, EXIT_HALT), // 11 700 bytes: stays inside both regions
        (1000, 10_000, EXIT_FAULT),     // the load runs off the end of A in iteration 945
        (900, 6 * 500 + 5, EXIT_OOG),   // the gas for 500 iterations, and not quite one more
    ];
    for (count, gas, exit) in runs {
        let mut regs = seed_regs();
        (regs[1], regs[2], regs[3]) = (A.base as u64 + 3, B.base as u64 + 1, count);
        let (scan, flat) = h.run_program(&prog, regs, gas, None, &[]);
        if let Some(d) = scan.difference(&flat) {
            panic!("the two paths disagree on the copy loop ({count} iterations, gas {gas}), scan vs table: {d}");
        }

        let mut model = Model::new(&h.regions, &h.contents);
        let mut want = Outcome { exit: EXIT_HALT, pc: 24, fault_page: 0, gas, regs, mem: Vec::new() };
        loop {
            if want.gas < 6 {
                (want.exit, want.pc) = (EXIT_OOG, 0); // the block that cannot be paid for is not charged
                break;
            }
            want.gas -= 6;
            let stopped = [(load, 0u32), (store, 4)].into_iter().find_map(|(step, pc)| {
                match model.verdict(step.address(&want.regs), 8, step.kind.is_store()) {
                    Expect::Ok => {
                        model.apply(&step, &mut want.regs);
                        None
                    }
                    Expect::Fault(page) => Some((EXIT_FAULT, pc, page)),
                    Expect::Panic => Some((EXIT_PANIC, pc, 0)),
                }
            });
            if let Some(stop) = stopped {
                (want.exit, want.pc, want.fault_page) = stop;
                break;
            }
            want.regs[1] += 13;
            want.regs[2] += 13;
            want.regs[3] -= 1;
            if want.regs[3] == 0 {
                want.gas -= 1; // the halting block
                break;
            }
        }
        want.mem = model.mem;
        if let Some(d) = flat.difference(&want) {
            panic!("the copy loop ({count} iterations, gas {gas}): got vs expected: {d}");
        }
        assert_eq!(flat.exit, exit, "{count} iterations, gas {gas}");
    }
}

#[derive(Default)]
struct Findings {
    disagree: Vec<String>,
    both_wrong: Vec<String>,
}

impl Findings {
    fn compare_at(&mut self, h: &mut Harness, addr: u32, width: u8, load: Expect, store: Expect) {
        for kind in kinds(width) {
            for via in [reg(addr as u64 - 0x40, 0x40), abs(addr as i64)] {
                let (step, regs) = via.step(kind, width, h.runs);
                let (scan, flat) = h.run(&[], &[step], regs, None);
                let expect = if kind.is_store() { store } else { load };
                let want = expected(&mut Model::new(&h.regions, &h.contents), 0, &[step], Some(&[expect]), regs);
                let form = if matches!(via, Via::Abs { .. }) { "absolute" } else { "indirect" };
                let what = format!("{kind:x?} of {width} at {addr:#010x} ({form}), which should be {expect:x?}");
                if scan != flat {
                    let verdict = |o: &Outcome| if *o == want { "right" } else { "WRONG" };
                    self.disagree.push(format!(
                        "{what}: region scan [{}] {}, page table [{}] {}",
                        scan.summary(),
                        verdict(&scan),
                        flat.summary(),
                        verdict(&flat)
                    ));
                } else if flat != want {
                    self.both_wrong.push(format!("{what}: both paths [{}]", flat.summary()));
                }
            }
        }
    }

    fn fail_if_any(&self, what: &str) {
        let section = |title: &str, list: &[String]| {
            let first: String = list.iter().take(8).map(|line| format!("\n    {line}")).collect();
            if list.is_empty() {
                String::new()
            } else {
                format!("\n  {title} on {} of them, the first being:{first}", list.len())
            }
        };
        assert!(
            self.disagree.is_empty() && self.both_wrong.is_empty(),
            "accesses {what}:{}{}",
            section("pvm_execute (region scan) and pvm_execute_flat (page table) DISAGREE", &self.disagree),
            section("both paths do the same wrong thing", &self.both_wrong)
        );
    }
}

#[test]
#[ignore = "known divergence in a layout the JVM does not build: regions that touch"]
fn divergence_access_straddling_two_regions_that_touch() {
    let regions = [
        ro(0x0004_0000, 0x2000), // read-only, then writable
        rw(0x0004_2000, 0x2000),
        rw(0x0005_0000, 0x2000), // writable, then read-only
        ro(0x0005_2000, 0x2000),
        rw(0x0006_0000, 0x2000), // writable twice
        rw(0x0006_2000, 0x2000),
        ro(0x0007_0000, 0x1000), // read-only twice
        ro(0x0007_1000, 0x1000),
    ];
    let mut h = Harness::new(&regions);
    let mut found = Findings::default();
    for width in [2u8, 4, 8] {
        for before in 1..width as u32 {
            found.compare_at(&mut h, 0x4_2000 - before, width, OK, fault(0x4_1000));
            found.compare_at(&mut h, 0x5_2000 - before, width, OK, fault(0x5_2000));
            found.compare_at(&mut h, 0x6_2000 - before, width, OK, OK);
            found.compare_at(&mut h, 0x7_1000 - before, width, OK, fault(0x7_0000));
        }
    }
    found.fail_if_any("that straddle two regions that touch");
}

#[test]
#[ignore = "known divergence in a layout the JVM does not build: a region ending at 2^32"]
fn divergence_region_ending_at_the_top_of_the_address_space() {
    let mut found = Findings::default();

    let mut regions = MAIN.to_vec();
    regions.push(rw(0xFFFF_E000, 0x2000));
    let mut h = Harness::new(&regions);
    for width in WIDTHS {
        let w = width as u32;
        for addr in [0u32.wrapping_sub(w), 0xFFFF_E000, 0xFFFF_F000 - w, 0xFFFF_F000] {
            found.compare_at(&mut h, addr, width, OK, OK);
        }
        if width > 1 {
            found.compare_at(&mut h, 0xFFFF_F000 - w / 2, width, OK, OK);
        }
    }

    let mut regions = MAIN.to_vec();
    regions.push(rw(0xFFFF_D800, 0x2800));
    let mut h = Harness::new(&regions);
    for width in WIDTHS {
        let w = width as u32;
        for addr in [0xFFFF_D800, 0xFFFF_E000 - w, 0u32.wrapping_sub(w)] {
            found.compare_at(&mut h, addr, width, OK, OK);
        }
        if width > 1 {
            found.compare_at(&mut h, 0xFFFF_E000 - w / 2, width, OK, OK);
        }
    }
    found.fail_if_any("to a region that ends at 2^32");
}
