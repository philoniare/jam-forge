
use super::{ri, with_pcs, xorshift64};
use crate::*;
use std::ffi::c_void;

const BASE: u32 = 0x1_0000;
const LEN: u32 = 0x2000;

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

struct Slices {
    regs: *mut u64,
    gas: *mut i64,
    held: i64,
    slice: i64,
}

impl Slices {
    unsafe fn restore(&mut self) {
        *self.gas += self.held;
        self.held = 0;
    }

    unsafe fn cut(&mut self) {
        if self.slice > 0 && *self.gas > self.slice {
            self.held = *self.gas - self.slice;
            *self.gas = self.slice;
        }
    }
}

const HOST_COST: i64 = 3;
const CALL_READ_GAS: i64 = 0;
const CALL_PANIC: i64 = 1;
const CALL_OOG_NEXT: i64 = 2;
const CALL_COSTLY: i64 = 3;

extern "C" fn host(ctx: *mut c_void, id: u64, _pc: u32) -> i64 {
    unsafe {
        let s = &mut *(ctx as *mut Slices);
        s.restore();
        let cost = if id as i64 == CALL_COSTLY { 1 << 40 } else { HOST_COST };
        *s.gas -= cost;
        if *s.gas < 0 {
            return HOST_OOG;
        }
        match id as i64 {
            CALL_READ_GAS => *s.regs.add(7) = *s.gas as u64,
            CALL_PANIC => return HOST_PANIC,
            CALL_OOG_NEXT => return HOST_OOG_NEXT,
            _ => {}
        }
        s.cut();
        HOST_CONTINUE
    }
}

#[derive(Debug, PartialEq)]
struct Outcome {
    exit: u32,
    pc: u32,
    fault_page: u32,
    gas: i64,
    regs: [u64; 13],
    mem: Vec<u8>,
}

fn run(block: &Block, regs0: &[u64; 13], gas: i64, slice: i64) -> (Outcome, u32) {
    let mut regs = *regs0;
    let mut cell = gas;
    let mut mem = vec![0u8; LEN as usize];
    let regions = [Region { base: BASE, len: LEN, buf_offset: 0, writable: 1 }];
    let regs_ptr = regs.as_mut_ptr();
    let gas_ptr = &mut cell as *mut i64;
    let mut s = Slices { regs: regs_ptr, gas: gas_ptr, held: 0, slice };
    let s_ptr = &mut s as *mut Slices;
    let mut entry = 0u32;
    let mut resumed = 0u32;
    let (exit, out) = unsafe {
        (*s_ptr).cut();
        loop {
            let mut out = ExecOut::default();
            let exit = pvm_execute(
                block.0,
                regs_ptr,
                gas_ptr,
                regions.as_ptr(),
                1,
                mem.as_mut_ptr(),
                12,
                entry,
                &mut out,
                Some(host),
                s_ptr as *mut c_void,
            );
            if exit == EXIT_OOG && (*s_ptr).held > 0 {
                if *gas_ptr == (*s_ptr).slice {
                    (*s_ptr).slice *= 2;
                }
                (*s_ptr).restore();
                (*s_ptr).cut();
                assert_eq!(out.pc % 4, 0);
                entry = out.pc / 4;
                resumed += 1;
                assert!(resumed < 10_000_000, "the run does not get anywhere");
                continue;
            }
            (*s_ptr).restore();
            break (exit, out);
        }
    };
    (Outcome { exit, pc: out.pc, fault_page: out.fault_page, gas: cell, regs, mem }, resumed)
}

enum Tail {
    Halt,
    Panic,
    Fault,
    HostPanic,
    HostOogNext,
    HostCostly,
}

fn looping(n: i64, skip: i64, tail: Tail) -> Vec<RawInstr> {
    let mut prog = vec![
        ri(OP_LOAD_IMM64, 1, 0, 0, n),             // 0  r1 = turns left
        ri(OP_LOAD_IMM64, 2, 0, 0, 0),             // 1
        ri(OP_LOAD_IMM64, 3, 0, 0, BASE as i64),   // 2  r3 = memory
        ri(OP_LOAD_IMM64, 9, 0, 0, DJUMP_HALT as i64), // 3
        ri(OP_ADD_IMM64, 2, 2, 0, 3),              // 4  loop head
        ri(OP_STORE_INDIRECT_U64, 2, 3, 0, 8),     // 5  mem[r3 + 8] = r2
        ri(OP_BRANCH_EQ_IMM, 1, 0, 0, skip),       // 6  -> 9
        ri(OP_ECALLI, 0, 0, 0, CALL_READ_GAS),     // 7  r7 = gas
        ri(OP_ADD64, 4, 4, 7, 0),                  // 8  r4 += what the guest saw
        ri(OP_LOAD_INDIRECT_U64, 5, 3, 0, 8),      // 9
        ri(OP_ADD64, 6, 6, 5, 0),                  // 10
        ri(OP_STORE_INDIRECT_U64, 4, 3, 0, 0x100), // 11
        ri(OP_ADD_IMM64, 1, 1, 0, -1),             // 12
        ri(OP_BRANCH_NE_IMM, 1, 0, 0, 0),          // 13 -> 4
        ri(OP_ADD_IMM64, 10, 6, 0, 77),            // 14 after the loop
    ];
    prog[6].imm2 = 9;
    prog[13].imm2 = 4;
    match tail {
        Tail::Halt => prog.push(ri(OP_JUMP_INDIRECT, 9, 0, 0, 0)),
        Tail::Panic => prog.push(ri(OP_PANIC, 0, 0, 0, 0)),
        Tail::Fault => prog.push(ri(OP_STORE_INDIRECT_U64, 2, 3, 0, LEN as i64 - 7)),
        Tail::HostPanic => prog.push(ri(OP_ECALLI, 0, 0, 0, CALL_PANIC)),
        Tail::HostOogNext => prog.push(ri(OP_ECALLI, 0, 0, 0, CALL_OOG_NEXT)),
        Tail::HostCostly => prog.push(ri(OP_ECALLI, 0, 0, 0, CALL_COSTLY)),
    }
    prog.push(ri(OP_PANIC, 0, 0, 0, 0));
    with_pcs(prog)
}

const SLICES: [i64; 12] = [1, 2, 3, 4, 5, 7, 10, 16, 33, 100, 1000, 1 << 24];

fn check(block: &Block, regs: &[u64; 13], gas: i64, slice: i64, what: &str) -> (Outcome, u32) {
    let (whole, never) = run(block, regs, gas, 0);
    assert_eq!(never, 0);
    let (sliced, resumed) = run(block, regs, gas, slice);
    assert_eq!(sliced, whole, "{what}: {gas} gas, {slice} at a time ({resumed} times entered again)");
    (whole, resumed)
}

#[test]
fn a_sliced_run_ends_as_a_whole_one_however_it_ends() {
    let tails = [
        (Tail::Halt, EXIT_HALT, "halt"),
        (Tail::Panic, EXIT_PANIC, "panic"),
        (Tail::Fault, EXIT_FAULT, "fault"),
        (Tail::HostPanic, EXIT_PANIC, "panic in a host call"),
        (Tail::HostOogNext, EXIT_OOG, "out of gas after a host call"),
        (Tail::HostCostly, EXIT_OOG, "a host call that costs more than there is"),
    ];
    for (tail, exit, what) in tails {
        let block = Block::compile(&looping(40, 17, tail));
        let regs = [0u64; 13];
        let mut most_resumed = 0;
        for slice in SLICES {
            let (whole, resumed) = check(&block, &regs, 100_000, slice, what);
            assert_eq!(whole.exit, exit, "{what}");
            most_resumed = most_resumed.max(resumed);
        }
        assert!(most_resumed > 10, "{what}: the slices were meant to interrupt the run, {most_resumed} did");
    }
}

#[test]
fn a_sliced_run_runs_out_of_gas_where_a_whole_one_does() {
    let block = Block::compile(&looping(25, 3, Tail::Halt));
    let regs = [0u64; 13];
    let (enough, _) = run(&block, &regs, 100_000, 0);
    assert_eq!(enough.exit, EXIT_HALT);
    let needed = 100_000 - enough.gas;
    let mut out_of_gas = 0;
    for gas in 0..=needed + 2 {
        for slice in [1, 2, 3, 5, 8, 13, 50] {
            let (whole, _) = check(&block, &regs, gas, slice, "running out");
            if gas < needed {
                assert_eq!(whole.exit, EXIT_OOG, "{gas} of {needed} gas");
                out_of_gas += 1;
            } else {
                assert_eq!(whole.exit, EXIT_HALT, "{gas} of {needed} gas");
                assert_eq!(whole.gas, gas - needed);
            }
        }
    }
    assert!(out_of_gas > 1000);
}

#[test]
fn the_guest_reads_the_same_gas_through_a_host_call_whatever_the_slice() {
    let block = Block::compile(&looping(30, 0, Tail::Halt));
    let regs = [0u64; 13];
    for slice in SLICES {
        let (whole, _) = check(&block, &regs, 50_000, slice, "gas readings");
        assert_eq!(whole.exit, EXIT_HALT);
        assert_ne!(whole.regs[4], 0);
        assert_eq!(u64::from_le_bytes(whole.mem[0x100..0x108].try_into().unwrap()), whole.regs[4]);
    }
}

#[test]
fn random_turns_gas_and_slices() {
    let mut state = 0x5EED_0F_51_1CE5u64;
    for _ in 0..400 {
        let n = 1 + (xorshift64(&mut state) % 60) as i64;
        let skip = (xorshift64(&mut state) % 70) as i64;
        let tail = match xorshift64(&mut state) % 6 {
            0 => Tail::Halt,
            1 => Tail::Panic,
            2 => Tail::Fault,
            3 => Tail::HostPanic,
            4 => Tail::HostOogNext,
            _ => Tail::HostCostly,
        };
        let block = Block::compile(&looping(n, skip, tail));
        let mut regs = [0u64; 13];
        for r in regs.iter_mut() {
            *r = xorshift64(&mut state);
        }
        let gas = (xorshift64(&mut state) % 2500) as i64;
        let slice = 1 + (xorshift64(&mut state) % 300) as i64;
        check(&block, &regs, gas, slice, "random");
    }
}
