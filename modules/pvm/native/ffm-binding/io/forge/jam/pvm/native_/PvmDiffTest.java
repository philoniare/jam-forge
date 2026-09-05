package io.forge.jam.pvm.native_;

import java.nio.file.Path;
import java.util.Random;

/**
 * Differential harness
 */
public final class PvmDiffTest {

    static final long DEFAULT_SEED = 0x6A414D5F53454544L; // "JAM_SEED"

    /**
     * Independent reference model of the skeleton subset
     */
    static final class Ref {
        int exit;
        long gasRemaining;
        final long[] regs;
        final byte[] mem;

        Ref(int[] op, int[] dst, int[] src, int[] src2, long[] imm, long gas, long[] init, byte[] mem0, int[] jumpTable) {
            int n = op.length;
            this.regs = init.clone();
            this.mem = mem0.clone();
            // Block decomposition: leaders = {0} ∪ static targets ∪ jump-table
            // (indirect) targets ∪ after-terminator.
            boolean[] leader = new boolean[n + 1];
            if (n > 0) leader[0] = true;
            for (int t : jumpTable) if (t < n) leader[t] = true;
            for (int i = 0; i < n; i++) {
                if (isTerminator(op[i])) {
                    Integer t = targetOf(op[i], imm[i]);
                    if (t != null && t < n) leader[t] = true;
                    if (i + 1 < n) leader[i + 1] = true;
                }
            }
            int[] hiOf = new int[n]; // block end (exclusive) for the block owning pc
            for (int i = 0; i < n; ) {
                int lo = i; i++;
                while (i < n && !leader[i]) i++;
                for (int p = lo; p < i; p++) hiOf[p] = i;
            }

            long g = gas;
            int pc = 0;
            while (true) {
                int hi = hiOf[pc];
                g -= (hi - pc); // block cost = instruction count
                if (g < 0) { exit = PvmRecompiler.EXIT_OOG; gasRemaining = g; return; }
                int next = -1;
                for (int p = pc; p < hi; p++) {
                    int o = op[p];
                    if (o == PvmRecompiler.OP_TRAP) { exit = PvmRecompiler.EXIT_PANIC; gasRemaining = g; return; }
                    switch (o) {
                        case PvmRecompiler.OP_LOAD_IMM64 -> regs[dst[p]] = imm[p];
                        case PvmRecompiler.OP_ADD_IMM64 -> regs[dst[p]] = regs[src[p]] + imm[p];
                        case PvmRecompiler.OP_ADD -> regs[dst[p]] = regs[src[p]] + regs[src2[p]];
                        case PvmRecompiler.OP_SUB -> regs[dst[p]] = regs[src[p]] - regs[src2[p]];
                        case PvmRecompiler.OP_MUL -> regs[dst[p]] = regs[src[p]] * regs[src2[p]];
                        case PvmRecompiler.OP_LOAD_U64 -> {
                            long addr = (regs[src[p]] + imm[p]) & 0xFFFFFFFFL;
                            if (addr + 8 > mem.length) { exit = PvmRecompiler.EXIT_FAULT; gasRemaining = g; return; }
                            regs[dst[p]] = readLE(mem, (int) addr);
                        }
                        case PvmRecompiler.OP_STORE_U64 -> {
                            long addr = (regs[src[p]] + imm[p]) & 0xFFFFFFFFL;
                            if (addr + 8 > mem.length) { exit = PvmRecompiler.EXIT_FAULT; gasRemaining = g; return; }
                            writeLE(mem, (int) addr, regs[dst[p]]);
                        }
                        case PvmRecompiler.OP_JUMP -> { next = (int) imm[p]; }
                        case PvmRecompiler.OP_BRANCH_EQ -> next = (regs[src[p]] == regs[src2[p]]) ? (int) imm[p] : hi;
                        case PvmRecompiler.OP_BRANCH_NE -> next = (regs[src[p]] != regs[src2[p]]) ? (int) imm[p] : hi;
                        case PvmRecompiler.OP_DJUMP -> {
                            long tv = regs[src[p]];
                            if (tv == PvmRecompiler.DJUMP_HALT) { exit = PvmRecompiler.EXIT_HALT; gasRemaining = g; return; }
                            int nx = -1;
                            for (int t : jumpTable) if (t < n && tv == t) { nx = t; break; }
                            if (nx < 0) { exit = PvmRecompiler.EXIT_PANIC; gasRemaining = g; return; }
                            next = nx;
                        }
                        default -> throw new IllegalStateException("bad opcode " + o);
                    }
                    if (next >= 0) break; // control-flow op ends the block
                }
                pc = (next >= 0) ? next : hi; // fall through to next block if no branch
                // Last instruction is always TRAP, so pc never runs off the end.
            }
        }
    }

    static boolean isTerminator(int op) {
        return op == PvmRecompiler.OP_TRAP || op == PvmRecompiler.OP_JUMP
                || op == PvmRecompiler.OP_BRANCH_EQ || op == PvmRecompiler.OP_BRANCH_NE
                || op == PvmRecompiler.OP_DJUMP;
    }

    static Integer targetOf(int op, long imm) {
        return switch (op) {
            case PvmRecompiler.OP_JUMP, PvmRecompiler.OP_BRANCH_EQ, PvmRecompiler.OP_BRANCH_NE -> (int) imm;
            default -> null;
        };
    }

    static long readLE(byte[] m, int off) {
        long v = 0;
        for (int i = 0; i < 8; i++) v |= (m[off + i] & 0xFFL) << (i * 8);
        return v;
    }

    static void writeLE(byte[] m, int off, long v) {
        for (int i = 0; i < 8; i++) m[off + i] = (byte) ((v >>> (i * 8)) & 0xFF);
    }

    public static void main(String[] args) {
        Path lib = Path.of(args.length > 0 ? args[0]
                : "modules/pvm/native/build/mac/libpvm_recompiler.dylib").toAbsolutePath();
        int iters = args.length > 1 ? Integer.parseInt(args[1]) : 100_000;
        long seed = args.length > 2 ? Long.parseLong(args[2]) : DEFAULT_SEED;

        Random rng = new Random(seed);
        final int R = PvmRecompiler.REG_COUNT;
        int mismatches = 0, checked = 0;

        try (var rc = new PvmRecompiler(lib)) {
            for (int it = 0; it < iters; it++) {
                // Guest memory: random size (0..128, multiple of 8) + random contents.
                int memLen = 8 * rng.nextInt(17);
                byte[] mem0 = new byte[memLen];
                rng.nextBytes(mem0);

                int n = 1 + rng.nextInt(24);
                int[] op = new int[n + 1];
                int[] dst = new int[n + 1];
                int[] src = new int[n + 1];
                int[] src2 = new int[n + 1];
                long[] imm = new long[n + 1];
                boolean memEnabled = memLen > 0;
                int total = n + 1; // includes the trailing trap; valid target range

                // Jump table: 0..3 valid indirect-jump target indices.
                int jtN = rng.nextInt(4);
                int[] jumpTable = new int[jtN];
                for (int j = 0; j < jtN; j++) jumpTable[j] = rng.nextInt(total);

                for (int i = 0; i < n; i++) {
                    if (i + 1 < n && rng.nextInt(12) == 0) {
                        int rD = 1 + rng.nextInt(R - 1);
                        long chosen = switch (rng.nextInt(3)) {
                            case 0 -> jtN > 0 ? jumpTable[rng.nextInt(jtN)] : rng.nextInt(total); // valid jump
                            case 1 -> PvmRecompiler.DJUMP_HALT; // clean halt
                            default -> rng.nextLong(); // untabled -> panic
                        };
                        op[i] = PvmRecompiler.OP_LOAD_IMM64; dst[i] = rD; src[i] = 0; src2[i] = 0; imm[i] = chosen;
                        i++;
                        op[i] = PvmRecompiler.OP_DJUMP; dst[i] = 0; src[i] = rD; src2[i] = 0; imm[i] = 0;
                        continue;
                    }

                    // Opcode menu: 1..5 arith, 6..7 mem (if enabled), 8..10 control.
                    op[i] = 1 + rng.nextInt(10);
                    if (!memEnabled && (op[i] == PvmRecompiler.OP_LOAD_U64 || op[i] == PvmRecompiler.OP_STORE_U64)) {
                        op[i] = 1 + rng.nextInt(5); // no memory -> arith only for this slot
                    }
                    dst[i] = 1 + rng.nextInt(R - 1); // never r0
                    src2[i] = rng.nextInt(R);
                    switch (op[i]) {
                        case PvmRecompiler.OP_LOAD_U64, PvmRecompiler.OP_STORE_U64 -> {
                            src[i] = 0; // base register (pinned 0)
                            imm[i] = 8L * rng.nextInt((memLen + 64) / 8); // some OOB
                        }
                        case PvmRecompiler.OP_JUMP, PvmRecompiler.OP_BRANCH_EQ, PvmRecompiler.OP_BRANCH_NE -> {
                            src[i] = rng.nextInt(R);
                            imm[i] = rng.nextInt(total); // valid instruction index (incl. trap)
                        }
                        default -> {
                            src[i] = rng.nextInt(R);
                            imm[i] = rng.nextLong();
                        }
                    }
                }
                op[n] = PvmRecompiler.OP_TRAP;

                long gas = switch (rng.nextInt(4)) {
                    case 0 -> (long) op.length - 1; // one short -> OOG boundary
                    case 1 -> (long) op.length;     // exactly enough
                    default -> (long) op.length + rng.nextInt(1000);
                };

                long[] init = new long[R];
                for (int i = 0; i < R; i++) init[i] = rng.nextLong();
                init[0] = 0; // memory base

                Ref ref = new Ref(op, dst, src, src2, imm, gas, init, mem0, jumpTable);

                long[] regsNat = init.clone();
                byte[] memNat = mem0.clone();
                var blk = rc.compile(op, dst, src, src2, imm, jumpTable);
                if (!blk.isValid()) { System.out.println("compile null at it=" + it); mismatches++; continue; }
                long[] out = rc.execute(blk, regsNat, gas, memNat);
                blk.close();
                int natExit = (int) out[0];
                long natGas = out[1];

                checked++;
                boolean bad = natExit != ref.exit || natGas != ref.gasRemaining
                        || !equals(regsNat, ref.regs) || !java.util.Arrays.equals(memNat, ref.mem);
                if (bad) {
                    mismatches++;
                    if (mismatches <= 5) {
                        System.out.printf("MISMATCH it=%d seed=%d n=%d gas=%d memLen=%d%n", it, seed, n, gas, memLen);
                        System.out.printf("  exit nat=%d ref=%d | gas nat=%d ref=%d | memEq=%b%n",
                                natExit, ref.exit, natGas, ref.gasRemaining, java.util.Arrays.equals(memNat, ref.mem));
                        for (int i = 0; i < R; i++)
                            if (regsNat[i] != ref.regs[i])
                                System.out.printf("  r%d nat=%d ref=%d%n", i, regsNat[i], ref.regs[i]);
                    }
                }
            }
        }
        System.out.printf("differential: checked=%d mismatches=%d seed=%d%n", checked, mismatches, seed);
        if (mismatches != 0) System.exit(1);
    }

    static boolean equals(long[] a, long[] b) {
        for (int i = 0; i < a.length; i++) if (a[i] != b[i]) return false;
        return true;
    }
}
