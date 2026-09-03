package io.forge.jam.pvm.native_;

import java.nio.file.Path;
import java.util.Random;

/**
 * Differential harness.
 */
public final class PvmDiffTest {

    static final long DEFAULT_SEED = 0x6A414D5F53454544L; // "JAM_SEED"

    /** Independent reference model of the skeleton subset. */
    static final class Ref {
        final int exit;
        final long gasRemaining;
        final long[] regs;
        final byte[] mem;

        Ref(int[] op, int[] dst, int[] src, int[] src2, long[] imm, long gas, long[] init, byte[] mem0) {
            long g = gas - op.length; // 1 gas/instr incl. terminator
            this.regs = init.clone();
            this.mem = mem0.clone();
            this.gasRemaining = g;
            if (g < 0) {
                this.exit = PvmRecompiler.EXIT_OOG; // regs + mem left at init
                return;
            }
            int ex = PvmRecompiler.EXIT_PANIC;
            for (int i = 0; i < op.length; i++) {
                if (op[i] == PvmRecompiler.OP_TRAP) break;
                switch (op[i]) {
                    case PvmRecompiler.OP_LOAD_IMM64 -> regs[dst[i]] = imm[i];
                    case PvmRecompiler.OP_ADD_IMM64 -> regs[dst[i]] = regs[src[i]] + imm[i];
                    case PvmRecompiler.OP_ADD -> regs[dst[i]] = regs[src[i]] + regs[src2[i]];
                    case PvmRecompiler.OP_SUB -> regs[dst[i]] = regs[src[i]] - regs[src2[i]];
                    case PvmRecompiler.OP_MUL -> regs[dst[i]] = regs[src[i]] * regs[src2[i]];
                    case PvmRecompiler.OP_LOAD_U64 -> {
                        long addr = (regs[src[i]] + imm[i]) & 0xFFFFFFFFL;
                        if (addr + 8 > mem.length) { ex = PvmRecompiler.EXIT_FAULT; i = op.length; break; }
                        regs[dst[i]] = readLE(mem, (int) addr);
                    }
                    case PvmRecompiler.OP_STORE_U64 -> {
                        long addr = (regs[src[i]] + imm[i]) & 0xFFFFFFFFL;
                        if (addr + 8 > mem.length) { ex = PvmRecompiler.EXIT_FAULT; i = op.length; break; }
                        writeLE(mem, (int) addr, regs[dst[i]]);
                    }
                    default -> throw new IllegalStateException("bad opcode " + op[i]);
                }
            }
            this.exit = ex;
        }
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
                for (int i = 0; i < n; i++) {
                    int maxOp = memEnabled ? 7 : 5;
                    op[i] = 1 + rng.nextInt(maxOp); // OP_LOAD_IMM64..(OP_STORE_U64)
                    dst[i] = 1 + rng.nextInt(R - 1); // never r0
                    src2[i] = rng.nextInt(R);
                    if (op[i] == PvmRecompiler.OP_LOAD_U64 || op[i] == PvmRecompiler.OP_STORE_U64) {
                        src[i] = 0; // base register (pinned 0)
                        // aligned offset in [0, memLen+64): mostly in-bounds, some OOB
                        imm[i] = 8L * rng.nextInt((memLen + 64) / 8);
                    } else {
                        src[i] = rng.nextInt(R);
                        imm[i] = rng.nextLong();
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

                Ref ref = new Ref(op, dst, src, src2, imm, gas, init, mem0);

                long[] regsNat = init.clone();
                byte[] memNat = mem0.clone();
                var blk = rc.compile(op, dst, src, src2, imm);
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
