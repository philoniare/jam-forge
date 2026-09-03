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

        Ref(int[] op, int[] dst, int[] src, int[] src2, long[] imm, long gas, long[] init) {
            long g = gas - op.length; // 1 gas/instr incl. terminator
            this.regs = init.clone();
            if (g < 0) {
                this.exit = PvmRecompiler.EXIT_OOG; // regs left at init
                this.gasRemaining = g;
                return;
            }
            int ex = PvmRecompiler.EXIT_PANIC;
            for (int i = 0; i < op.length; i++) {
                if (op[i] == PvmRecompiler.OP_TRAP) { ex = PvmRecompiler.EXIT_PANIC; break; }
                switch (op[i]) {
                    case PvmRecompiler.OP_LOAD_IMM64 -> regs[dst[i]] = imm[i];
                    case PvmRecompiler.OP_ADD_IMM64 -> regs[dst[i]] = regs[src[i]] + imm[i];
                    case PvmRecompiler.OP_ADD -> regs[dst[i]] = regs[src[i]] + regs[src2[i]];
                    case PvmRecompiler.OP_SUB -> regs[dst[i]] = regs[src[i]] - regs[src2[i]];
                    case PvmRecompiler.OP_MUL -> regs[dst[i]] = regs[src[i]] * regs[src2[i]];
                    default -> throw new IllegalStateException("bad opcode " + op[i]);
                }
            }
            this.exit = ex;
            this.gasRemaining = g;
        }
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
                int n = 1 + rng.nextInt(24);
                int[] op = new int[n + 1];
                int[] dst = new int[n + 1];
                int[] src = new int[n + 1];
                int[] src2 = new int[n + 1];
                long[] imm = new long[n + 1];
                for (int i = 0; i < n; i++) {
                    op[i] = 1 + rng.nextInt(5); // OP_LOAD_IMM64..OP_MUL
                    dst[i] = rng.nextInt(R);
                    src[i] = rng.nextInt(R);
                    src2[i] = rng.nextInt(R);
                    imm[i] = rng.nextLong();
                }
                op[n] = PvmRecompiler.OP_TRAP;

                long gas = switch (rng.nextInt(4)) {
                    case 0 -> (long) op.length - 1; // one short -> OOG boundary
                    case 1 -> (long) op.length;     // exactly enough
                    default -> (long) op.length + rng.nextInt(1000);
                };

                long[] init = new long[R];
                for (int i = 0; i < R; i++) init[i] = rng.nextLong();

                Ref ref = new Ref(op, dst, src, src2, imm, gas, init);

                long[] regsNat = init.clone();
                var blk = rc.compile(op, dst, src, src2, imm);
                if (!blk.isValid()) { System.out.println("compile null at it=" + it); mismatches++; continue; }
                long[] out = rc.execute(blk, regsNat, gas);
                blk.close();
                int natExit = (int) out[0];
                long natGas = out[1];

                checked++;
                if (natExit != ref.exit || natGas != ref.gasRemaining || !equals(regsNat, ref.regs)) {
                    mismatches++;
                    if (mismatches <= 5) {
                        System.out.printf("MISMATCH it=%d seed=%d n=%d gas=%d%n", it, seed, n, gas);
                        System.out.printf("  exit nat=%d ref=%d | gas nat=%d ref=%d%n",
                                natExit, ref.exit, natGas, ref.gasRemaining);
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
