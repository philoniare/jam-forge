package io.forge.jam.pvm.native_;

import java.lang.foreign.*;
import java.lang.invoke.MethodHandle;
import java.nio.file.Path;

/**
 * FFM binding to the native PVM recompiler
 */
public final class PvmRecompiler implements AutoCloseable {

    // RawInstr layout — must match the Rust #[repr(C)] struct exactly:
    //   u32 opcode, u32 a, u32 b, u32 c, i64 imm, i64 imm2   (32 bytes, align 8)
    private static final MemoryLayout RAW_INSTR = MemoryLayout.structLayout(
            ValueLayout.JAVA_INT.withName("opcode"),
            ValueLayout.JAVA_INT.withName("a"),
            ValueLayout.JAVA_INT.withName("b"),
            ValueLayout.JAVA_INT.withName("c"),
            ValueLayout.JAVA_LONG.withName("imm"),
            ValueLayout.JAVA_LONG.withName("imm2")
    );
    private static final long RAW_INSTR_SIZE = RAW_INSTR.byteSize(); // 32

    
    public static final long DJUMP_HALT = 0xFFFF_0000L;

    public static final int EXIT_HALT = 0;
    public static final int EXIT_PANIC = 1;
    public static final int EXIT_OOG = 2;
    public static final int EXIT_FAULT = 3;

    public static final int REG_COUNT = 13;

    private final MethodHandle compile;
    private final MethodHandle execute;
    private final MethodHandle free;
    private final Arena arena;

    /** Open the binding against the recompiler shared library at {@code libPath}. */
    public PvmRecompiler(Path libPath) {
        this.arena = Arena.ofShared();
        Linker linker = Linker.nativeLinker();
        SymbolLookup lookup = SymbolLookup.libraryLookup(libPath, arena);

        this.compile = linker.downcallHandle(
                lookup.find("pvm_compile").orElseThrow(() -> missing("pvm_compile")),
                FunctionDescriptor.of(ValueLayout.ADDRESS,
                        ValueLayout.ADDRESS,   // instrs
                        ValueLayout.JAVA_LONG, // n
                        ValueLayout.ADDRESS,   // jump table
                        ValueLayout.JAVA_LONG  // jt_n
                ));
        this.execute = linker.downcallHandle(
                lookup.find("pvm_execute").orElseThrow(() -> missing("pvm_execute")),
                FunctionDescriptor.of(ValueLayout.JAVA_INT,
                        ValueLayout.ADDRESS,   // block
                        ValueLayout.ADDRESS,   // regs
                        ValueLayout.ADDRESS,   // gas
                        ValueLayout.ADDRESS,   // mem base
                        ValueLayout.JAVA_LONG  // mem len
                ));
        this.free = linker.downcallHandle(
                lookup.find("pvm_free").orElseThrow(() -> missing("pvm_free")),
                FunctionDescriptor.ofVoid(ValueLayout.ADDRESS));
    }

    private static IllegalStateException missing(String sym) {
        return new IllegalStateException("recompiler symbol not found: " + sym);
    }

    /** A compiled block handle. Null native pointer means the program had an
     *  unsupported opcode (caller must deopt to the interpreter). */
    public final class Block implements AutoCloseable {
        private MemorySegment handle; // native *mut CompiledBlock

        private Block(MemorySegment handle) {
            this.handle = handle;
        }

        public boolean isValid() {
            return handle != null && !handle.equals(MemorySegment.NULL);
        }

        @Override
        public void close() {
            if (handle != null && !handle.equals(MemorySegment.NULL)) {
                try {
                    free.invoke(handle);
                } catch (Throwable t) {
                    throw new RuntimeException("pvm_free failed", t);
                }
                handle = null;
            }
        }
    }

    /**
     * Compile a pre-decoded program. Each instruction i is
     * (opcodes[i], a[i], b[i], c[i], imm[i], imm2[i]) — opcodes[i] is the real
     * PVM opcode value; for control-flow ops the branch/jump target
     * instruction index is carried in {@code imm[i]} (or {@code imm2[i]} for
     * the reg+imm+imm branch family). {@code jumpTable} lists the valid
     * indirect ({@code JumpIndirect}) target instruction indices.
     * Returns a Block; check {@link Block#isValid()} before executing.
     */
    public Block compile(int[] opcodes, int[] a, int[] b, int[] c, long[] imm, long[] imm2, int[] jumpTable) {
        int n = opcodes.length;
        MemorySegment buf = arena.allocate(RAW_INSTR_SIZE * n);
        for (int i = 0; i < n; i++) {
            long base = i * RAW_INSTR_SIZE;
            buf.set(ValueLayout.JAVA_INT, base, opcodes[i]);
            buf.set(ValueLayout.JAVA_INT, base + 4, a[i]);
            buf.set(ValueLayout.JAVA_INT, base + 8, b[i]);
            buf.set(ValueLayout.JAVA_INT, base + 12, c[i]);
            buf.set(ValueLayout.JAVA_LONG, base + 16, imm[i]);
            buf.set(ValueLayout.JAVA_LONG, base + 24, imm2[i]);
        }
        MemorySegment jt = jumpTable.length == 0
                ? MemorySegment.NULL
                : arena.allocate(ValueLayout.JAVA_INT, jumpTable.length);
        for (int i = 0; i < jumpTable.length; i++) {
            jt.setAtIndex(ValueLayout.JAVA_INT, i, jumpTable[i]);
        }
        try {
            MemorySegment h = (MemorySegment) compile.invoke(buf, (long) n, jt, (long) jumpTable.length);
            return new Block(h);
        } catch (Throwable t) {
            throw new RuntimeException("pvm_compile failed", t);
        }
    }

    /** Convenience overload with no indirect-jump targets. */
    public Block compile(int[] opcodes, int[] a, int[] b, int[] c, long[] imm, long[] imm2) {
        return compile(opcodes, a, b, c, imm, imm2, new int[0]);
    }

    /**
     * Execute a block over a 13-register file, a gas budget, and a guest-memory
     * region. {@code regs} and {@code mem} are read and written in place.
     * Returns {@code {exitCode, gasRemaining}}. Pass a zero-length {@code mem}
     * for register-only programs.
     */
    public long[] execute(Block block, long[] regs, long gas, byte[] mem) {
        if (regs.length != REG_COUNT) {
            throw new IllegalArgumentException("regs must have length " + REG_COUNT);
        }
        try (Arena call = Arena.ofConfined()) {
            MemorySegment regSeg = call.allocate(ValueLayout.JAVA_LONG, REG_COUNT);
            for (int i = 0; i < REG_COUNT; i++) {
                regSeg.setAtIndex(ValueLayout.JAVA_LONG, i, regs[i]);
            }
            MemorySegment gasSeg = call.allocate(ValueLayout.JAVA_LONG);
            gasSeg.set(ValueLayout.JAVA_LONG, 0, gas);

            MemorySegment memSeg = mem.length == 0
                    ? MemorySegment.NULL
                    : call.allocate(mem.length);
            if (mem.length > 0) {
                MemorySegment.copy(mem, 0, memSeg, ValueLayout.JAVA_BYTE, 0, mem.length);
            }

            int exit = (int) execute.invoke(block.handle, regSeg, gasSeg, memSeg, (long) mem.length);

            for (int i = 0; i < REG_COUNT; i++) {
                regs[i] = regSeg.getAtIndex(ValueLayout.JAVA_LONG, i);
            }
            if (mem.length > 0) {
                MemorySegment.copy(memSeg, ValueLayout.JAVA_BYTE, 0, mem, 0, mem.length);
            }
            long gasRemaining = gasSeg.get(ValueLayout.JAVA_LONG, 0);
            return new long[]{exit, gasRemaining};
        } catch (Throwable t) {
            throw new RuntimeException("pvm_execute failed", t);
        }
    }

    /** Register-only convenience overload (empty guest memory). */
    public long[] execute(Block block, long[] regs, long gas) {
        return execute(block, regs, gas, new byte[0]);
    }

    @Override
    public void close() {
        arena.close();
    }
}
