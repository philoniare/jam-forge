package io.forge.jam.pvm.native_;

import java.lang.foreign.*;
import java.lang.invoke.MethodHandle;
import java.nio.file.Path;

/**
 * FFM binding to the native PVM recompiler.
 *
 * Boundary: native engine only, register-file in / register-file out.
 * No host-call upcalls or shared guest memory yet.
 */
public final class PvmRecompiler implements AutoCloseable {

    // RawInstr layout — must match the Rust #[repr(C)] struct exactly:
    //   u32 opcode, u32 dst, u32 src, u32 src2, u64 imm   (24 bytes, align 8)
    private static final MemoryLayout RAW_INSTR = MemoryLayout.structLayout(
            ValueLayout.JAVA_INT.withName("opcode"),
            ValueLayout.JAVA_INT.withName("dst"),
            ValueLayout.JAVA_INT.withName("src"),
            ValueLayout.JAVA_INT.withName("src2"),
            ValueLayout.JAVA_LONG.withName("imm")
    );
    private static final long RAW_INSTR_SIZE = RAW_INSTR.byteSize(); // 24

    // Opcode / exit constants mirror lib.rs.
    public static final int OP_TRAP = 0;
    public static final int OP_LOAD_IMM64 = 1;
    public static final int OP_ADD_IMM64 = 2;
    public static final int OP_ADD = 3;
    public static final int OP_SUB = 4;
    public static final int OP_MUL = 5;
    public static final int OP_LOAD_U64 = 6;
    public static final int OP_STORE_U64 = 7;
    public static final int OP_LOAD_U8 = 12;
    public static final int OP_LOAD_U16 = 13;
    public static final int OP_LOAD_U32 = 14;
    public static final int OP_LOAD_I8 = 15;
    public static final int OP_LOAD_I16 = 16;
    public static final int OP_LOAD_I32 = 17;
    public static final int OP_STORE_U8 = 18;
    public static final int OP_STORE_U16 = 19;
    public static final int OP_STORE_U32 = 20;
    public static final int OP_JUMP = 8;
    public static final int OP_BRANCH_EQ = 9;
    public static final int OP_BRANCH_NE = 10;
    public static final int OP_DJUMP = 11;
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
     * (opcodes[i], dsts[i], srcs[i], src2s[i], imms[i]); for control-flow ops the
     * branch/jump target instruction index is carried in {@code imms[i]}.
     * {@code jumpTable} lists the valid indirect ({@code djump}) target indices.
     * Returns a Block; check {@link Block#isValid()} before executing.
     */
    public Block compile(int[] opcodes, int[] dsts, int[] srcs, int[] src2s, long[] imms, int[] jumpTable) {
        int n = opcodes.length;
        MemorySegment buf = arena.allocate(RAW_INSTR_SIZE * n);
        for (int i = 0; i < n; i++) {
            long base = i * RAW_INSTR_SIZE;
            buf.set(ValueLayout.JAVA_INT, base, opcodes[i]);
            buf.set(ValueLayout.JAVA_INT, base + 4, dsts[i]);
            buf.set(ValueLayout.JAVA_INT, base + 8, srcs[i]);
            buf.set(ValueLayout.JAVA_INT, base + 12, src2s[i]);
            buf.set(ValueLayout.JAVA_LONG, base + 16, imms[i]);
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
    public Block compile(int[] opcodes, int[] dsts, int[] srcs, int[] src2s, long[] imms) {
        return compile(opcodes, dsts, srcs, src2s, imms, new int[0]);
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
