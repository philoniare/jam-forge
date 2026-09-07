package io.forge.jam.pvm.native_;

import java.lang.foreign.*;
import java.lang.invoke.MethodHandle;
import java.nio.file.Path;

/**
 * FFM binding to the native PVM recompiler
 */
public final class PvmRecompiler implements AutoCloseable {

    // RawInstr layout — must match the Rust #[repr(C)] struct exactly:
    //   u32 opcode, u32 a, u32 b, u32 c, u32 pc, <4-byte pad>, i64 imm, i64 imm2
    //   (40 bytes, align 8)
    private static final MemoryLayout RAW_INSTR = MemoryLayout.structLayout(
            ValueLayout.JAVA_INT.withName("opcode"),
            ValueLayout.JAVA_INT.withName("a"),
            ValueLayout.JAVA_INT.withName("b"),
            ValueLayout.JAVA_INT.withName("c"),
            ValueLayout.JAVA_INT.withName("pc"),
            MemoryLayout.paddingLayout(4),
            ValueLayout.JAVA_LONG.withName("imm"),
            ValueLayout.JAVA_LONG.withName("imm2")
    );
    private static final long RAW_INSTR_SIZE = RAW_INSTR.byteSize(); // 40

    // Region layout — must match the Rust #[repr(C)] struct exactly:
    //   u32 base, u32 len, u32 buf_offset, u32 writable   (16 bytes, align 4)
    private static final MemoryLayout REGION = MemoryLayout.structLayout(
            ValueLayout.JAVA_INT.withName("base"),
            ValueLayout.JAVA_INT.withName("len"),
            ValueLayout.JAVA_INT.withName("buf_offset"),
            ValueLayout.JAVA_INT.withName("writable")
    );
    private static final long REGION_SIZE = REGION.byteSize(); // 16

    // ExecOut layout — must match the Rust #[repr(C)] struct exactly:
    //   u32 pc, u32 fault_page   (8 bytes, align 4)
    private static final MemoryLayout EXEC_OUT = MemoryLayout.structLayout(
            ValueLayout.JAVA_INT.withName("pc"),
            ValueLayout.JAVA_INT.withName("fault_page")
    );
    private static final long EXEC_OUT_SIZE = EXEC_OUT.byteSize(); // 8

    /** Indirect-jump halt sentinel (see native lib). */
    public static final long DJUMP_HALT = 0xFFFF_0000L;

    public static final int EXIT_HALT = 0;
    public static final int EXIT_PANIC = 1;
    public static final int EXIT_OOG = 2;
    public static final int EXIT_FAULT = 3;

    public static final int REG_COUNT = 13;

    /**
     * One guest-memory region: {@code [base, base+len)}, page-aligned, backed
     * by bytes {@code backing[bufOffset .. bufOffset+len)} in the caller's
     * single shared backing buffer passed to {@link #execute}. Regions must
     * be non-overlapping; {@code writable} is {@code true} for ReadWrite,
     * {@code false} for ReadOnly (matches {@code PageAccess} in PageMap.scala).
     */
    public record Region(long base, long len, long bufOffset, boolean writable) { }

    /**
     * Exit metadata from {@link #execute}: {@code pc} is meaningful on every
     * exit kind; {@code faultPage} only when {@code exit == EXIT_FAULT}.
     */
    public record ExecResult(int exit, long gasRemaining, long pc, long faultPage) { }

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
                        ValueLayout.ADDRESS,   // regions
                        ValueLayout.JAVA_LONG, // n_regions
                        ValueLayout.ADDRESS,   // backing
                        ValueLayout.JAVA_INT,  // page_shift
                        ValueLayout.JAVA_INT,  // entry_index
                        ValueLayout.ADDRESS    // out (ExecOut*)
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
     * (opcodes[i], a[i], b[i], c[i], pc[i], imm[i], imm2[i]) — opcodes[i] is
     * the real PVM opcode value; pc[i] is that instruction's byte offset in
     * the original code blob; for control-flow ops the branch/jump target
     * instruction index is carried in {@code imm[i]} (or {@code imm2[i]} for
     * the reg+imm+imm branch family). {@code jumpTable} lists the valid
     * indirect ({@code JumpIndirect}) target instruction indices.
     * Returns a Block; check {@link Block#isValid()} before executing.
     */
    public Block compile(int[] opcodes, int[] a, int[] b, int[] c, int[] pc, long[] imm, long[] imm2, int[] jumpTable) {
        int n = opcodes.length;
        MemorySegment buf = arena.allocate(RAW_INSTR_SIZE * n);
        for (int i = 0; i < n; i++) {
            long base = i * RAW_INSTR_SIZE;
            buf.set(ValueLayout.JAVA_INT, base, opcodes[i]);
            buf.set(ValueLayout.JAVA_INT, base + 4, a[i]);
            buf.set(ValueLayout.JAVA_INT, base + 8, b[i]);
            buf.set(ValueLayout.JAVA_INT, base + 12, c[i]);
            buf.set(ValueLayout.JAVA_INT, base + 16, pc[i]);
            // bytes [20,24) are the explicit RAW_INSTR padding — left zeroed.
            buf.set(ValueLayout.JAVA_LONG, base + 24, imm[i]);
            buf.set(ValueLayout.JAVA_LONG, base + 32, imm2[i]);
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
    public Block compile(int[] opcodes, int[] a, int[] b, int[] c, int[] pc, long[] imm, long[] imm2) {
        return compile(opcodes, a, b, c, pc, imm, imm2, new int[0]);
    }

    /**
     * Execute a block over a 13-register file, a gas budget, and
     * permission-aware guest memory
     */
    public ExecResult execute(Block block, long[] regs, long gas, Region[] regions, byte[] backing,
                               int pageShift, int entryIndex) {
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

            MemorySegment regionsSeg = regions.length == 0
                    ? MemorySegment.NULL
                    : call.allocate(REGION_SIZE * regions.length);
            for (int i = 0; i < regions.length; i++) {
                long off = i * REGION_SIZE;
                Region r = regions[i];
                regionsSeg.set(ValueLayout.JAVA_INT, off, (int) r.base());
                regionsSeg.set(ValueLayout.JAVA_INT, off + 4, (int) r.len());
                regionsSeg.set(ValueLayout.JAVA_INT, off + 8, (int) r.bufOffset());
                regionsSeg.set(ValueLayout.JAVA_INT, off + 12, r.writable() ? 1 : 0);
            }

            MemorySegment backingSeg = backing.length == 0
                    ? MemorySegment.NULL
                    : call.allocate(backing.length);
            if (backing.length > 0) {
                MemorySegment.copy(backing, 0, backingSeg, ValueLayout.JAVA_BYTE, 0, backing.length);
            }

            MemorySegment outSeg = call.allocate(EXEC_OUT_SIZE);

            int exit = (int) execute.invoke(block.handle, regSeg, gasSeg, regionsSeg,
                    (long) regions.length, backingSeg, pageShift, entryIndex, outSeg);

            for (int i = 0; i < REG_COUNT; i++) {
                regs[i] = regSeg.getAtIndex(ValueLayout.JAVA_LONG, i);
            }
            if (backing.length > 0) {
                MemorySegment.copy(backingSeg, ValueLayout.JAVA_BYTE, 0, backing, 0, backing.length);
            }
            long gasRemaining = gasSeg.get(ValueLayout.JAVA_LONG, 0);
            long pc = Integer.toUnsignedLong(outSeg.get(ValueLayout.JAVA_INT, 0));
            long faultPage = Integer.toUnsignedLong(outSeg.get(ValueLayout.JAVA_INT, 4));
            return new ExecResult(exit, gasRemaining, pc, faultPage);
        } catch (Throwable t) {
            throw new RuntimeException("pvm_execute failed", t);
        }
    }

    /** Register-only, no-memory, whole-program-entry convenience overload. */
    public ExecResult execute(Block block, long[] regs, long gas) {
        return execute(block, regs, gas, new Region[0], new byte[0], 12, 0);
    }

    @Override
    public void close() {
        arena.close();
    }
}
