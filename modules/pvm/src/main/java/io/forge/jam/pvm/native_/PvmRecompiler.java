package io.forge.jam.pvm.native_;

import java.lang.foreign.*;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
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
    public static final long HOST_CONTINUE = 0L;
    public static final long HOST_PANIC = 1L;
    public static final long HOST_OOG = 2L;

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
    private final Linker linker;
    private static final FunctionDescriptor HOST_FN_DESCRIPTOR = FunctionDescriptor.of(
            ValueLayout.JAVA_LONG, // return: HOST_CONTINUE/HOST_PANIC/HOST_OOG
            ValueLayout.ADDRESS,   // ctx (host_ctx, round-tripped opaquely)
            ValueLayout.JAVA_LONG, // host_call_id
            ValueLayout.JAVA_INT   // pc
    );

    private static final FunctionDescriptor SBRK_FN_DESCRIPTOR = FunctionDescriptor.of(
            ValueLayout.JAVA_LONG, // return: HOST_CONTINUE/HOST_PANIC (never HOST_OOG — sbrk has no gas cost)
            ValueLayout.ADDRESS,   // ctx (host_ctx, SAME opaque pointer host_fn receives)
            ValueLayout.JAVA_INT,  // dst (destination register index, 0..12)
            ValueLayout.JAVA_LONG, // size (current value of reg[src], truncated to u32 by the handler)
            ValueLayout.JAVA_INT   // pc
    );

    public interface HostCallHandler {
        long onHostCall(long hostCallId, int pc);
    }

    public interface SbrkCallHandler {
        long onSbrk(int dst, long size, int pc);
    }

    /** Open the binding against the recompiler shared library at {@code libPath}. */
    public PvmRecompiler(Path libPath) {
        this.arena = Arena.ofShared();
        this.linker = Linker.nativeLinker();
        SymbolLookup lookup = SymbolLookup.libraryLookup(libPath, arena);

        this.compile = linker.downcallHandle(
                lookup.find("pvm_compile").orElseThrow(() -> missing("pvm_compile")),
                FunctionDescriptor.of(ValueLayout.ADDRESS,
                        ValueLayout.ADDRESS,   // instrs
                        ValueLayout.JAVA_LONG, // n
                        ValueLayout.ADDRESS,   // jump table
                        ValueLayout.JAVA_LONG, // jt_n
                        ValueLayout.JAVA_INT   // code_len (byte length of the decoded code section)
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
                        ValueLayout.ADDRESS,   // out (ExecOut*)
                        ValueLayout.ADDRESS,   // host_fn (Option<HostFn> — null means None)
                        ValueLayout.ADDRESS,   // host_ctx
                        ValueLayout.ADDRESS    // sbrk_fn (Option<SbrkFn> — null means None; Task 18/H2)
                ));
        this.free = linker.downcallHandle(
                lookup.find("pvm_free").orElseThrow(() -> missing("pvm_free")),
                FunctionDescriptor.ofVoid(ValueLayout.ADDRESS));
    }

    private static IllegalStateException missing(String sym) {
        return new IllegalStateException("recompiler symbol not found: " + sym);
    }

    private static final int MMAP_PROT_READ = 0x1;
    private static final int MMAP_PROT_WRITE = 0x2;
    private static final int MMAP_MAP_PRIVATE = 0x0002;
    private static final int MMAP_MAP_ANON = 0x1000; // MAP_ANONYMOUS on Linux glibc has the same value
    private static final MemorySegment MMAP_FAILED = MemorySegment.ofAddress(-1L);
    private static final MethodHandle MMAP_MH;
    private static final MethodHandle MUNMAP_MH;
    static {
        Linker sysLinker = Linker.nativeLinker();
        SymbolLookup stdlib = sysLinker.defaultLookup();
        MMAP_MH = sysLinker.downcallHandle(
                stdlib.find("mmap").orElseThrow(() -> missing("mmap")),
                FunctionDescriptor.of(ValueLayout.ADDRESS,
                        ValueLayout.ADDRESS,   // addr
                        ValueLayout.JAVA_LONG, // length
                        ValueLayout.JAVA_INT,  // prot
                        ValueLayout.JAVA_INT,  // flags
                        ValueLayout.JAVA_INT,  // fd
                        ValueLayout.JAVA_LONG  // offset
                ));
        MUNMAP_MH = sysLinker.downcallHandle(
                stdlib.find("munmap").orElseThrow(() -> missing("munmap")),
                FunctionDescriptor.of(ValueLayout.JAVA_INT, ValueLayout.ADDRESS, ValueLayout.JAVA_LONG));
    }

    private static MemorySegment mmapAnonRw(long length, Arena confinedArena) {
        if (length == 0) {
            return MemorySegment.NULL;
        }
        try {
            MemorySegment addr = (MemorySegment) MMAP_MH.invokeExact(
                    MemorySegment.NULL, length, MMAP_PROT_READ | MMAP_PROT_WRITE,
                    MMAP_MAP_PRIVATE | MMAP_MAP_ANON, -1, 0L);
            if (addr.address() == MMAP_FAILED.address()) {
                throw new RuntimeException("mmap(" + length + ") failed (MAP_FAILED)");
            }
            return addr.reinterpret(length, confinedArena, seg -> munmapChecked(seg, length));
        } catch (Throwable t) {
            if (t instanceof RuntimeException re) throw re;
            throw new RuntimeException("mmap(" + length + ") failed", t);
        }
    }

    /** Release a segment obtained from {@link #mmapAnonRw}. No-op on {@code MemorySegment.NULL}. */
    private static void munmapChecked(MemorySegment seg, long length) {
        if (seg == MemorySegment.NULL || length == 0) {
            return;
        }
        try {
            int rc = (int) MUNMAP_MH.invokeExact(seg, length);
            if (rc != 0) {
                throw new RuntimeException("munmap(" + length + ") failed, rc=" + rc);
            }
        } catch (Throwable t) {
            if (t instanceof RuntimeException re) throw re;
            throw new RuntimeException("munmap(" + length + ") failed", t);
        }
    }

    private static final MethodHandle UPCALL_TARGET_MH;
    private static final MethodHandle UPCALL_SBRK_TARGET_MH;
    static {
        try {
            UPCALL_TARGET_MH = MethodHandles.lookup().findStatic(
                    PvmRecompiler.class, "upcallTarget",
                    MethodType.methodType(long.class, UpcallState.class, MemorySegment.class, long.class, int.class));
            UPCALL_SBRK_TARGET_MH = MethodHandles.lookup().findStatic(
                    PvmRecompiler.class, "upcallSbrkTarget",
                    MethodType.methodType(long.class, SbrkUpcallState.class, MemorySegment.class, int.class, long.class, int.class));
        } catch (NoSuchMethodException | IllegalAccessException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    private static final class UpcallState {
        final HostCallHandler handler;
        Throwable pending;

        UpcallState(HostCallHandler handler) {
            this.handler = handler;
        }
    }

    private static long upcallTarget(UpcallState state, MemorySegment ctxIgnored, long hostCallId, int pc) {
        try {
            return state.handler.onHostCall(hostCallId, pc);
        } catch (Throwable t) {
            state.pending = t;
            return HOST_PANIC;
        }
    }

    private static final class SbrkUpcallState {
        final SbrkCallHandler handler;
        Throwable pending;

        SbrkUpcallState(SbrkCallHandler handler) {
            this.handler = handler;
        }
    }

    private static long upcallSbrkTarget(SbrkUpcallState state, MemorySegment ctxIgnored, int dst, long size, int pc) {
        try {
            return state.handler.onSbrk(dst, size, pc);
        } catch (Throwable t) {
            state.pending = t;
            return HOST_PANIC;
        }
    }

    private UpcallStub newUpcallStub(HostCallHandler handler, Arena stubArena) {
        UpcallState state = new UpcallState(handler);
        java.lang.invoke.MethodHandle bound = UPCALL_TARGET_MH.bindTo(state);
        MemorySegment stub = linker.upcallStub(bound, HOST_FN_DESCRIPTOR, stubArena);
        return new UpcallStub(stub, state);
    }

    /** {@code host_fn} native pointer + the mutable state to check for a
     *  stashed exception after the call returns. */
    private record UpcallStub(MemorySegment fnPointer, UpcallState state) {
        /** Rethrow the handler's exception if one occurred, wrapped so the
         *  original stack trace is preserved as the cause. Idempotent to
         *  call when {@code state.pending == null} (no-op). */
        void rethrowIfPending() {
            if (state.pending != null) {
                Throwable t = state.pending;
                state.pending = null;
                if (t instanceof RuntimeException re) throw re;
                if (t instanceof Error err) throw err;
                throw new RuntimeException("host call handler threw a checked exception", t);
            }
        }
    }

    private SbrkUpcallStub newSbrkUpcallStub(SbrkCallHandler handler, Arena stubArena) {
        SbrkUpcallState state = new SbrkUpcallState(handler);
        java.lang.invoke.MethodHandle bound = UPCALL_SBRK_TARGET_MH.bindTo(state);
        MemorySegment stub = linker.upcallStub(bound, SBRK_FN_DESCRIPTOR, stubArena);
        return new SbrkUpcallStub(stub, state);
    }

    private record SbrkUpcallStub(MemorySegment fnPointer, SbrkUpcallState state) {
        /** Same contract as {@link UpcallStub#rethrowIfPending()}. */
        void rethrowIfPending() {
            if (state.pending != null) {
                Throwable t = state.pending;
                state.pending = null;
                if (t instanceof RuntimeException re) throw re;
                if (t instanceof Error err) throw err;
                throw new RuntimeException("sbrk call handler threw a checked exception", t);
            }
        }
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
    public Block compile(int[] opcodes, int[] a, int[] b, int[] c, int[] pc, long[] imm, long[] imm2, int[] jumpTable, int codeLen) {
        int n = opcodes.length;
        try (Arena call = Arena.ofConfined()) {
            MemorySegment buf = call.allocate(RAW_INSTR_SIZE * n);
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
                    : call.allocate(ValueLayout.JAVA_INT, jumpTable.length);
            for (int i = 0; i < jumpTable.length; i++) {
                jt.setAtIndex(ValueLayout.JAVA_INT, i, jumpTable[i]);
            }
            MemorySegment h = (MemorySegment) compile.invoke(buf, (long) n, jt, (long) jumpTable.length, codeLen);
            return new Block(h);
        } catch (Throwable t) {
            throw new RuntimeException("pvm_compile failed", t);
        }
    }

    /** Convenience overload with no indirect-jump targets. */
    public Block compile(int[] opcodes, int[] a, int[] b, int[] c, int[] pc, long[] imm, long[] imm2, int codeLen) {
        return compile(opcodes, a, b, c, pc, imm, imm2, new int[0], codeLen);
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
                    (long) regions.length, backingSeg, pageShift, entryIndex, outSeg,
                    MemorySegment.NULL, MemorySegment.NULL, MemorySegment.NULL);

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

    public LiveExecution executeLive(Block block, long[] regs, long gas, Region[] regions, byte[] backing,
                                      int pageShift, int entryIndex) {
        return executeLive(block, regs, gas, regions, backing, pageShift, entryIndex, null, null);
    }

    public LiveExecution executeLive(Block block, long[] regs, long gas, Region[] regions, byte[] backing,
                                      int pageShift, int entryIndex, HostCallHandler hostCallHandler) {
        return executeLive(block, regs, gas, regions, backing, pageShift, entryIndex, hostCallHandler, null);
    }

    public LiveExecution executeLive(Block block, long[] regs, long gas, Region[] regions, byte[] backing,
                                      int pageShift, int entryIndex, HostCallHandler hostCallHandler,
                                      SbrkCallHandler sbrkCallHandler) {
        return executeLive(block, regs, gas, regions, backing, pageShift, entryIndex, hostCallHandler, sbrkCallHandler, 0L);
    }

    public LiveExecution executeLive(Block block, long[] regs, long gas, Region[] regions, byte[] backing,
                                      int pageShift, int entryIndex, HostCallHandler hostCallHandler,
                                      SbrkCallHandler sbrkCallHandler, long extraBackingSlack) {
        if (regs.length != REG_COUNT) {
            throw new IllegalArgumentException("regs must have length " + REG_COUNT);
        }
        if (extraBackingSlack < 0) {
            throw new IllegalArgumentException("extraBackingSlack must be >= 0");
        }
        Arena call = Arena.ofConfined();
        long mmapTotalLen = backing.length + extraBackingSlack;
        try {
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
                if (r.bufOffset() < 0 || r.bufOffset() > 0xFFFFFFFFL) {
                    throw new IllegalArgumentException("Region.bufOffset() " + r.bufOffset() + " does not fit in u32 (region base=" + r.base() + ")");
                }
                if (r.base() < 0 || r.base() > 0xFFFFFFFFL) {
                    throw new IllegalArgumentException("Region.base() " + r.base() + " does not fit in u32");
                }
                if (r.len() < 0 || r.len() > 0xFFFFFFFFL) {
                    throw new IllegalArgumentException("Region.len() " + r.len() + " does not fit in u32 (region base=" + r.base() + ")");
                }
                regionsSeg.set(ValueLayout.JAVA_INT, off, (int) r.base());
                regionsSeg.set(ValueLayout.JAVA_INT, off + 4, (int) r.len());
                regionsSeg.set(ValueLayout.JAVA_INT, off + 8, (int) r.bufOffset());
                regionsSeg.set(ValueLayout.JAVA_INT, off + 12, r.writable() ? 1 : 0);
            }

            MemorySegment backingSeg = mmapAnonRw(mmapTotalLen, call);
            long compactOff = 0L;
            for (Region r : regions) {
                long len = r.len();
                if (len > 0) {
                    MemorySegment.copy(backing, (int) compactOff, backingSeg, ValueLayout.JAVA_BYTE, r.bufOffset(), (int) len);
                }
                compactOff += len;
            }

            MemorySegment outSeg = call.allocate(EXEC_OUT_SIZE);

            UpcallStub stub = hostCallHandler == null ? null : newUpcallStub(hostCallHandler, call);
            MemorySegment hostFnSeg = stub == null ? MemorySegment.NULL : stub.fnPointer();
            SbrkUpcallStub sbrkStub = sbrkCallHandler == null ? null : newSbrkUpcallStub(sbrkCallHandler, call);
            MemorySegment sbrkFnSeg = sbrkStub == null ? MemorySegment.NULL : sbrkStub.fnPointer();
            MemorySegment hostCtxSeg = MemorySegment.NULL;

            return new LiveExecution(call, block, regSeg, gasSeg, regionsSeg, regions.length,
                    backingSeg, pageShift, entryIndex, outSeg, regs, backing, hostFnSeg, hostCtxSeg, stub,
                    sbrkFnSeg, sbrkStub, regions);
        } catch (Throwable t) {
            call.close();
            throw new RuntimeException("executeLive setup failed", t);
        }
    }

    public final class LiveExecution implements AutoCloseable {
        private final Arena call;
        private final Block block;
        private final MemorySegment regSeg;
        private final MemorySegment gasSeg;
        private final MemorySegment regionsSeg;
        private final long nRegions;
        private final MemorySegment backingSeg;
        private final int pageShift;
        private final int entryIndex;
        private final MemorySegment outSeg;
        private final long[] regsOut;
        private final byte[] backingOut;
        private final MemorySegment hostFnSeg;
        private final MemorySegment hostCtxSeg;
        private final UpcallStub upcallStub; // null when no HostCallHandler was supplied
        private final MemorySegment sbrkFnSeg;
        private final SbrkUpcallStub sbrkUpcallStub; // null when no SbrkCallHandler was supplied (Task 18/H2)
        private final Region[] regionsForCopy;
        private boolean closed = false;
        private boolean ran = false;

        private LiveExecution(Arena call, Block block, MemorySegment regSeg, MemorySegment gasSeg,
                               MemorySegment regionsSeg, long nRegions, MemorySegment backingSeg,
                               int pageShift, int entryIndex, MemorySegment outSeg,
                               long[] regsOut, byte[] backingOut,
                               MemorySegment hostFnSeg, MemorySegment hostCtxSeg, UpcallStub upcallStub,
                               MemorySegment sbrkFnSeg, SbrkUpcallStub sbrkUpcallStub,
                               Region[] regionsForCopy) {
            this.call = call;
            this.block = block;
            this.regSeg = regSeg;
            this.gasSeg = gasSeg;
            this.regionsSeg = regionsSeg;
            this.nRegions = nRegions;
            this.backingSeg = backingSeg;
            this.pageShift = pageShift;
            this.regionsForCopy = regionsForCopy;
            this.entryIndex = entryIndex;
            this.outSeg = outSeg;
            this.regsOut = regsOut;
            this.backingOut = backingOut;
            this.hostFnSeg = hostFnSeg;
            this.hostCtxSeg = hostCtxSeg;
            this.upcallStub = upcallStub;
            this.sbrkFnSeg = sbrkFnSeg;
            this.sbrkUpcallStub = sbrkUpcallStub;
        }

        /** The live 13-register segment ({@code long[13]}, matches emitted
         *  code's {@code [x0 + i*8]} register-file layout). */
        public MemorySegment regsSegment() { return regSeg; }

        /** The live single-{@code long} gas cell. */
        public MemorySegment gasSegment() { return gasSeg; }

        /** The live region table ({@code REGION_SIZE}-byte records: base,
         *  len, bufOffset, writable), {@link #regionCount()} entries. */
        public MemorySegment regionsSegment() { return regionsSeg; }

        /** Number of entries in {@link #regionsSegment()}. */
        public long regionCount() { return nRegions; }

        /** The live packed backing buffer all regions' bytes are stored in,
         *  addressed via each region's {@code bufOffset}. */
        public MemorySegment backingSegment() { return backingSeg; }

        /** {@code log2(pageSize)}, as passed to {@code pvm_execute}. */
        public int pageShift() { return pageShift; }

        public ExecResult run() {
            if (ran) {
                throw new IllegalStateException("LiveExecution.run() already called");
            }
            ran = true;
            try {
                int exit = (int) execute.invoke(block.handle, regSeg, gasSeg, regionsSeg,
                        nRegions, backingSeg, pageShift, entryIndex, outSeg, hostFnSeg, hostCtxSeg, sbrkFnSeg);
                if (upcallStub != null) {
                    upcallStub.rethrowIfPending();
                }
                if (sbrkUpcallStub != null) {
                    sbrkUpcallStub.rethrowIfPending();
                }
                long gasRemaining = gasSeg.get(ValueLayout.JAVA_LONG, 0);
                long pc = Integer.toUnsignedLong(outSeg.get(ValueLayout.JAVA_INT, 0));
                long faultPage = Integer.toUnsignedLong(outSeg.get(ValueLayout.JAVA_INT, 4));
                return new ExecResult(exit, gasRemaining, pc, faultPage);
            } catch (Throwable t) {
                if (t instanceof RuntimeException re) throw re;
                if (t instanceof Error err) throw err;
                throw new RuntimeException("pvm_execute failed", t);
            }
        }

        @Override
        public void close() {
            if (closed) {
                return;
            }
            closed = true;
            for (int i = 0; i < REG_COUNT; i++) {
                regsOut[i] = regSeg.getAtIndex(ValueLayout.JAVA_LONG, i);
            }
            long compactOff = 0L;
            for (Region r : regionsForCopy) {
                long len = r.len();
                if (len > 0) {
                    MemorySegment.copy(backingSeg, ValueLayout.JAVA_BYTE, r.bufOffset(), backingOut, (int) compactOff, (int) len);
                }
                compactOff += len;
            }
            call.close();
        }
    }

    @Override
    public void close() {
        arena.close();
    }
}
