package io.forge.jam.vrfs;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public final class NativeCpu {

    public static final String SKIP_PROPERTY = "jam.crypto.skipCpuCheck";

    private static final List<String> REQUIRED = List.of("avx", "avx2", "bmi1", "bmi2", "f16c", "fma", "abm", "movbe");

    private NativeCpu() {}

    public static List<String> missingFeatures(String osName, String osArch, String cpuinfo) {
        String os = osName == null ? "" : osName.toLowerCase();
        String arch = osArch == null ? "" : osArch.toLowerCase();
        if (!os.contains("linux") || !(arch.equals("amd64") || arch.equals("x86_64")) || cpuinfo == null) {
            return List.of();
        }
        for (String line : cpuinfo.split("\n")) {
            int colon = line.indexOf(':');
            if (colon > 0 && line.substring(0, colon).trim().equals("flags")) {
                Set<String> flags = new HashSet<>(Arrays.asList(line.substring(colon + 1).trim().split("\\s+")));
                List<String> missing = new ArrayList<>();
                for (String name : REQUIRED) {
                    if (!flags.contains(name)) {
                        missing.add(name);
                    }
                }
                return missing;
            }
        }
        return List.of();
    }

    public static void check(String osName, String osArch, String cpuinfo) {
        List<String> missing = missingFeatures(osName, osArch, cpuinfo);
        if (!missing.isEmpty()) {
            throw new UnsupportedOperationException(
                "this CPU lacks " + String.join(", ", missing) + ": the Bandersnatch library is built for x86-64-v3 "
                    + "(AVX2, BMI2, FMA) and would end the JVM at its first call. Run on a CPU that has them, or "
                    + "rebuild modules/crypto/native/bandersnatch-vrfs-wrapper without target-cpu=x86-64-v3 and start "
                    + "with -D" + SKIP_PROPERTY + "=true"
            );
        }
    }

    public static void check() {
        if (Boolean.getBoolean(SKIP_PROPERTY)) {
            return;
        }
        String cpuinfo;
        try {
            cpuinfo = Files.readString(Path.of("/proc/cpuinfo"), StandardCharsets.ISO_8859_1);
        } catch (Exception unreadable) {
            return;
        }
        check(System.getProperty("os.name"), System.getProperty("os.arch"), cpuinfo);
    }
}
