package io.forge.jam.vrfs;

import java.io.File;

/**
 * Java JNI wrapper for the Bandersnatch VRF native library.
 *
 * This class provides the JNI interface with method names that match the
 * native library's expected function signatures (Java_io_forge_jam_vrfs_BandersnatchWrapper_*).
 *
 * The native library expects this exact package and class name for JNI method resolution.
 */
public class BandersnatchWrapper {

    private static boolean libraryLoaded = false;
    private static final Object loadLock = new Object();

    /**
     * Load the native library from known paths.
     */
    public static void ensureLibraryLoaded() {
        synchronized (loadLock) {
            if (libraryLoaded) {
                return;
            }

            String libraryName = "bandersnatch_vrfs_wrapper";
            String osName = System.getProperty("os.name").toLowerCase();
            String libFileName;
            String osDirName;

            if (osName.contains("mac")) {
                libFileName = "lib" + libraryName + ".dylib";
                osDirName = "mac";
            } else if (osName.contains("linux")) {
                libFileName = "lib" + libraryName + ".so";
                osDirName = "linux";
            } else if (osName.contains("windows")) {
                libFileName = libraryName + ".dll";
                osDirName = "windows";
            } else {
                throw new RuntimeException("Unsupported operating system: " + osName);
            }

            // Get base directory from system property or use current working directory
            String baseDir = System.getProperty("jam.base.dir", System.getProperty("user.dir"));

            // Try to load from multiple possible locations
            String[] possiblePaths = {
                // From crypto module's native build output (platform-specific)
                baseDir + "/modules/crypto/native/build/" + osDirName + "/" + libFileName,
                // From crypto module's resources
                baseDir + "/modules/crypto/src/main/resources/" + libFileName,
                // From Rust target directory (direct build)
                baseDir + "/modules/crypto/native/bandersnatch-vrfs-wrapper/target/release/" + libFileName,
                // Legacy: from protocol module resources
                baseDir + "/modules/protocol/src/main/resources/" + libFileName,
                // Legacy: from protocol native-libs
                baseDir + "/modules/protocol/native-libs/" + osDirName + "/" + libFileName
            };

            boolean loaded = false;
            for (String path : possiblePaths) {
                File file = new File(path);
                if (file.exists()) {
                    try {
                        System.load(file.getAbsolutePath());
                        libraryLoaded = true;
                        loaded = true;
                        break;
                    } catch (UnsatisfiedLinkError e) {
                        // Continue to next path
                    }
                }
            }

            if (!loaded) {
                // Try loading from java.library.path as last resort
                try {
                    System.loadLibrary(libraryName);
                    libraryLoaded = true;
                } catch (UnsatisfiedLinkError e) {
                    throw new RuntimeException(
                        "Failed to load native library " + libFileName + ". Searched paths: " +
                        String.join(", ", possiblePaths), e
                    );
                }
            }
        }
    }

    /**
     * Check if the native library is loaded.
     */
    public static boolean isLibraryLoaded() {
        return libraryLoaded;
    }

    // Native method declarations - these must match the JNI function signatures
    // in the Rust library (Java_io_forge_jam_vrfs_BandersnatchWrapper_*)

    /**
     * Initialize the ring context with SRS data and ring size.
     */
    public static native byte[] initializeContext(byte[] srsData, int ringSize);

    /**
     * Get the verifier commitment (ring root) from public keys.
     */
    public static native byte[] getVerifierCommitment(int ringSize, byte[] keys);

    /**
     * Verify a ring VRF proof.
     */
    public static native byte[] verifierRingVrfVerify(
        byte[] entropy,
        long attempt,
        byte[] signature,
        byte[] commitment,
        int ringSize
    );

    /**
     * Create an anonymous ring VRF proof (Safrole ticket) for
     * jam_ticket_seal ++ entropy ++ attempt with empty aux data.
     *
     * @param secret       serialized Bandersnatch secret (from secretFromSeed)
     * @param ringKeys     ringSize * 32 bytes of compressed public keys
     * @param proverIndex  this key's position in the ring
     * @param ringSize     number of ring members
     * @param entropy      32-byte epochal entropy (eta_2')
     * @param attempt      ticket entry index
     * @return the serialized ring VRF signature (output ++ ring proof)
     */
    public static native byte[] ringVrfSign(
        byte[] secret,
        byte[] ringKeys,
        int proverIndex,
        int ringSize,
        byte[] entropy,
        byte attempt
    );

    /**
     * Get the VRF output from an IETF VRF signature.
     */
    public static native byte[] getIetfVrfOutput(byte[] signature);

    /**
     * Create a secret key from a seed.
     */
    public static native byte[] secretFromSeed(byte[] seed);

    /**
     * Get the public key from a secret key.
     */
    public static native byte[] publicFromSecret(byte[] secretBytes);

    /**
     * Sign using IETF VRF.
     */
    public static native byte[] ietfVrfSign(byte[] secretBytes, byte[] vrfInput, byte[] auxData);

    /**
     * Verify an IETF VRF signature.
     */
    public static native byte[] ietfVrfVerify(
        byte[] publicBytes,
        byte[] vrfInput,
        byte[] auxData,
        byte[] signature
    );

    /**
     * Get VRF output directly from secret key and input.
     */
    public static native byte[] getVrfOutput(byte[] secretBytes, byte[] vrfInput);
}
