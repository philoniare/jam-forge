package io.forge.jam.crypto;

import java.io.File;

/**
 * Java JNI wrapper for the Ed25519-Zebra native library.
 *
 * Ed25519-Zebra is a ZIP-215 compliant Ed25519 implementation from the Zcash
 * Foundation. ZIP-215 defines precise validation rules for Ed25519 signatures
 * that are critical for blockchain consensus.
 *
 * @see <a href="https://zips.z.cash/zip-0215">ZIP-215 Specification</a>
 * @see <a href="https://github.com/ZcashFoundation/ed25519-zebra">ed25519-zebra
 *      on GitHub</a>
 */
public class Ed25519ZebraWrapper {

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

			String libraryName = "ed25519_zebra_wrapper";
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
					baseDir + "/modules/crypto/native/ed25519-zebra-wrapper/target/release/" + libFileName };

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
					throw new RuntimeException("Failed to load native library " + libFileName + ". Searched paths: "
							+ String.join(", ", possiblePaths), e);
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

	// Native method declarations

	/**
	 * Verify an Ed25519 signature using ZIP-215 compliant verification.
	 *
	 * @param publicKey 32-byte Ed25519 public key
	 * @param message   The message that was signed
	 * @param signature 64-byte Ed25519 signature
	 * @return true if verification succeeds
	 */
	public static native boolean verify(byte[] publicKey, byte[] message, byte[] signature);

	/**
	 * Verify n Ed25519 signatures at once with ed25519-zebra's batch verifier (same ZIP-215 rules as verify).
	 *
	 * @param publicKeys  n 32-byte public keys end to end
	 * @param messages    the n messages end to end
	 * @param messageEnds where each message ends in messages
	 * @param signatures  n 64-byte signatures end to end
	 * @return true if every signature is valid
	 */
	public static native boolean verifyBatch(byte[] publicKeys, byte[] messages, int[] messageEnds, byte[] signatures);

	/**
	 * Sign a message using Ed25519.
	 *
	 * @param secretKey 32-byte Ed25519 secret key seed
	 * @param message   The message to sign
	 * @return 64-byte signature, or null on failure
	 */
	public static native byte[] sign(byte[] secretKey, byte[] message);

	/**
	 * Get the public key from a secret key.
	 *
	 * @param secretKey 32-byte Ed25519 secret key seed
	 * @return 32-byte public key, or null on failure
	 */
	public static native byte[] publicFromSecret(byte[] secretKey);
}
