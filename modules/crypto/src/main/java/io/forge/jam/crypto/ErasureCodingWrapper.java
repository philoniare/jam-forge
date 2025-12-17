package io.forge.jam.crypto;

import java.io.File;

/**
 * Java JNI wrapper for the Erasure Coding native library.
 *
 * This wrapper provides Reed-Solomon erasure coding functionality using the
 * reed-solomon-simd Rust crate, which provides SIMD-accelerated encoding and decoding.
 *
 * @see <a href="https://crates.io/crates/reed-solomon-simd">reed-solomon-simd on crates.io</a>
 */
public class ErasureCodingWrapper {

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

			String libraryName = "erasure_coding_wrapper";
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
					baseDir + "/modules/crypto/native/erasure-coding-wrapper/target/release/" + libFileName };

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
	 * Encode original shards into recovery shards using Reed-Solomon erasure coding.
	 *
	 * @param original      2D byte array of original shards (each shard must have even length)
	 * @param recoveryCount Number of recovery shards to generate
	 * @return 2D byte array of recovery shards, or null on failure
	 */
	public static native byte[][] encode(byte[][] original, int recoveryCount);

	/**
	 * Recover original shards from a subset of recovery/original shards.
	 *
	 * @param shards        2D byte array of available shards
	 * @param indices       Array of shard indices (index < originalCount means original shard,
	 *                      index >= originalCount means recovery shard at index - originalCount)
	 * @param originalCount Total number of original shards
	 * @param recoveryCount Total number of recovery shards
	 * @return 2D byte array of recovered original shards, or null on failure
	 */
	public static native byte[][] recover(byte[][] shards, int[] indices, int originalCount, int recoveryCount);
}
