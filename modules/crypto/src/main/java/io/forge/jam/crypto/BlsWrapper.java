package io.forge.jam.crypto;

/**
 * JNI bindings for BLS12-381 (w3f-bls TinyBLS381, double keys of eprint 2022/1611). The functions live in the
 * Ed25519-Zebra native library, so loading goes through {@link Ed25519ZebraWrapper#ensureLibraryLoaded()}.
 */
public class BlsWrapper {

	public static void ensureLibraryLoaded() {
		Ed25519ZebraWrapper.ensureLibraryLoaded();
	}

	/** 144-byte double public key (G1 ++ G2) for a secret seed, or null. */
	public static native byte[] publicFromSeed(byte[] seed);

	/** 112-byte double signature (G1 signature ++ DLEQ proof) on message, or null. */
	public static native byte[] sign(byte[] seed, byte[] message);

	public static native boolean verify(byte[] publicKey, byte[] message, byte[] signature);

	/** Whether the key's G1 and G2 halves have the same secret. */
	public static native boolean keyConsistent(byte[] publicKey);

	/** 48-byte sum of n 112-byte signatures end to end, or null. */
	public static native byte[] aggregate(byte[] signatures);

	/** Checks an aggregate against n 144-byte keys end to end that all signed message. */
	public static native boolean verifyAggregate(byte[] publicKeys, byte[] message, byte[] aggregate);
}
