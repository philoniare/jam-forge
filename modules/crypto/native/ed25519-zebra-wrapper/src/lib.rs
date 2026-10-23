pub mod bls;

use ed25519_zebra::{batch, Signature, SigningKey, VerificationKey, VerificationKeyBytes};
use jni::objects::{JByteArray, JClass, JIntArray};
use jni::sys::{jboolean, jbyteArray, JNI_FALSE, JNI_TRUE};
use rand_core::OsRng;
use jni::JNIEnv;


/// Verifies one Ed25519 signature under ZIP-215 rules (ed25519-zebra's `verify`).
#[no_mangle]
pub extern "system" fn Java_io_forge_jam_crypto_Ed25519ZebraWrapper_verify(
    mut env: JNIEnv,
    _class: JClass,
    public_key: JByteArray,
    message: JByteArray,
    signature: JByteArray,
) -> jboolean {
    let ok = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        verify_impl(&mut env, public_key, message, signature)
    }))
    .unwrap_or(false);
    if ok { JNI_TRUE } else { JNI_FALSE }
}

/// Verifies `n` signatures at once with ed25519-zebra's batch verifier: `public_keys` holds n 32-byte keys,
/// `signatures` n 64-byte signatures, and `messages` the n messages end to end, message i ending at
/// `message_ends[i]`. True iff every signature is valid. Batch and single verification follow the same ZIP-215
/// rules, so this accepts exactly when verifying each one would (bar the batch's 2^-128 chance of accepting an
/// invalid set).
#[no_mangle]
pub extern "system" fn Java_io_forge_jam_crypto_Ed25519ZebraWrapper_verifyBatch(
    mut env: JNIEnv,
    _class: JClass,
    public_keys: JByteArray,
    messages: JByteArray,
    message_ends: JIntArray,
    signatures: JByteArray,
) -> jboolean {
    let ok = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        verify_batch_impl(&mut env, public_keys, messages, message_ends, signatures)
    }))
    .unwrap_or(false);
    if ok { JNI_TRUE } else { JNI_FALSE }
}

fn verify_batch_impl(
    env: &mut JNIEnv,
    public_keys: JByteArray,
    messages: JByteArray,
    message_ends: JIntArray,
    signatures: JByteArray,
) -> bool {
    let (Ok(pks), Ok(msgs), Ok(sigs)) = (
        env.convert_byte_array(&public_keys),
        env.convert_byte_array(&messages),
        env.convert_byte_array(&signatures),
    ) else {
        return false;
    };
    let Ok(n) = env.get_array_length(&message_ends) else { return false };
    let mut ends = vec![0i32; n as usize];
    if env.get_int_array_region(&message_ends, 0, &mut ends).is_err() {
        return false;
    }
    let ends: Vec<usize> = ends.iter().map(|&e| e as usize).collect();
    verify_batch(&pks, &msgs, &ends, &sigs)
}

/// The batch check of `Java_..._verifyBatch` on plain slices.
fn verify_batch(pks: &[u8], msgs: &[u8], ends: &[usize], sigs: &[u8]) -> bool {
    let n = ends.len();
    if pks.len() != n * 32 || sigs.len() != n * 64 {
        return false;
    }
    let mut verifier = batch::Verifier::new();
    let mut start = 0usize;
    for i in 0..n {
        let end = ends[i];
        if end < start || end > msgs.len() {
            return false;
        }
        let pk: [u8; 32] = pks[i * 32..(i + 1) * 32].try_into().unwrap();
        let sig: [u8; 64] = sigs[i * 64..(i + 1) * 64].try_into().unwrap();
        verifier.queue((VerificationKeyBytes::from(pk), Signature::from(sig), &msgs[start..end]));
        start = end;
    }
    verifier.verify(OsRng).is_ok()
}

fn verify_impl(
    env: &mut JNIEnv,
    public_key: JByteArray,
    message: JByteArray,
    signature: JByteArray,
) -> bool {
    // Convert public key bytes
    let pk_bytes = match env.convert_byte_array(&public_key) {
        Ok(bytes) => bytes,
        Err(_) => return false,
    };

    if pk_bytes.len() != 32 {
        return false;
    }

    // Convert to fixed-size array
    let pk_array: [u8; 32] = match pk_bytes.try_into() {
        Ok(bytes) => bytes,
        Err(_) => return false,
    };

    // Convert signature bytes
    let sig_bytes = match env.convert_byte_array(&signature) {
        Ok(bytes) => bytes,
        Err(_) => return false,
    };

    if sig_bytes.len() != 64 {
        return false;
    }

    // Convert message bytes
    let msg_bytes = match env.convert_byte_array(&message) {
        Ok(bytes) => bytes,
        Err(_) => return false,
    };

    // Create verification key from bytes
    let vk_bytes = VerificationKeyBytes::from(pk_array);
    let vk: VerificationKey = match vk_bytes.try_into() {
        Ok(vk) => vk,
        Err(_) => return false,
    };

    // Create signature
    let sig_array: [u8; 64] = match sig_bytes.try_into() {
        Ok(bytes) => bytes,
        Err(_) => return false,
    };
    let sig = Signature::from(sig_array);

    // Verify using ed25519-zebra
    vk.verify(&sig, &msg_bytes).is_ok()
}

/// Sign a message using Ed25519.
///
/// # Arguments
/// * `secret_key` - 32-byte Ed25519 secret key seed
/// * `message` - The message to sign
///
/// # Returns
/// * 64-byte signature on success
/// * null on failure
#[no_mangle]
pub extern "system" fn Java_io_forge_jam_crypto_Ed25519ZebraWrapper_sign(
    mut env: JNIEnv,
    _class: JClass,
    secret_key: JByteArray,
    message: JByteArray,
) -> jbyteArray {
    let return_null = || -> jbyteArray { std::ptr::null_mut() };

    // Convert secret key bytes
    let sk_bytes = match env.convert_byte_array(&secret_key) {
        Ok(bytes) => bytes,
        Err(_) => return return_null(),
    };

    if sk_bytes.len() != 32 {
        return return_null();
    }

    // Convert message bytes
    let msg_bytes = match env.convert_byte_array(&message) {
        Ok(bytes) => bytes,
        Err(_) => return return_null(),
    };

    // Create signing key from seed
    let sk_array: [u8; 32] = match sk_bytes.try_into() {
        Ok(bytes) => bytes,
        Err(_) => return return_null(),
    };
    let signing_key = SigningKey::from(sk_array);

    // Sign the message
    let signature = signing_key.sign(&msg_bytes);
    let sig_bytes: [u8; 64] = signature.into();

    match env.byte_array_from_slice(&sig_bytes) {
        Ok(array) => array.into_raw(),
        Err(_) => return_null(),
    }
}

/// Get the public key from a secret key.
///
/// # Arguments
/// * `secret_key` - 32-byte Ed25519 secret key seed
///
/// # Returns
/// * 32-byte public key on success
/// * null on failure
#[no_mangle]
pub extern "system" fn Java_io_forge_jam_crypto_Ed25519ZebraWrapper_publicFromSecret(
    mut env: JNIEnv,
    _class: JClass,
    secret_key: JByteArray,
) -> jbyteArray {
    let return_null = || -> jbyteArray { std::ptr::null_mut() };

    // Convert secret key bytes
    let sk_bytes = match env.convert_byte_array(&secret_key) {
        Ok(bytes) => bytes,
        Err(_) => return return_null(),
    };

    if sk_bytes.len() != 32 {
        return return_null();
    }

    // Create signing key from seed
    let sk_array: [u8; 32] = match sk_bytes.try_into() {
        Ok(bytes) => bytes,
        Err(_) => return return_null(),
    };
    let signing_key = SigningKey::from(sk_array);

    // Get public key
    let vk = VerificationKey::from(&signing_key);
    let vk_bytes: [u8; 32] = vk.into();

    match env.byte_array_from_slice(&vk_bytes) {
        Ok(array) => array.into_raw(),
        Err(_) => return_null(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sign_and_verify() {
        // Create a signing key
        let seed = [1u8; 32];
        let signing_key = SigningKey::from(seed);
        let vk = VerificationKey::from(&signing_key);

        // Sign a message
        let message = b"test message";
        let signature = signing_key.sign(message);

        // Verify the signature
        assert!(vk.verify(&signature, message).is_ok());
    }

    /// Batch verification accepts exactly the sets whose every signature verifies on its own.
    #[test]
    fn test_batch_matches_single_verification() {
        let n = 40;
        let mut pks = Vec::new();
        let mut msgs = Vec::new();
        let mut ends = Vec::new();
        let mut sigs = Vec::new();
        for i in 0..n {
            let sk = SigningKey::from([i as u8 + 1; 32]);
            let msg = vec![i as u8; i % 7 + 1];
            let sig: [u8; 64] = sk.sign(&msg).into();
            let pk: [u8; 32] = VerificationKey::from(&sk).into();
            pks.extend_from_slice(&pk);
            msgs.extend_from_slice(&msg);
            ends.push(msgs.len());
            sigs.extend_from_slice(&sig);
        }
        assert!(verify_batch(&pks, &msgs, &ends, &sigs));
        assert!(verify_batch(&[], &[], &[], &[]));

        // One bad signature, message or key anywhere fails the batch.
        for i in [0, 17, n - 1] {
            let mut bad = sigs.clone();
            bad[i * 64 + 40] ^= 1;
            assert!(!verify_batch(&pks, &msgs, &ends, &bad));
            let mut bad_msg = msgs.clone();
            bad_msg[ends[i] - 1] ^= 1;
            assert!(!verify_batch(&pks, &bad_msg, &ends, &sigs));
            let mut bad_pk = pks.clone();
            bad_pk[i * 32] ^= 1;
            assert!(!verify_batch(&bad_pk, &msgs, &ends, &sigs));
        }
        // Malformed lengths are rejected, not read past.
        assert!(!verify_batch(&pks[..32], &msgs, &ends, &sigs));
        assert!(!verify_batch(&pks, &msgs, &[usize::MAX; 40], &sigs));
    }

}
