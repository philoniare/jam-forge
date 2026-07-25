use ed25519_zebra::{Signature, SigningKey, VerificationKey, VerificationKeyBytes};
use jni::objects::{JByteArray, JClass};
use jni::sys::jbyteArray;
use jni::JNIEnv;


#[no_mangle]
pub extern "system" fn Java_io_forge_jam_crypto_Ed25519ZebraWrapper_verify(
    mut env: JNIEnv,
    _class: JClass,
    public_key: JByteArray,
    message: JByteArray,
    signature: JByteArray,
) -> jbyteArray {
    let result = verify_impl(&mut env, public_key, message, signature);

    let result_byte: [u8; 1] = if result { [1] } else { [0] };

    match env.byte_array_from_slice(&result_byte) {
        Ok(array) => array.into_raw(),
        Err(_) => std::ptr::null_mut(),
    }
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

}
