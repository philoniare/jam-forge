//! BLS12-381 signatures for BEEFY: w3f-bls `TinyBLS381` with the double public key
//! of eprint 2022/1611 (G1 48 bytes ++ G2 96 bytes = the 144-byte validator BLS key) and the double signature
//! (G1 signature 48 bytes ++ DLEQ proof 64 bytes = 112 bytes), as Substrate's `bls381` keys use.

use jni::objects::{JByteArray, JClass};
use jni::sys::{jboolean, jbyteArray, JNI_FALSE, JNI_TRUE};
use jni::JNIEnv;
use w3f_bls::{
    DoublePublicKey, DoublePublicKeyScheme, DoubleSignature, EngineBLS, Message, PublicKey, SecretKeyVT,
    SerializableToBytes, Signature, TinyBLS381,
};

pub const PUBLIC_KEY_SIZE: usize = 144;
pub const SIGNATURE_SIZE: usize = 112;
pub const AGGREGATE_SIZE: usize = 48;

type E = TinyBLS381;

fn message(m: &[u8]) -> Message {
    Message::new(b"", m)
}

pub fn public_from_seed(seed: &[u8]) -> Vec<u8> {
    SecretKeyVT::<E>::from_seed(seed).into_double_public_key().to_bytes()
}

pub fn sign(seed: &[u8], m: &[u8]) -> Vec<u8> {
    let mut sk = SecretKeyVT::<E>::from_seed(seed);
    DoublePublicKeyScheme::sign(&mut sk, &message(m)).to_bytes()
}

pub fn verify(pk: &[u8], m: &[u8], sig: &[u8]) -> bool {
    if pk.len() != PUBLIC_KEY_SIZE || sig.len() != SIGNATURE_SIZE {
        return false;
    }
    let (Ok(pk), Ok(sig)) = (DoublePublicKey::<E>::from_bytes(pk), DoubleSignature::<E>::from_bytes(sig)) else {
        return false;
    };
    sig.verify(&message(m), &pk)
}

/// Whether the key's G1 and G2 halves share one secret: e(g1, pk2) == e(pk1, g2). The double signature is checked
/// against the G1 half only, while aggregates are checked against G2 halves, so a key is checked once when learned.
pub fn key_consistent(pk: &[u8]) -> bool {
    if pk.len() != PUBLIC_KEY_SIZE {
        return false;
    }
    let Ok(pk) = DoublePublicKey::<E>::from_bytes(pk) else { return false };
    let g1 = E::prepare_signature(E::generator_of_signature_group());
    E::verify_prepared(E::prepare_signature(pk.0), &[(E::prepare_public_key(pk.1), g1)])
}

/// Sum of the G1 signatures of `n` 112-byte double signatures: one 48-byte BLS signature for the same message.
pub fn aggregate(sigs: &[u8]) -> Option<Vec<u8>> {
    if sigs.is_empty() || sigs.len() % SIGNATURE_SIZE != 0 {
        return None;
    }
    let mut sum = <E as EngineBLS>::SignatureGroup::default();
    for s in sigs.chunks(SIGNATURE_SIZE) {
        sum += DoubleSignature::<E>::from_bytes(s).ok()?.0;
    }
    Some(Signature::<E>(sum).to_bytes())
}

/// Checks a 48-byte aggregate against the sum of the G2 halves of `n` 144-byte keys, which all signed `m`. Sound
/// only for keys whose possession was proven (here: keys in the on-chain validator set, see [[key_consistent]]).
pub fn verify_aggregate(pks: &[u8], m: &[u8], agg: &[u8]) -> bool {
    if pks.is_empty() || pks.len() % PUBLIC_KEY_SIZE != 0 || agg.len() != AGGREGATE_SIZE {
        return false;
    }
    let mut sum = <E as EngineBLS>::PublicKeyGroup::default();
    for k in pks.chunks(PUBLIC_KEY_SIZE) {
        let Ok(k) = DoublePublicKey::<E>::from_bytes(k) else { return false };
        sum += k.1;
    }
    let Ok(sig) = Signature::<E>::from_bytes(agg) else { return false };
    sig.verify(&message(m), &PublicKey::<E>(sum))
}

fn guarded<T>(f: impl FnOnce() -> Option<T>) -> Option<T> {
    std::panic::catch_unwind(std::panic::AssertUnwindSafe(f)).ok().flatten()
}

fn to_java(env: &mut JNIEnv, bytes: Option<Vec<u8>>) -> jbyteArray {
    bytes
        .and_then(|b| env.byte_array_from_slice(&b).ok())
        .map(|a| a.into_raw())
        .unwrap_or(std::ptr::null_mut())
}

fn flag(ok: Option<bool>) -> jboolean {
    if ok.unwrap_or(false) { JNI_TRUE } else { JNI_FALSE }
}

#[no_mangle]
pub extern "system" fn Java_io_forge_jam_crypto_BlsWrapper_publicFromSeed(
    mut env: JNIEnv,
    _class: JClass,
    seed: JByteArray,
) -> jbyteArray {
    let out = guarded(|| Some(public_from_seed(&env.convert_byte_array(&seed).ok()?)));
    to_java(&mut env, out)
}

#[no_mangle]
pub extern "system" fn Java_io_forge_jam_crypto_BlsWrapper_sign(
    mut env: JNIEnv,
    _class: JClass,
    seed: JByteArray,
    msg: JByteArray,
) -> jbyteArray {
    let out = guarded(|| {
        let seed = env.convert_byte_array(&seed).ok()?;
        let msg = env.convert_byte_array(&msg).ok()?;
        Some(sign(&seed, &msg))
    });
    to_java(&mut env, out)
}

#[no_mangle]
pub extern "system" fn Java_io_forge_jam_crypto_BlsWrapper_verify(
    env: JNIEnv,
    _class: JClass,
    pk: JByteArray,
    msg: JByteArray,
    sig: JByteArray,
) -> jboolean {
    flag(guarded(|| {
        let pk = env.convert_byte_array(&pk).ok()?;
        let msg = env.convert_byte_array(&msg).ok()?;
        let sig = env.convert_byte_array(&sig).ok()?;
        Some(verify(&pk, &msg, &sig))
    }))
}

#[no_mangle]
pub extern "system" fn Java_io_forge_jam_crypto_BlsWrapper_keyConsistent(
    env: JNIEnv,
    _class: JClass,
    pk: JByteArray,
) -> jboolean {
    flag(guarded(|| Some(key_consistent(&env.convert_byte_array(&pk).ok()?))))
}

#[no_mangle]
pub extern "system" fn Java_io_forge_jam_crypto_BlsWrapper_aggregate(
    mut env: JNIEnv,
    _class: JClass,
    sigs: JByteArray,
) -> jbyteArray {
    let out = guarded(|| aggregate(&env.convert_byte_array(&sigs).ok()?));
    to_java(&mut env, out)
}

#[no_mangle]
pub extern "system" fn Java_io_forge_jam_crypto_BlsWrapper_verifyAggregate(
    env: JNIEnv,
    _class: JClass,
    pks: JByteArray,
    msg: JByteArray,
    agg: JByteArray,
) -> jboolean {
    flag(guarded(|| {
        let pks = env.convert_byte_array(&pks).ok()?;
        let msg = env.convert_byte_array(&msg).ok()?;
        let agg = env.convert_byte_array(&agg).ok()?;
        Some(verify_aggregate(&pks, &msg, &agg))
    }))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sizes_sign_verify() {
        let pk = public_from_seed(&[7u8; 32]);
        assert_eq!(pk.len(), PUBLIC_KEY_SIZE);
        let sig = sign(&[7u8; 32], b"$jam_beefy msg");
        assert_eq!(sig.len(), SIGNATURE_SIZE);
        assert!(verify(&pk, b"$jam_beefy msg", &sig));
        assert!(!verify(&pk, b"$jam_beefy msh", &sig));
        assert!(!verify(&public_from_seed(&[8u8; 32]), b"$jam_beefy msg", &sig));
        assert!(key_consistent(&pk));
        // Signing is deterministic (the DLEQ nonce is derived from key and message).
        assert_eq!(sig, sign(&[7u8; 32], b"$jam_beefy msg"));
    }

    #[test]
    fn mixed_key_halves_are_inconsistent() {
        let a = public_from_seed(&[1u8; 32]);
        let b = public_from_seed(&[2u8; 32]);
        let mixed = [&a[..48], &b[48..]].concat();
        assert!(!key_consistent(&mixed));
    }

    #[test]
    fn aggregate_verifies_against_summed_keys() {
        let m = b"commitment";
        let seeds: Vec<[u8; 32]> = (1..=5u8).map(|i| [i; 32]).collect();
        let pks: Vec<u8> = seeds.iter().flat_map(|s| public_from_seed(s)).collect();
        let sigs: Vec<u8> = seeds.iter().flat_map(|s| sign(s, m)).collect();
        let agg = aggregate(&sigs).unwrap();
        assert_eq!(agg.len(), AGGREGATE_SIZE);
        assert!(verify_aggregate(&pks, m, &agg));
        assert!(!verify_aggregate(&pks[..4 * PUBLIC_KEY_SIZE], m, &agg));
        assert!(!verify_aggregate(&pks, b"other", &agg));
    }
}
