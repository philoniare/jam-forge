use ark_ec_vrfs::reexports::ark_ec::{AdditiveGroup, AffineRepr, CurveGroup};
use ark_ec_vrfs::reexports::ark_ff::{BigInteger, PrimeField, Zero};
use ark_ec_vrfs::reexports::ark_serialize;
use ark_ec_vrfs::ring::RingProofParams;
use ark_ec_vrfs::suites::bandersnatch::BandersnatchSha512Ell2;
use ark_ec_vrfs::{Input, Output, Public, Secret, Suite};
use ark_serialize::{CanonicalDeserialize, CanonicalSerialize};

// Type aliases for IETF VRF
type IetfProof = ark_ec_vrfs::ietf::Proof<BandersnatchSha512Ell2>;

// Type aliases for Bandersnatch suite
type RingProof = ark_ec_vrfs::ring::Proof<BandersnatchSha512Ell2>;
type PcsParams = ark_ec_vrfs::ring::PcsParams<BandersnatchSha512Ell2>;
type RingParams = RingProofParams<BandersnatchSha512Ell2>;
use jni::objects::{JByteArray, JClass};
use jni::sys::{jbyte, jbyteArray, jint};
use jni::JNIEnv;
use std::collections::HashMap;
use std::sync::OnceLock;
use std::sync::{Arc, Mutex, MutexGuard};

// Shared, not cloned per use: a context holds the whole SRS-derived setup.
static RING_CONTEXTS: OnceLock<Mutex<HashMap<usize, Arc<RingParams>>>> = OnceLock::new();

/// Locks `m`, recovering it if a panic poisoned it: every value guarded here is replaced whole or read, never left
/// half-updated, so a panicking caller cannot leave it inconsistent. Without this one panic turns every later lock
/// into another panic, and every ring check into a silent failure until restart.
fn lock<T>(m: &Mutex<T>) -> MutexGuard<'_, T> {
    m.lock().unwrap_or_else(|poisoned| poisoned.into_inner())
}
const ERROR_RESULT: [u8; 32] = [0; 32];

macro_rules! jni_guard {
    ($env:expr, $default:expr, $body:block) => {
        match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| $body)) {
            Ok(v) => v,
            Err(_) => {
                // The unwinding panic already printed its message via the default hook.
                let _ = $env.throw_new(
                    "java/lang/RuntimeException",
                    "native panic (see stderr); converted to exception",
                );
                $default
            }
        }
    };
}

type RingCommitment = ark_ec_vrfs::ring::RingCommitment<BandersnatchSha512Ell2>;
type RingVerifier = ark_ec_vrfs::ring::RingVerifier<BandersnatchSha512Ell2>;

/// The ring verifier for the most recently used commitment. Building one from a commitment costs about 1 to 2 ms;
/// the commitment (gamma_z) changes once per epoch, so every ticket of an epoch reuses the same verifier.
static LAST_VERIFIER: Mutex<Option<(Vec<u8>, usize, Arc<RingVerifier>)>> = Mutex::new(None);

fn ring_verifier(commitment: &[u8], ring_size: jint) -> Result<Arc<RingVerifier>, VrfError> {
    let size = ring_size as usize;
    if let Some((c, n, v)) = lock(&LAST_VERIFIER).as_ref() {
        if *n == size && c.as_slice() == commitment {
            return Ok(v.clone());
        }
    }
    let parsed = RingCommitment::deserialize_compressed(commitment)
        .map_err(|_| VrfError::DeserializationError("invalid commitment format"))?;
    let ring_ctx = ring_context(ring_size)?;
    let verifier = Arc::new(ring_ctx.verifier(ring_ctx.verifier_key_from_commitment(parsed)));
    *lock(&LAST_VERIFIER) = Some((commitment.to_vec(), size, verifier.clone()));
    Ok(verifier)
}
type BanderInput = Input<BandersnatchSha512Ell2>;
type BanderOutput = Output<BandersnatchSha512Ell2>;
type BanderPublic = Public<BandersnatchSha512Ell2>;
type BanderSecret = Secret<BandersnatchSha512Ell2>;

/// Public keys already decoded with the subgroup check, by their encoding. Block seals and entropy
/// sources are signed by the current validators, so the same few keys come back every block.
static PUBLIC_KEYS: OnceLock<Mutex<HashMap<[u8; 32], BanderPublic>>> = OnceLock::new();
const PUBLIC_KEYS_MAX: usize = 4096;

fn checked_public(bytes: &[u8]) -> Option<BanderPublic> {
    let key: [u8; 32] = bytes.try_into().ok()?;
    let cache = PUBLIC_KEYS.get_or_init(|| Mutex::new(HashMap::new()));
    if let Some(p) = lock(cache).get(&key) {
        return Some(p.clone());
    }
    let public = BanderPublic::deserialize_compressed(bytes).ok()?;
    let mut map = lock(cache);
    if map.len() >= PUBLIC_KEYS_MAX {
        map.clear();
    }
    map.insert(key, public.clone());
    Some(public)
}

#[derive(Debug)]
enum VrfError {
    DeserializationError(&'static str),
    VerificationError,
    ConversionError,
    RingContextMissing(usize),
}

impl std::fmt::Display for VrfError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VrfError::DeserializationError(msg) => write!(f, "Deserialization error: {}", msg),
            VrfError::VerificationError => write!(f, "VRF verification failed"),
            VrfError::ConversionError => write!(f, "Data conversion error"),
            VrfError::RingContextMissing(n) => write!(f, "Ring context for ring size {} is not initialized", n),
        }
    }
}

#[derive(CanonicalSerialize, CanonicalDeserialize)]
struct RingVrfSignature {
    output: BanderOutput,
    // This contains both the Pedersen proof and actual ring proof.
    proof: RingProof,
}

fn vrf_input_point(vrf_input_data: &[u8]) -> BanderInput {
    BanderInput::new(vrf_input_data).unwrap()
}

fn initialize_ring_context(srs_data: &[u8], ring_size: jint) -> Result<(), String> {
    let ring_size = ring_size as usize;
    let contexts = RING_CONTEXTS.get_or_init(|| Mutex::new(HashMap::new()));
    let mut contexts = lock(contexts);

    if !contexts.contains_key(&ring_size) {
        let pcs_params = PcsParams::deserialize_uncompressed_unchecked(&mut &srs_data[..])
            .map_err(|e| format!("Failed to deserialize PCS params: {}", e))?;

        let ring_ctx = RingParams::from_pcs_params(ring_size, pcs_params)
            .map_err(|e| format!("Failed to create ring context: {:?}", e))?;

        contexts.insert(ring_size, Arc::new(ring_ctx));
    }

    Ok(())
}

/// The ring context for `ring_size`, or an error if `initializeContext` has not set it up. Never panics, so the lock
/// is never poisoned here.
fn ring_context(ring_size: jint) -> Result<Arc<RingParams>, VrfError> {
    let size = ring_size as usize;
    RING_CONTEXTS
        .get()
        .and_then(|contexts| lock(contexts).get(&size).cloned())
        .ok_or(VrfError::RingContextMissing(size))
}

#[no_mangle]
pub extern "system" fn Java_io_forge_jam_vrfs_BandersnatchWrapper_initializeContext(
    mut env: JNIEnv,
    _class: JClass,
    srs_data: JByteArray,
    ring_size: jint,
) -> jbyteArray {
    jni_guard!(env, std::ptr::null_mut(), {
        let srs_bytes = match env.convert_byte_array(srs_data) {
            Ok(data) => data,
            Err(e) => return throw_exception(&mut env, &format!("Failed to convert SRS data: {}", e)),
        };

        if let Err(e) = initialize_ring_context(&srs_bytes, ring_size) {
            return throw_exception(&mut env, &format!("Failed to initialize context: {}", e));
        }

        // Return empty array on success
        match env.byte_array_from_slice(&[]) {
            Ok(array) => array.into_raw(),
            Err(e) => throw_exception(&mut env, &format!("Failed to create return array: {}", e)),
        }
    })
}

type RingBuilder = ark_ec_vrfs::ring::VerifierKeyBuilder<BandersnatchSha512Ell2>;
type RingBuilderPcsParams = ark_ec_vrfs::ring::RingBuilderPcsParams<BandersnatchSha512Ell2>;

static RING_BUILDERS: Mutex<Vec<(usize, Arc<(RingBuilder, RingBuilderPcsParams)>)>> = Mutex::new(Vec::new());

fn ring_builder(ring_size: jint) -> Result<Arc<(RingBuilder, RingBuilderPcsParams)>, VrfError> {
    let size = ring_size as usize;
    let mut builders = lock(&RING_BUILDERS);
    if let Some((_, b)) = builders.iter().find(|(n, _)| *n == size) {
        return Ok(b.clone());
    }
    let built = Arc::new(ring_context(ring_size)?.verifier_key_builder());
    builders.push((size, built.clone()));
    Ok(built)
}

// Verifier actor.
struct Verifier {
    pub commitment: RingCommitment,
}

impl Verifier {
    fn new(ring: Vec<BanderPublic>, ring_size: jint) -> Result<Self, VrfError> {
        // Backend currently requires the wrapped type (plain affine points)
        let pts: Vec<_> = ring.iter().map(|pk| pk.0).collect();
        let builders = ring_builder(ring_size)?;
        let (empty, lagrange) = &*builders;
        let mut builder = empty.clone();
        builder
            .append(&pts, lagrange)
            .map_err(|_| VrfError::DeserializationError("ring larger than the ring context it was built for"))?;
        let commitment = builder.finalize().commitment();
        Ok(Self { commitment })
    }

    /// Anonymous VRF signature verification.
    ///
    /// Used for tickets verification.
    ///
    /// On success returns the VRF output hash.
    fn ring_vrf_verify(
        entropy: &[u8],
        attempt: u8,
        signature: &[u8],
        commitment: &[u8],
        ring_size: jint,
    ) -> Result<[u8; 32], VrfError> {
        use ark_ec_vrfs::ring::Verifier as _;

        let signature = RingVrfSignature::deserialize_compressed(signature)
            .map_err(|_| VrfError::DeserializationError("invalid signature format"))?;
        let input_data = [b"jam_ticket_seal", &entropy[..], &[attempt]].concat();

        let input = vrf_input_point(&input_data);
        let output = signature.output;

        let verifier = ring_verifier(commitment, ring_size)?;
        if BanderPublic::verify(input, output, &[], &signature.proof, &verifier).is_err() {
            return Err(VrfError::VerificationError);
        }
        output.hash()[..32]
            .try_into()
            .map_err(|_| VrfError::ConversionError)
    }

    /// Verifies the ring VRF signatures of a block's tickets together: `signatures` is one signature after
    /// another, each `signatures.len() / attempts.len()` bytes, the i-th made with `attempts[i]`. Every proof is
    /// decoded and prepared on its own, in parallel, then one pairing check decides all the ring proofs and one
    /// multi-scalar multiplication all the Pedersen proofs, where verifying one by one pays both per proof. On
    /// success returns the VRF output hashes in order; if any signature fails to decode or verify, an error and
    /// no outputs, without saying which.
    ///
    /// This is ark-vrf's `ring::BatchVerifier` with its two halves kept apart so that they can be verified at
    /// the same time.
    fn ring_vrf_verify_batch(
        entropy: &[u8],
        attempts: &[u8],
        signatures: &[u8],
        commitment: &[u8],
        ring_size: jint,
    ) -> Result<Vec<u8>, VrfError> {
        use ark_ec_vrfs::utils::te_sw_map::TEMapping;
        use rayon::prelude::*;
        type PedersenBatch = ark_ec_vrfs::pedersen::BatchVerifier<BandersnatchSha512Ell2>;

        if attempts.is_empty() || signatures.len() % attempts.len() != 0 {
            return Err(VrfError::DeserializationError("signatures are not one per attempt"));
        }
        let each = signatures.len() / attempts.len();
        // The batch verifier takes a ring verifier of its own, so one is built here, not shared. Checking the
        // commitment and building the verifier is most of a millisecond, so the signatures are decoded meanwhile.
        let build = || {
            let parsed = RingCommitment::deserialize_compressed(commitment)
                .map_err(|_| VrfError::DeserializationError("invalid commitment format"))?;
            let ring_ctx = ring_context(ring_size)?;
            Ok(ring_ctx.verifier(ring_ctx.verifier_key_from_commitment(parsed)).kzg_batch_verifier())
        };
        let decode = || -> Vec<Result<_, VrfError>> {
            signatures
                .par_chunks(each)
                .zip(attempts.par_iter())
                .map(|(signature, &attempt)| {
                    let signature = decode_ring_signature(signature)
                        .ok_or(VrfError::DeserializationError("invalid signature format"))?;
                    let input_data = [b"jam_ticket_seal", entropy, &[attempt]].concat();
                    let input = input_point(&input_data).ok_or(VrfError::VerificationError)?;
                    Ok((signature, input))
                })
                .collect()
        };
        let (built, decoded): (Result<_, VrfError>, _) = rayon::join(build, decode);
        let mut ring_batch = built?;
        let decoded = decoded.into_iter().collect::<Result<Vec<_>, VrfError>>()?;
        let mut pedersen_batch = PedersenBatch::new();

        let prepared: Vec<_> = decoded
            .into_par_iter()
            .map(|(signature, input)| {
                let pedersen = PedersenBatch::prepare(input, signature.output, [], &signature.proof.pedersen_proof);
                let key_commitment = signature.proof.pedersen_proof.key_commitment().into_te();
                let hash: Result<[u8; 32], VrfError> =
                    signature.output.hash()[..32].try_into().map_err(|_| VrfError::ConversionError);
                let ring = ring_batch.prepare(signature.proof.ring_proof, key_commitment);
                Ok((pedersen, ring, hash?))
            })
            .collect();

        let mut outputs = Vec::with_capacity(32 * attempts.len());
        for item in prepared {
            let (pedersen, ring, hash): (_, _, [u8; 32]) = item?;
            pedersen_batch.push_prepared(pedersen);
            ring_batch.push_prepared(ring);
            outputs.extend_from_slice(&hash);
        }
        let (pedersen_ok, ring_ok) = rayon::join(|| pedersen_batch.verify().is_ok(), || ring_batch.verify());
        if pedersen_ok && ring_ok {
            Ok(outputs)
        } else {
            Err(VrfError::VerificationError)
        }
    }
}

/// Decodes a ring VRF signature as `RingVrfSignature::deserialize_compressed` does, with the validity checks of
/// its parts (each curve point on its curve and in its prime-order subgroup, which is two thirds of the decoding)
/// run in parallel. Each pattern names every field, so a field added upstream fails to compile here rather than
/// going unchecked.
fn decode_ring_signature(bytes: &[u8]) -> Option<RingVrfSignature> {
    use ark_serialize::Valid;
    use rayon::prelude::*;
    let signature = RingVrfSignature::deserialize_compressed_unchecked(bytes).ok()?;
    let RingVrfSignature { output, proof } = &signature;
    let RingProof { pedersen_proof, ring_proof } = proof;
    let ark_ec_vrfs::ring::RingBareProof::<BandersnatchSha512Ell2> {
        column_commitments,
        columns_at_zeta,
        quotient_commitment,
        lin_at_zeta_omega,
        agg_at_zeta_proof,
        lin_at_zeta_omega_proof,
    } = ring_proof;
    let checks: [&(dyn Fn() -> bool + Sync); 8] = [
        &|| output.0.is_on_curve() && in_prime_subgroup(&output.0),
        &|| pedersen_proof.check().is_ok(),
        &|| column_commitments.check().is_ok(),
        &|| columns_at_zeta.check().is_ok(),
        &|| quotient_commitment.check().is_ok(),
        &|| lin_at_zeta_omega.check().is_ok(),
        &|| agg_at_zeta_proof.check().is_ok(),
        &|| lin_at_zeta_omega_proof.check().is_ok(),
    ];
    checks.par_iter().all(|check| check()).then_some(signature)
}

#[no_mangle]
pub extern "system" fn Java_io_forge_jam_vrfs_BandersnatchWrapper_getVerifierCommitment(
    mut env: JNIEnv,
    _class: JClass,
    ring_size: jint,
    keys: JByteArray,
) -> jbyteArray {
    jni_guard!(env, std::ptr::null_mut(), {
    // Helper function to throw exception and return null
    fn throw_and_return_null(env: &mut JNIEnv, message: &str) -> jbyteArray {
        let _ = env.throw_new("java/lang/RuntimeException", message);
        std::ptr::null_mut()
    }

    // Step 1: Convert the JByteArray to a Rust Vec<u8>
    let key_bytes = match env.convert_byte_array(keys) {
        Ok(bytes) => bytes,
        Err(e) => {
            return throw_and_return_null(
                &mut env,
                &format!("Failed to convert byte array: {:?}", e),
            );
        }
    };

    // Step 2: Determine the size of each public key
    const PUBLIC_KEY_SIZE: usize = 32;

    // Step 3: Validate the total length of the byte array
    let expected_length = (ring_size as usize) * PUBLIC_KEY_SIZE;
    if key_bytes.len() != expected_length {
        return throw_and_return_null(
            &mut env,
            &format!(
                "Invalid key bytes length: expected {}, got {}",
                expected_length,
                key_bytes.len()
            ),
        );
    }

    // Step 4: Split the byte array into individual keys
    let mut pub_keys = Vec::with_capacity(ring_size as usize);
    for i in 0..ring_size as usize {
        let start = i * PUBLIC_KEY_SIZE;
        let end = start + PUBLIC_KEY_SIZE;
        let key_slice = &key_bytes[start..end];
        pub_keys.push(key_slice.to_vec());
    }

    // Step 5: Deserialize each public key using unchecked deserialization
    let mut ring = Vec::with_capacity(ring_size as usize);
    for key in pub_keys.iter() {
        match BanderPublic::deserialize_compressed_unchecked(&mut &key[..]) {
            Ok(public_key) => ring.push(public_key),
            Err(_e) => {
                // Use a padding point as key
                ring.push(BanderPublic::from_affine(RingParams::padding_point()));
            }
        }
    }

    // Step 6: Create the Verifier
    let verifier = match Verifier::new(ring, ring_size) {
        Ok(v) => v,
        Err(e) => return throw_and_return_null(&mut env, &e.to_string()),
    };

    // Step 7: Serialize the commitment
    let mut buf = Vec::new();
    if verifier.commitment.serialize_compressed(&mut buf).is_err() {
        return throw_and_return_null(&mut env, "Failed to serialize commitment");
    }

    // Step 8: Convert to JByteArray
    match env.byte_array_from_slice(&buf) {
        Ok(array) => array.into_raw(),
        Err(e) => throw_and_return_null(&mut env, &format!("Failed to create output array: {}", e)),
    }
    })
}

fn throw_exception(env: &mut JNIEnv, message: &str) -> jbyteArray {
    let _ = env.throw_new("java/lang/RuntimeException", message);
    std::ptr::null_mut()
}

/// Ring VRF sign: create an anonymous Safrole ticket proof for
/// `jam_ticket_seal ++ entropy ++ attempt`
#[no_mangle]
pub extern "system" fn Java_io_forge_jam_vrfs_BandersnatchWrapper_ringVrfSign(
    mut env: JNIEnv,
    _class: JClass,
    secret_bytes: JByteArray,
    ring_keys: JByteArray,
    prover_index: jint,
    ring_size: jint,
    entropy: JByteArray,
    attempt: jbyte,
) -> jbyteArray {
    jni_guard!(env, std::ptr::null_mut(), {
    use ark_ec_vrfs::ring::Prover as _;

    let return_error_local = |env: &mut JNIEnv, error_msg: &str| -> jbyteArray {
        let _ = env.throw_new("java/lang/RuntimeException", error_msg);
        std::ptr::null_mut()
    };

    let secret_data = match env.convert_byte_array(&secret_bytes) {
        Ok(data) => data,
        Err(_) => return return_error_local(&mut env, "Failed to convert secret data"),
    };
    let key_bytes = match env.convert_byte_array(&ring_keys) {
        Ok(data) => data,
        Err(_) => return return_error_local(&mut env, "Failed to convert ring keys"),
    };
    let entropy_data = match env.convert_byte_array(&entropy) {
        Ok(data) => data,
        Err(_) => return return_error_local(&mut env, "Failed to convert entropy"),
    };

    const PUBLIC_KEY_SIZE: usize = 32;
    let expected_length = (ring_size as usize) * PUBLIC_KEY_SIZE;
    if key_bytes.len() != expected_length {
        return return_error_local(
            &mut env,
            &format!(
                "Invalid ring keys length: expected {}, got {}",
                expected_length,
                key_bytes.len()
            ),
        );
    }
    if prover_index < 0 || prover_index >= ring_size {
        return return_error_local(&mut env, "Prover index out of range");
    }

    let secret = match BanderSecret::deserialize_compressed(&secret_data[..]) {
        Ok(s) => s,
        Err(_) => return return_error_local(&mut env, "Failed to deserialize secret key"),
    };

    // Parse the ring exactly like the verifier-commitment path (padding
    // point for undecodable keys).
    let mut ring = Vec::with_capacity(ring_size as usize);
    for i in 0..ring_size as usize {
        let key_slice = &key_bytes[i * PUBLIC_KEY_SIZE..(i + 1) * PUBLIC_KEY_SIZE];
        match BanderPublic::deserialize_compressed_unchecked(&mut &key_slice[..]) {
            Ok(public_key) => ring.push(public_key),
            Err(_e) => ring.push(BanderPublic::from_affine(RingParams::padding_point())),
        }
    }

    let input_data = [b"jam_ticket_seal", &entropy_data[..], &[attempt as u8]].concat();
    let input = vrf_input_point(&input_data);
    let output = secret.output(input);

    let pts: Vec<_> = ring.iter().map(|pk| pk.0).collect();
    let ring_ctx = match ring_context(ring_size) {
        Ok(ctx) => ctx,
        Err(e) => return return_error_local(&mut env, &e.to_string()),
    };
    let prover_key = ring_ctx.prover_key(&pts);
    let prover = ring_ctx.prover(prover_key, prover_index as usize);

    let proof = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        secret.prove(input, output, &[], &prover)
    })) {
        Ok(p) => p,
        Err(_) => return return_error_local(&mut env, "Panic during ring proof generation"),
    };

    let signature = RingVrfSignature { output, proof };
    let mut buf = Vec::new();
    if signature.serialize_compressed(&mut buf).is_err() {
        return return_error_local(&mut env, "Failed to serialize ring signature");
    }

    match env.byte_array_from_slice(&buf) {
        Ok(array) => array.into_raw(),
        Err(_) => return_error_local(&mut env, "Failed to create output array"),
    }
    })
}

#[no_mangle]
pub extern "system" fn Java_io_forge_jam_vrfs_BandersnatchWrapper_verifierRingVrfVerify(
    mut env: JNIEnv,
    _class: JClass,
    entropy: JByteArray,
    attempt: jbyte,
    signature: JByteArray,
    commitment: JByteArray,
    ring_size: jint,
) -> jbyteArray {
    jni_guard!(env, std::ptr::null_mut(), {
    let return_error = |env: &mut JNIEnv, error_msg: &str| -> jbyteArray {
        let _ = env.throw_new("java/lang/RuntimeException", error_msg);
        match env.byte_array_from_slice(&ERROR_RESULT) {
            Ok(array) => array.into_raw(),
            Err(_) => std::ptr::null_mut(),
        }
    };

    // Convert input parameters
    let entropy_data = match env.convert_byte_array(entropy) {
        Ok(data) => data,
        Err(_) => return return_error(&mut env, "Failed to convert entropy data"),
    };

    let signature_data = match env.convert_byte_array(signature) {
        Ok(data) => data,
        Err(_) => return return_error(&mut env, "Failed to convert signature data"),
    };

    let commitment_data = match env.convert_byte_array(commitment) {
        Ok(data) => data,
        Err(_) => return return_error(&mut env, "Failed to convert commitment data"),
    };

    let attempt_value = attempt as u8;

    // Perform verification
    match Verifier::ring_vrf_verify(
        &entropy_data,
        attempt_value,
        &signature_data,
        &commitment_data,
        ring_size,
    ) {
        Ok(output_hash) => match env.byte_array_from_slice(&output_hash) {
            Ok(array) => array.into_raw(),
            Err(_) => return_error(&mut env, "Failed to create output array"),
        },
        Err(e) => return_error(&mut env, &e.to_string()),
    }
    })
}

/// Verifies a block's ticket signatures together (see `Verifier::ring_vrf_verify_batch`). `attempts` has one byte
/// per ticket and `signatures` the signatures one after another. Returns the 32-byte VRF outputs in order, or
/// throws if any ticket fails.
#[no_mangle]
pub extern "system" fn Java_io_forge_jam_vrfs_BandersnatchWrapper_verifierRingVrfVerifyBatch(
    mut env: JNIEnv,
    _class: JClass,
    entropy: JByteArray,
    attempts: JByteArray,
    signatures: JByteArray,
    commitment: JByteArray,
    ring_size: jint,
) -> jbyteArray {
    jni_guard!(env, std::ptr::null_mut(), {
    let return_error = |env: &mut JNIEnv, error_msg: &str| -> jbyteArray {
        let _ = env.throw_new("java/lang/RuntimeException", error_msg);
        std::ptr::null_mut()
    };
    let inputs = [entropy, attempts, signatures, commitment].map(|array| env.convert_byte_array(array).ok());
    let [Some(entropy), Some(attempts), Some(signatures), Some(commitment)] = inputs else {
        return return_error(&mut env, "Failed to convert input data");
    };
    match Verifier::ring_vrf_verify_batch(&entropy, &attempts, &signatures, &commitment, ring_size) {
        Ok(outputs) => match env.byte_array_from_slice(&outputs) {
            Ok(array) => array.into_raw(),
            Err(_) => return_error(&mut env, "Failed to create output array"),
        },
        Err(e) => return_error(&mut env, &e.to_string()),
    }
    })
}

/// IETF VRF signature structure (96 bytes: 32 output + 64 proof)
#[derive(CanonicalSerialize, CanonicalDeserialize)]
struct IetfVrfSignature {
    output: BanderOutput,
    proof: IetfProof,
}

type BanderAffine = ark_ec_vrfs::AffinePoint<BandersnatchSha512Ell2>;
type BanderProjective = <BanderAffine as AffineRepr>::Group;
type BanderScalar = ark_ec_vrfs::ScalarField<BandersnatchSha512Ell2>;

/// wNAF window for the generator, whose table is built once, and for the points that differ per signature.
const GENERATOR_WINDOW: usize = 8;
const POINT_WINDOW: usize = 5;

/// The odd multiples `P, 3P, .., (2^(window-1) - 1)P`: one entry per non-zero digit magnitude of a width-`window` NAF.
fn odd_multiples(point: &BanderAffine, window: usize) -> Vec<BanderProjective> {
    let twice = point.into_group().double();
    let mut multiple = point.into_group();
    let mut table = Vec::with_capacity(1 << (window - 2));
    table.push(multiple);
    for _ in 1..(1usize << (window - 2)) {
        multiple += twice;
        table.push(multiple);
    }
    table
}

fn generator_multiples() -> &'static [BanderProjective] {
    static TABLE: OnceLock<Vec<BanderProjective>> = OnceLock::new();
    TABLE.get_or_init(|| odd_multiples(&BandersnatchSha512Ell2::generator(), GENERATOR_WINDOW))
}

fn wnaf_digits(scalar: &BanderScalar, window: usize) -> Vec<i64> {
    // `find_wnaf` is None only for a window outside 2..64.
    scalar.into_bigint().find_wnaf(window).unwrap_or_default()
}

fn add_digit(acc: &mut BanderProjective, table: &[BanderProjective], digit: i64) {
    if digit > 0 {
        *acc += &table[(digit as usize) >> 1];
    } else if digit < 0 {
        *acc -= &table[(digit.unsigned_abs() as usize) >> 1];
    }
}

/// `a*P + b*Q` with one shared run of doublings (Straus), given the odd multiples of `P` and `Q` for their windows.
/// Not constant time: for verification only, where every input is public.
fn double_scalar_mul(
    p_multiples: &[BanderProjective],
    p_window: usize,
    a: &BanderScalar,
    q_multiples: &[BanderProjective],
    q_window: usize,
    b: &BanderScalar,
) -> BanderProjective {
    let a_digits = wnaf_digits(a, p_window);
    let b_digits = wnaf_digits(b, q_window);
    let mut acc = BanderProjective::zero();
    for i in (0..a_digits.len().max(b_digits.len())).rev() {
        acc.double_in_place();
        if let Some(&digit) = a_digits.get(i) {
            add_digit(&mut acc, p_multiples, digit);
        }
        if let Some(&digit) = b_digits.get(i) {
            add_digit(&mut acc, q_multiples, digit);
        }
    }
    acc
}

/// IETF VRF verification (RFC 9381 section 5.3 with additional data), the same check as ark-vrf's
/// `ietf::Verifier::verify`: recompute `u = s*G - c*Y` and `v = s*I - c*O` and compare the challenge. ark-vrf
/// computes the four products one by one, which is most of a verification; here each pair shares its doublings.
/// The reference the tests hold [`ietf_verify_signature`] to; the JNI entry runs that one.
#[cfg(test)]
fn ietf_verify(public: &BanderPublic, input: BanderInput, output: BanderOutput, aux: &[u8], proof: &IetfProof) -> bool {
    let minus_c = -proof.c;
    let u = double_scalar_mul(
        generator_multiples(),
        GENERATOR_WINDOW,
        &proof.s,
        &odd_multiples(&public.0, POINT_WINDOW),
        POINT_WINDOW,
        &minus_c,
    );
    let v = double_scalar_mul(
        &odd_multiples(&input.0, POINT_WINDOW),
        POINT_WINDOW,
        &proof.s,
        &odd_multiples(&output.0, POINT_WINDOW),
        POINT_WINDOW,
        &minus_c,
    );
    let norms = BanderProjective::normalize_batch(&[u, v]);
    let expected = BandersnatchSha512Ell2::challenge(&[&public.0, &input.0, &output.0, &norms[0], &norms[1]], aux);
    expected == proof.c
}

/// `a*P` given the odd multiples of `P` for `window`. Not constant time: verification only.
fn scalar_mul(multiples: &[BanderProjective], window: usize, a: &BanderScalar) -> BanderProjective {
    let digits = wnaf_digits(a, window);
    let mut acc = BanderProjective::zero();
    for &digit in digits.iter().rev() {
        acc.double_in_place();
        add_digit(&mut acc, multiples, digit);
    }
    acc
}

type BanderBase = <BanderAffine as AffineRepr>::BaseField;

/// `e`, the Montgomery `u`-coordinate of one of the curve's two points of order 2 at infinity:
/// `(-(a + d) + 2*sqrt(a*d)) / (a - d)` for the twisted Edwards coefficients `a` and `d`, whose product is a square.
fn two_torsion_u() -> &'static BanderBase {
    use ark_ec_vrfs::reexports::ark_ec::twisted_edwards::TECurveConfig;
    use ark_ec_vrfs::reexports::ark_ff::Field;
    type Config = <BanderAffine as AffineRepr>::Config;
    static E: OnceLock<BanderBase> = OnceLock::new();
    E.get_or_init(|| {
        let (a, d) = (<Config as TECurveConfig>::COEFF_A, <Config as TECurveConfig>::COEFF_D);
        let root = (a * d).sqrt().expect("a*d is a square on Bandersnatch");
        (root.double() - a - d) / (a - d)
    })
}

/// Whether the curve point `p` is in the prime-order subgroup: the same answer as arkworks' check, which
/// multiplies by the group order, for the cost of two Legendre symbols.
///
/// Bandersnatch's group is Z2 x Z2 x Zr with all three points of order 2 rational, so the prime-order subgroup is
/// exactly the doubles 2E, and a point is a double when the 2-descent characters agree with those of the identity.
/// For the order-2 point `(0, -1)` the character is the quadratic character of `1 - a*x^2`; for one at infinity,
/// with `u = (1 + y)/(1 - y)` the Montgomery coordinate, it is that of `(u - e)`, taken here as
/// `(1 - y)*((1 + y) - e*(1 - y))`, which is a non-residue on the subgroup. Two characters fix the third. Where an
/// argument is zero (the identity) the symbol says nothing and the group-order check decides.
fn in_prime_subgroup(p: &BanderAffine) -> bool {
    use ark_ec_vrfs::reexports::ark_ec::twisted_edwards::TECurveConfig;
    use ark_ec_vrfs::reexports::ark_ff::{Field, LegendreSymbol, One};
    type Config = <BanderAffine as AffineRepr>::Config;
    let one = BanderBase::one();
    let first = (one - <Config as TECurveConfig>::COEFF_A * p.x.square()).legendre();
    if first == LegendreSymbol::QuadraticNonResidue {
        return false;
    }
    let second = ((one - p.y) * ((one + p.y) - *two_torsion_u() * (one - p.y))).legendre();
    match (first, second) {
        (LegendreSymbol::QuadraticResidue, LegendreSymbol::QuadraticNonResidue) => true,
        (LegendreSymbol::QuadraticResidue, LegendreSymbol::QuadraticResidue) => false,
        _ => p.is_in_correct_subgroup_assuming_on_curve(),
    }
}

/// Decodes a VRF output point as `BanderOutput::deserialize_compressed` does (canonical encoding, on the curve, in
/// the prime-order subgroup), with [`in_prime_subgroup`] for the last check.
fn decode_output(bytes: &[u8]) -> Option<BanderOutput> {
    let point = BanderAffine::deserialize_compressed_unchecked(bytes).ok()?;
    in_prime_subgroup(&point).then(|| BanderOutput::from_affine(point))
}

type Job = Box<dyn FnOnce() + Send + 'static>;

/// A few threads that take the side work of a verification. The seal and the entropy source of a block are
/// verified at the same time and each hands over two pieces, so four keeps every piece on its own thread.
const VERIFY_WORKERS: usize = 4;

fn verify_pool() -> &'static Mutex<std::sync::mpsc::Sender<Job>> {
    static POOL: OnceLock<Mutex<std::sync::mpsc::Sender<Job>>> = OnceLock::new();
    POOL.get_or_init(|| {
        let (sender, receiver) = std::sync::mpsc::channel::<Job>();
        let receiver = Arc::new(Mutex::new(receiver));
        for i in 0..VERIFY_WORKERS {
            let receiver = Arc::clone(&receiver);
            let _ = std::thread::Builder::new().name(format!("bandersnatch-verify-{i}")).spawn(move || loop {
                let job = lock(&receiver).recv();
                match job {
                    // A panicking job drops its result sender, which its caller sees as a failed verification.
                    Ok(job) => drop(std::panic::catch_unwind(std::panic::AssertUnwindSafe(job))),
                    Err(_) => break,
                }
            });
        }
        Mutex::new(sender)
    })
}

/// Runs `job` on a pool thread and returns where its result arrives. If the pool cannot take it (no thread could
/// be started), the job runs here, so the result is always there to receive.
fn on_pool<T: Send + 'static>(job: impl FnOnce() -> T + Send + 'static) -> std::sync::mpsc::Receiver<T> {
    let (result_sender, result) = std::sync::mpsc::sync_channel::<T>(1);
    let boxed: Job = Box::new(move || {
        let _ = result_sender.send(job());
    });
    if let Err(rejected) = lock(verify_pool()).send(boxed) {
        (rejected.0)();
    }
    result
}

/// Input points of VRF inputs that come back, by their data. A fallback seal's input is the same for a whole
/// epoch and a ticket seal's one of a few, while an entropy source's is new with every block. Hashing to the curve
/// is a quarter of a verification, so an input is kept from the second time it is seen: the repeating ones are
/// then found here, and the ones seen once never fill the table.
struct InputPoints {
    points: HashMap<Vec<u8>, BanderInput>,
    /// Fingerprints of inputs seen once, the oldest overwritten first.
    seen_once: [u64; 16],
    next: usize,
}
const INPUT_POINTS_MAX: usize = 64;

fn input_point(data: &[u8]) -> Option<BanderInput> {
    use std::hash::{BuildHasher, Hasher};
    static INPUTS: OnceLock<(Mutex<InputPoints>, std::collections::hash_map::RandomState)> = OnceLock::new();
    let (inputs, hasher) = INPUTS.get_or_init(|| {
        let empty = InputPoints { points: HashMap::new(), seen_once: [0; 16], next: 0 };
        (Mutex::new(empty), std::collections::hash_map::RandomState::new())
    });
    if let Some(point) = lock(inputs).points.get(data) {
        return Some(*point);
    }
    let point = BanderInput::new(data)?;
    let mut h = hasher.build_hasher();
    h.write(data);
    let fingerprint = h.finish();
    let mut inputs = lock(inputs);
    if inputs.seen_once.contains(&fingerprint) {
        if inputs.points.len() >= INPUT_POINTS_MAX {
            inputs.points.clear();
        }
        inputs.points.insert(data.to_vec(), point);
    } else {
        let at = inputs.next;
        inputs.seen_once[at] = fingerprint;
        inputs.next = (at + 1) % inputs.seen_once.len();
    }
    Some(point)
}

/// Verifies the 96-byte IETF VRF signature `sig` (output point, then proof) by `public` over the input hashed from
/// `input_data`, and returns the output point. The same check as [`ietf_verify`] on the decoded values, with its
/// three independent parts run at once: `u = s*G - c*Y` goes to one pool thread, decoding the output point `O`
/// and `-c*O` to another, and this thread hashes to the input point `I` and computes `s*I`, the longest part, so
/// that it is not the one waiting for a thread to wake. The caller waits for under half the work of a
/// verification in place of all of it.
fn ietf_verify_signature(public: &BanderPublic, input_data: &[u8], aux: &[u8], sig: &[u8]) -> Option<BanderOutput> {
    if sig.len() < 96 {
        return None;
    }
    // The proof is two scalars, cheap to read, and every part needs them.
    let proof = IetfProof::deserialize_compressed(&sig[32..96]).ok()?;
    let (c, s) = (proof.c, proof.s);
    let minus_c = -c;

    let public_point = public.0;
    let u_result = on_pool(move || {
        double_scalar_mul(
            generator_multiples(),
            GENERATOR_WINDOW,
            &s,
            &odd_multiples(&public_point, POINT_WINDOW),
            POINT_WINDOW,
            &minus_c,
        )
    });
    let output_bytes: [u8; 32] = sig[..32].try_into().ok()?;
    let output_result = on_pool(move || {
        let output = decode_output(&output_bytes)?;
        let minus_c_output = scalar_mul(&odd_multiples(&output.0, POINT_WINDOW), POINT_WINDOW, &minus_c);
        Some((output, minus_c_output))
    });

    let input = input_point(input_data);
    let s_input = input.as_ref().map(|i| scalar_mul(&odd_multiples(&i.0, POINT_WINDOW), POINT_WINDOW, &s));

    // Both results are received whatever the outcome, so no job is left writing to a dropped channel.
    let u = u_result.recv().ok();
    let output = output_result.recv().ok().flatten();
    let (input, s_input, u, (output, minus_c_output)) = (input?, s_input?, u?, output?);

    let v = s_input + minus_c_output;
    // One inversion each: with ark's `parallel` feature a batch normalisation of two goes through its thread pool.
    let (u, v) = (u.into_affine(), v.into_affine());
    let expected = BandersnatchSha512Ell2::challenge(&[&public.0, &input.0, &output.0, &u, &v], aux);
    (expected == c).then_some(output)
}

/// Create a secret key from a 32-byte seed
#[no_mangle]
pub extern "system" fn Java_io_forge_jam_vrfs_BandersnatchWrapper_secretFromSeed(
    mut env: JNIEnv,
    _class: JClass,
    seed: JByteArray,
) -> jbyteArray {
    jni_guard!(env, std::ptr::null_mut(), {
    let return_error_local = |env: &mut JNIEnv, error_msg: &str| -> jbyteArray {
        let _ = env.throw_new("java/lang/RuntimeException", error_msg);
        std::ptr::null_mut()
    };

    let seed_data = match env.convert_byte_array(&seed) {
        Ok(data) => data,
        Err(_) => return return_error_local(&mut env, "Failed to convert seed data"),
    };

    if seed_data.len() != 32 {
        return return_error_local(&mut env, "Seed must be exactly 32 bytes");
    }

    let secret = BanderSecret::from_seed(&seed_data);

    // Serialize the secret key (scalar)
    let mut buf = Vec::new();
    if secret.serialize_compressed(&mut buf).is_err() {
        return return_error_local(&mut env, "Failed to serialize secret key");
    }

    match env.byte_array_from_slice(&buf) {
        Ok(array) => array.into_raw(),
        Err(_) => return_error_local(&mut env, "Failed to create output array"),
    }
    })
}

/// Get the public key from a secret key (serialized)
#[no_mangle]
pub extern "system" fn Java_io_forge_jam_vrfs_BandersnatchWrapper_publicFromSecret(
    mut env: JNIEnv,
    _class: JClass,
    secret_bytes: JByteArray,
) -> jbyteArray {
    jni_guard!(env, std::ptr::null_mut(), {
    let return_error_local = |env: &mut JNIEnv, error_msg: &str| -> jbyteArray {
        let _ = env.throw_new("java/lang/RuntimeException", error_msg);
        std::ptr::null_mut()
    };

    let secret_data = match env.convert_byte_array(&secret_bytes) {
        Ok(data) => data,
        Err(_) => return return_error_local(&mut env, "Failed to convert secret data"),
    };

    let secret = match BanderSecret::deserialize_compressed(&secret_data[..]) {
        Ok(s) => s,
        Err(_) => return return_error_local(&mut env, "Failed to deserialize secret key"),
    };

    let public = secret.public();

    let mut buf = Vec::new();
    if public.serialize_compressed(&mut buf).is_err() {
        return return_error_local(&mut env, "Failed to serialize public key");
    }

    match env.byte_array_from_slice(&buf) {
        Ok(array) => array.into_raw(),
        Err(_) => return_error_local(&mut env, "Failed to create output array"),
    }
    })
}

/// IETF VRF sign - creates a 96-byte signature
#[no_mangle]
pub extern "system" fn Java_io_forge_jam_vrfs_BandersnatchWrapper_ietfVrfSign(
    mut env: JNIEnv,
    _class: JClass,
    secret_bytes: JByteArray,
    vrf_input: JByteArray,
    aux_data: JByteArray,
) -> jbyteArray {
    jni_guard!(env, std::ptr::null_mut(), {
    use ark_ec_vrfs::ietf::Prover as _;

    let return_error_local = |env: &mut JNIEnv, error_msg: &str| -> jbyteArray {
        let _ = env.throw_new("java/lang/RuntimeException", error_msg);
        std::ptr::null_mut()
    };

    let secret_data = match env.convert_byte_array(&secret_bytes) {
        Ok(data) => data,
        Err(_) => return return_error_local(&mut env, "Failed to convert secret data"),
    };

    let vrf_input_data = match env.convert_byte_array(&vrf_input) {
        Ok(data) => data,
        Err(_) => return return_error_local(&mut env, "Failed to convert VRF input data"),
    };

    let aux = match env.convert_byte_array(&aux_data) {
        Ok(data) => data,
        Err(_) => return return_error_local(&mut env, "Failed to convert aux data"),
    };

    let secret = match BanderSecret::deserialize_compressed(&secret_data[..]) {
        Ok(s) => s,
        Err(_) => return return_error_local(&mut env, "Failed to deserialize secret key"),
    };

    let input = vrf_input_point(&vrf_input_data);
    let output = secret.output(input);
    let proof = secret.prove(input, output, &aux);

    // IETF signature structure: output (32 bytes) + proof (64 bytes) = 96 bytes
    #[derive(CanonicalSerialize)]
    struct IetfSig {
        output: BanderOutput,
        proof: IetfProof,
    }

    let signature = IetfSig { output, proof };
    let mut buf = Vec::new();
    if signature.serialize_compressed(&mut buf).is_err() {
        return return_error_local(&mut env, "Failed to serialize signature");
    }

    match env.byte_array_from_slice(&buf) {
        Ok(array) => array.into_raw(),
        Err(_) => return_error_local(&mut env, "Failed to create output array"),
    }
    })
}

/// IETF VRF verify - verifies signature and returns 32-byte output hash
#[no_mangle]
pub extern "system" fn Java_io_forge_jam_vrfs_BandersnatchWrapper_ietfVrfVerify(
    mut env: JNIEnv,
    _class: JClass,
    public_bytes: JByteArray,
    vrf_input: JByteArray,
    aux_data: JByteArray,
    signature: JByteArray,
) -> jbyteArray {
    jni_guard!(env, std::ptr::null_mut(), {
    let return_error_local = |env: &mut JNIEnv, error_msg: &str| -> jbyteArray {
        let _ = env.throw_new("java/lang/RuntimeException", error_msg);
        match env.byte_array_from_slice(&ERROR_RESULT) {
            Ok(array) => array.into_raw(),
            Err(_) => std::ptr::null_mut(),
        }
    };

    let public_data = match env.convert_byte_array(&public_bytes) {
        Ok(data) => data,
        Err(_) => return return_error_local(&mut env, "Failed to convert public key data"),
    };

    let vrf_input_data = match env.convert_byte_array(&vrf_input) {
        Ok(data) => data,
        Err(_) => return return_error_local(&mut env, "Failed to convert VRF input data"),
    };

    let aux = match env.convert_byte_array(&aux_data) {
        Ok(data) => data,
        Err(_) => return return_error_local(&mut env, "Failed to convert aux data"),
    };

    let sig_data = match env.convert_byte_array(&signature) {
        Ok(data) => data,
        Err(_) => return return_error_local(&mut env, "Failed to convert signature data"),
    };

    let public = match checked_public(&public_data[..]) {
        Some(p) => p,
        None => return return_error_local(&mut env, "Failed to deserialize public key"),
    };

    let output = match ietf_verify_signature(&public, &vrf_input_data, &aux, &sig_data) {
        Some(output) => output,
        None => return return_error_local(&mut env, "IETF VRF verification failed"),
    };

    let output_hash: [u8; 32] = output.hash()[..32].try_into().unwrap_or([0u8; 32]);

    match env.byte_array_from_slice(&output_hash) {
        Ok(array) => array.into_raw(),
        Err(_) => return_error_local(&mut env, "Failed to create output array"),
    }
    })
}

/// Get VRF output directly from secret key and input (without creating signature)
#[no_mangle]
pub extern "system" fn Java_io_forge_jam_vrfs_BandersnatchWrapper_getVrfOutput(
    mut env: JNIEnv,
    _class: JClass,
    secret_bytes: JByteArray,
    vrf_input: JByteArray,
) -> jbyteArray {
    jni_guard!(env, std::ptr::null_mut(), {
    let return_error_local = |env: &mut JNIEnv, error_msg: &str| -> jbyteArray {
        let _ = env.throw_new("java/lang/RuntimeException", error_msg);
        match env.byte_array_from_slice(&ERROR_RESULT) {
            Ok(array) => array.into_raw(),
            Err(_) => std::ptr::null_mut(),
        }
    };

    let secret_data = match env.convert_byte_array(&secret_bytes) {
        Ok(data) => data,
        Err(_) => return return_error_local(&mut env, "Failed to convert secret data"),
    };

    let vrf_input_data = match env.convert_byte_array(&vrf_input) {
        Ok(data) => data,
        Err(_) => return return_error_local(&mut env, "Failed to convert VRF input data"),
    };

    let secret = match BanderSecret::deserialize_compressed(&secret_data[..]) {
        Ok(s) => s,
        Err(_) => return return_error_local(&mut env, "Failed to deserialize secret key"),
    };

    let input = vrf_input_point(&vrf_input_data);
    let output = secret.output(input);
    let output_hash: [u8; 32] = output.hash()[..32].try_into().unwrap_or([0u8; 32]);

    match env.byte_array_from_slice(&output_hash) {
        Ok(array) => array.into_raw(),
        Err(_) => return_error_local(&mut env, "Failed to create output array"),
    }
    })
}

/// Extract the VRF output from an IETF VRF signature.
/// The signature is 96 bytes and the output is 32 bytes.
#[no_mangle]
pub extern "system" fn Java_io_forge_jam_vrfs_BandersnatchWrapper_getIetfVrfOutput(
    mut env: JNIEnv,
    _class: JClass,
    signature: JByteArray,
) -> jbyteArray {
    jni_guard!(env, std::ptr::null_mut(), {
    let return_error_local = |env: &mut JNIEnv, error_msg: &str| -> jbyteArray {
        let _ = env.throw_new("java/lang/RuntimeException", error_msg);
        match env.byte_array_from_slice(&ERROR_RESULT) {
            Ok(array) => array.into_raw(),
            Err(_) => std::ptr::null_mut(),
        }
    };

    // Get signature bytes
    let signature_data: Vec<u8> = match env.convert_byte_array(&signature) {
        Ok(data) => data,
        Err(_) => return return_error_local(&mut env, "Failed to convert signature data"),
    };

    if signature_data.len() < 96 {
        return return_error_local(&mut env, "Signature must be at least 96 bytes");
    }

    // Deserialize the IETF VRF signature
    let signature = match IetfVrfSignature::deserialize_compressed_unchecked(&signature_data[..]) {
        Ok(sig) => sig,
        Err(_) => return return_error_local(&mut env, "Failed to deserialize IETF VRF signature"),
    };

    // Extract the output hash (32 bytes)
    let output_hash = signature.output.hash();
    let output_bytes: [u8; 32] = output_hash[..32].try_into().unwrap_or([0u8; 32]);

    match env.byte_array_from_slice(&output_bytes) {
        Ok(array) => array.into_raw(),
        Err(_) => return_error_local(&mut env, "Failed to create output array"),
    }
    })
}

#[no_mangle]
pub extern "system" fn Java_io_forge_jam_vrfs_BandersnatchWrapper_getRingVrfOutput(
    mut env: JNIEnv,
    _class: JClass,
    signature: JByteArray,
) -> jbyteArray {
    jni_guard!(env, std::ptr::null_mut(), {
    let sig = match env.convert_byte_array(&signature) {
        Ok(data) => data,
        Err(_) => return throw_exception(&mut env, "Failed to convert signature data"),
    };
    if sig.len() < 32 {
        return throw_exception(&mut env, "Ring VRF signature too short");
    }
    let output = match BanderOutput::deserialize_compressed(&sig[..32]) {
        Ok(o) => o,
        Err(_) => return throw_exception(&mut env, "Invalid ring VRF output point"),
    };
    let hash: [u8; 32] = output.hash()[..32].try_into().unwrap_or([0u8; 32]);
    match env.byte_array_from_slice(&hash) {
        Ok(array) => array.into_raw(),
        Err(_) => throw_exception(&mut env, "Failed to create output array"),
    }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use ark_serialize::{CanonicalDeserialize, CanonicalSerialize};

    #[test]
    fn test_zeros_key_becomes_padding_point() {
        // All zeros should fail to deserialize and be replaced with padding point
        let zeros = [0u8; 32];

        let result = BanderPublic::deserialize_compressed(&zeros[..]);

        match &result {
            Ok(_) => println!("WARNING: Zeros deserializes OK - may need explicit check"),
            Err(e) => println!("GOOD: Zeros fails to deserialize: {:?}", e),
        }

        // Check what we'd actually use in ring construction
        let point_for_ring = match BanderPublic::deserialize_compressed(&zeros[..]) {
            Ok(pk) => pk,
            Err(_) => BanderPublic::from_affine(RingParams::padding_point()),
        };

        // Serialize the padding point
        let padding = RingParams::padding_point();
        let mut padding_buf = Vec::new();
        padding.serialize_compressed(&mut padding_buf).unwrap();

        // Serialize what we got
        let mut actual_buf = Vec::new();
        point_for_ring
            .0
            .serialize_compressed(&mut actual_buf)
            .unwrap();

        println!("Padding point:     {}", hex::encode(&padding_buf));
        println!("Actual used point: {}", hex::encode(&actual_buf));

        // Expected from spec: 92ca79e61dd90c1573a8693f199bf6e1e86835cc715cdcf93f5ef222560023aa
        let expected = "92ca79e61dd90c1573a8693f199bf6e1e86835cc715cdcf93f5ef222560023aa";
        println!("Expected padding:  {}", expected);

        assert_eq!(
            hex::encode(&padding_buf),
            expected,
            "Padding point should match spec"
        );
    }

    fn test_scalar(rng: &mut rand_chacha::ChaCha20Rng) -> BanderScalar {
        use ark_ec_vrfs::reexports::ark_std::UniformRand;
        BanderScalar::rand(rng)
    }

    #[test]
    fn double_scalar_mul_matches_two_plain_multiplications() {
        use rand_chacha::rand_core::SeedableRng;
        let mut rng = rand_chacha::ChaCha20Rng::from_seed([3u8; 32]);
        let p = BanderSecret::from_seed(&[1u8; 32]).public().0;
        let q = vrf_input_point(b"some input").0;
        let one = BanderScalar::from(1u64);
        let mut scalars = vec![BanderScalar::zero(), one, -one, BanderScalar::from(2u64), BanderScalar::from(15u64), BanderScalar::from(16u64)];
        scalars.extend((0..40).map(|_| test_scalar(&mut rng)));
        for (p_window, q_window) in [(GENERATOR_WINDOW, POINT_WINDOW), (POINT_WINDOW, POINT_WINDOW), (2, 3)] {
            let p_multiples = odd_multiples(&p, p_window);
            let q_multiples = odd_multiples(&q, q_window);
            for a in &scalars {
                for b in &scalars {
                    let got = double_scalar_mul(&p_multiples, p_window, a, &q_multiples, q_window, b);
                    assert_eq!(got.into_affine(), (p * a + q * b).into_affine());
                }
            }
        }
    }

    #[test]
    fn ietf_verify_agrees_with_ark_vrf() {
        use ark_ec_vrfs::ietf::{Prover as _, Verifier as _};
        use rand_chacha::rand_core::SeedableRng;
        let mut rng = rand_chacha::ChaCha20Rng::from_seed([5u8; 32]);
        let one = BanderScalar::from(1u64);
        let mut accepted = 0;
        for i in 0..40u8 {
            let secret = BanderSecret::from_seed(&[i; 32]);
            let other = BanderSecret::from_seed(&[i.wrapping_add(100); 32]);
            let input = vrf_input_point(&[b"jam_fallback_seal".as_slice(), &[i; 32]].concat());
            let other_input = vrf_input_point(&[b"jam_entropy".as_slice(), &[i; 32]].concat());
            let output = secret.output(input);
            let aux = vec![i; i as usize * 7];
            let proof = secret.prove(input, output, &aux);
            let zero = BanderScalar::zero();
            let proofs = [
                proof.clone(),
                IetfProof { c: proof.c + one, s: proof.s },
                IetfProof { c: proof.c, s: proof.s + one },
                IetfProof { c: proof.s, s: proof.c },
                IetfProof { c: zero, s: proof.s },
                IetfProof { c: proof.c, s: zero },
                IetfProof { c: zero, s: zero },
                IetfProof { c: test_scalar(&mut rng), s: test_scalar(&mut rng) },
            ];
            let mut longer_aux = aux.clone();
            longer_aux.push(0);
            for candidate in &proofs {
                for public in [secret.public(), other.public()] {
                    for (inp, out) in [(input, output), (other_input, output), (input, other.output(input))] {
                        for ad in [&aux, &longer_aux] {
                            let expected = public.verify(inp, out, ad, candidate).is_ok();
                            assert_eq!(ietf_verify(&public, inp, out, ad, candidate), expected);
                            accepted += expected as usize;
                        }
                    }
                }
            }
        }
        // Only the untouched proof under its own key, input, output and additional data verifies.
        assert_eq!(accepted, 40);
    }

    /// What the JNI entry did before the parts ran in parallel: decode the whole signature, then verify.
    fn sequential_verify(public: &BanderPublic, input_data: &[u8], aux: &[u8], sig: &[u8]) -> Option<BanderOutput> {
        use ark_ec_vrfs::ietf::Verifier as _;
        let decoded = IetfVrfSignature::deserialize_compressed(sig).ok()?;
        let input = vrf_input_point(input_data);
        public.verify(input, decoded.output, aux, &decoded.proof).ok()?;
        Some(decoded.output)
    }

    #[test]
    fn ietf_verify_signature_agrees_with_ark_vrf_on_signature_bytes() {
        use ark_ec_vrfs::ietf::Prover as _;
        use rand_chacha::rand_core::{RngCore, SeedableRng};
        let mut rng = rand_chacha::ChaCha20Rng::from_seed([11u8; 32]);
        let (mut accepted, mut rejected) = (0, 0);
        for i in 0..24u8 {
            let secret = BanderSecret::from_seed(&[i; 32]);
            let other = BanderSecret::from_seed(&[i.wrapping_add(50); 32]);
            let data = [b"jam_fallback_seal".as_slice(), &[i; 32]].concat();
            let other_data = [b"jam_entropy".as_slice(), &[i; 32]].concat();
            let aux = vec![i; i as usize * 5];
            let input = vrf_input_point(&data);
            let output = secret.output(input);
            let proof = secret.prove(input, output, &aux);
            let mut good = Vec::new();
            IetfVrfSignature { output, proof }.serialize_compressed(&mut good).unwrap();
            assert_eq!(good.len(), 96);

            let mut candidates = vec![good.clone(), good[..95].to_vec(), good[..32].to_vec(), Vec::new()];
            // Trailing bytes are ignored, as by the decoder this replaces.
            candidates.push([good.as_slice(), &[1, 2, 3]].concat());
            // Another signer's output point in a good proof, and the reverse.
            let mut foreign = Vec::new();
            other.output(input).serialize_compressed(&mut foreign).unwrap();
            candidates.push([&foreign[..], &good[32..]].concat());
            // Every byte position flipped once: off-curve and out-of-subgroup output points, non-canonical
            // scalars and wrong but well-formed values all come up.
            for at in 0..96 {
                let mut bad = good.clone();
                bad[at] ^= 1 << (rng.next_u32() % 8);
                candidates.push(bad);
            }
            for _ in 0..40 {
                let mut random = vec![0u8; 96];
                rng.fill_bytes(&mut random);
                candidates.push(random);
            }
            // Random output points with the good proof: most decode to a curve point outside the prime subgroup.
            for _ in 0..40 {
                let mut bad = good.clone();
                rng.fill_bytes(&mut bad[..32]);
                candidates.push(bad);
            }

            for sig in &candidates {
                for public in [secret.public(), other.public()] {
                    for (d, ad) in [(&data, &aux), (&other_data, &aux), (&data, &other_data)] {
                        let expected = sequential_verify(&public, d, ad, sig);
                        let got = ietf_verify_signature(&public, d, ad, sig);
                        assert_eq!(got, expected, "signature {}", hex::encode(sig));
                        if expected.is_some() { accepted += 1 } else { rejected += 1 }
                    }
                }
            }
        }
        // Per signer: the good signature and the one with trailing bytes, under its own key, input and data.
        assert_eq!(accepted, 24 * 2);
        assert!(rejected > 24 * 1000);
    }

    #[test]
    fn ietf_verify_signature_is_safe_from_many_threads() {
        use ark_ec_vrfs::ietf::Prover as _;
        let cases: Vec<_> = (0..16u8)
            .map(|i| {
                let secret = BanderSecret::from_seed(&[i; 32]);
                let data = vec![i; 40];
                let input = vrf_input_point(&data);
                let output = secret.output(input);
                let mut sig = Vec::new();
                IetfVrfSignature { output, proof: secret.prove(input, output, b"x") }.serialize_compressed(&mut sig).unwrap();
                (secret.public(), data, sig, output)
            })
            .collect();
        std::thread::scope(|scope| {
            for t in 0..12usize {
                let cases = &cases;
                scope.spawn(move || {
                    for round in 0..200 {
                        let (public, data, sig, output) = &cases[(t + round) % cases.len()];
                        assert_eq!(ietf_verify_signature(public, data, b"x", sig), Some(*output));
                        let (wrong_key, ..) = &cases[(t + round + 1) % cases.len()];
                        assert_eq!(ietf_verify_signature(wrong_key, data, b"x", sig), None);
                    }
                });
            }
        });
    }

    #[test]
    fn input_point_is_the_hashed_point_whether_kept_or_not() {
        // Each input several times over, so every one is computed, noted, kept and then found; and more distinct
        // inputs than the table holds, so it is emptied and refilled on the way.
        for round in 0..4 {
            for i in 0..200u32 {
                let data = [b"jam_fallback_seal".as_slice(), &i.to_le_bytes()].concat();
                assert_eq!(input_point(&data), BanderInput::new(&data), "input {i}, round {round}");
            }
        }
        assert_eq!(input_point(b""), BanderInput::new(b""));
    }

    #[test]
    fn checked_public_caches_valid_keys_and_rejects_bad_ones() {
        let secret = BanderSecret::from_seed(&[7u8; 32]);
        let mut bytes = Vec::new();
        secret.public().serialize_compressed(&mut bytes).unwrap();
        let first = checked_public(&bytes).expect("valid key");
        let again = checked_public(&bytes).expect("cached key");
        assert_eq!(first, again);
        assert_eq!(first, secret.public());

        // Not a point on the curve, and the wrong length: rejected every time, never cached.
        let bad = [0xffu8; 32];
        assert!(checked_public(&bad).is_none());
        assert!(checked_public(&bad).is_none());
        assert!(checked_public(&bytes[..31]).is_none());
    }

    #[test]
    fn in_prime_subgroup_agrees_with_the_group_order_check() {
        use ark_ec_vrfs::reexports::ark_ff::One;
        use rand_chacha::rand_core::{RngCore, SeedableRng};
        let mut rng = rand_chacha::ChaCha20Rng::from_seed([1u8; 32]);
        let (mut inside, mut outside) = (0, 0);
        // Random encodings decode to curve points spread evenly over the four cosets of the subgroup.
        while inside + outside < 20_000 {
            let mut bytes = [0u8; 32];
            rng.fill_bytes(&mut bytes);
            let Ok(p) = BanderAffine::deserialize_compressed_unchecked(&bytes[..]) else { continue };
            let expected = p.is_in_correct_subgroup_assuming_on_curve();
            assert_eq!(in_prime_subgroup(&p), expected, "point {}", hex::encode(bytes));
            assert_eq!(decode_output(&bytes).is_some(), BanderOutput::deserialize_compressed(&bytes[..]).is_ok());
            if expected { inside += 1 } else { outside += 1 }
        }
        assert!(inside > 4000 && outside > 13000, "{inside} inside, {outside} outside");

        // The identity, the affine point of order 2, and subgroup points moved by it.
        let identity = BanderAffine::zero();
        let order_two = BanderAffine::new_unchecked(BanderBase::zero(), -BanderBase::one());
        assert!(order_two.is_on_curve() && !order_two.is_in_correct_subgroup_assuming_on_curve());
        assert!(in_prime_subgroup(&identity));
        assert!(!in_prime_subgroup(&order_two));
        for i in 0..200u8 {
            let p = BanderSecret::from_seed(&[i; 32]).public().0;
            assert!(in_prime_subgroup(&p));
            assert!(!in_prime_subgroup(&(p + order_two).into_affine()));
        }
    }

    /// A ring of `size` keys set up from the SRS the node ships, its commitment, and a signer for each member.
    fn test_ring(size: usize) -> (Vec<u8>, Vec<BanderSecret>, Vec<BanderAffine>) {
        let srs = std::fs::read("../../src/main/resources/zcash-srs-2-11-uncompressed.bin").expect("SRS file");
        initialize_ring_context(&srs, size as jint).unwrap();
        let secrets: Vec<_> = (0..size).map(|i| BanderSecret::from_seed(&[i as u8, (i >> 8) as u8, 9])).collect();
        let ring: Vec<_> = secrets.iter().map(|s| s.public()).collect();
        let points = ring.iter().map(|p| p.0).collect();
        let mut commitment = Vec::new();
        Verifier::new(ring, size as jint).unwrap().commitment.serialize_compressed(&mut commitment).unwrap();
        (commitment, secrets, points)
    }

    fn ticket_signature(size: usize, secrets: &[BanderSecret], points: &[BanderAffine], who: usize, entropy: &[u8], attempt: u8) -> Vec<u8> {
        use ark_ec_vrfs::ring::Prover as _;
        let ctx = ring_context(size as jint).unwrap();
        let prover = ctx.prover(ctx.prover_key(points), who);
        let input = vrf_input_point(&[b"jam_ticket_seal", entropy, &[attempt]].concat());
        let output = secrets[who].output(input);
        let proof = secrets[who].prove(input, output, &[], &prover);
        let mut bytes = Vec::new();
        RingVrfSignature { output, proof }.serialize_compressed(&mut bytes).unwrap();
        bytes
    }

    #[test]
    fn ring_batch_agrees_with_one_by_one_verification() {
        let size = 6;
        let (commitment, secrets, points) = test_ring(size);
        let entropy = [42u8; 32];
        let attempts: Vec<u8> = vec![0, 1, 2, 0, 1];
        let signatures: Vec<Vec<u8>> =
            attempts.iter().enumerate().map(|(i, &a)| ticket_signature(size, &secrets, &points, i, &entropy, a)).collect();
        let each = signatures[0].len();
        let single = |sig: &[u8], attempt: u8| Verifier::ring_vrf_verify(&entropy, attempt, sig, &commitment, size as jint).ok();
        let batch = |sigs: &[Vec<u8>], attempts: &[u8]| {
            Verifier::ring_vrf_verify_batch(&entropy, attempts, &sigs.concat(), &commitment, size as jint).ok()
        };

        for n in 1..=attempts.len() {
            let expected: Vec<u8> = (0..n).flat_map(|i| single(&signatures[i], attempts[i]).expect("valid")).collect();
            assert_eq!(batch(&signatures[..n], &attempts[..n]), Some(expected), "{n} tickets");
        }

        // One bad ticket anywhere fails the batch, as it fails that ticket alone.
        for bad in 0..attempts.len() {
            // A wrong attempt, a flipped proof byte, a flipped output byte, a signature from another position.
            let mut wrong_attempt = attempts.clone();
            wrong_attempt[bad] ^= 1;
            assert!(single(&signatures[bad], wrong_attempt[bad]).is_none());
            assert_eq!(batch(&signatures, &wrong_attempt), None, "attempt of ticket {bad}");
            for at in [0, 31, 40, each / 2, each - 1] {
                let mut tampered = signatures.clone();
                tampered[bad][at] ^= 4;
                assert!(single(&tampered[bad], attempts[bad]).is_none(), "byte {at} of ticket {bad} alone");
                assert_eq!(batch(&tampered, &attempts), None, "byte {at} of ticket {bad}");
            }
        }
        // Another entropy, a truncated buffer, no tickets.
        assert!(Verifier::ring_vrf_verify_batch(&[1u8; 32], &attempts, &signatures.concat(), &commitment, size as jint).is_err());
        let all = signatures.concat();
        assert!(Verifier::ring_vrf_verify_batch(&entropy, &attempts, &all[..all.len() - 1], &commitment, size as jint).is_err());
        assert!(Verifier::ring_vrf_verify_batch(&entropy, &[], &[], &commitment, size as jint).is_err());
    }

    #[test]
    fn decode_ring_signature_agrees_with_the_checked_decoder() {
        use rand_chacha::rand_core::{RngCore, SeedableRng};
        let size = 6;
        let (_, secrets, points) = test_ring(size);
        let good = ticket_signature(size, &secrets, &points, 2, &[3u8; 32], 1);
        let same = |bytes: &[u8]| {
            let expected = RingVrfSignature::deserialize_compressed(bytes).ok().map(|sig| {
                let mut out = Vec::new();
                sig.serialize_compressed(&mut out).unwrap();
                out
            });
            let got = decode_ring_signature(bytes).map(|sig| {
                let mut out = Vec::new();
                sig.serialize_compressed(&mut out).unwrap();
                out
            });
            assert_eq!(got, expected, "signature {}", hex::encode(bytes));
            expected.is_some()
        };
        assert!(same(&good));
        assert!(!same(&good[..good.len() - 1]));
        assert!(!same(&[]));
        // A flipped bit in every byte: each point comes up off its curve, outside its subgroup, or as another
        // valid point, and each scalar out of range or changed.
        let mut rng = rand_chacha::ChaCha20Rng::from_seed([2u8; 32]);
        let (mut accepted, mut rejected) = (0, 0);
        for at in 0..good.len() {
            let mut bad = good.clone();
            bad[at] ^= 1 << (rng.next_u32() % 8);
            if same(&bad) { accepted += 1 } else { rejected += 1 }
        }
        assert!(accepted > 50 && rejected > 50, "{accepted} decoded, {rejected} did not");
    }
}
