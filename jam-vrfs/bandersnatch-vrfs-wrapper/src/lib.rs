use ark_ec_vrfs::suites::bandersnatch::edwards as bandersnatch;
use ark_ec_vrfs::{prelude::ark_serialize, suites::bandersnatch::edwards::RingContext};
use ark_serialize::{CanonicalDeserialize, CanonicalSerialize};
use bandersnatch::{Input, Output, Public, RingProof, Secret};
use jni::objects::{JByteArray, JClass};
use jni::sys::{jbyte, jbyteArray, jint, jlong};
use jni::JNIEnv;
use std::collections::HashMap;
use std::sync::OnceLock;
use std::sync::{Arc, Mutex};

static RING_CONTEXTS: OnceLock<Mutex<HashMap<usize, RingContext>>> = OnceLock::new();
const ERROR_RESULT: [u8; 32] = [0; 32];

type RingCommitment = ark_ec_vrfs::ring::RingCommitment<bandersnatch::BandersnatchSha512Ell2>;

#[derive(Debug)]
enum VrfError {
    DeserializationError(&'static str),
    VerificationError,
    ConversionError,
}

impl std::fmt::Display for VrfError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VrfError::DeserializationError(msg) => write!(f, "Deserialization error: {}", msg),
            VrfError::VerificationError => write!(f, "VRF verification failed"),
            VrfError::ConversionError => write!(f, "Data conversion error"),
        }
    }
}

#[derive(CanonicalSerialize, CanonicalDeserialize)]
struct RingVrfSignature {
    output: Output,
    // This contains both the Pedersen proof and actual ring proof.
    proof: RingProof,
}

fn vrf_input_point(vrf_input_data: &[u8]) -> Input {
    Input::new(vrf_input_data).unwrap()
}

fn initialize_ring_context(srs_data: &[u8], ring_size: jint) -> Result<(), String> {
    use bandersnatch::PcsParams;
    let ring_size = ring_size as usize;
    let contexts = RING_CONTEXTS.get_or_init(|| Mutex::new(HashMap::new()));
    let mut contexts = contexts.lock().unwrap();

    if !contexts.contains_key(&ring_size) {
        let pcs_params = PcsParams::deserialize_uncompressed_unchecked(&mut &srs_data[..])
            .map_err(|e| format!("Failed to deserialize PCS params: {}", e))?;

        let ring_ctx = RingContext::from_srs(ring_size, pcs_params)
            .map_err(|e| format!("Failed to create ring context: {:?}", e))?;

        contexts.insert(ring_size, ring_ctx);
    }

    Ok(())
}

// "Static" ring context data
fn ring_context(ring_size: jint) -> Arc<RingContext> {
    let ring_size = ring_size as usize;
    let contexts = RING_CONTEXTS.get()
        .expect("Ring contexts not initialized")
        .lock()
        .unwrap();

    contexts.get(&ring_size)
        .expect("Ring context not found for given size")
        .clone().into()
}

#[no_mangle]
pub extern "system" fn Java_io_forge_jam_vrfs_BandersnatchWrapper_initializeContext(
    env: JNIEnv,
    _class: JClass,
    srs_data: JByteArray,
    ring_size: jint,
) -> jbyteArray {
    let srs_bytes = match env.convert_byte_array(srs_data) {
        Ok(data) => data,
        Err(e) => return throw_exception(env, &format!("Failed to convert SRS data: {}", e)),
    };

    if let Err(e) = initialize_ring_context(&srs_bytes, ring_size) {
        return throw_exception(env, &format!("Failed to initialize context: {}", e));
    }

    // Return empty array on success
    match env.byte_array_from_slice(&[]) {
        Ok(array) => array.into_raw(),
        Err(e) => throw_exception(env, &format!("Failed to create return array: {}", e)),
    }
}

// Prover actor.
struct Prover {
    pub prover_idx: usize,
    pub secret: Secret,
    pub ring: Vec<Public>,
}

impl Prover {
    pub fn new(ring: Vec<Public>, prover_idx: usize) -> Self {
        Self {
            prover_idx,
            secret: Secret::from_seed(&prover_idx.to_le_bytes()),
            ring,
        }
    }

    // pub fn ring_vrf_sign(&self, vrf_input_data: &[u8], aux_data: &[u8]) -> Vec<u8> {
    //     use ark_ec_vrfs::ring::Prover as _;
    //
    //     let input = vrf_input_point(vrf_input_data);
    //     let output = self.secret.output(input);
    //
    //     // Backend currently requires the wrapped type (plain affine points)
    //     let pts: Vec<_> = self.ring.iter().map(|pk| pk.0).collect();
    //
    //     // Proof construction
    //     let ring_ctx = ring_context();
    //     let prover_key = ring_ctx.prover_key(&pts);
    //     let prover = ring_ctx.prover(prover_key, self.prover_idx);
    //     let proof = self.secret.prove(input, output, aux_data, &prover);
    //
    //     // Output and Ring Proof bundled together (as per section 2.2)
    //     let signature = RingVrfSignature { output, proof };
    //     let mut buf = Vec::new();
    //     signature.serialize_compressed(&mut buf).unwrap();
    //     buf
    // }
}

// Verifier actor.
struct Verifier {
    pub commitment: RingCommitment,
}

impl Verifier {
    fn new(ring: Vec<Public>, ring_size: jint) -> Self {
        // Backend currently requires the wrapped type (plain affine points)
        let pts: Vec<_> = ring.iter().map(|pk| pk.0).collect();
        let verifier_key = ring_context(ring_size).verifier_key(&pts);
        let commitment = verifier_key.commitment();
        Self { commitment }
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

        let commitment = RingCommitment::deserialize_compressed(commitment)
            .map_err(|_| VrfError::DeserializationError("invalid commitment format"))?;

        let signature = RingVrfSignature::deserialize_compressed(signature)
            .map_err(|_| VrfError::DeserializationError("invalid signature format"))?;
        let input_data = [b"jam_ticket_seal", &entropy[..], &[attempt]].concat();

        let input = vrf_input_point(&input_data);
        let output = signature.output;

        let ring_ctx = ring_context(ring_size);

        let verifier_key = ring_ctx.verifier_key_from_commitment(commitment);
        let verifier = ring_ctx.verifier(verifier_key);
        if Public::verify(input, output, &[], &signature.proof, &verifier).is_err() {
            return Err(VrfError::VerificationError);
        }
        output.hash()[..32]
            .try_into()
            .map_err(|_| VrfError::ConversionError)
    }
}

#[no_mangle]
pub extern "system" fn Java_io_forge_jam_vrfs_BandersnatchWrapper_createProver(
    _env: JNIEnv,
    _class: JClass,
    ring_size: jint,
    prover_key_index: jint,
) -> jlong {
    let ring_size = ring_size as usize;
    let prover_key_index = prover_key_index as usize;

    // Initialize the ring
    let ring: Vec<_> = (0..ring_size)
        .map(|i| Secret::from_seed(&i.to_le_bytes()).public())
        .collect();

    // Create the Prover
    let prover = Prover::new(ring, prover_key_index);

    // Return a raw pointer to the Prover
    Box::into_raw(Box::new(prover)) as jlong
}

#[no_mangle]
pub extern "system" fn Java_io_forge_jam_vrfs_BandersnatchWrapper_destroyProver(
    _env: JNIEnv,
    _class: JClass,
    prover_ptr: jlong,
) {
    if !prover_ptr != 0 {
        unsafe {
            let _ = Box::from_raw(prover_ptr as *mut Prover);
        }
    }
}

#[no_mangle]
pub extern "system" fn Java_io_forge_jam_vrfs_BandersnatchWrapper_getVerifierCommitment(
    mut env: JNIEnv,
    _class: JClass,
    ring_size: jint,
    keys: JByteArray,
) -> jbyteArray {
    // Helper function to throw exception and return null
    fn throw_and_return_null(env: &mut JNIEnv, message: &str) -> jbyteArray {
        let _ = env.throw_new("java/lang/RuntimeException", message);
        std::ptr::null_mut()
    }

    // Step 1: Convert the JByteArray to a Rust Vec<u8>
    let key_bytes = match env.convert_byte_array(keys) {
        Ok(bytes) => bytes,
        Err(e) => {
            return throw_and_return_null(&mut env, &format!("Failed to convert byte array: {:?}", e));
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

    // Step 5: Deserialize each public key
    let mut ring = Vec::with_capacity(ring_size as usize);
    for key in pub_keys.iter() {
        match Public::deserialize_compressed(&mut &key[..]) {
            Ok(public_key) => ring.push(public_key),
            Err(e) => {
                return throw_and_return_null(
                    &mut env,
                    &format!("Failed to deserialize public key: {}", e),
                );
            }
        }
    }

    // Step 6: Create the Verifier
    let verifier = Verifier::new(ring, ring_size);

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
}

#[no_mangle]
pub extern "system" fn Java_io_forge_jam_vrfs_BandersnatchWrapper_destroyVerifier(
    _env: JNIEnv,
    _class: JClass,
    verifier_ptr: jlong,
) {
    if verifier_ptr != 0 {
        unsafe {
            let _ = Box::from_raw(verifier_ptr as *mut Verifier);
        }
    }
}

fn throw_exception(mut env: JNIEnv, message: &str) -> jbyteArray {
    let _ = env.throw_new("java/lang/RuntimeException", message);
    std::ptr::null_mut()
}

// #[no_mangle]
// pub extern "system" fn Java_io_forge_jam_vrfs_BandersnatchWrapper_proverRingVrfSign(
//     env: JNIEnv,
//     _class: JClass,
//     prover_ptr: jlong,
//     vrf_input_data: JByteArray,
//     aux_data: JByteArray,
// ) -> jbyteArray {
//     if prover_ptr == 0 {
//         return throw_exception(env, "Null prover pointer");
//     }
//
//     unsafe {
//         let prover = &*(prover_ptr as *mut Prover);
//
//         let vrf_input_data = match env.convert_byte_array(vrf_input_data) {
//             Ok(data) => data,
//             Err(e) => return throw_exception(env, &format!("Failed to convert input data: {}", e)),
//         };
//
//         let aux_data = match env.convert_byte_array(aux_data) {
//             Ok(data) => data,
//             Err(e) => return throw_exception(env, &format!("Failed to convert aux data: {}", e)),
//         };
//
//         let signature = match std::panic::catch_unwind(|| {
//             prover.ring_vrf_sign(&vrf_input_data, &aux_data)
//         }) {
//             Ok(sig) => sig,
//             Err(_) => return throw_exception(env, "Panic during signature generation"),
//         };
//
//         match env.byte_array_from_slice(&signature) {
//             Ok(array) => array.into_raw(),
//             Err(e) => throw_exception(env, &format!("Failed to create output array: {}", e)),
//         }
//     }
// }

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
    let return_error = |env: &mut JNIEnv, error_msg: &str| -> jbyteArray {
        let _ = env.throw_new("java/lang/RuntimeException", error_msg);
        match env.byte_array_from_slice(&ERROR_RESULT) {
            Ok(array) => array.into_raw(),
            Err(_) => std::ptr::null_mut()
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
    match Verifier::ring_vrf_verify(&entropy_data, attempt_value, &signature_data, &commitment_data, ring_size) {
        Ok(output_hash) => match env.byte_array_from_slice(&output_hash) {
            Ok(array) => array.into_raw(),
            Err(_) => return_error(&mut env, "Failed to create output array")
        },
        Err(e) => return_error(&mut env, &e.to_string())
    }
}

#[no_mangle]
pub extern "system" fn Java_io_forge_jam_vrfs_BandersnatchWrapper_rustFree(
    _env: JNIEnv,
    _class: JClass,
    ptr: jlong,
    len: jlong,
) {
    unsafe {
        if ptr != 0 {
            let _ = Vec::from_raw_parts(ptr as *mut u8, len as usize, len as usize);
        }
    }
}
