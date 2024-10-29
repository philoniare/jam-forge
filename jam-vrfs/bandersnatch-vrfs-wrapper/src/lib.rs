use ark_ec_vrfs::suites::bandersnatch::edwards as bandersnatch;
use ark_ec_vrfs::{prelude::ark_serialize, suites::bandersnatch::edwards::RingContext};
use ark_serialize::{CanonicalDeserialize, CanonicalSerialize};
use bandersnatch::{Input, Output, Public, RingProof, Secret};
use jni::objects::{JByteArray, JClass};
use jni::sys::{jbyteArray, jint, jlong};
use jni::JNIEnv;
use std::sync::OnceLock;

const RING_SIZE: usize = 6;
static RING_CTX: OnceLock<RingContext> = OnceLock::new();

type RingCommitment = ark_ec_vrfs::ring::RingCommitment<bandersnatch::BandersnatchSha512Ell2>;


#[derive(CanonicalSerialize, CanonicalDeserialize)]
struct RingVrfSignature {
    output: Output,
    // This contains both the Pedersen proof and actual ring proof.
    proof: RingProof,
}

fn vrf_input_point(vrf_input_data: &[u8]) -> Input {
    Input::new(vrf_input_data).unwrap()
}

fn initialize_ring_context(srs_data: &[u8]) -> Result<(), String> {
    if RING_CTX.get().is_some() {
        return Ok(());  // Already initialized
    }

    use bandersnatch::PcsParams;

    let pcs_params = PcsParams::deserialize_uncompressed_unchecked(&mut &srs_data[..])
        .map_err(|e| format!("Failed to deserialize PCS params: {}", e))?;

    let ring_ctx = match RingContext::from_srs(RING_SIZE, pcs_params) {
        Ok(ctx) => ctx,
        Err(e) => return Err(format!("Failed to create ring context: {:?}", e)),
    };

    RING_CTX.set(ring_ctx)
        .map_err(|_| "Ring context already initialized".to_string())?;

    Ok(())
}

// "Static" ring context data
fn ring_context() -> &'static RingContext {
    RING_CTX.get()
        .expect("Ring context not initialized. Call initialize_ring_context first.")
}

#[no_mangle]
pub extern "system" fn Java_io_forge_jam_vrfs_RustLibrary_initializeContext(
    env: JNIEnv,
    _class: JClass,
    srs_data: JByteArray,
) -> jbyteArray {
    let srs_bytes = match env.convert_byte_array(srs_data) {
        Ok(data) => data,
        Err(e) => return throw_exception(env, &format!("Failed to convert SRS data: {}", e)),
    };

    if let Err(e) = initialize_ring_context(&srs_bytes) {
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

    /// Anonymous VRF signature.
    ///
    /// Used for tickets submission.
    pub fn ring_vrf_sign(&self, vrf_input_data: &[u8], aux_data: &[u8]) -> Vec<u8> {
        use ark_ec_vrfs::ring::Prover as _;

        let input = vrf_input_point(vrf_input_data);
        let output = self.secret.output(input);

        // Backend currently requires the wrapped type (plain affine points)
        let pts: Vec<_> = self.ring.iter().map(|pk| pk.0).collect();

        // Proof construction
        let ring_ctx = ring_context();
        let prover_key = ring_ctx.prover_key(&pts);
        let prover = ring_ctx.prover(prover_key, self.prover_idx);
        let proof = self.secret.prove(input, output, aux_data, &prover);

        // Output and Ring Proof bundled together (as per section 2.2)
        let signature = RingVrfSignature { output, proof };
        let mut buf = Vec::new();
        signature.serialize_compressed(&mut buf).unwrap();
        buf
    }
}

// Verifier actor.
struct Verifier {
    pub commitment: RingCommitment,
}

impl Verifier {
    fn new(ring: Vec<Public>) -> Self {
        // Backend currently requires the wrapped type (plain affine points)
        let pts: Vec<_> = ring.iter().map(|pk| pk.0).collect();
        let verifier_key = ring_context().verifier_key(&pts);
        let commitment = verifier_key.commitment();
        Self { commitment }
    }

    /// Anonymous VRF signature verification.
    ///
    /// Used for tickets verification.
    ///
    /// On success returns the VRF output hash.
    pub fn ring_vrf_verify(
        &self,
        vrf_input_data: &[u8],
        aux_data: &[u8],
        signature: &[u8],
    ) -> Result<[u8; 32], ()> {
        use ark_ec_vrfs::ring::Verifier as _;

        let signature = RingVrfSignature::deserialize_compressed(signature).unwrap();

        let input = vrf_input_point(vrf_input_data);
        let output = signature.output;

        let ring_ctx = ring_context();

        // The verifier key is reconstructed from the commitment and the constant
        // verifier key component of the SRS in order to verify some proof.
        // As an alternative we can construct the verifier key using the
        // RingContext::verifier_key() method, but is more expensive.
        // In other words, we prefer computing the commitment once, when the keyset changes.
        let verifier_key = ring_ctx.verifier_key_from_commitment(self.commitment.clone());
        let verifier = ring_ctx.verifier(verifier_key);
        if Public::verify(input, output, aux_data, &signature.proof, &verifier).is_err() {
            return Err(());
        }
        // This truncated hash is the actual value used as ticket-id/score in JAM
        let vrf_output_hash: [u8; 32] = output.hash()[..32].try_into().unwrap();
        Ok(vrf_output_hash)
    }
}

#[no_mangle]
pub extern "system" fn Java_io_forge_jam_vrfs_RustLibrary_createProver(
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
pub extern "system" fn Java_io_forge_jam_vrfs_RustLibrary_destroyProver(
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
pub extern "system" fn Java_io_forge_jam_vrfs_RustLibrary_getVerifierCommitment(
    env: JNIEnv,
    _class: JClass,
    verifier_ptr: jlong,
) -> jbyteArray {
    if verifier_ptr == 0 {
        return throw_exception(env, "Null verifier pointer");
    }

    unsafe {
        let verifier = &*(verifier_ptr as *mut Verifier);

        let mut buf = Vec::new();
        if verifier.commitment.serialize_compressed(&mut buf).is_err() {
            return throw_exception(env, "Failed to serialize commitment");
        }

        match env.byte_array_from_slice(&buf) {
            Ok(array) => array.into_raw(),
            Err(e) => throw_exception(env, &format!("Failed to create output array: {}", e)),
        }
    }
}

#[no_mangle]
pub extern "system" fn Java_io_forge_jam_vrfs_RustLibrary_createVerifier(
    env: JNIEnv,
    _class: JClass,
    ring_size: jint,
    keys: JByteArray,
) -> jlong {
    // Step 1: Convert the JByteArray to a Rust Vec<u8>
    let key_bytes = match env.convert_byte_array(keys) {
        Ok(bytes) => bytes,
        Err(e) => {
            eprintln!("Failed to convert byte array: {:?}", e);
            return 0; // Return 0 or handle the error as appropriate
        }
    };

    // Step 2: Determine the size of each public key
    // This example assumes each public key is 32 bytes. Adjust as needed.
    const PUBLIC_KEY_SIZE: usize = 32;

    // Step 3: Validate the total length of the byte array
    let expected_length = (ring_size as usize) * PUBLIC_KEY_SIZE;
    if key_bytes.len() != expected_length {
        eprintln!(
            "Invalid key bytes length: expected {}, got {}",
            expected_length,
            key_bytes.len()
        );
        return 0; // Return 0 or handle the error as appropriate
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
                eprintln!("Failed to deserialize public key: {}", e);
                return 0; // Return 0 or handle the error as appropriate
            }
        }
    }

    // Step 6: Create the Verifier
    let verifier = Verifier::new(ring);

    // Step 7: Return a raw pointer to the Verifier as jlong
    Box::into_raw(Box::new(verifier)) as jlong
}

#[no_mangle]
pub extern "system" fn Java_io_forge_jam_vrfs_RustLibrary_destroyVerifier(
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

#[no_mangle]
pub extern "system" fn Java_io_forge_jam_vrfs_RustLibrary_proverRingVrfSign(
    env: JNIEnv,
    _class: JClass,
    prover_ptr: jlong,
    vrf_input_data: JByteArray,
    aux_data: JByteArray,
) -> jbyteArray {
    if prover_ptr == 0 {
        return throw_exception(env, "Null prover pointer");
    }

    unsafe {
        let prover = &*(prover_ptr as *mut Prover);

        let vrf_input_data = match env.convert_byte_array(vrf_input_data) {
            Ok(data) => data,
            Err(e) => return throw_exception(env, &format!("Failed to convert input data: {}", e)),
        };

        let aux_data = match env.convert_byte_array(aux_data) {
            Ok(data) => data,
            Err(e) => return throw_exception(env, &format!("Failed to convert aux data: {}", e)),
        };

        let signature = match std::panic::catch_unwind(|| {
            prover.ring_vrf_sign(&vrf_input_data, &aux_data)
        }) {
            Ok(sig) => sig,
            Err(_) => return throw_exception(env, "Panic during signature generation"),
        };

        match env.byte_array_from_slice(&signature) {
            Ok(array) => array.into_raw(),
            Err(e) => throw_exception(env, &format!("Failed to create output array: {}", e)),
        }
    }
}

#[no_mangle]
pub extern "system" fn Java_io_forge_jam_vrfs_RustLibrary_verifierRingVrfVerify(
    env: JNIEnv,
    _class: JClass,
    verifier_ptr: jlong,
    vrf_input_data: JByteArray,
    aux_data: JByteArray,
    signature: JByteArray,
) -> jbyteArray {
    if verifier_ptr == 0 {
        return throw_exception(env, "Null verifier pointer");
    }

    unsafe {
        let verifier = &*(verifier_ptr as *mut Verifier);

        let vrf_input_data = match env.convert_byte_array(vrf_input_data) {
            Ok(data) => data,
            Err(e) => return throw_exception(env, &format!("Failed to convert input data: {}", e)),
        };

        let aux_data = match env.convert_byte_array(aux_data) {
            Ok(data) => data,
            Err(e) => return throw_exception(env, &format!("Failed to convert aux data: {}", e)),
        };

        let signature_data = match env.convert_byte_array(signature) {
            Ok(data) => data,
            Err(e) => return throw_exception(env, &format!("Failed to convert signature: {}", e)),
        };

        match verifier.ring_vrf_verify(&vrf_input_data, &aux_data, &signature_data) {
            Ok(output_hash) => {
                match env.byte_array_from_slice(&output_hash) {
                    Ok(array) => array.into_raw(),
                    Err(e) => throw_exception(env, &format!("Failed to create output array: {}", e)),
                }
            }
            Err(_) => throw_exception(env, "Verification failed"),
        }
    }
}

#[no_mangle]
pub extern "system" fn Java_io_forge_jam_vrfs_RustLibrary_rustFree(
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
