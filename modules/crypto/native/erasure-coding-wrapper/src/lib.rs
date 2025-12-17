use jni::objects::{JByteArray, JClass, JIntArray, JObjectArray};
use jni::sys::{jint, jobjectArray};
use jni::JNIEnv;

/// Encode original shards into recovery shards using Reed-Solomon erasure coding.
///
/// # Arguments
/// * `original` - 2D byte array of original shards (each shard must have even length)
/// * `recovery_count` - Number of recovery shards to generate
///
/// # Returns
/// * 2D byte array of recovery shards, or null on failure
#[no_mangle]
pub extern "system" fn Java_io_forge_jam_crypto_ErasureCodingWrapper_encode(
    mut env: JNIEnv,
    _class: JClass,
    original: JObjectArray,
    recovery_count: jint,
) -> jobjectArray {
    let return_null = || -> jobjectArray { std::ptr::null_mut() };

    // Get original shard count
    let original_count = match env.get_array_length(&original) {
        Ok(len) => len as usize,
        Err(_) => return return_null(),
    };

    if original_count == 0 || recovery_count <= 0 {
        return return_null();
    }

    let recovery_count = recovery_count as usize;

    // Extract original shards
    let mut original_shards: Vec<Vec<u8>> = Vec::with_capacity(original_count);
    let mut shard_size = 0usize;

    for i in 0..original_count {
        let shard_obj = match env.get_object_array_element(&original, i as i32) {
            Ok(obj) => obj,
            Err(_) => return return_null(),
        };

        let shard_array = JByteArray::from(shard_obj);
        let shard_bytes = match env.convert_byte_array(&shard_array) {
            Ok(bytes) => bytes,
            Err(_) => return return_null(),
        };

        if i == 0 {
            shard_size = shard_bytes.len();
            if shard_size == 0 || shard_size % 2 != 0 {
                return return_null();
            }
        } else if shard_bytes.len() != shard_size {
            return return_null();
        }

        original_shards.push(shard_bytes);
    }

    // Convert to slices for reed_solomon_simd
    let original_slices: Vec<&[u8]> = original_shards.iter().map(|v| v.as_slice()).collect();

    // Encode
    let recovery_vecs =
        match reed_solomon_simd::encode(original_count, recovery_count, original_slices) {
            Ok(vecs) => vecs,
            Err(_) => return return_null(),
        };

    // Create Java 2D byte array for results (original + recovery shards)
    let total_shards = original_count + recovery_count;
    let byte_array_class = match env.find_class("[B") {
        Ok(cls) => cls,
        Err(_) => return return_null(),
    };

    let result_array = match env.new_object_array(
        total_shards as i32,
        &byte_array_class,
        JByteArray::default(),
    ) {
        Ok(arr) => arr,
        Err(_) => return return_null(),
    };

    // First add original shards
    for (i, original_shard) in original_shards.iter().enumerate() {
        let java_shard = match env.byte_array_from_slice(original_shard) {
            Ok(arr) => arr,
            Err(_) => return return_null(),
        };

        if env
            .set_object_array_element(&result_array, i as i32, java_shard)
            .is_err()
        {
            return return_null();
        }
    }

    // Then add recovery shards
    for (i, recovery_shard) in recovery_vecs.iter().enumerate() {
        let java_shard = match env.byte_array_from_slice(recovery_shard) {
            Ok(arr) => arr,
            Err(_) => return return_null(),
        };

        if env
            .set_object_array_element(&result_array, (original_count + i) as i32, java_shard)
            .is_err()
        {
            return return_null();
        }
    }

    result_array.into_raw()
}

/// Recover original shards from a subset of recovery/original shards.
///
/// # Arguments
/// * `shards` - 2D byte array of available shards
/// * `indices` - Array of shard indices (which shard each one is)
/// * `original_count` - Total number of original shards
/// * `recovery_count` - Total number of recovery shards
///
/// # Returns
/// * 2D byte array of recovered original shards, or null on failure
#[no_mangle]
pub extern "system" fn Java_io_forge_jam_crypto_ErasureCodingWrapper_recover(
    mut env: JNIEnv,
    _class: JClass,
    shards: JObjectArray,
    indices: JIntArray,
    original_count: jint,
    recovery_count: jint,
) -> jobjectArray {
    let return_null = || -> jobjectArray { std::ptr::null_mut() };

    if original_count <= 0 || recovery_count <= 0 {
        return return_null();
    }

    let original_count = original_count as usize;
    let recovery_count = recovery_count as usize;

    // Get shard count
    let shard_count = match env.get_array_length(&shards) {
        Ok(len) => len as usize,
        Err(_) => return return_null(),
    };

    // Need at least original_count shards to recover
    if shard_count < original_count {
        return return_null();
    }

    // Get indices
    let indices_len = match env.get_array_length(&indices) {
        Ok(len) => len as usize,
        Err(_) => return return_null(),
    };

    if indices_len != shard_count {
        return return_null();
    }

    let mut indices_vec = vec![0i32; shard_count];
    if env
        .get_int_array_region(&indices, 0, &mut indices_vec)
        .is_err()
    {
        return return_null();
    }

    // Extract shards
    let mut shard_data: Vec<Vec<u8>> = Vec::with_capacity(shard_count);
    let mut shard_size = 0usize;

    for i in 0..shard_count {
        let shard_obj = match env.get_object_array_element(&shards, i as i32) {
            Ok(obj) => obj,
            Err(_) => return return_null(),
        };

        let shard_array = JByteArray::from(shard_obj);
        let shard_bytes = match env.convert_byte_array(&shard_array) {
            Ok(bytes) => bytes,
            Err(_) => return return_null(),
        };

        if i == 0 {
            shard_size = shard_bytes.len();
            if shard_size == 0 || shard_size % 2 != 0 {
                return return_null();
            }
        } else if shard_bytes.len() != shard_size {
            return return_null();
        }

        shard_data.push(shard_bytes);
    }

    // Split shards into original and recovery based on index
    // Indices < original_count are original shards
    // Indices >= original_count are recovery shards (need to subtract original_count)
    let mut original_shards_for_decode: Vec<(usize, Vec<u8>)> = Vec::new();
    let mut recovery_shards: Vec<(usize, Vec<u8>)> = Vec::new();

    for (i, &idx) in indices_vec.iter().enumerate() {
        let idx = idx as usize;
        if idx < original_count {
            // Original shard - index is in [0, original_count)
            original_shards_for_decode.push((idx, shard_data[i].clone()));
        } else {
            // Recovery shard - need to map index from [original_count, total) to [0, recovery_count)
            let recovery_idx = idx - original_count;
            recovery_shards.push((recovery_idx, shard_data[i].clone()));
        }
    }

    // Decode using reed_solomon_simd
    let restored = match reed_solomon_simd::decode(
        original_count,
        recovery_count,
        original_shards_for_decode,
        recovery_shards,
    ) {
        Ok(map) => map,
        Err(_) => return return_null(),
    };

    // Create Java 2D byte array for results
    let byte_array_class = match env.find_class("[B") {
        Ok(cls) => cls,
        Err(_) => return return_null(),
    };

    let result_array = match env.new_object_array(
        original_count as i32,
        &byte_array_class,
        JByteArray::default(),
    ) {
        Ok(arr) => arr,
        Err(_) => return return_null(),
    };

    for i in 0..original_count {
        let data = match restored.get(&i) {
            Some(d) => d,
            None => return return_null(),
        };

        let java_shard = match env.byte_array_from_slice(data) {
            Ok(arr) => arr,
            Err(_) => return return_null(),
        };

        if env
            .set_object_array_element(&result_array, i as i32, java_shard)
            .is_err()
        {
            return return_null();
        }
    }

    result_array.into_raw()
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_encode_decode_roundtrip() {
        let original_count = 2;
        let recovery_count = 5;

        let original_data = vec![vec![0u8, 15u8], vec![30u8, 45u8]];
        let original_slices: Vec<&[u8]> = original_data.iter().map(|v| v.as_slice()).collect();

        // Encode
        let recovery =
            reed_solomon_simd::encode(original_count, recovery_count, original_slices).unwrap();

        // Use only recovery shards to restore
        let mut recovery_shards = Vec::new();
        for i in (recovery_count - original_count)..recovery_count {
            recovery_shards.push((i, recovery[i].clone()));
        }

        // Decode
        let restored = reed_solomon_simd::decode(
            original_count,
            recovery_count,
            Vec::<(usize, Vec<u8>)>::new(),
            recovery_shards,
        )
        .unwrap();

        // Verify
        for i in 0..original_count {
            assert_eq!(restored.get(&i).unwrap(), &original_data[i]);
        }
    }
}
