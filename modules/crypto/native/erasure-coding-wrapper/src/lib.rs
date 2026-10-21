use jni::objects::{JByteArray, JClass, JIntArray, JObjectArray};
use jni::sys::{jbyteArray, jint, jobjectArray};
use jni::JNIEnv;
use reed_solomon_simd::{ReedSolomonDecoder, ReedSolomonEncoder};

macro_rules! jni_guard {
    ($env:expr, $default:expr, $body:block) => {
        match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| $body)) {
            Ok(v) => v,
            Err(_) => {
                let _ = $env.throw_new(
                    "java/lang/RuntimeException",
                    "native panic (see stderr); converted to exception",
                );
                $default
            }
        }
    };
}

// The Gray Paper erasure-codes each 2-byte position of a shard as its own GF(2^16) word, which the Scala side used to
// do with one JNI call per position. reed-solomon-simd codes whole shards, but stores them as 64-byte blocks holding
// the low bytes of 32 words followed by their high bytes (a 2-byte shard is word 0: low byte first). Converting
// between the two layouts lets one encoder or decoder call cover every position at once.

/// Bytes reed-solomon-simd needs for a shard of `words` 2-byte words.
fn simd_len(words: usize) -> usize {
    words.div_ceil(32) * 64
}

/// Writes GP-layout `gp` (word p in bytes 2p, 2p+1; missing trailing bytes read as zero) into `out` in
/// reed-solomon-simd layout. `out` must be zeroed and `simd_len` long.
fn to_simd_layout(gp: &[u8], out: &mut [u8]) {
    for (p, word) in gp.chunks(2).enumerate() {
        let base = (p / 32) * 64 + p % 32;
        out[base] = word[0];
        if word.len() > 1 {
            out[base + 32] = word[1];
        }
    }
}

/// Inverse of `to_simd_layout` for the first `out.len() / 2` words.
fn from_simd_layout(simd: &[u8], out: &mut [u8]) {
    for (p, word) in out.chunks_exact_mut(2).enumerate() {
        let base = (p / 32) * 64 + p % 32;
        word[0] = simd[base];
        word[1] = simd[base + 32];
    }
}

/// C_k on whole shards: `data` (at most `original_count * 2k` bytes, zero-padded) is cut into `original_count`
/// shards of 2k bytes, then `recovery_count` recovery shards are appended. Matches coding each 2-byte position
/// separately.
fn chunk_shards(
    data: &[u8],
    original_count: usize,
    recovery_count: usize,
    k: usize,
) -> Option<Vec<Vec<u8>>> {
    let shard_bytes = 2 * k;
    if k == 0 || original_count == 0 || recovery_count == 0 || data.len() > original_count * shard_bytes {
        return None;
    }
    let originals: Vec<Vec<u8>> = (0..original_count)
        .map(|i| {
            let mut shard = vec![0u8; shard_bytes];
            let start = (i * shard_bytes).min(data.len());
            let end = ((i + 1) * shard_bytes).min(data.len());
            shard[..end - start].copy_from_slice(&data[start..end]);
            shard
        })
        .collect();

    let len = simd_len(k);
    let mut encoder = ReedSolomonEncoder::new(original_count, recovery_count, len).ok()?;
    let mut buf = vec![0u8; len];
    for shard in &originals {
        buf.fill(0);
        to_simd_layout(shard, &mut buf);
        encoder.add_original_shard(&buf).ok()?;
    }
    let result = encoder.encode().ok()?;

    let mut shards = originals;
    shards.reserve(recovery_count);
    for recovery in result.recovery_iter() {
        let mut shard = vec![0u8; shard_bytes];
        from_simd_layout(recovery, &mut shard);
        shards.push(shard);
    }
    Some(shards)
}

/// C_k of `segment_count` segments of one length, laid end to end in `data`, as one code over shards of
/// `segment_count * 2k` bytes: the coding is word-wise, so segment j's words at positions [j*k, (j+1)*k) of every
/// shard code exactly as that segment alone would. Returns the `original_count + recovery_count` shards end to end;
/// bytes [j*2k, (j+1)*2k) of shard i are segment j's shard i.
fn chunk_segments(
    data: &[u8],
    segment_count: usize,
    original_count: usize,
    recovery_count: usize,
    k: usize,
) -> Option<Vec<u8>> {
    let shard_bytes = 2 * k;
    if segment_count == 0 || k == 0 || original_count == 0 || recovery_count == 0 || data.len() % segment_count != 0
    {
        return None;
    }
    let segment_len = data.len() / segment_count;
    if segment_len == 0 || segment_len > original_count * shard_bytes {
        return None;
    }
    let row = segment_count * shard_bytes;
    let mut out = vec![0u8; (original_count + recovery_count) * row];
    for (j, segment) in data.chunks_exact(segment_len).enumerate() {
        for (i, piece) in segment.chunks(shard_bytes).enumerate() {
            let at = i * row + j * shard_bytes;
            out[at..at + piece.len()].copy_from_slice(piece);
        }
    }

    let len = simd_len(segment_count * k);
    let mut encoder = ReedSolomonEncoder::new(original_count, recovery_count, len).ok()?;
    let mut buf = vec![0u8; len];
    for shard in out[..original_count * row].chunks_exact(row) {
        buf.fill(0);
        to_simd_layout(shard, &mut buf);
        encoder.add_original_shard(&buf).ok()?;
    }
    let result = encoder.encode().ok()?;
    for (recovery, shard) in result.recovery_iter().zip(out[original_count * row..].chunks_exact_mut(row)) {
        from_simd_layout(recovery, shard);
    }
    Some(out)
}

/// R_k on whole shards: rebuilds the `original_count` original shards from `(index, shard)` pairs (indices at or
/// above `original_count` are recovery shards) and returns them joined. Odd-length shards are zero-padded to a whole
/// word, as the per-position split did. Provided originals are copied through.
fn reconstruct_shards(
    shards: &[(usize, Vec<u8>)],
    original_count: usize,
    recovery_count: usize,
) -> Option<Vec<u8>> {
    if original_count == 0 || recovery_count == 0 || shards.len() < original_count {
        return None;
    }
    let shard_size = shards[0].1.len();
    if shard_size == 0 || shards.iter().any(|(_, s)| s.len() != shard_size) {
        return None;
    }
    let k = shard_size.div_ceil(2);
    let shard_bytes = 2 * k;
    let len = simd_len(k);

    let mut out = vec![0u8; original_count * shard_bytes];
    let mut provided = vec![false; original_count];
    let mut decoder = ReedSolomonDecoder::new(original_count, recovery_count, len).ok()?;
    let mut buf = vec![0u8; len];
    for (index, shard) in shards {
        buf.fill(0);
        to_simd_layout(shard, &mut buf);
        if *index < original_count {
            decoder.add_original_shard(*index, &buf).ok()?;
            out[index * shard_bytes..index * shard_bytes + shard_size].copy_from_slice(shard);
            provided[*index] = true;
        } else {
            decoder.add_recovery_shard(index - original_count, &buf).ok()?;
        }
    }
    let result = decoder.decode().ok()?;
    for (i, done) in provided.iter().enumerate() {
        if !done {
            let restored = result.restored_original(i)?;
            from_simd_layout(restored, &mut out[i * shard_bytes..(i + 1) * shard_bytes]);
        }
    }
    Some(out)
}

/// Erasure-code a whole bundle in one call (GP C_k).
///
/// # Arguments
/// * `data` - The data, at most `original_count * 2 * k` bytes; the rest is zero padding
/// * `original_count` - Number of original shards (basic size / 2)
/// * `recovery_count` - Number of recovery shards to generate
/// * `k` - Number of 2-byte words per shard
///
/// # Returns
/// * 2D byte array of `original_count + recovery_count` shards of `2 * k` bytes, or null on failure
#[no_mangle]
pub extern "system" fn Java_io_forge_jam_crypto_ErasureCodingWrapper_chunk(
    mut env: JNIEnv,
    _class: JClass,
    data: JByteArray,
    original_count: jint,
    recovery_count: jint,
    k: jint,
) -> jobjectArray {
    jni_guard!(env, std::ptr::null_mut(), {
    let return_null = || -> jobjectArray { std::ptr::null_mut() };

    if original_count <= 0 || recovery_count <= 0 || k <= 0 {
        return return_null();
    }
    let data = match env.convert_byte_array(&data) {
        Ok(bytes) => bytes,
        Err(_) => return return_null(),
    };
    let shards = match chunk_shards(&data, original_count as usize, recovery_count as usize, k as usize) {
        Some(shards) => shards,
        None => return return_null(),
    };

    let byte_array_class = match env.find_class("[B") {
        Ok(cls) => cls,
        Err(_) => return return_null(),
    };
    let result_array =
        match env.new_object_array(shards.len() as i32, &byte_array_class, JByteArray::default()) {
            Ok(arr) => arr,
            Err(_) => return return_null(),
        };
    for (i, shard) in shards.iter().enumerate() {
        let java_shard = match env.byte_array_from_slice(shard) {
            Ok(arr) => arr,
            Err(_) => return return_null(),
        };
        if env.set_object_array_element(&result_array, i as i32, &java_shard).is_err() {
            return return_null();
        }
        // Free the local reference now: a bundle makes over a thousand of them.
        let _ = env.delete_local_ref(java_shard);
    }

    result_array.into_raw()
    })
}

/// Erasure-code many segments of one length in one call (GP C_k of each).
///
/// # Arguments
/// * `data` - The segments end to end, each at most `original_count * 2 * k` bytes
/// * `segment_count` - Number of segments in `data`
/// * `original_count` - Number of original shards (basic size / 2)
/// * `recovery_count` - Number of recovery shards to generate
/// * `k` - Number of 2-byte words per shard of one segment
///
/// # Returns
/// * `original_count + recovery_count` shards end to end, each `segment_count * 2 * k` bytes holding every segment's
///   shard in segment order, or null on failure
#[no_mangle]
pub extern "system" fn Java_io_forge_jam_crypto_ErasureCodingWrapper_chunkSegments(
    mut env: JNIEnv,
    _class: JClass,
    data: JByteArray,
    segment_count: jint,
    original_count: jint,
    recovery_count: jint,
    k: jint,
) -> jbyteArray {
    jni_guard!(env, std::ptr::null_mut(), {
    let return_null = || -> jbyteArray { std::ptr::null_mut() };

    if segment_count <= 0 || original_count <= 0 || recovery_count <= 0 || k <= 0 {
        return return_null();
    }
    let data = match env.convert_byte_array(&data) {
        Ok(bytes) => bytes,
        Err(_) => return return_null(),
    };
    let shards = match chunk_segments(
        &data,
        segment_count as usize,
        original_count as usize,
        recovery_count as usize,
        k as usize,
    ) {
        Some(shards) => shards,
        None => return return_null(),
    };
    match env.byte_array_from_slice(&shards) {
        Ok(arr) => arr.into_raw(),
        Err(_) => return_null(),
    }
    })
}

/// Reconstruct a whole bundle in one call (GP R_k).
///
/// # Arguments
/// * `shards` - 2D byte array of available shards, all the same length
/// * `indices` - Array of shard indices (index < originalCount means original shard)
/// * `original_count` - Total number of original shards
/// * `recovery_count` - Total number of recovery shards
///
/// # Returns
/// * The original shards joined (`original_count * 2 * ceil(len / 2)` bytes), or null on failure
#[no_mangle]
pub extern "system" fn Java_io_forge_jam_crypto_ErasureCodingWrapper_reconstruct(
    mut env: JNIEnv,
    _class: JClass,
    shards: JObjectArray,
    indices: JIntArray,
    original_count: jint,
    recovery_count: jint,
) -> jbyteArray {
    jni_guard!(env, std::ptr::null_mut(), {
    let return_null = || -> jbyteArray { std::ptr::null_mut() };

    if original_count <= 0 || recovery_count <= 0 {
        return return_null();
    }
    let shard_count = match env.get_array_length(&shards) {
        Ok(len) => len as usize,
        Err(_) => return return_null(),
    };
    match env.get_array_length(&indices) {
        Ok(len) if len as usize == shard_count => {}
        _ => return return_null(),
    }
    let mut indices_vec = vec![0i32; shard_count];
    if env.get_int_array_region(&indices, 0, &mut indices_vec).is_err() {
        return return_null();
    }

    let mut pairs: Vec<(usize, Vec<u8>)> = Vec::with_capacity(shard_count);
    for (i, &index) in indices_vec.iter().enumerate() {
        if index < 0 {
            return return_null();
        }
        let shard_obj = match env.get_object_array_element(&shards, i as i32) {
            Ok(obj) => obj,
            Err(_) => return return_null(),
        };
        let shard_array = JByteArray::from(shard_obj);
        let bytes = match env.convert_byte_array(&shard_array) {
            Ok(bytes) => bytes,
            Err(_) => return return_null(),
        };
        let _ = env.delete_local_ref(shard_array);
        pairs.push((index as usize, bytes));
    }

    let data = match reconstruct_shards(&pairs, original_count as usize, recovery_count as usize) {
        Some(data) => data,
        None => return return_null(),
    };
    match env.byte_array_from_slice(&data) {
        Ok(arr) => arr.into_raw(),
        Err(_) => return_null(),
    }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Deterministic xorshift bytes, so the tests need no extra crates.
    fn bytes(len: usize, seed: u64) -> Vec<u8> {
        let mut x = seed | 1;
        (0..len)
            .map(|_| {
                x ^= x << 13;
                x ^= x >> 7;
                x ^= x << 17;
                x as u8
            })
            .collect()
    }

    /// The old Scala path: one reed-solomon-simd call per 2-byte position.
    fn chunk_per_position(data: &[u8], original_count: usize, recovery_count: usize, k: usize) -> Vec<Vec<u8>> {
        let mut padded = data.to_vec();
        padded.resize(original_count * 2 * k, 0);
        let mut shards = vec![Vec::with_capacity(2 * k); original_count + recovery_count];
        for p in 0..k {
            let words: Vec<&[u8]> =
                (0..original_count).map(|i| &padded[i * 2 * k + 2 * p..i * 2 * k + 2 * p + 2]).collect();
            let recovery = reed_solomon_simd::encode(original_count, recovery_count, &words).unwrap();
            for (i, w) in words.iter().enumerate() {
                shards[i].extend_from_slice(w);
            }
            for (j, w) in recovery.iter().enumerate() {
                shards[original_count + j].extend_from_slice(w);
            }
        }
        shards
    }

    #[test]
    fn test_chunk_matches_per_position_coding() {
        // Word counts on both sides of the 32-word block boundary, plus GP's full and tiny shapes.
        for &(original_count, recovery_count, k) in
            &[(342, 681, 1), (342, 681, 31), (342, 681, 33), (342, 681, 70), (3, 3, 64), (2, 4, 5)]
        {
            for &len in &[1usize, original_count * 2 * k - 1, original_count * 2 * k] {
                let data = bytes(len, (len * 31 + k) as u64);
                let batched = chunk_shards(&data, original_count, recovery_count, k).unwrap();
                assert_eq!(batched, chunk_per_position(&data, original_count, recovery_count, k));
            }
        }
    }

    #[test]
    fn test_chunk_segments_matches_chunk_per_segment() {
        for &(original_count, recovery_count, k, segment_len, segment_count) in
            &[(342, 681, 6, 4104, 7), (342, 681, 6, 4000, 3), (342, 681, 1, 1, 2), (3, 3, 2, 12, 40), (2, 4, 5, 17, 1)]
        {
            let data = bytes(segment_len * segment_count, (segment_len + segment_count) as u64);
            let flat = chunk_segments(&data, segment_count, original_count, recovery_count, k).unwrap();
            let row = segment_count * 2 * k;
            assert_eq!(flat.len(), (original_count + recovery_count) * row);
            for (j, segment) in data.chunks_exact(segment_len).enumerate() {
                let alone = chunk_shards(segment, original_count, recovery_count, k).unwrap();
                for (i, shard) in alone.iter().enumerate() {
                    assert_eq!(&flat[i * row + j * 2 * k..i * row + (j + 1) * 2 * k], shard.as_slice());
                }
            }
        }
        // Segments longer than k words per shard, or not dividing evenly, are refused.
        assert!(chunk_segments(&[0u8; 30], 2, 3, 3, 2).is_none());
        assert!(chunk_segments(&[0u8; 25], 2, 3, 3, 3).is_none());
    }

    #[test]
    fn test_reconstruct_from_recovery_and_mixed_shards() {
        let (original_count, recovery_count, k) = (342, 681, 45);
        let data = bytes(original_count * 2 * k - 7, 11);
        let shards = chunk_shards(&data, original_count, recovery_count, k).unwrap();
        let mut expected = data.clone();
        expected.resize(original_count * 2 * k, 0);

        // Recovery shards only.
        let recovery: Vec<(usize, Vec<u8>)> =
            (original_count..original_count + recovery_count).map(|i| (i, shards[i].clone())).collect();
        assert_eq!(reconstruct_shards(&recovery, original_count, recovery_count).unwrap(), expected);

        // Every other shard: some originals copied through, the rest restored.
        let mixed: Vec<(usize, Vec<u8>)> =
            (0..original_count + recovery_count).step_by(2).map(|i| (i, shards[i].clone())).collect();
        assert_eq!(reconstruct_shards(&mixed, original_count, recovery_count).unwrap(), expected);

        // Too few shards.
        assert!(reconstruct_shards(&mixed[..original_count - 1], original_count, recovery_count).is_none());
    }

    #[test]
    fn test_reconstruct_pads_odd_shards_like_per_position_split() {
        let (original_count, recovery_count) = (2, 4);
        let shards = chunk_shards(&bytes(20, 5), original_count, recovery_count, 5).unwrap();
        // Drop the last byte of each recovery shard; the missing byte reads as zero.
        let odd: Vec<(usize, Vec<u8>)> = (2..6).map(|i| (i, shards[i][..9].to_vec())).collect();
        let mut zeroed: Vec<(usize, Vec<u8>)> = odd.clone();
        for (_, s) in zeroed.iter_mut() {
            s.push(0);
        }
        assert_eq!(
            reconstruct_shards(&odd, original_count, recovery_count),
            reconstruct_shards(&zeroed, original_count, recovery_count)
        );
    }

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
