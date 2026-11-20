//! Executable memory for emitted code.
pub struct ExecMem {
    ptr: *mut libc::c_void,
    len: usize,
}

// SAFETY: the pointer is owned solely by this struct; execution is single-threaded
// per block in the current design.
unsafe impl Send for ExecMem {}
unsafe impl Sync for ExecMem {}

#[cfg(all(target_os = "macos", target_arch = "aarch64"))]
extern "C" {
    fn pthread_jit_write_protect_np(enabled: libc::c_int);
    fn sys_icache_invalidate(start: *mut libc::c_void, len: libc::size_t);
}

impl ExecMem {
    /// Map an executable page, copy `code` into it, and make it runnable.
    pub fn from_code(code: &[u8]) -> Option<ExecMem> {
        if code.is_empty() {
            return None;
        }
        let page = 4096usize;
        let len = (code.len() + page - 1) & !(page - 1);

        #[cfg(all(target_os = "macos", target_arch = "aarch64"))]
        let (flags, prot) = (
            libc::MAP_PRIVATE | libc::MAP_ANON | libc::MAP_JIT,
            libc::PROT_READ | libc::PROT_WRITE | libc::PROT_EXEC,
        );
        #[cfg(not(all(target_os = "macos", target_arch = "aarch64")))]
        let (flags, prot) = (libc::MAP_PRIVATE | libc::MAP_ANON, libc::PROT_READ | libc::PROT_WRITE);

        let ptr = unsafe { libc::mmap(std::ptr::null_mut(), len, prot, flags, -1, 0) };
        if ptr == libc::MAP_FAILED {
            return None;
        }

        unsafe {
            #[cfg(all(target_os = "macos", target_arch = "aarch64"))]
            {
                // Enter write mode for this thread, copy, leave write mode, flush i-cache.
                pthread_jit_write_protect_np(0);
                std::ptr::copy_nonoverlapping(code.as_ptr(), ptr as *mut u8, code.len());
                pthread_jit_write_protect_np(1);
                sys_icache_invalidate(ptr, code.len());
            }
            #[cfg(not(all(target_os = "macos", target_arch = "aarch64")))]
            {
                std::ptr::copy_nonoverlapping(code.as_ptr(), ptr as *mut u8, code.len());
                if libc::mprotect(ptr, len, libc::PROT_READ | libc::PROT_EXEC) != 0 {
                    libc::munmap(ptr, len);
                    return None;
                }
                dynasmrt::cache_control::synchronize_icache(std::slice::from_raw_parts(ptr as *const u8, code.len()));
            }
        }

        Some(ExecMem { ptr, len })
    }

    pub fn as_ptr(&self) -> *const u8 {
        self.ptr as *const u8
    }

    pub fn len(&self) -> usize {
        self.len
    }
}

impl Drop for ExecMem {
    fn drop(&mut self) {
        unsafe {
            libc::munmap(self.ptr, self.len);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(target_os = "linux")]
    fn protection_of(addr: usize) -> Option<String> {
        let maps = std::fs::read_to_string("/proc/self/maps").ok()?;
        maps.lines().find_map(|line| {
            let mut fields = line.split_whitespace();
            let (lo, hi) = fields.next()?.split_once('-')?;
            let perms = fields.next()?;
            let (lo, hi) = (usize::from_str_radix(lo, 16).ok()?, usize::from_str_radix(hi, 16).ok()?);
            (lo <= addr && addr < hi).then(|| perms.to_string())
        })
    }

    #[test]
    #[cfg(target_os = "linux")]
    fn emitted_code_is_executable_and_not_writable() {
        let mem = ExecMem::from_code(&[0xc3; 16]).expect("mapped");
        assert_eq!(protection_of(mem.as_ptr() as usize).as_deref(), Some("r-xp"));
    }
}
