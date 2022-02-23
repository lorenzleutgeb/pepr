/// Macro to mark unreachable code. When debugging, reaching
/// it will panic. When not debugging, reaching it is UB.
#[macro_export]
macro_rules! debug_unreachable {
    () => {{
        if cfg!(debug_assertions) {
            core::unreachable!()
        } else {
            unsafe { std::hint::unreachable_unchecked() }
        }
    }};
}
