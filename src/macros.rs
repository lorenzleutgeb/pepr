/// Macro to mark unreachable code. When debugging, reaching
/// it will panic. When not debugging, reaching it is UB.
#[macro_export]
macro_rules! debug_unreachable {
    () => {{
        if cfg!(debug_assertions) {
            core::unreachable!()
        } else {
            // SAFETY: This code was explicitly declared as
            // unreachable by the user of the macro, with
            // additional runtime checking via panics in
            // debug builds. It should therefore be safe
            // to declare unreachability.
            unsafe { std::hint::unreachable_unchecked() }
        }
    }};
}
