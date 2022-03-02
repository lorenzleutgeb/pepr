pub(crate) fn default_combine<T: Default, U: Default, V, F: Fn(T, U) -> V>(
    f: F,
    a: Option<T>,
    b: Option<U>,
) -> Option<V> {
    if a.is_none() && b.is_none() {
        None
    } else {
        Some(f(a.unwrap_or_default(), b.unwrap_or_default()))
    }
}
