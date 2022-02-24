#[cfg(test)]
mod tests {

    use crate::State;

    #[test]
    fn bakery() {
        let mut state: State = State::parse_str(include_str!("./bakery.ftcnf")).unwrap();
        state.tautology_check();
    }
}
