#[cfg(test)]
mod tests {

    use crate::saturation::Result;
    use crate::State;

    #[test]
    fn bakery() {
        let mut state: State = State::parse_str(include_str!("./bakery.ftcnf")).unwrap();
        state.saturate();
    }

    #[test]
    fn e1() {
        let mut state: State = State::parse_str(include_str!("./lc_e1.ftcnf")).unwrap();
        assert_eq!(Result::Unsatisfiable, state.saturate());
    }

    #[test]
    fn e2() {
        let mut state: State = State::parse_str(include_str!("./lc_e2.ftcnf")).unwrap();
        assert_eq!(Result::Unknown, state.saturate());
    }
}
