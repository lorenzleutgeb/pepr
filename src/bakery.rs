#[cfg(test)]
mod tests {
    use std::mem::size_of;

    use super::*;

    use crate::{
        parser::{FTCNFParser, Rule},
        State,
    };

    #[test]
    fn bakery() {
        let mut state: State = State::parse_str(include_str!("./bakery.ftcnf")).unwrap();
        state.tautology_check();
    }
}
