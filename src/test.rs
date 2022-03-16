#[cfg(test)]
mod tests {

    use crate::saturation::Result;
    use crate::State;

    #[test]
    fn bakery() {
        let mut state: State = State::parse_str(include_str!("./bakery.ftcnf")).unwrap();
        println!("{}", &state);
        assert_eq!(Result::Unsatisfiable, state.saturate());
    }

    #[test]
    fn fiddle2() {
        let mut state: State = State::parse_str("
        >(x0, x1), ≠(x2, x3) ∥ LaneSafe(x4, x3, x5), EgoCar(x4, x2, x6, x7), DistanceBehind(x4, x3, x8, x1, x5), SpeedBehind(x4, x3, x8, x0, x5) → SafeBehindDisproven(x4, x3, x2, x6, x7, x5). 
        → SpeedBehind(0, 2, 4, 5, aaccelerateleft).
        ").unwrap();

        println!("{}", &state);

        // Expected:
        // <(x0 5) || Neq(x1,2),LaneSafe(0,2,aaccelerateleft),EgoCar(0,x1,x2,x3),DistanceBehind(0,2,4,x0,aaccelerateleft) -> SafeBehindDisproven(0,2,x1,x2,x3,aaccelerateleft)
        assert_eq!(Result::Unsatisfiable, state.saturate());
    }

    #[test]
    fn fiddle() {
        let mut state: State = State::parse_str("CertifiedVariant(x0), UpdatedVariant(x1), LaneNotSafe(x0, x2, x3) → ExcludedAction(x1, x3).\n→ LaneNotSafe(0, 2, aaccelerateleft).").unwrap();
        println!("{}", &state);
        assert_eq!(Result::Unsatisfiable, state.saturate());
    }

    #[test]
    fn e1() {
        let mut state: State = State::parse_str(include_str!("./lc_e1.ftcnf")).unwrap();
        println!("{}", &state);
        assert_eq!(Result::Unsatisfiable, state.saturate());
    }

    #[test]
    fn e2() {
        let mut state: State = State::parse_str(include_str!("./lc_e2.ftcnf")).unwrap();
        assert_eq!(Result::Unknown, state.saturate());
    }

    #[test]
    fn uuf() {
        let mut state: State =
            State::parse_dimacs(include_str!("./UUF50.218.1000/uuf50-01.cnf")).unwrap();
        println!("{}", &state);
        assert_eq!(Result::Unknown, state.saturate());
    }
}
