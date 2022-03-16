use crate::{
    symbols::*,
    unification::{self, unify_at},
    Clause, ClausePosition, Constraint, Parents,
};

fn resolve(clause: [&Clause; 2], index: [usize; 2]) -> Result<Clause, unification::Error> {
    match unify_at(clause, index) {
        Ok(sigma) => Ok(resolve_with_subst(clause, index, sigma)),
        Err(e) => Err(e),
    }
}

pub(crate) fn resolve_with_subst(
    clause: [&Clause; 2],
    index: [usize; 2],
    sigma: Vec<Option<Term>>,
) -> Clause {
    debug_assert!((index[0] < clause[0].arrow) ^ (index[1] < clause[1].arrow));
    debug_assert!(clause[0].atoms[index[0]].predicate == clause[1].atoms[index[1]].predicate);

    debug_assert!(!clause[0].is_empty());
    debug_assert!(!clause[1].is_empty());

    let mut catoms =
        Vec::with_capacity(clause[0].constraint.atoms.len() + clause[1].constraint.atoms.len());

    let offset = clause[0].typs.len();

    for catom in clause[0].constraint.atoms.iter() {
        catoms.push(catom.clone());
    }

    for catom in clause[1].constraint.atoms.iter() {
        catoms.push(catom.clone().substitute_with_offset(&sigma, offset));
    }

    let mut atoms = Vec::with_capacity(clause[0].atoms.len() + clause[1].atoms.len() - 2);
    let splits = clause.map(|c| c.split_at_arrow());

    for (j, atom) in splits[0].0.iter().enumerate() {
        if j != index[0] {
            atoms.push(atom.clone().substitute(&sigma));
        }
    }

    for (j, atom) in splits[1].0.iter().enumerate() {
        if j != index[1] {
            atoms.push(atom.clone().substitute_with_offset(&sigma, offset));
        }
    }

    for (j, atom) in splits[0].1.iter().enumerate() {
        if j + clause[0].arrow != index[0] {
            atoms.push(atom.clone().substitute(&sigma));
        }
    }

    for (j, atom) in splits[1].1.iter().enumerate() {
        if j + clause[1].arrow != index[1] {
            atoms.push(atom.clone().substitute_with_offset(&sigma, offset));
        }
    }

    let mut typs = Vec::with_capacity(clause[0].typs.len() + clause[1].typs.len());
    for i in 0..2 {
        for tau in clause[i].typs.iter() {
            typs.push(*tau)
        }
    }

    let mut resolvent = Clause {
        id: 0,
        constraint: Constraint {
            atoms: catoms.into_boxed_slice(),
            solution: None,
        },
        atoms: atoms.into_boxed_slice(),
        arrow: clause[0].arrow + clause[1].arrow - 1,
        typs: typs.into_boxed_slice(),
        parents: Parents::Resolvent(
            ClausePosition {
                clause: clause[0].id,
                literal: index[0],
            },
            ClausePosition {
                clause: clause[1].id,
                literal: index[1],
            },
        ),
    };
    // dbg!(&resolvent.constraint.atoms);
    // dbg!(&resolvent.atoms);
    resolvent.normalize();
    // dbg!(&resolvent.constraint.atoms);
    // dbg!(&resolvent.atoms);
    resolvent
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;

    use crate::{symbols::Predicate, Atom, Typ};

    const P: Predicate = 0;
    const Q: Predicate = 1;
    const R: Predicate = 2;
    const S: Predicate = 3;

    /// { ¬P, Q }  { P }
    /// ----------------
    ///      { Q }
    #[test]
    fn simple() {
        let resolvent = resolve(
            [
                &Clause::dummy(vec![P.into()], vec![Q.into()]),
                &Clause::dummy(vec![], vec![P.into()]),
            ],
            [0, 0],
        )
        .unwrap();
        assert_eq!(resolvent.atoms, vec![Q.into()].into_boxed_slice());
        assert_eq!(resolvent.arrow, 0);
    }

    /// { P }  { ¬P }
    /// -------------
    ///     { }
    #[test]
    fn empty() {
        assert!(resolve(
            [
                &Clause::dummy(vec![], vec![P.into()]),
                &Clause::dummy(vec![P.into()], vec![]),
            ],
            [0, 0]
        )
        .unwrap()
        .is_empty());
    }

    /// { P, Q }   { ¬P, Q }
    /// --------------------
    ///      { Q, Q }
    #[test]
    fn multi() {
        let resolvent = resolve(
            [
                &Clause::dummy(vec![], vec![P.into(), Q.into()]),
                &Clause::dummy(vec![P.into()], vec![Q.into()]),
            ],
            [0, 0],
        )
        .unwrap();
        assert_eq!(resolvent.atoms, vec![Q.into(), Q.into()].into_boxed_slice());
        assert_eq!(resolvent.arrow, 0);
    }

    /// { ¬R, P, Q }   { ¬P, ¬Q, R }
    /// ----------------------------
    ///      { ¬R, ¬Q, Q, R }
    #[test]
    fn many() {
        let resolvent = resolve(
            [
                &Clause::dummy(vec![R.into()], vec![P.into(), Q.into()]),
                &Clause::dummy(vec![P.into(), Q.into()], vec![R.into()]),
            ],
            [1, 0],
        )
        .unwrap();

        assert_eq!(
            resolvent.atoms,
            vec![R.into(), Q.into(), Q.into(), R.into()].into_boxed_slice()
        );
        assert_eq!(resolvent.arrow, 2);
    }

    /// { ¬R(x), P(a, y), Q(y) }   { ¬P(x, b), ¬Q(x), R(y) }
    /// ----------------------------------------------------
    ///      { ¬R(x), ¬Q(a), Q(b), R(y) }
    #[test]
    fn with_terms() {
        let x = Term::Variable(0, Typ::F);
        let y = Term::Variable(1, Typ::F);
        let a = Term::Constant(0, Typ::F);
        let b = Term::Constant(1, Typ::F);
        let resolvent = resolve(
            [
                &Clause::dummy(
                    vec![Atom::new(R, vec![x])],
                    vec![Atom::new(P, vec![a, y]), Atom::new(Q, vec![y])],
                ),
                &Clause::dummy(
                    vec![Atom::new(P, vec![x, b]), Atom::new(Q, vec![x])],
                    vec![Atom::new(R, vec![y])],
                ),
            ],
            [1, 0],
        )
        .unwrap();

        assert_eq!(
            resolvent.atoms,
            vec![
                Atom::new(R, vec![x]),
                Atom::new(Q, vec![a]),
                Atom::new(Q, vec![b]),
                Atom::new(R, vec![y])
            ]
            .into_boxed_slice(),
        );
        assert_eq!(resolvent.arrow, 2);
    }

    #[test]
    fn with_constraint() {
        /*
        let a = Clause {
            id: 0,
            arrow: 5,
            atoms: vec![
                Atom { predicate: 1, atoms: vec![]}
            ].into_boxed_slice(),
            constraint: Constraint {
                atoms: vec![CAtom {
                    predicate: CPredicate::Ge,
                    left: CTerm::Inj(Term::Variable(0, Typ::R)),
                    right: CTerm::Inj(Term::Variable(0, Typ::R)),
                }].into_boxed_slice(),
                solution: None,
            }
        };

        let b = Clause::dummy(vec![], vec![Atom {
            predicate: 0,
            terms: vec![
                Term::Integer(0),
                Term::Integer(2),
                Term::Integer(4),
                Term::Integer(0),
                Term::Constant(0, Typ::F),
            ].into_boxed_slice(),
        }]);
        */

        // resolve([&a, &b], [3, 1]);
        // x0 > x1 ∥ ≠(x2, x3), LaneSafe(x4, x3, x5), EgoCar(x4, x2, x6, x7), DistanceBehind(x4, x3, x8, x1, x5), SpeedBehind(x4, x3, x8, x0, x5) → SafeBehindDisproven(x4, x3, x2, x6, x7, x5).
        // -> DistanceBehind(0, 2, 4, 0, aaccelerateleft).
    }
}
