use crate::{Clause, ClausePosition, Constraint, Parents};

fn resolve(clause: [&Clause; 2], index: [usize; 2]) -> Clause {
    debug_assert!((index[0] < clause[0].arrow) ^ (index[1] < clause[1].arrow));
    debug_assert!(clause[0].atoms[index[0]].predicate == clause[1].atoms[index[1]].predicate);

    let mut catoms =
        Vec::with_capacity(clause[0].constraint.atoms.len() + clause[1].constraint.atoms.len());

    for i in 0..2 {
        for catom in clause[i].constraint.atoms.iter() {
            catoms.push(catom.clone());
        }
    }

    let arrow = clause[0].arrow + clause[1].arrow - 1;

    let mut atoms = Vec::with_capacity(clause[0].atoms.len() + clause[1].atoms.len() - 2);

    let splits = clause.map(|c| c.split_at_arrow());

    for i in 0..2 {
        for (j, atom) in splits[i].0.iter().enumerate() {
            if j != index[i] {
                atoms.push(atom.clone());
            }
        }
    }
    for i in 0..2 {
        for (j, atom) in splits[i].1.iter().enumerate() {
            if j + clause[i].arrow != index[i] {
                atoms.push(atom.clone());
            }
        }
    }

    Clause {
        id: 0,
        constraint: Constraint {
            atoms: catoms.into_boxed_slice(),
        },
        atoms: atoms.into_boxed_slice(),
        arrow,
        typs: vec![].into_boxed_slice(),
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
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;

    use crate::symbols::Predicate;

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
        );
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
        );
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
        );

        assert_eq!(
            resolvent.atoms,
            vec![R.into(), Q.into(), Q.into(), R.into()].into_boxed_slice()
        );
        assert_eq!(resolvent.arrow, 2);
    }
}
