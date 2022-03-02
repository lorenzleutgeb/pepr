use crate::{
    substitution::{self, *},
    symbols::Term,
    Clause,
};

impl Clause {
    pub(crate) fn subsumes(&self, other: &Clause) -> bool {
        if self.conclusions() > other.conclusions() || self.premises() > other.premises() {
            false
        } else {
            let mut sigma: SubstitutionWithHistory<Vec<Option<Term>>> = Default::default();
            self.subsumes_rec(other, &mut sigma, 0)
        }
    }

    pub(crate) fn subsumes_with_substitution<T: Substitution + Snapshots>(
        &self,
        other: &Clause,
        substitution: &mut T,
    ) -> bool {
        if self.conclusions() > other.conclusions() || self.premises() > other.premises() {
            false
        } else {
            self.subsumes_rec(other, substitution, 0)
        }
    }

    fn subsumes_rec<T: Substitution + Snapshots>(
        &self,
        other: &Clause,
        substitution: &mut T,
        index: usize,
    ) -> bool {
        if index >= self.atoms.len() {
            return true;
        }

        let (premises, conclusions) = other.split_at_arrow();
        let candidates = if index < self.arrow {
            premises
        } else {
            conclusions
        };

        for candidate in candidates {
            let snapshot = substitution.snapshot();
            if substitution.match_atoms(&(self.atoms[index]), candidate)
                && self.subsumes_rec(other, substitution, index.checked_add(1).unwrap())
            {
                return true;
            }
            substitution.rollback(snapshot);
        }

        false
    }

    fn subsumes_c(&self, other: &Clause, sigma: &dyn Substitution) -> bool {
        other
            .constraint
            .implies(&self.constraint.subsititute(sigma))
    }
}

#[cfg(test)]
mod tests {

    use itertools::Itertools;

    use crate::{substitution, symbols::Term, Atom, Typ};

    use super::*;

    #[test]
    fn simple() {
        let x0 = Term::Variable(0, Typ::F);
        let c0 = Term::Constant(0, Typ::F);

        let subsumes = Clause::dummy(
            vec![Atom {
                predicate: 0,
                terms: vec![x0].into_boxed_slice(),
            }],
            vec![],
        );

        let subsumed = Clause::dummy(
            vec![Atom {
                predicate: 0,
                terms: vec![c0].into_boxed_slice(),
            }],
            vec![],
        );

        assert!(subsumes.subsumes(&subsumed))
    }

    #[test]
    fn simple_negative() {
        let x0 = Term::Variable(0, Typ::F);
        let c0 = Term::Constant(0, Typ::F);

        let subsumed = Clause::dummy(
            vec![Atom {
                predicate: 0,
                terms: vec![x0].into_boxed_slice(),
            }],
            vec![],
        );

        let subsumes = Clause::dummy(
            vec![Atom {
                predicate: 0,
                terms: vec![c0].into_boxed_slice(),
            }],
            vec![],
        );

        assert!(!subsumes.subsumes(&subsumed))
    }

    #[test]
    fn sat() {
        // Boolean atoms.
        let x = Term::Variable(0, Typ::F);
        let y = Term::Variable(1, Typ::F);
        let z = Term::Variable(2, Typ::F);

        let c_false = Term::Constant(0, Typ::F);
        let c_true = Term::Constant(1, Typ::F);

        let r: usize = 0;

        // p cnf 3 3
        // c x y ¬z
        // 1 2 -3 0
        // c ¬x ¬y z
        // -1 -2 3 0
        // c ¬x y z
        // -1 2 3 0
        let l1 = Clause::dummy(
            vec![],
            vec![
                Atom {
                    predicate: r,
                    terms: vec![x, c_true, y, c_true, z, c_false].into_boxed_slice(),
                },
                Atom {
                    predicate: r,
                    terms: vec![x, c_false, y, c_false, z, c_true].into_boxed_slice(),
                },
                Atom {
                    predicate: r,
                    terms: vec![x, c_false, y, c_true, z, c_false].into_boxed_slice(),
                },
            ],
        );

        let l2 = Clause::dummy(
            vec![],
            vec![
                Atom {
                    predicate: r,
                    terms: vec![x, c_true, x, c_true, x, c_true].into_boxed_slice(),
                },
                Atom {
                    predicate: r,
                    terms: vec![x, c_false, x, c_false, x, c_false].into_boxed_slice(),
                },
            ],
        );

        let truth = vec![c_true, c_false]
            .into_iter()
            .combinations_with_replacement(2)
            .map(|x| (x[0], x[1]));

        let r = Clause::dummy(
            vec![],
            itertools::iproduct!(truth.clone(), truth.clone(), truth)
                .filter(|assignment| {
                    assignment.0 .0 == assignment.0 .1
                        || assignment.1 .0 == assignment.1 .1
                        || assignment.2 .0 == assignment.2 .1
                })
                .map(|assignment| Atom {
                    predicate: r,
                    terms: vec![
                        assignment.0 .0,
                        assignment.0 .1,
                        assignment.1 .0,
                        assignment.1 .1,
                        assignment.2 .0,
                        assignment.2 .1,
                    ]
                    .into_boxed_slice(),
                })
                .collect(),
        );

        assert!(l1.subsumes(&r));
        assert!(!l2.subsumes(&r));
    }
}
