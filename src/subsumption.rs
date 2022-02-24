use crate::{substitution::Substitution, Clause};

impl Clause {
    fn subsumes(&self, other: &Clause, substitution: &mut Substitution) -> bool {
        if self.conclusions() > other.conclusions() || self.premises() > other.premises() {
            return false;
        }
        substitution.start();
        let result = self.subsumes_rec(other, substitution, 0);
        substitution.stop();
        result
    }

    fn subsumes_rec(&self, other: &Clause, substitution: &mut Substitution, index: usize) -> bool {
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
            substitution.backtrack(snapshot);
        }

        false
    }
}

#[cfg(test)]
mod tests {

    use itertools::Itertools;

    use crate::{symbols::Term, Atom, Typ};

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

        let s = &mut Substitution::new();

        assert!(subsumes.subsumes(&subsumed, s))
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

        let s = &mut Substitution::new();

        assert!(!subsumes.subsumes(&subsumed, s))
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

        let substitution = &mut Substitution::new();
        assert!(l1.subsumes(&r, substitution));
        assert!(!l2.subsumes(&r, substitution));
    }
}
