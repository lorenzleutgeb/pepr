use core::panic;
use std::env::VarError;

use crate::{substitution::*, symbols::*, *};

impl Term {
    fn normalize(&self, sigma: &mut dyn Substitution, next: Variable) -> (Term, Variable) {
        match self {
            Term::Variable(x, tau) => {
                if let Some(y) = sigma.get(*x) {
                    // println!("Getting {:?} to {:?}, therefore accepting that.", *x, y);
                    (y, next)
                } else {
                    let y = Term::Variable(next, *tau);
                    // println!("Getting {:?} to none, therefore setting it to {:?}", *x, y);
                    sigma.set(*x, y);
                    (y, next + 1)
                }
            }
            t => (*t, next),
        }
    }
}

impl CTerm {
    fn normalize(&self, sigma: &mut dyn Substitution, next: Variable) -> (CTerm, Variable) {
        match self {
            CTerm::Inj(term) => {
                let result = term.normalize(sigma, next);
                (CTerm::Inj(result.0), result.1)
            }
            CTerm::Add(args) => {
                let left = args[0].normalize(sigma, next);
                let right = args[1].normalize(sigma, left.1);
                (CTerm::Add(Box::new([left.0, right.0])), right.1)
            }
            CTerm::Sub(args) => {
                let left = args[0].normalize(sigma, next);
                let right = args[1].normalize(sigma, left.1);
                (CTerm::Sub(Box::new([left.0, right.0])), right.1)
            }
            CTerm::Mul(args) => {
                let left = args[0].normalize(sigma, next);
                let right = args[1].normalize(sigma, left.1);
                (CTerm::Mul(Box::new([left.0, right.0])), right.1)
            }
            CTerm::Neg(args) => {
                let result = args[0].normalize(sigma, next);
                (CTerm::Neg(Box::new([result.0])), result.1)
            }
        }
    }

    pub(crate) fn force_immediate(self, next: Variable) -> (Term, Option<CAtom>) {
        match self {
            CTerm::Inj(t) => (t, None),
            CTerm::Add(args) => {
                let x = Term::Variable(next, Typ::R);
                (
                    x,
                    Some(CAtom {
                        predicate: CPredicate::Eq,
                        left: CTerm::Inj(x),
                        right: CTerm::Add(args),
                    }),
                )
            }
            CTerm::Mul(args) => {
                let x = Term::Variable(next, Typ::R);
                (
                    x,
                    Some(CAtom {
                        predicate: CPredicate::Eq,
                        left: CTerm::Inj(x),
                        right: CTerm::Mul(args),
                    }),
                )
            }
            CTerm::Sub(args) => {
                let x = Term::Variable(next, Typ::R);
                (
                    x,
                    Some(CAtom {
                        predicate: CPredicate::Eq,
                        left: CTerm::Inj(x),
                        right: CTerm::Sub(args),
                    }),
                )
            }
            CTerm::Neg(args) => {
                let x = Term::Variable(next, Typ::R);
                (
                    x,
                    Some(CAtom {
                        predicate: CPredicate::Eq,
                        left: CTerm::Inj(x),
                        right: CTerm::Neg(args),
                    }),
                )
            }
        }
    }
}

impl Atom {
    fn normalize(&self, sigma: &mut dyn Substitution, mut next: Variable) -> (Atom, Variable) {
        let mut normalized = Vec::with_capacity(self.len());
        for term in self.into_iter() {
            let normalized_term = term.normalize(sigma, next);
            next = normalized_term.1;
            normalized.push(normalized_term.0);
        }
        (
            Atom {
                predicate: self.predicate,
                terms: normalized.into_boxed_slice(),
            },
            next,
        )
    }
}

impl CAtom {
    fn normalize(&self, sigma: &mut dyn Substitution, next: Variable) -> (CAtom, Variable) {
        let left = self.left.normalize(sigma, next);
        let right = self.right.normalize(sigma, left.1);
        (
            CAtom {
                predicate: self.predicate,
                left: left.0,
                right: right.0,
            },
            right.1,
        )
    }
}

impl Clause {
    pub(crate) fn normalize(&mut self) {
        let mut sigma: Vec<Option<Term>> = Vec::with_capacity(self.typs.len());
        let mut next = 0;
        for i in 0..self.constraint.atoms.len() {
            // println!("next before catom: {}", next);
            let normalized = self.constraint.atoms[i].normalize(&mut sigma, next);
            next = normalized.1;
            // println!("next after catom: {}", next);
            self.constraint.atoms[i] = normalized.0;
        }
        // dbg!(&sigma);
        for i in 0..self.atoms.len() {
            // println!("next before atom: {}", next);
            let normalized = self.atoms[i].normalize(&mut sigma, next);
            next = normalized.1;
            // println!("next after atom: {}", next);
            self.atoms[i] = normalized.0;
            // dbg!(&sigma);
        }

        let max_var = *self.max_var().unwrap_or(&0);
        let mut typs = vec![Default::default(); (max_var + 1) as usize];
        for x in 0..=max_var {
            if let Some(Term::Variable(y, tau)) = sigma.chase(x) {
                typs[y as usize] = tau;
            }
        }
        self.typs = typs.into_boxed_slice();
    }

    pub(crate) fn rewrite_ne(self, symbols: &Symbols) -> Clause {
        let has = self
            .constraint
            .atoms
            .iter()
            .any(|atom| atom.predicate == CPredicate::Ne);

        if !has {
            self
        } else {
            let first_next: Variable = self.max_var().map_or(0, |x| x + 1);
            let mut next = first_next;
            let mut ne = 0;

            let mut catoms: Vec<CAtom> = vec![];
            let mut atoms: Vec<Atom> = vec![];

            for catom in Vec::from(self.constraint.atoms) {
                match catom.predicate {
                    CPredicate::Ne => {
                        ne += 1;

                        let left = catom.left.force_immediate(next);
                        if let Some(catom) = left.1 {
                            catoms.push(catom);
                            next += 1;
                        }

                        let right = catom.right.force_immediate(next);
                        if let Some(catom) = right.1 {
                            catoms.push(catom);
                            next += 1;
                        }

                        atoms.push(Atom {
                            predicate: symbols.predicates.interner.get(NEQ).unwrap(),
                            terms: vec![left.0, right.0].into_boxed_slice(),
                        });
                    }
                    _ => catoms.push(catom.clone()),
                }
            }

            let mut typs: Vec<Typ> = Vec::from(self.typs);
            typs.append(&mut vec![Typ::R; (next - first_next) as usize]);

            atoms.append(&mut Vec::from(self.atoms));

            Clause {
                id: self.id,
                arrow: self.arrow + ne,
                parents: self.parents,
                atoms: atoms.into_boxed_slice(),
                constraint: Constraint {
                    solution: None,
                    atoms: catoms.into_boxed_slice(),
                },
                typs: typs.into_boxed_slice(),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn normalize_atom() {
        assert_eq!(
            Atom {
                predicate: 0,
                terms: vec![Term::Variable(4, Typ::F), Term::Variable(2, Typ::F),]
                    .into_boxed_slice()
            }
            .normalize(&mut vec![None::<Term>; 2], 0)
            .0
            .terms,
            vec![Term::Variable(0, Typ::F), Term::Variable(1, Typ::F)].into_boxed_slice()
        );

        assert_eq!(
            Atom {
                predicate: 0,
                terms: vec![Term::Variable(1, Typ::R), Term::Integer(1)].into_boxed_slice()
            }
            .normalize(&mut vec![None::<Term>; 2], 0)
            .0
            .terms,
            vec![Term::Variable(0, Typ::R), Term::Integer(1)].into_boxed_slice()
        );
    }

    #[test]
    fn normalize_clause() {
        let mut clause = Clause::dummy(
            vec![],
            vec![
                Atom {
                    predicate: 0,
                    terms: vec![Term::Variable(1, Typ::R), Term::Integer(1)].into_boxed_slice(),
                },
                Atom {
                    predicate: 1,
                    terms: vec![Term::Variable(1, Typ::R), Term::Integer(0)].into_boxed_slice(),
                },
            ],
        );

        clause.normalize();

        assert_eq!(clause.atoms[0].predicate, 0);
        assert_eq!(clause.atoms[1].predicate, 1);

        assert_eq!(
            clause.atoms[0].terms,
            vec![Term::Variable(0, Typ::R), Term::Integer(1)].into_boxed_slice()
        );
        assert_eq!(
            clause.atoms[1].terms,
            vec![Term::Variable(0, Typ::R), Term::Integer(0)].into_boxed_slice(),
        );
    }

    #[test]
    fn normalize_clause_2() {
        let mut clause = Clause::dummy(
            vec![],
            vec![
                Atom {
                    predicate: 0,
                    terms: vec![Term::Integer(1), Term::Variable(1, Typ::R)].into_boxed_slice(),
                },
                Atom {
                    predicate: 2,
                    terms: vec![
                        Term::Integer(0),
                        Term::Variable(1, Typ::R),
                        Term::Variable(3, Typ::R),
                        Term::Variable(4, Typ::R),
                    ]
                    .into_boxed_slice(),
                },
                Atom {
                    predicate: 3,
                    terms: vec![
                        Term::Integer(0),
                        Term::Integer(1),
                        Term::Variable(1, Typ::R),
                        Term::Variable(3, Typ::R),
                        Term::Variable(4, Typ::R),
                        Term::Constant(0, Typ::R),
                    ]
                    .into_boxed_slice(),
                },
            ],
        );
        clause.normalize();

        assert_eq!(
            clause.atoms[2].terms,
            vec![
                Term::Integer(0,),
                Term::Integer(1,),
                Term::Variable(0, Typ::R,),
                Term::Variable(1, Typ::R,),
                Term::Variable(2, Typ::R,),
                Term::Constant(0, Typ::R,),
            ]
            .into_boxed_slice()
        )
    }
}
