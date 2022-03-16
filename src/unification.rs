use std::env::var;

use crate::{substitution::Substitution, symbols::*, *};

type Equivalence<'a> = (Variable, &'a Term);

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Error {
    Type,
    Match,
}

impl Term {
    fn unify<'a>(&'a self, other: &'a Term) -> Result<Option<Equivalence>, Error> {
        if *self == *other {
            return Ok(None);
        }

        match (self, other) {
            (Term::Integer(_), Term::Integer(_)) => Err(Error::Match),
            (Term::Variable(v, tv), Term::Integer(_)) => {
                if tv.contains(Typ::I) {
                    Ok(Some((*v, other)))
                } else {
                    Err(Error::Type)
                }
            }
            (Term::Integer(_), Term::Variable(v, tv)) => {
                if tv.contains(Typ::I) {
                    Ok(Some((*v, self)))
                } else {
                    Err(Error::Type)
                }
            }
            (Term::Constant(_, _), Term::Constant(_, _)) => Err(Error::Match),
            (Term::Constant(_c, tc), Term::Variable(v, tv)) => {
                if tv.contains(*tc) {
                    Ok(Some((*v, self)))
                } else {
                    Err(Error::Type)
                }
            }
            (Term::Variable(v, tv), Term::Constant(_c, tc)) => {
                if tv.contains(*tc) {
                    Ok(Some((*v, other)))
                } else {
                    Err(Error::Type)
                }
            }
            (Term::Variable(_, tx), Term::Variable(y, ty)) => {
                if tx == ty {
                    Ok(Some((*y, self)))
                } else if ty.contains(*tx) {
                    todo!()
                } else if tx.contains(*ty) {
                    todo!()
                } else {
                    Err(Error::Type)
                }
            }
            _ => debug_unreachable!(),
        }
    }
}

impl std::ops::Add<usize> for Term {
    type Output = Term;

    fn add(self, rhs: usize) -> Term {
        match self {
            // TODO: Check for overflow.
            Term::Variable(v, t) => Term::Variable(v + (rhs as u32), t),
            _ => self,
        }
    }
}

fn chase(substitution: &Vec<Option<Term>>, term: Term) -> Term {
    match term {
        Term::Variable(v, _) => match substitution[v as usize] {
            None => term,
            Some(term) => chase(substitution, term),
        },
        _ => term,
    }
}

pub(crate) fn unify_at(
    clause: [&Clause; 2],
    index: [usize; 2],
) -> Result<Vec<Option<Term>>, Error> {
    debug_assert!(index[0] < clause[0].atoms.len());
    debug_assert!(index[1] < clause[1].atoms.len());

    let atoms = [&clause[0].atoms[index[0]], &clause[1].atoms[index[1]]];
    unify_atoms(atoms, [clause[0].typs.len(), clause[1].typs.len()])
}

fn unify_atoms(atoms: [&Atom; 2], variables: [usize; 2]) -> Result<Vec<Option<Term>>, Error> {
    if atoms[0].predicate != atoms[1].predicate {
        return Err(Error::Match);
    }
    debug_assert!(atoms[0].terms.len() == atoms[1].terms.len());

    let mut sigma: Vec<Option<Term>> = vec![None; variables[0] + variables[1]];

    for (l, r) in atoms[0].terms.iter().zip(atoms[1].terms.iter()) {
        let lc = l.substitute(&sigma);
        let ro = *r + variables[0];
        let rc = ro.substitute(&sigma);

        match lc.unify(&rc) {
            Ok(Some((x, t))) => sigma.set(x, *t),
            Ok(None) => {}
            Err(e) => return Err(e),
        }
    }

    Ok(sigma)
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;

    #[test]
    fn table() {
        let table: Vec<(Atom, Atom, Result<Vec<Option<Term>>, Error>)> = vec![
            (1.into(), 1.into(), Ok(vec![])),
            (
                vec![Term::Variable(0, Typ::R)].into(),
                vec![Term::Variable(0, Typ::R)].into(),
                Ok(vec![None, Some(Term::Variable(0, Typ::R))]),
            ),
            (
                vec![Term::Variable(0, Typ::R)].into(),
                vec![Term::Constant(1, Typ::R)].into(),
                Ok(vec![Some(Term::Constant(1, Typ::R))]),
            ),
            (
                vec![Term::Variable(0, Typ::I)].into(),
                vec![Term::Integer(1)].into(),
                Ok(vec![Some(Term::Integer(1))]),
            ),
            (
                vec![Term::Variable(0, Typ::R)].into(),
                vec![Term::Integer(1)].into(),
                Ok(vec![Some(Term::Integer(1))]),
            ),
            (
                vec![Term::Variable(0, Typ::F)].into(),
                vec![Term::Integer(1)].into(),
                Err(Error::Type),
            ),
            (
                vec![Term::Variable(0, Typ::I)].into(),
                vec![Term::Constant(0, Typ::F)].into(),
                Err(Error::Type),
            ),
            (
                vec![Term::Variable(0, Typ::F), Term::Constant(0, Typ::F)].into(),
                vec![Term::Constant(0, Typ::F), Term::Variable(0, Typ::F)].into(),
                Ok(vec![
                    Some(Term::Constant(0, Typ::F)),
                    Some(Term::Constant(0, Typ::F)),
                ]),
            ),
            (
                vec![Term::Variable(0, Typ::F), Term::Variable(1, Typ::F)].into(),
                vec![Term::Variable(0, Typ::F), Term::Variable(1, Typ::F)].into(),
                Ok(vec![
                    None,
                    None,
                    Some(Term::Variable(0, Typ::F)),
                    Some(Term::Variable(1, Typ::F)),
                ]),
            ),
            (
                vec![Term::Variable(0, Typ::F), Term::Variable(0, Typ::F)].into(),
                vec![Term::Constant(0, Typ::F), Term::Constant(1, Typ::F)].into(),
                Err(Error::Match),
            ),
            (
                vec![Term::Constant(0, Typ::F), Term::Variable(0, Typ::F)].into(),
                vec![Term::Variable(0, Typ::F), Term::Constant(1, Typ::F)].into(),
                Ok(vec![
                    Some(Term::Constant(1, Typ::F)),
                    Some(Term::Constant(0, Typ::F)),
                ]),
            ),
            (
                vec![
                    Term::Variable(0, Typ::R),
                    Term::Variable(1, Typ::R),
                    Term::Variable(2, Typ::F),
                ]
                .into(),
                vec![
                    Term::Integer(0),
                    Term::Integer(1),
                    Term::Constant(0, Typ::F),
                ]
                .into(),
                Ok(vec![
                    Some(Term::Integer(0)),
                    Some(Term::Integer(1)),
                    Some(Term::Constant(0, Typ::F)),
                ]),
            ),
        ];
        for (index, (l, r, result)) in table.iter().enumerate() {
            assert_eq!(
                unify_atoms([l, r], [l.count_vars(), r.count_vars()]),
                *result,
                "{}/{}",
                index + 1,
                table.len(),
            );

            /*
            assert_eq!(
                unify_atoms([r, l], [r.count_vars(), l.count_vars()]),
                *result,
                "{} reversed",
                index
            );
            */
        }
    }
}
