use crate::{symbols::*, *};

type Equivalence<'a> = (Variable, &'a Term);

#[derive(Debug, PartialEq, Eq)]
enum Error {
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
            (Term::Variable(v, tv), Term::Integer(_i)) => {
                if tv.contains(Typ::I) {
                    Ok(Some((*v, other)))
                } else {
                    Err(Error::Type)
                }
            }
            (Term::Integer(_i), Term::Variable(v, tv)) => {
                if tv.contains(Typ::I) {
                    Ok(Some((*v, other)))
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
            (Term::Variable(x, tx), Term::Variable(_y, ty)) => {
                if tx == ty {
                    Ok(Some((*x, other)))
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

fn unify_clauses(clause: [&Clause; 2], index: [usize; 2]) -> Result<(), Error> {
    let atoms = [&clause[0].atoms[index[0]], &clause[1].atoms[index[1]]];
    if atoms[0].predicate != atoms[1].predicate {
        return Err(Error::Match);
    }
    debug_assert!(atoms[0].terms.len() == atoms[1].terms.len());

    let offset = clause[0].typs.len();

    let mut subst: Vec<Option<Term>> = vec![None; offset + clause[1].typs.len()];

    for (l, r) in atoms[0].terms.iter().zip(atoms[1].terms.iter()) {
        let lc = chase(&subst, *l);
        let ro = *r + offset;
        let rc = chase(&subst, ro);

        match lc.unify(&rc) {
            Ok(Some((x, t))) => subst[x as usize] = Some(*t),
            Ok(None) => {}
            Err(e) => return Err(e),
        }
    }

    Ok(())
}

fn unify_atoms(atoms: [&Atom; 2], offset: usize, len: usize) -> Result<(), Error> {
    if atoms[0].predicate != atoms[1].predicate {
        return Err(Error::Match);
    }
    debug_assert!(atoms[0].terms.len() == atoms[1].terms.len());

    let mut subst: Vec<Option<Term>> = vec![None; offset + len];

    for (l, r) in atoms[0].terms.iter().zip(atoms[1].terms.iter()) {
        let lc = chase(&subst, *l);
        let ro = *r + offset;
        let rc = chase(&subst, ro);

        match lc.unify(&rc) {
            Ok(Some((x, t))) => subst[x as usize] = Some(*t),
            Ok(None) => {}
            Err(e) => return Err(e),
        }
    }

    println!("{:?}", subst);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    impl Atom {
        fn count_vars(&self) -> usize {
            self.terms
                .iter()
                .map(|term| {
                    if matches!(term, Term::Variable(_, _)) {
                        1
                    } else {
                        0
                    }
                })
                .sum()
        }
    }

    #[test]
    fn table() {
        let table: Vec<(Atom, Atom, Result<(), Error>)> = vec![
            (1.into(), 1.into(), Ok(())),
            (
                vec![Term::Variable(0, Typ::R)].into(),
                vec![Term::Variable(0, Typ::R)].into(),
                Ok(()),
            ),
            (
                vec![Term::Variable(0, Typ::R)].into(),
                vec![Term::Constant(1, Typ::R)].into(),
                Ok(()),
            ),
            (
                vec![Term::Variable(0, Typ::I)].into(),
                vec![Term::Integer(1)].into(),
                Ok(()),
            ),
            (
                vec![Term::Variable(0, Typ::R)].into(),
                vec![Term::Integer(1)].into(),
                Ok(()),
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
                Ok(()),
            ),
            (
                vec![Term::Variable(0, Typ::F), Term::Variable(1, Typ::F)].into(),
                vec![Term::Variable(0, Typ::F), Term::Variable(1, Typ::F)].into(),
                Ok(()),
            ),
            (
                vec![Term::Variable(0, Typ::F), Term::Variable(0, Typ::F)].into(),
                vec![Term::Constant(0, Typ::F), Term::Constant(1, Typ::F)].into(),
                Err(Error::Match),
            ),
        ];
        for (index, (l, r, result)) in table.iter().enumerate() {
            assert_eq!(
                unify_atoms([l, r], l.count_vars(), r.count_vars()),
                *result,
                "{}",
                index
            );
            assert_eq!(
                unify_atoms([r, l], r.count_vars(), l.count_vars()),
                *result,
                "{} reversed",
                index
            );
        }
    }
}
