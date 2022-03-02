use core::panic;
use std::env::VarError;

use crate::{substitution::*, symbols::*, *};

impl Term {
    fn normalize(&self, sigma: &mut dyn Substitution, next: Variable) -> (Term, Variable) {
        match self {
            Term::Variable(x, tau) => {
                if let Some(y) = sigma.chase(*x) {
                    (y, next)
                } else {
                    let y = Term::Variable(next, *tau);
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
            left.1,
        )
    }
}

impl Clause {
    pub(crate) fn normalize(&mut self) {
        let mut sigma: Vec<Option<Term>> = Vec::with_capacity(self.typs.len());
        let mut next = 0;
        for i in 0..self.constraint.atoms.len() {
            //println!("normalizing {}", self.constraint.atoms[i]);
            let normalized = self.constraint.atoms[i].normalize(&mut sigma, next);
            next = normalized.1;
            self.constraint.atoms[i] = normalized.0;
        }
        for i in 0..self.atoms.len() {
            //println!("normalizing {:?}", self.atoms[i]);
            let normalized = self.atoms[i].normalize(&mut sigma, next);
            next = normalized.1;
            self.atoms[i] = normalized.0;
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn normalize() {
        let mut sigma: Vec<Option<Term>> = vec![None::<Term>; 2];
        assert_eq!(
            Atom {
                predicate: 0,
                terms: vec![Term::Variable(4, Typ::F), Term::Variable(2, Typ::F),]
                    .into_boxed_slice()
            }
            .normalize(&mut sigma, 0)
            .0
            .terms,
            vec![Term::Variable(0, Typ::F), Term::Variable(1, Typ::F)].into_boxed_slice()
        )
    }
}
