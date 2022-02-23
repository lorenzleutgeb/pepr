use std::{convert::TryInto, fmt::Debug};

use crate::{
    symbols::{Term, Variable},
    Atom,
};

const MAX_VARS: usize = 256;

#[derive(Clone, Copy)]
struct Mapping {
    term: Option<Term>,
    generation: usize,
}

struct History {
    variable: Variable,
    previous: Option<Term>,
}

impl History {
    fn identity(variable: Variable) -> History {
        History {
            variable,
            previous: None,
        }
    }
}

pub struct Substitution {
    mapping: [Mapping; MAX_VARS],
    generation: usize,
    history: Vec<History>,
    #[cfg(debug_assertions)]
    active: bool,
}

impl Substitution {
    pub fn new() -> Substitution {
        Substitution {
            generation: 0,
            mapping: [Mapping {
                term: None,
                generation: 0,
            }; MAX_VARS],
            history: Vec::with_capacity(MAX_VARS),
            #[cfg(debug_assertions)]
            active: false,
        }
    }

    pub fn start(&mut self) {
        debug_assert!(!self.active);
        if cfg!(debug_assertions) {
            self.active = true;
        }
        self.generation = self.generation.wrapping_add(1);
    }

    pub fn stop(&mut self) {
        debug_assert!(self.active);
        if cfg!(debug_assertions) {
            self.active = false;
        }
        self.history.clear();
    }

    fn get(&self, variable: Variable) -> Option<Term> {
        debug_assert!(self.active);

        let i = variable.to_index();

        assert!(i <= MAX_VARS);

        let x = &self.mapping[i];

        if x.generation == self.generation {
            x.term
        } else {
            None
        }
    }

    fn set(&mut self, variable: Variable, term: Term) {
        debug_assert!(self.active);

        let i = variable.to_index();

        assert!(i <= MAX_VARS);
        debug_assert!(Term::from(variable) != term);

        if self.mapping[i].generation == self.generation {
            self.history.push(History {
                variable,
                previous: self.mapping[i].term,
            });
        } else {
            self.history.push(History::identity(variable))
        }

        self.mapping[i] = Mapping {
            term: Some(term),
            generation: self.generation,
        }
    }

    #[inline]
    pub fn snapshot(&self) -> usize {
        debug_assert!(self.active);
        self.history.len()
    }

    pub fn backtrack(&mut self, snapshot: usize) {
        debug_assert!(self.active);
        debug_assert!(snapshot <= self.history.len());

        while self.history.len() != snapshot {
            let top = self.history.pop().unwrap();
            let i = top.variable.to_index();

            debug_assert!(self.mapping[i].generation == self.generation);

            self.mapping[i] = Mapping {
                term: top.previous,
                generation: self.generation,
            }
        }
    }

    pub fn match_atoms(&mut self, left: &Atom, right: &Atom) -> bool {
        if left.predicate != right.predicate || left.len() != right.len() {
            false
        } else {
            left.into_iter()
                .zip(right.into_iter())
                .map(|pair| self.match_terms(pair.0, pair.1))
                .all(std::convert::identity)
        }
    }

    fn match_terms(&mut self, left: &Term, right: &Term) -> bool {
        if left == right {
            return true;
        } else {
            match (*left).try_into() {
                Ok(v) => match self.get(v) {
                    Some(mapped) => mapped == (*right),
                    None => {
                        self.set(v, *right);
                        return true;
                    }
                },
                _ => false,
            }
        }
    }

    fn unify_atoms(&mut self, left: &Atom, right: &Atom) -> bool {
        if left.predicate != right.predicate || left.len() != right.len() {
            false
        } else {
            left.into_iter()
                .zip(right.into_iter())
                .map(|pair| self.unify(pair.0, pair.1))
                .all(std::convert::identity)
        }
    }

    fn unify(&mut self, left: &Term, right: &Term) -> bool {
        if left == right {
            return true;
        }

        let left_var: Result<Variable, ()> = (*left).try_into();
        if left_var.is_ok() {
            self.set(left_var.unwrap(), *right);
            return true;
        }

        let right_var: Result<Variable, ()> = (*right).try_into();
        if right_var.is_ok() {
            self.set(right_var.unwrap(), *left);
            return true;
        }

        return false;
    }
}

impl Drop for Substitution {
    fn drop(&mut self) {
        debug_assert!(std::thread::panicking() || !self.active)
    }
}

#[cfg(test)]
mod tests {
    use crate::symbols::Constant;

    use super::*;

    #[test]
    fn backtracking() {
        let x0 = Variable::from_index(0);
        let c0: Term = Constant::from_index(0).into();
        let c1: Term = Constant::from_index(1).into();

        let s = &mut Substitution::new();
        s.start();
        s.set(x0, c0);
        assert_eq!(s.get(x0), Some(c0));
        let snapshot = s.snapshot();
        s.set(x0, c1);
        assert_eq!(s.get(x0), Some(c1));
        s.backtrack(snapshot);
        assert_eq!(s.get(x0), Some(c0));
        s.stop();
    }

    #[test]
    #[should_panic]
    fn identity() {
        let x0 = Variable::from_index(0);
        let s = &mut Substitution::new();
        s.start();
        s.set(x0, x0.into());
        s.stop();
    }

    #[test]
    fn match_positive() {
        let x: Variable = Variable::from_index(0);
        let a: Constant = Constant::from_index(0);

        let l: Atom = Atom {
            predicate: 1,
            terms: vec![x.into()].into_boxed_slice(),
        };
        let r: Atom = Atom {
            predicate: 1,
            terms: vec![a.into()].into_boxed_slice(),
        };

        let s = &mut Substitution::new();
        s.start();
        assert!(s.match_atoms(&l, &r));
        assert_eq!(s.get(x), Some(a.into()));
        s.stop();
    }

    #[test]
    #[should_panic]
    fn match_negative() {
        let x: Variable = Variable::from_index(0);
        let a: Constant = Constant::from_index(0);

        let l: Atom = Atom {
            predicate: 1,
            terms: vec![a.into()].into_boxed_slice(),
        };
        let r: Atom = Atom {
            predicate: 1,
            terms: vec![x.into()].into_boxed_slice(),
        };

        let s = &mut Substitution::new();
        s.start();
        assert!(s.match_atoms(&l, &r));
        assert_eq!(s.get(x), Some(a.into()));
        s.stop();
    }
}
