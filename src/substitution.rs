use std::default::Default;

use crate::{
    symbols::{Term, Variable},
    Atom,
};

const MAX_VARS: usize = 256;

#[derive(Clone, Copy, Default, Debug, Eq, PartialEq)]
struct Mapping {
    term: Option<Term>,
    generation: usize,
}

#[derive(Clone, Copy, Default, Debug)]
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

impl Default for Substitution {
    fn default() -> Self {
        Self {
            mapping: [Default::default(); MAX_VARS],
            generation: Default::default(),
            history: Default::default(),

            #[cfg(debug_assertions)]
            active: Default::default(),
        }
    }
}

impl Substitution {
    pub fn new() -> Substitution {
        Default::default()
    }

    pub fn start(&mut self) {
        #[cfg(debug_assertions)]
        {
            assert!(!self.active);
            self.active = true;
        }
        self.generation = if self.generation == usize::MAX {
            self.mapping = [Default::default(); MAX_VARS];
            0
        } else {
            self.generation + 1
        }
    }

    pub fn stop(&mut self) {
        #[cfg(debug_assertions)]
        {
            assert!(self.active);
            self.active = false;
        }
        self.history.clear();
    }

    fn get(&self, variable: Variable) -> Option<Term> {
        #[cfg(debug_assertions)]
        {
            assert!(self.active);
        }

        let index = variable as usize;
        assert!(index <= MAX_VARS);
        let x = &self.mapping[index];

        if x.generation == self.generation {
            x.term
        } else {
            None
        }
    }

    fn set(&mut self, variable: Variable, term: Term) {
        #[cfg(debug_assertions)]
        {
            assert!(self.active);
            if let Term::Variable(v, _) = term {
                assert_ne!(v, variable)
            }
        }

        let index = variable as usize;
        assert!(index <= MAX_VARS);

        if self.mapping[index].generation == self.generation {
            self.history.push(History {
                variable,
                previous: self.mapping[index].term,
            });
        } else {
            self.history.push(History::identity(variable))
        }

        self.mapping[index] = Mapping {
            term: Some(term),
            generation: self.generation,
        }
    }

    #[inline]
    pub fn snapshot(&self) -> usize {
        #[cfg(debug_assertions)]
        {
            assert!(self.active);
        }
        self.history.len()
    }

    pub fn backtrack(&mut self, snapshot: usize) {
        #[cfg(debug_assertions)]
        {
            assert!(self.active);
            assert!(snapshot <= self.history.len());
        }

        while self.history.len() != snapshot {
            let top = self.history.pop().unwrap();
            let i = top.variable as usize;

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
            true
        } else {
            match *left {
                Term::Variable(v, _) => match self.get(v) {
                    Some(mapped) => mapped == *right,
                    None => {
                        self.set(v, *right);
                        true
                    }
                },
                _ => false,
            }
        }
    }
}

impl Drop for Substitution {
    fn drop(&mut self) {
        #[cfg(debug_assertions)]
        debug_assert!(std::thread::panicking() || !self.active)
    }
}

#[cfg(test)]
mod tests {
    use crate::Typ;

    use super::*;

    #[test]
    fn backtracking() {
        let x0 = 0;
        let c0: Term = Term::Constant(0, Typ::R);
        let c1: Term = Term::Constant(1, Typ::R);

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
        let x0 = 0;
        let s = &mut Substitution::new();
        s.start();
        s.set(x0, Term::Variable(x0, Typ::R));
        s.stop();
    }

    #[test]
    fn match_positive() {
        let x = 0;
        let a = Term::Constant(0, Typ::R);

        let l: Atom = Atom {
            predicate: 1,
            terms: vec![Term::Variable(x, Typ::R)].into_boxed_slice(),
        };
        let r: Atom = Atom {
            predicate: 1,
            terms: vec![a].into_boxed_slice(),
        };

        let s = &mut Substitution::new();
        s.start();
        assert!(s.match_atoms(&l, &r));
        assert_eq!(s.get(x), Some(a));
        s.stop();
    }

    #[test]
    #[should_panic]
    fn match_negative() {
        let x = 0;
        let a = Term::Constant(0, Typ::R);

        let l: Atom = Atom {
            predicate: 1,
            terms: vec![a].into_boxed_slice(),
        };
        let r: Atom = Atom {
            predicate: 1,
            terms: vec![Term::Variable(x, Typ::R)].into_boxed_slice(),
        };

        let s = &mut Substitution::new();
        s.start();
        assert!(s.match_atoms(&l, &r));
        assert_eq!(s.get(x), Some(a));
        s.stop();
    }

    #[test]
    fn checked_add() {
        let x: u8 = 254;
        assert!(x.checked_add(1).is_some());
        assert_eq!(x, 254);
    }

    #[test]
    fn start_overflow() {
        let generation = usize::MAX;
        let mut s = Substitution {
            generation,
            mapping: [Mapping {
                generation,
                term: None,
            }; MAX_VARS],
            history: vec![],
            active: false,
        };
        for i in 0..MAX_VARS {
            s.mapping[i] = Mapping {
                generation,
                term: Some(Term::Constant(i as u32, Typ::R)),
            }
        }
        s.start();
        for i in 0..MAX_VARS {
            assert_eq!(s.get(i as u32), None);
            assert_eq!(s.mapping[i], Default::default());
        }
        s.stop();
    }
}
