use std::convert::TryInto;

use crate::{symbols::Term, Atom};

const MAX_VARS: usize = 256;

#[derive(Clone, Copy)]
struct Mapping {
    term: Option<Term>,
    generation: usize,
}

struct History {
    variable: u32,
    previous: Option<Term>,
}

impl History {
    fn identity(variable: u32) -> History {
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
        #[cfg(debug_assertions)]
        {
            assert!(!self.active);
            self.active = true;
        }
        self.generation = self.generation.wrapping_add(1);
    }

    pub fn stop(&mut self) {
        #[cfg(debug_assertions)]
        {
            assert!(self.active);
            self.active = false;
        }
        self.history.clear();
    }

    fn get(&self, variable: u32) -> Option<Term> {
        #[cfg(debug_assertions)]
        {
            assert!(self.active);
        }

        let i = variable as usize;

        assert!(i <= MAX_VARS);

        let x = &self.mapping[i];

        if x.generation == self.generation {
            x.term
        } else {
            None
        }
    }

    fn set(&mut self, variable: u32, term: Term) {
        #[cfg(debug_assertions)]
        {
            assert!(self.active);
            if let Term::Variable(v, _) = term {
                assert_ne!(v, variable)
            }
        }

        let i = variable as usize;

        assert!(i <= MAX_VARS);

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
        {
            debug_assert!(std::thread::panicking() || !self.active)
        }
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
        let x = 0;
        let a = Term::Constant(0, Typ::R);

        let l: Atom = Atom {
            predicate: 1,
            terms: vec![a.into()].into_boxed_slice(),
        };
        let r: Atom = Atom {
            predicate: 1,
            terms: vec![Term::Variable(x, Typ::R)].into_boxed_slice(),
        };

        let s = &mut Substitution::new();
        s.start();
        assert!(s.match_atoms(&l, &r));
        assert_eq!(s.get(x), Some(a.into()));
        s.stop();
    }
}
