use std::{default::Default, env::VarError, ops::Index};

use crate::{
    symbols::{Term, Variable},
    Atom, CAtom, CTerm, Clause, Constraint,
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

pub(crate) trait Substitution {
    fn set(&mut self, x: Variable, term: Term);
    fn get(&self, x: Variable) -> Option<Term>;
    fn chase(&self, x: Variable) -> Option<Term> {
        let mut next = x;
        let mut min = x;
        let mut norm = self.get(x);

        norm?;

        loop {
            let once = self.get(next);
            match once {
                Some(Term::Variable(y, _)) => {
                    if y == min || y == x {
                        return norm;
                    } else if y < min {
                        min = y;
                        norm = once;
                        next = y;
                    } else {
                        next = y;
                    }
                }
                None => return norm,
                other => return other,
            }
        }
    }

    fn flatten(&mut self);

    fn match_atoms(&mut self, left: &Atom, right: &Atom) -> bool {
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

impl Term {
    pub(crate) fn substitute(self, sigma: &dyn Substitution) -> Term {
        self.substitute_with_offset(sigma, 0)
    }

    pub(crate) fn substitute_with_offset(self, sigma: &dyn Substitution, offset: usize) -> Term {
        // TODO: Check overflow.
        match self {
            Term::Variable(x, _) => sigma.chase(x + (offset as u32)).unwrap_or(self + offset),
            _ => self,
        }
    }
}

impl Atom {
    pub(crate) fn substitute(&self, sigma: &dyn Substitution) -> Atom {
        self.substitute_with_offset(sigma, 0)
    }

    pub(crate) fn substitute_with_offset(&self, sigma: &dyn Substitution, offset: usize) -> Atom {
        Atom {
            predicate: self.predicate,
            terms: self
                .terms
                .iter()
                .map(|t| t.substitute_with_offset(sigma, offset))
                .collect(),
        }
    }
}

impl CTerm {
    pub(crate) fn substitute(&self, sigma: &dyn Substitution) -> CTerm {
        self.substitute_with_offset(sigma, 0)
    }

    pub(crate) fn substitute_with_offset(&self, sigma: &dyn Substitution, offset: usize) -> CTerm {
        match self {
            CTerm::Inj(t) => CTerm::Inj(t.substitute_with_offset(sigma, offset)),
            CTerm::Add(t1, t2) => CTerm::Add(
                Box::new(t1.substitute_with_offset(sigma, offset)),
                Box::new(t2.substitute_with_offset(sigma, offset)),
            ),
            CTerm::Mul(t1, t2) => CTerm::Mul(
                Box::new(t1.substitute_with_offset(sigma, offset)),
                Box::new(t2.substitute_with_offset(sigma, offset)),
            ),
            CTerm::Sub(t1, t2) => CTerm::Sub(
                Box::new(t1.substitute_with_offset(sigma, offset)),
                Box::new(t2.substitute_with_offset(sigma, offset)),
            ),
            CTerm::Neg(t) => CTerm::Neg(Box::new(t.substitute_with_offset(sigma, offset))),
        }
    }
}

impl CAtom {
    pub(crate) fn substitute(&self, sigma: &dyn Substitution) -> CAtom {
        self.substitute_with_offset(sigma, 0)
    }

    pub(crate) fn substitute_with_offset(&self, sigma: &dyn Substitution, offset: usize) -> CAtom {
        CAtom {
            predicate: self.predicate,
            left: self.left.substitute_with_offset(sigma, offset),
            right: self.right.substitute_with_offset(sigma, offset),
        }
    }
}

impl Constraint {
    pub(crate) fn subsititute(&self, sigma: &dyn Substitution) -> Constraint {
        self.subsititute_with_offset(sigma, 0)
    }

    pub(crate) fn subsititute_with_offset(
        &self,
        sigma: &dyn Substitution,
        offset: usize,
    ) -> Constraint {
        Constraint {
            atoms: self
                .atoms
                .iter()
                .map(|atom| atom.substitute_with_offset(sigma, offset))
                .collect(),
            solution: None,
        }
    }
}

impl Clause {
    pub(crate) fn subsititute(self, sigma: &dyn Substitution) -> Clause {
        self.subsititute_with_offset(sigma, 0)
    }

    pub(crate) fn subsititute_with_offset(self, sigma: &dyn Substitution, offset: usize) -> Clause {
        Clause {
            id: 0,
            constraint: self.constraint.subsititute_with_offset(sigma, offset),
            arrow: self.arrow,
            atoms: self
                .atoms
                .iter()
                .map(|a| a.substitute_with_offset(sigma, offset))
                .collect(),
            typs: self.typs,
            parents: self.parents,
        }
    }
}

pub(crate) trait Snapshots {
    type Snapshot;

    fn snapshot(&self) -> Self::Snapshot;
    fn rollback(&mut self, snapshot: Self::Snapshot);
}

impl Substitution for Vec<Option<Term>> {
    fn set(&mut self, x: Variable, term: Term) {
        let i = x as usize;
        if self.len() <= i {
            self.resize(i + 1, Default::default());
        }
        self[i] = Some(term);
    }

    fn get(&self, x: Variable) -> Option<Term> {
        let i = x as usize;
        if i < self.len() {
            self[i]
        } else {
            None
        }
    }

    fn flatten(&mut self) {
        for i in 0..self.len() {
            let x = i as u32;
            let mut previous = None;
            let initial = self.get(x);
            if initial.is_none() {
                continue;
            }

            let tau = initial.unwrap().get_type();

            let mut y = x;
            let mut min = x;
            let mut flattened: Option<Term>;
            loop {
                let step = self.get(y);
                match step {
                    Some(Term::Variable(z, _)) => {
                        if z == min {
                            flattened = None;
                            break;
                        } else {
                            previous = step;
                            min = z.min(min);
                            y = z;
                            continue;
                        }
                    }
                    None => {
                        flattened = previous;
                        break;
                    }
                    term => {
                        flattened = term;
                        break;
                    }
                }
            }

            if flattened.is_none() && x > min {
                flattened = Some(Term::Variable(min, tau))
            }

            self[i] = flattened;
        }
    }
}

impl Substitution for [Option<Term>] {
    fn set(&mut self, x: Variable, term: Term) {
        let i = x as usize;
        assert!(self.len() > i);
        self[i] = Some(term);
    }

    fn get(&self, x: Variable) -> Option<Term> {
        let i = x as usize;
        if i < self.len() {
            self[i]
        } else {
            None
        }
    }

    fn flatten(&mut self) {
        todo!()
    }
}

struct Tagged<Value, Tag> {
    value: Value,
    tag: Tag,
}

impl<Value, Tag> Clone for Tagged<Value, Tag>
where
    Value: Clone,
    Tag: Clone,
{
    fn clone(&self) -> Self {
        Self {
            value: self.value.clone(),
            tag: self.tag.clone(),
        }
    }
}

impl<Value, Tag> Copy for Tagged<Value, Tag>
where
    Value: Copy,
    Tag: Copy,
{
}

struct TaggedArray<Value, Tag> {
    storage: [Tagged<Value, Tag>; MAX_VARS],
    tag: Tag,
}

#[derive(Default)]
pub(crate) struct SubstitutionWithHistory<T>
where
    T: Substitution + Default,
{
    history: Vec<History>,
    substitution: T,
}

impl<Value: Default + Copy, Tag: Default + Copy> Default for TaggedArray<Value, Tag> {
    fn default() -> Self {
        TaggedArray {
            storage: [Tagged {
                value: Default::default(),
                tag: Default::default(),
            }; MAX_VARS],
            tag: Default::default(),
        }
    }
}

/*/
impl<T: Default> Default for SubstitutionWithHistory<T> {
    fn default() -> Self {
        Self { history: Default::default(), substitution: Default::default() }
    }
}
*/

impl<Value, Tag: Eq> Index<usize> for TaggedArray<Option<Value>, Tag> {
    type Output = Option<Value>;

    fn index(&self, index: usize) -> &Self::Output {
        let mapping = &self.storage[index];
        if mapping.tag != self.tag {
            &None
        } else {
            &mapping.value
        }
    }
}

impl TaggedArray<Option<Term>, usize> {
    pub fn new() -> TaggedArray<Option<Term>, usize> {
        TaggedArray {
            storage: [Tagged {
                value: None,
                tag: 0,
            }; MAX_VARS],
            tag: 0,
        }
    }

    pub fn start(&mut self) {
        self.tag = if self.tag == usize::MAX {
            self.storage = [Tagged {
                value: None,
                tag: 0,
            }; MAX_VARS];
            0
        } else {
            self.tag + 1
        }
    }
}

impl Substitution for TaggedArray<Option<Term>, usize> {
    fn get(&self, variable: Variable) -> Option<Term> {
        let index = variable as usize;
        assert!(index <= MAX_VARS);
        let x = &self.storage[index];

        if x.tag == self.tag {
            x.value
        } else {
            None
        }
    }

    fn set(&mut self, variable: Variable, term: Term) {
        let index = variable as usize;
        assert!(index <= MAX_VARS);

        self.storage[index] = Tagged {
            value: Some(term),
            tag: self.tag,
        }
    }

    fn flatten(&mut self) {
        todo!()
    }
}

impl<T: Substitution + Default> Snapshots for SubstitutionWithHistory<T> {
    type Snapshot = usize;

    #[inline]
    fn snapshot(&self) -> usize {
        self.history.len()
    }

    fn rollback(&mut self, snapshot: usize) {
        #[cfg(debug_assertions)]
        {
            assert!(snapshot <= self.history.len());
        }

        while self.history.len() != snapshot {
            let top = self.history.pop().unwrap();
            if let Some(term) = top.previous {
                self.substitution.set(top.variable, term);
            }
        }
    }
}

impl<T: Substitution + Default> Substitution for SubstitutionWithHistory<T> {
    fn set(&mut self, x: Variable, term: Term) {
        self.history.push(History {
            variable: x,
            previous: self.get(x),
        });
        self.substitution.set(x, term);
    }

    fn get(&self, x: Variable) -> Option<Term> {
        self.substitution.get(x)
    }

    fn flatten(&mut self) {
        todo!()
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

        let mut s: SubstitutionWithHistory<Vec<Option<Term>>> = Default::default();
        //s.start();
        s.set(x0, c0);
        assert_eq!(s.get(x0), Some(c0));
        let snapshot = s.snapshot();
        s.set(x0, c1);
        assert_eq!(s.get(x0), Some(c1));
        s.rollback(snapshot);
        assert_eq!(s.get(x0), Some(c0));
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

        //let s = &mut PersistentSubstitution::new();
        let mut s: SubstitutionWithHistory<Vec<Option<Term>>> = Default::default();
        //s.start();
        assert!(s.match_atoms(&l, &r));
        assert_eq!(s.get(x), Some(a));
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

        //let s = &mut PersistentSubstitution::new();
        let mut s: SubstitutionWithHistory<Vec<Option<Term>>> = Default::default();
        //s.start();
        assert!(s.match_atoms(&l, &r));
        assert_eq!(s.get(x), Some(a));
    }

    #[test]
    fn checked_add() {
        let x: u8 = 254;
        assert!(x.checked_add(1).is_some());
        assert_eq!(x, 254);
    }

    #[test]
    fn start_overflow() {
        let tag = usize::MAX;
        let mut s = TaggedArray {
            tag,
            storage: [Tagged { tag, value: None }; MAX_VARS],
        };
        for i in 0..MAX_VARS {
            s.storage[i] = Tagged {
                tag,
                value: Some(Term::Constant(i as u32, Typ::R)),
            }
        }
        s.start();
        for i in 0..MAX_VARS {
            assert_eq!(s.get(i as u32), None);
            //assert_eq!(s.storage[i], Default::default());
        }
    }

    #[test]
    fn chase() {
        let sigma: Vec<Option<Term>> = vec![None, Some(Term::Variable(0, Typ::F))];
        assert_eq!(sigma.chase(1), Some(Term::Variable(0, Typ::F)))
    }
}
