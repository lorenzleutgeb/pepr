use std::fmt::Display;
use std::num::NonZeroUsize;

use crate::util::default_combine;

use crate::substitution::Substitution;

pub type Variable = u32;
pub type Constant = u32;
pub type Integer = i32;
pub type Predicate = usize;

/// FO predicate symbol used for axiomatisation of LRA predicate '!='.
/// This does not parse as FO predicate symbol on purpose.
pub const NEQ: &str = "≠";

#[derive(Debug, PartialEq, Eq, PartialOrd, Clone, Copy, Ord, Hash)]
pub(crate) enum Typ {
    R,
    I,
    F,
}

impl Default for Typ {
    fn default() -> Self {
        Typ::R
    }
}

impl Typ {
    /// Decides subtyping relation.
    /// Types are subtypes of themselves for convenience.
    pub(crate) fn contains(self, other: Typ) -> bool {
        self == other || (self == Typ::R && other == Typ::I)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum Term {
    Variable(Variable, Typ),
    Constant(Constant, Typ),
    Integer(Integer),
}

impl Term {
    /// Initializes a variable of type 'R'.
    #[inline]
    pub(crate) fn r(variable: Variable) -> Term {
        Term::Variable(variable, Typ::R)
    }

    /// Initializes a variable of type 'F'.
    #[inline]
    pub(crate) fn f(variable: Variable) -> Term {
        Term::Variable(variable, Typ::F)
    }

    /// Initializes a variable of type 'I'.
    #[inline]
    pub(crate) fn i(variable: Variable) -> Term {
        Term::Variable(variable, Typ::I)
    }

    pub(crate) fn get_type(self) -> Typ {
        match self {
            Term::Variable(_, tau) => tau,
            Term::Constant(_, tau) => tau,
            Term::Integer(_) => Typ::I,
        }
    }

    pub(crate) fn max_var(&self) -> Option<Variable> {
        match *self {
            Term::Variable(x, _) => Some(x),
            _ => None,
        }
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Constant(c, _) => write!(f, "c{}", c),
            Term::Variable(v, _) => write!(f, "x{}", v),
            Term::Integer(i) => write!(f, "{}", i),
        }
    }
}

impl From<Integer> for Term {
    #[inline]
    fn from(i: Integer) -> Self {
        Term::Integer(i)
    }
}

impl TryInto<Integer> for Term {
    type Error = ();

    fn try_into(self) -> Result<Integer, Self::Error> {
        if let Term::Integer(i) = self {
            Ok(i)
        } else {
            Err(())
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum CPredicate {
    Eq,
    Ne,
    Le,
    Lt,
    Ge,
    Gt,
}

impl Display for CPredicate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CPredicate::Eq => write!(f, "="),
            CPredicate::Ne => write!(f, "!="),
            CPredicate::Le => write!(f, "<="),
            CPredicate::Lt => write!(f, "<"),
            CPredicate::Ge => write!(f, ">="),
            CPredicate::Gt => write!(f, ">"),
        }
    }
}

impl CPredicate {
    pub(crate) fn negate(&self) -> CPredicate {
        match self {
            CPredicate::Eq => CPredicate::Ne,
            CPredicate::Ne => CPredicate::Eq,
            CPredicate::Le => CPredicate::Gt,
            CPredicate::Lt => CPredicate::Ge,
            CPredicate::Ge => CPredicate::Lt,
            CPredicate::Gt => CPredicate::Le,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum CTerm {
    /// Injection of [Term].
    Inj(Term),

    /// Negation of a [CTerm].
    Neg(Box<CTerm>),

    /// Addition on [CTerm]. Restricted to 2 elements for simplicity.
    Add(Box<CTerm>, Box<CTerm>),

    /// Subtraction on [CTerm].
    Sub(Box<CTerm>, Box<CTerm>),

    /// Multiplication on [CTerm]. Restricted to 2 elements for simplicity.
    Mul(Box<CTerm>, Box<CTerm>),
}

impl Display for CTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CTerm::Inj(term) => write!(f, "{}", term),
            CTerm::Neg(t) => write!(f, "-({})", t),
            CTerm::Add(t1, t2) => write!(f, "+({},{})", t1, t2),
            CTerm::Sub(t1, t2) => write!(f, "-({},{})", t1, t2),
            CTerm::Mul(t1, t2) => write!(f, "*({},{})", t1, t2),
        }
    }
}

impl CTerm {
    pub(crate) fn is_immdediate(&self) -> bool {
        match self {
            CTerm::Inj(_) => true,
            _ => false,
        }
    }

    pub(crate) fn max_var(&self) -> Option<Variable> {
        match self {
            CTerm::Inj(t) => t.max_var(),
            CTerm::Add(t1, t2) => default_combine(|x, y| x.max(y), t1.max_var(), t2.max_var()),
            CTerm::Mul(t1, t2) => default_combine(|x, y| x.max(y), t1.max_var(), t2.max_var()),
            CTerm::Sub(t1, t2) => default_combine(|x, y| x.max(y), t1.max_var(), t2.max_var()),
            CTerm::Neg(t) => t.max_var(),
        }
    }

    pub(crate) fn var_occurrences(&self) -> Vec<Variable> {
        match self {
            CTerm::Inj(Term::Variable(x, _)) => vec![*x],
            CTerm::Inj(_) => vec![],
            CTerm::Add(t1, t2) => {
                let mut result: Vec<Variable> = t1.var_occurrences();
                result.append(&mut t2.var_occurrences());
                result
            }
            CTerm::Mul(t1, t2) => {
                let mut result: Vec<Variable> = t1.var_occurrences();
                result.append(&mut t2.var_occurrences());
                result
            }
            CTerm::Sub(t1, t2) => {
                let mut result: Vec<Variable> = t1.var_occurrences();
                result.append(&mut t2.var_occurrences());
                result
            }
            CTerm::Neg(t) => t.var_occurrences(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CAtom {
    pub(crate) predicate: CPredicate,
    pub(crate) left: CTerm,
    pub(crate) right: CTerm,
}

impl Display for CAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({},{})", self.predicate, self.left, self.right)
    }
}

impl CAtom {
    pub(crate) fn max_var(&self) -> Option<u32> {
        let left = self.left.max_var();
        let right = self.right.max_var();
        default_combine(|x, y| x.max(y), left, right)
    }

    pub(crate) fn negate(&self) -> Vec<CAtom> {
        if matches!(self.predicate, CPredicate::Eq) {
            vec![
                CAtom {
                    predicate: CPredicate::Gt,
                    left: self.left.clone(),
                    right: self.right.clone(),
                },
                CAtom {
                    predicate: CPredicate::Lt,
                    left: self.left.clone(),
                    right: self.right.clone(),
                },
            ]
        } else {
            vec![CAtom {
                predicate: self.predicate.negate(),
                left: self.left.clone(),
                right: self.right.clone(),
            }]
        }
    }

    pub(crate) fn var_occurrences(&self) -> Vec<Variable> {
        let mut result = self.left.var_occurrences();
        result.append(&mut self.right.var_occurrences());
        result
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Atom {
    pub(crate) predicate: Predicate,
    pub(crate) terms: Box<[Term]>,
}

impl Atom {
    pub(crate) fn new(predicate: Predicate, terms: Vec<Term>) -> Atom {
        Atom {
            predicate,
            terms: terms.into_boxed_slice(),
        }
    }

    pub(crate) fn propositional(predicate: Predicate) -> Atom {
        Atom {
            predicate,
            terms: Vec::with_capacity(0).into_boxed_slice(),
        }
    }

    pub(crate) fn len(&self) -> usize {
        self.terms.len()
    }

    pub(crate) fn count_vars(&self) -> usize {
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

    pub(crate) fn max_var(&self) -> Option<&u32> {
        self.terms
            .iter()
            .filter_map(|term| {
                if let Term::Variable(x, _) = term {
                    Some(x)
                } else {
                    None
                }
            })
            .max()
    }
}

#[cfg(test)]
impl From<Vec<Term>> for Atom {
    fn from(terms: Vec<Term>) -> Self {
        Atom::new(0, terms)
    }
}

#[cfg(test)]
impl From<Predicate> for Atom {
    fn from(predicate: Predicate) -> Self {
        Atom::propositional(predicate)
    }
}

impl<'a> IntoIterator for &'a Atom {
    type Item = <std::slice::Iter<'a, Term> as Iterator>::Item;
    type IntoIter = std::slice::Iter<'a, Term>;

    fn into_iter(self) -> Self::IntoIter {
        self.terms.iter()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum ClauseId {
    Input(u32),
    Axiom(u32),
    Resolvent(u32),
}

impl Into<u32> for &ClauseId {
    fn into(self) -> u32 {
        match self {
            ClauseId::Input(x) | ClauseId::Axiom(x) | ClauseId::Resolvent(x) => *x,
        }
    }
}

impl Display for ClauseId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let modifier = match self {
            ClauseId::Input(_) => "INP",
            ClauseId::Axiom(_) => "AXM",
            ClauseId::Resolvent(_) => "",
        };
        write!(f, "{}{}", modifier, <&Self as Into<u32>>::into(self))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct ClausePosition {
    pub(crate) clause: usize,
    pub(crate) literal: usize,
}

impl ClausePosition {
    pub(crate) fn new(clause: usize, literal: usize) -> ClausePosition {
        ClausePosition { clause, literal }
    }
}

impl Display for ClausePosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Add one to get human-readable atom indices.
        write!(f, "{}.{}", self.clause, self.literal + 1)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Parents {
    Axiom,
    Input,
    Resolvent(ClausePosition, ClausePosition),
}

impl Display for Parents {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Parents::Axiom => write!(f, "AXM"),
            Parents::Input => write!(f, "INP"),
            Parents::Resolvent(p1, p2) => write!(f, "{}-{}", p1, p2),
        }
    }
}

impl Default for Parents {
    fn default() -> Self {
        Parents::Input
    }
}

impl Display for Typ {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Typ::R => write!(f, "R"),
            Typ::I => write!(f, "I"),
            Typ::F => write!(f, "F"),
        }
    }
}

pub(crate) struct Constraint {
    pub(crate) atoms: Box<[CAtom]>,

    // TODO: We cannot translate a MinilpSolution back to a substitution.
    pub(crate) solution: Option<good_lp::solvers::minilp::MiniLpSolution>,
}

impl std::fmt::Debug for Constraint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Constraint")
            .field("atoms", &self.atoms)
            .field("solution", &self.solution.is_some())
            .finish()
    }
}

impl Constraint {
    pub(crate) fn new(atoms: Box<[CAtom]>) -> Constraint {
        Constraint {
            atoms,
            solution: None,
        }
    }
}

#[derive(Debug)]
pub(crate) struct Clause {
    pub(crate) id: usize,
    pub(crate) constraint: Constraint,
    pub(crate) atoms: Box<[Atom]>,

    /// Position of the arrow which splits
    /// premises and conclusions in [atoms].
    /// If it is zero, then all atoms are conclusions, i.e. positive literals.
    /// If it is `atoms.len()` then all atoms are premises, i.e. negative literals.
    pub(crate) arrow: usize,

    pub(crate) typs: Box<[Typ]>,
    pub(crate) parents: Parents,
}

impl Display for Clause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let split = self.split_at_arrow();
        write!(
            f,
            "{:?} || {:?} -> {:?}",
            self.constraint.atoms, split.0, split.1
        )
    }
}

impl Clause {
    fn len(&self) -> usize {
        self.atoms.len()
    }

    #[cfg(test)]
    pub(crate) fn dummy(premises: Vec<Atom>, conclusions: Vec<Atom>) -> Clause {
        let arrow = premises.len();
        let atoms = [premises.as_slice(), conclusions.as_slice()]
            .concat()
            .into_boxed_slice();

        let vars = atoms
            .iter()
            .map(|a| a.max_var())
            .max()
            .map_or(0, |max| max.map_or(0, |v| (*v + 1) as usize));
        let mut sigma: Vec<Option<Term>> = vec![None; vars];

        for atom in atoms.iter() {
            for term in atom.terms.iter() {
                if let Term::Variable(x, _) = term {
                    sigma.set(*x, *term)
                }
            }
        }

        Clause {
            id: 0,
            constraint: Constraint {
                atoms: vec![].into_boxed_slice(),
                solution: None,
            },
            atoms,
            arrow,
            typs: (0..vars)
                .map(|x| {
                    if let Some(term) = sigma.chase(x as u32) {
                        term.get_type()
                    } else {
                        Default::default()
                    }
                })
                .collect(),
            parents: Parents::Input,
        }
    }

    pub(crate) fn max_var(&self) -> Option<&u32> {
        if self.atoms.is_empty() {
            None
        } else {
            self.atoms.iter().map(|a| a.max_var()).max().unwrap_or(None)
        }
    }

    #[inline]
    pub(crate) fn split_at_arrow(&self) -> (&[Atom], &[Atom]) {
        self.atoms.split_at(self.arrow)
    }

    #[inline]
    pub(crate) fn is_unit(&self) -> bool {
        self.atoms.len() == 1
    }

    #[inline]
    pub(crate) fn is_horn(&self) -> bool {
        self.arrow <= 1
    }

    #[inline]
    pub(crate) fn is_positive(&self) -> bool {
        self.arrow == 0
    }

    #[inline]
    pub(crate) fn is_negative(&self) -> bool {
        self.arrow == self.atoms.len()
    }

    #[inline]
    pub(crate) fn is_empty(&self) -> bool {
        self.atoms.len() == 0
    }

    #[inline]
    pub(crate) fn conclusions(&self) -> usize {
        self.atoms.len() - self.arrow
    }

    #[inline]
    pub(crate) fn premises(&self) -> usize {
        self.arrow
    }

    #[inline]
    pub(crate) fn polarity(&self, index: usize) -> bool {
        index >= self.arrow
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::mem::size_of;

    #[test]
    fn term_fits_pointer() {
        assert!(size_of::<Term>() == size_of::<usize>());
    }

    #[test]
    fn term_option_is_free() {
        assert!(size_of::<Term>() == size_of::<Option<Term>>());
    }
}
