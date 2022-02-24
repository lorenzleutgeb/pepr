// There are lots of unused functions that I sketched,
// let them stick around.
#![allow(dead_code)]

mod bakery;
mod cli;
mod fmt;
mod la;
mod macros;
mod parser;
mod resolution;
mod substitution;
mod subsumption;
mod symbols;
mod unification;

use crate::fmt::DisplayWithSymbols;
use crate::symbols::Term;
use clap::Parser as ClapParser;

use cli::Cli;

use log::warn;
use parser::{FTCNFParser, Rule};
use pest::Parser as PestParser;
use std::fmt::Display;
use std::fs;
use std::iter::IntoIterator;

use std::process;

use string_interner::backend::BucketBackend;
use string_interner::StringInterner;
use symbols::{Predicate, SymbolU32};

type Interner<T> = StringInterner<BucketBackend<T>>;

#[derive(Debug, PartialEq, Eq, PartialOrd, Clone, Copy, Ord, Hash)]
pub enum Typ {
    R,
    I,
    F,
}

impl Typ {
    /// Decides subtyping relation.
    /// Types are subtypes of themselves for convenience.
    fn contains(self, other: Typ) -> bool {
        self == other || (self == Typ::R && other == Typ::I)
    }
}

#[derive(Debug, PartialEq, Eq)]
struct ClausePosition {
    clause: usize,
    literal: usize,
}

impl Display for ClausePosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.clause, self.literal)
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Parents {
    Input,
    Resolvent(ClausePosition, ClausePosition),
}

impl Display for Parents {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Parents::Input => write!(f, "I"),
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

fn main() {
    let cli = Cli::parse();

    env_logger::Builder::new()
        .filter_level(cli.verbosity.log_level_filter())
        .init();

    let unparsed_file = fs::read_to_string(cli.input).expect("cannot read file");
    match FTCNFParser::parse(Rule::input, &unparsed_file) {
        Ok(parse) => {
            let state = State::parse(parse);
            tautology_check(&state);
            resolution(&state);
        }
        Err(e) => {
            eprintln!("Parsing error!");
            eprintln!("{}", e);
            process::exit(1);
        }
    }
}

fn tautology_check(state: &State) {
    for clause in state.clauses.iter() {
        println!("{}", clause.display(&state.symbols));
        match clause.solve(&state.symbols) {
            Some(solution) => {
                println!("{:?}", &solution);
            }
            None => {}
        }
    }
}

fn resolution(state: &State) {
    for unit in state.clauses.iter() {
        if !unit.is_unit() {
            continue;
        }

        /*
        let unit_atom = unit.atoms[0];

        for candidate in state.clauses.iter() {
            let split = candidate.split_at_arrow();
            let candidate_atoms = if unit.arrow == 0 {
                split.0
            } else {
                split.1
            };
            for candidate_atom in candidate_atoms.iter() {
                if candidate_atom.predicate == unit_atom.predicate {
                    println!()
                }
            }
        }
        */
    }
}

struct State {
    symbols: Symbols,
    clauses: Vec<Clause>,
}

impl State {
    fn tautology_check(&mut self) {}
}

struct PredicateData {
    arity: usize,
    // TODO: Also store the types of arguments.
    // typs: Box<[Typ]>,
}

struct Predicates {
    interner: Interner<Predicate>,
    data: Vec<PredicateData>,
}

impl Predicates {
    fn arity(&self, predicate: Predicate) -> usize {
        self.data[predicate].arity
    }
}

struct ConstantData {
    typ: Typ,
}

struct Constants {
    interner: Interner<SymbolU32>,
    data: Vec<ConstantData>,
}

struct Symbols {
    predicates: Predicates,
    constants: Constants,
    variables: Variables,
}

struct VariableData {
    typ: Typ,
}

struct Variables {
    interner: Interner<SymbolU32>,
    data: Vec<VariableData>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum CPredicate {
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

#[derive(Debug, Clone, PartialEq, Eq)]
enum CTerm {
    /// Injection of [Term].
    Inj(Term),

    /// Negation of a [CTerm].
    Neg(Box<[CTerm; 1]>),

    /// Addition on [CTerm]. Restricted to 2 elements for simplicity.
    Add(Box<[CTerm; 2]>),

    /// Subtraction on [CTerm].
    Sub(Box<[CTerm; 2]>),

    /// Multiplication on [CTerm]. Restricted to 2 elements for simplicity.
    Mul(Box<[CTerm; 2]>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct CAtom {
    predicate: CPredicate,
    left: CTerm,
    right: CTerm,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Atom {
    predicate: Predicate,
    terms: Box<[Term]>,
}

impl Atom {
    fn new(predicate: Predicate, terms: Vec<Term>) -> Atom {
        Atom {
            predicate,
            terms: terms.into_boxed_slice(),
        }
    }

    fn propositional(predicate: Predicate) -> Atom {
        Atom {
            predicate,
            terms: Vec::with_capacity(0).into_boxed_slice(),
        }
    }

    fn len(&self) -> usize {
        self.terms.len()
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

struct Clauses {
    clauses: Vec<Clause>,
}

#[derive(Debug, PartialEq, Eq)]
struct Constraint {
    atoms: Box<[CAtom]>,
}

#[derive(Debug, PartialEq, Eq)]
struct Clause {
    id: usize,
    constraint: Constraint,
    atoms: Box<[Atom]>,

    /// Position of the arrow which splits
    /// premises and conclusions in [atoms].
    /// If it is zero, then all atoms are conclusions, i.e. positive literals.
    /// If it is `atoms.len()` then all atoms are premises, i.e. negative literals.
    arrow: usize,

    typs: Box<[Typ]>,
    parents: Parents,
}

impl Clause {
    #[cfg(test)]
    fn dummy(premises: Vec<Atom>, conclusions: Vec<Atom>) -> Clause {
        let arrow = premises.len();
        let atoms = [premises.as_slice(), conclusions.as_slice()]
            .concat()
            .into_boxed_slice();
        Clause {
            id: 0,
            constraint: Constraint {
                atoms: vec![].into_boxed_slice(),
            },
            atoms,
            arrow,
            typs: vec![].into_boxed_slice(),
            parents: Parents::Input,
        }
    }

    #[inline]
    fn split_at_arrow(&self) -> (&[Atom], &[Atom]) {
        self.atoms.split_at(self.arrow)
    }

    #[inline]
    fn is_unit(&self) -> bool {
        self.atoms.len() == 1
    }

    #[inline]
    fn is_horn(&self) -> bool {
        self.arrow <= 1
    }

    #[inline]
    fn is_positive(&self) -> bool {
        self.arrow == 0
    }

    #[inline]
    fn is_negative(&self) -> bool {
        self.arrow == self.atoms.len()
    }

    #[inline]
    fn is_empty(&self) -> bool {
        self.atoms.len() == 0
    }

    #[inline]
    fn conclusions(&self) -> usize {
        self.atoms.len() - self.arrow
    }

    #[inline]
    fn premises(&self) -> usize {
        self.arrow
    }
}

fn bail(display: impl Display, code: i32) {
    eprintln!("{}", display);
    process::exit(code);
}

fn polarity_str(polarity: bool) -> &'static str {
    if polarity {
        ""
    } else {
        "¬"
    }
}
