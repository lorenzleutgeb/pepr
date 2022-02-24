// There are lots of unused functions that I sketched,
// let them stick around.
#![allow(dead_code)]

mod bakery;
mod cli;
mod fmt;
mod la;
mod macros;
mod parser;
mod substitution;
mod subsumption;
mod symbols;

use crate::fmt::DisplayWithSymbols;
use crate::symbols::{Constant, Integer, Term, Variable};
use clap::Parser as ClapParser;
use clap_verbosity_flag::Verbosity;
use cli::Cli;
use core::mem::size_of;
use good_lp::{Expression, IntoAffineExpression, ProblemVariables, VariableDefinition};
use log::{debug, error, info, trace, warn};
use parser::{FTCNFParser, Rule};
use pest::Parser as PestParser;
use std::fmt::Display;
use std::fs;
use std::iter::IntoIterator;
use std::ops::Mul;
use std::process;
use std::str::FromStr;
use std::{convert::TryInto, ops::Deref};
use string_interner::backend::BucketBackend;
use string_interner::StringInterner;
use symbols::SymbolU32;

type Interner<T> = StringInterner<BucketBackend<T>>;

#[derive(Debug, PartialEq, Eq, PartialOrd, Clone, Copy, Ord, Hash)]
pub enum Typ {
    R,
    I,
    F,
}

impl Typ {
    /// Decides subtyping relation.
    fn contains(self, other: Typ) -> bool {
        match (self, other) {
            (Typ::R, Typ::I) => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
struct ClausePosition {
    clause: usize,
    literal: usize,
}

impl Display for ClausePosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.clause, self.literal)
    }
}

#[derive(Debug)]
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
    interner: Interner<usize>,
    data: Vec<PredicateData>,
}

impl Predicates {
    fn arity(&self, predicate: usize) -> usize {
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
struct CAtom {
    predicate: CPredicate,
    left: CTerm,
    right: CTerm,
}

#[derive(Debug, Clone)]
struct Atom {
    predicate: usize,
    terms: Box<[Term]>,
}

impl Atom {
    fn len(&self) -> usize {
        self.terms.len()
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

#[derive(Debug)]
struct Constraint {
    atoms: Box<[CAtom]>,
    problem: minilp::Problem,
}

#[derive(Debug)]
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
                problem: minilp::Problem::new(minilp::OptimizationDirection::Minimize),
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
