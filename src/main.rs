// There are lots of unused functions that I sketched,
// let them stick around.
#![allow(dead_code)]

mod cli;
mod fmt;
mod index;
mod la;
mod macros;
mod normalize;
mod parser;
mod resolution;
mod saturation;
mod substitution;
mod subsumption;
mod symbols;
mod test;
mod unification;
mod util;

use crate::symbols::Term;
use clap::Parser as ClapParser;

use cli::Cli;

use log::warn;
use parser::{FTCNFParser, Rule};
use pest::Parser as PestParser;
use std::fs;
use std::iter::IntoIterator;
use std::{fmt::Display, vec};
use substitution::Substitution;
use util::default_combine;

use std::process;

use string_interner::backend::BucketBackend;
use string_interner::StringInterner;
use symbols::*;

use std::num::NonZeroU32;

use string_interner::Symbol;

type Interner<T> = StringInterner<BucketBackend<T>>;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct SymbolU32 {
    value: NonZeroU32,
}

impl TryFrom<u32> for SymbolU32 {
    type Error = std::num::TryFromIntError;

    #[inline]
    fn try_from(value: u32) -> Result<Self, Self::Error> {
        NonZeroU32::try_from(value.wrapping_add(1)).map(|value| Self { value })
    }
}

impl Into<u32> for SymbolU32 {
    #[inline]
    fn into(self) -> u32 {
        self.value.get() - 1
    }
}

impl Into<usize> for SymbolU32 {
    #[inline]
    fn into(self) -> usize {
        <SymbolU32 as Into<u32>>::into(self) as usize
    }
}

impl Symbol for SymbolU32 {
    #[inline]
    fn try_from_usize(index: usize) -> Option<Self> {
        // TODO: What if index > u32::MAX?
        if index >= u32::MAX as usize {
            panic!("reached maximum number of symbols")
        }
        Self::try_from(index as u32).ok()
    }

    #[inline]
    fn to_usize(self) -> usize {
        <SymbolU32 as Into<usize>>::into(self)
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
            let mut state = State::parse(parse);
            state.saturate();
        }
        Err(e) => {
            eprintln!("Parsing error!");
            eprintln!("{}", e);
            process::exit(1);
        }
    }
}

#[derive(Debug)]
struct Index {
    /// Index keys are predicates. Indexed values are positive occurrences.
    positive: Vec<Vec<ClausePosition>>,

    /// Index keys are predicates. Indexed values are negative occurrences.
    negative: Vec<Vec<ClausePosition>>,

    units: Vec<usize>,
}

#[derive(Debug)]
struct State {
    symbols: Symbols,
    clauses: Vec<Clause>,
    index: Index,
}

impl State {
    fn new() -> State {
        let mut result = State {
            symbols: Symbols {
                predicates: Predicates {
                    interner: Interner::new(),
                    data: vec![],
                },
                constants: Constants {
                    interner: StringInterner::new(),
                    data: vec![],
                },
                variables: Variables {
                    interner: StringInterner::new(),
                    data: vec![],
                },
            },
            clauses: vec![],
            index: Index {
                positive: vec![],
                negative: vec![],
                units: vec![],
            },
        };

        result.add_axioms();
        result
    }

    fn add_axioms(&mut self) {
        debug_assert!(self.clauses.is_empty());
        debug_assert!(self.symbols.constants.data.is_empty());
        debug_assert!(self.symbols.constants.interner.len() == 0);
        let neq = self.symbols.constants.interner.get_or_intern_static("Neq");

        self.clauses.push(Clause {
            constraint: Constraint {
                atoms: vec![CAtom {
                    predicate: CPredicate::Gt,
                    left: CTerm::Inj(Term::Variable(0, Typ::R)),
                    right: CTerm::Inj(Term::Variable(1, Typ::R)),
                }]
                .into_boxed_slice(),
                solution: None,
            },
            atoms: vec![Atom::new(
                neq.into(),
                vec![Term::Variable(0, Typ::R), Term::Variable(1, Typ::R)],
            )]
            .into_boxed_slice(),
            id: 0,
            arrow: 0,
            typs: vec![Typ::R, Typ::R].into_boxed_slice(),
            parents: Parents::Axiom,
        });

        self.clauses.push(Clause {
            constraint: Constraint {
                atoms: vec![CAtom {
                    predicate: CPredicate::Gt,
                    left: CTerm::Inj(Term::Variable(1, Typ::R)),
                    right: CTerm::Inj(Term::Variable(0, Typ::R)),
                }]
                .into_boxed_slice(),
                solution: None,
            },
            atoms: vec![Atom::new(
                neq.into(),
                vec![Term::Variable(0, Typ::R), Term::Variable(1, Typ::R)],
            )]
            .into_boxed_slice(),
            id: 0,
            arrow: 0,
            typs: vec![Typ::R, Typ::R].into_boxed_slice(),
            parents: Parents::Axiom,
        });
    }
}

#[derive(Debug)]
struct PredicateData {
    arity: usize,
    // TODO: Also store the types of arguments.
    // typs: Box<[Typ]>,
}

#[derive(Debug)]
struct Predicates {
    interner: Interner<Predicate>,
    data: Vec<PredicateData>,
}

impl Predicates {
    fn arity(&self, predicate: Predicate) -> usize {
        self.data[predicate].arity
    }
}

#[derive(Debug)]
struct ConstantData {
    typ: Typ,
}

#[derive(Debug)]
struct Constants {
    interner: Interner<SymbolU32>,
    data: Vec<ConstantData>,
}

#[derive(Debug)]
struct VariableData {
    typ: Typ,
}

#[derive(Debug)]
struct Variables {
    interner: Interner<SymbolU32>,
    data: Vec<VariableData>,
}

#[derive(Debug)]
struct Symbols {
    predicates: Predicates,
    constants: Constants,
    variables: Variables,
}

fn bail(display: impl Display, code: i32) {
    eprintln!("{}", display);
    process::exit(code);
}
