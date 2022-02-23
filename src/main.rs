// There are lots of unused functions that I sketched,
// let them stick around.
#![allow(dead_code)]

mod cli;
mod macros;
mod parser;
mod substitution;
mod subsumption;
mod symbols;

use crate::symbols::Term;
use clap::Parser as ClapParser;
use cli::{Cli, Commands};
use parser::{FTCNFParser, Rule};
use pest::Parser as PestParser;
use std::convert::TryInto;
use std::fmt::Display;
use std::fs;
use std::iter::IntoIterator;
use std::process;
use string_interner::backend::BucketBackend;
use string_interner::StringInterner;
use symbols::{Constant, Variable};

type Interner<T> = StringInterner<BucketBackend<T>>;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Typ {
    R,
    I,
    F,
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

    // You can check the value provided by positional arguments, or option arguments
    if let Some(name) = cli.name.as_deref() {
        println!("Value for name: {}", name);
    }

    if let Some(config_path) = cli.config.as_deref() {
        println!("Value for config: {}", config_path.display());
    }

    // You can see how many times a particular flag or argument occurred
    // Note, only flags can have multiple occurrences
    match cli.debug {
        0 => println!("Debug mode is off"),
        1 => println!("Debug mode is kind of on"),
        2 => println!("Debug mode is on"),
        _ => println!("Don't be crazy"),
    }

    // You can check for the existence of subcommands, and if found use their
    // matches just as you would the top level cmd
    match &cli.command {
        Some(Commands::Horn { input }) => {
            let unparsed_file = fs::read_to_string(input).expect("cannot read file");
            match FTCNFParser::parse(Rule::input, &unparsed_file) {
                Ok(parse) => {
                    // println!("{:?}", parse);
                    let state = State::parse(parse);
                    resolution(state);
                }
                Err(e) => {
                    eprintln!("Parsing error!");
                    eprintln!("{}", e);
                    process::exit(1);
                }
            }
        }
        Some(Commands::Hammer {}) => {
            println!("Hammertime!");
        }
        None => {
            eprintln!("Please specify a subcommand.");
            process::exit(1);
        }
    }
}

fn resolution(state: State) {
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
    interner: Interner<Constant>,
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
    interner: Interner<Variable>,
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
    Idy(Term),
    Neg(Term),
    Add(Term, Term),
    Sub(Term, Term),
    Mul(Term, Term),
}

#[derive(Debug, Clone)]
struct CAtom {
    predicate: CPredicate,
    left: Term,
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

impl Symbols {
    fn format_cterm(&self, term: &CTerm) -> String {
        match &term {
            CTerm::Idy(t) => self.format_term(t),
            CTerm::Add(l, r) => format!("+({}, {})", self.format_term(l), self.format_term(r)),
            CTerm::Mul(l, r) => format!("*({}, {})", self.format_term(l), self.format_term(r)),
            CTerm::Sub(l, r) => format!("-({}, {})", self.format_term(l), self.format_term(r)),
            CTerm::Neg(t) => format!("-{}", self.format_term(t)),
        }
    }

    fn format_catom(&self, atom: &CAtom) -> String {
        format!(
            "{} {} {}",
            self.format_term(&atom.left),
            atom.predicate,
            self.format_cterm(&atom.right),
        )
    }

    fn format_term(&self, t: &Term) -> String {
        match (*t).try_into() {
            Ok(c) => self.constants.interner.resolve(c).unwrap().to_owned(),
            Err(_) => match (*t).try_into() {
                Ok::<Variable, _>(v) => v.to_string(),
                Err(_) => unreachable!(),
            },
        }
    }

    fn format(&self, atom: &Atom) -> String {
        let terms = atom
            .terms
            .iter()
            .map(|&t| self.format_term(&t))
            .collect::<Vec<String>>()
            .join(", ");
        format!(
            "{}({})",
            self.predicates.interner.resolve(atom.predicate).unwrap(),
            terms
        )
    }

    fn format_clause(&self, clause: &Clause) -> String {
        let (premises, conclusions) = clause.split_at_arrow();

        let constraint_str = if clause.constraint.atoms.is_empty() {
            String::from("")
        } else {
            clause
                .constraint
                .atoms
                .iter()
                .map(|atom| self.format_catom(atom))
                .collect::<Vec<String>>()
                .join(", ")
                + " ∥ "
        };

        let premise_str = premises
            .iter()
            .map(|atom| self.format(atom))
            .collect::<Vec<String>>()
            .join(", ");

        let conclusion_str = conclusions
            .iter()
            .map(|atom| self.format(atom))
            .collect::<Vec<String>>()
            .join(", ");

        let sorts = clause
            .typs
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<String>>()
            .join("");

        format!(
            "{}:{}:{}:{}:{}: {}{} → {}.",
            clause.id,
            clause.atoms.len(),
            clause.typs.len(),
            clause.parents,
            sorts,
            constraint_str,
            premise_str,
            conclusion_str
        )
    }
}

#[derive(Debug)]
struct Constraint {
    atoms: Box<[CAtom]>,
    problem: minilp::Problem,
}

// TODO: Improve locality of a clause. We want it to have a contiguous layout in memory.
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
