#![feature(test)]

use crate::{symbols::*, *};

use ::dimacs::{parse_dimacs, Clause as DimacsClause, Instance, ParseError, Sign};

#[derive(Debug)]
pub(crate) enum DimacsError {
    ParseError(ParseError),
    FileTypeNotSupported,
}

impl State {
    pub(crate) fn parse_dimacs(input: &str) -> Result<State, DimacsError> {
        let instance = parse_dimacs(input).map_err(DimacsError::ParseError)?;

        if let Instance::Cnf { num_vars, clauses } = instance {
            Ok(State::from_instance(num_vars, clauses))
        } else {
            Err(DimacsError::FileTypeNotSupported)
        }
    }

    fn from_instance(num_vars: u64, clauses: Box<[DimacsClause]>) -> State {
        // NOTE: Predicate '0' is wasted.

        let data = vec![PredicateData { arity: 0 }; (num_vars + 1) as usize];

        let mut interner: Interner<usize> = Interner::new();
        for i in 0..=num_vars {
            interner.get_or_intern(i.to_string());
        }

        let mut result = State {
            symbols: Symbols {
                predicates: Predicates { interner, data },
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

        for (id, clause) in clauses.iter().enumerate() {
            let mut premises: Vec<Atom> = vec![];
            let mut conclusions: Vec<Atom> = vec![];

            for lit in clause.lits() {
                let atom = Atom::propositional(lit.var().to_u64() as usize);
                match lit.sign() {
                    Sign::Pos => conclusions.push(atom),
                    Sign::Neg => premises.push(atom),
                }
            }

            let mut atoms = premises;
            let arrow = atoms.len();
            atoms.append(&mut conclusions);

            result.ingest_clause(Clause {
                id,
                constraint: Constraint {
                    atoms: vec![].into_boxed_slice(),
                    solution: None,
                },
                atoms: atoms.into_boxed_slice(),
                arrow,
                typs: vec![].into_boxed_slice(),
                parents: Parents::Input,
            })
        }

        result
    }
}
