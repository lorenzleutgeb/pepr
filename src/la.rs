//! Test

use crate::fmt::DisplayWithSymbols;
use crate::{symbols::*, *};
use clap::Parser as ClapParser;
use good_lp::solvers::minilp::MiniLpSolution;
use good_lp::Variable as LPVariable;
use good_lp::{Expression, IntoAffineExpression, ProblemVariables, VariableDefinition};
use good_lp::{ResolutionError, Solution, SolverModel};
use std::env::var;
use std::fs;
use std::intrinsics::transmute;
use std::ops::{AddAssign, Mul};
use std::str::FromStr;
use std::{convert::TryInto, ops::Deref};

const EPISLON: f64 = 1. / 1000.;

impl CTerm {
    fn encode(&self, symbols: &Symbols, variables: &Vec<LPVariable>) -> Expression {
        match self {
            CTerm::Inj(t) => match t {
                Term::Variable(_, Typ::F) => debug_unreachable!(),
                Term::Variable(index, _) => variables[*index as usize].into(),
                Term::Constant(_, _) => unimplemented!("free constant"),
                Term::Integer(value) => (*value as f64).into(),
            },
            CTerm::Add(args) => {
                args[0].encode(symbols, variables) + args[1].encode(symbols, variables)
            }
            CTerm::Sub(args) => {
                args[0].encode(symbols, variables) - args[1].encode(symbols, variables)
            }
            CTerm::Neg(args) => -args[0].encode(symbols, variables),
            CTerm::Mul(args) => {
                args[0]
                    .encode(symbols, variables)
                    .mul(if let CTerm::Inj(term) = args[1] {
                        match term {
                            Term::Integer(value) => (value as f64),
                            _ => debug_unreachable!(),
                        }
                    } else {
                        debug_unreachable!()
                    })
            }
        }
    }
}

impl CAtom {
    fn encode(
        &self,
        symbols: &Symbols,
        variables: &Vec<LPVariable>,
    ) -> Option<good_lp::Constraint> {
        let left: Expression = self.left.encode(symbols, variables);
        let right: Expression = self.right.encode(symbols, variables);
        match self.predicate {
            CPredicate::Eq => Some(left.eq(right)),
            CPredicate::Le => Some(left.leq(right)),
            CPredicate::Ge => Some(left.geq(right)),
            CPredicate::Gt => Some(left.geq(right + <Expression>::from(EPISLON))),
            CPredicate::Lt => Some(left.leq(right - <Expression>::from(EPISLON))),
            _ => {
                warn!("Predicate '{}' is not implemented. The atom '{}' will be dropped. Results might be incorrect.", self.predicate, self.display(symbols));
                None
            }
        }
    }
}

impl Clause {
    pub fn solve(&self, symbols: &Symbols) -> Option<Vec<f64>> {
        let mut variables = ProblemVariables::new();
        let mut vars: Vec<LPVariable> = vec![];
        let mut sum = <Expression>::from(0.);

        for i in 0..self.typs.len() {
            // TODO: We assume that all variables are relevant for the constraint.
            let next = variables.add(VariableDefinition::new().name(format!("_{}", i)));
            vars.push(next);
            sum = sum + &next;
        }

        debug_assert!(vars.len() == self.typs.len());

        let e1 = variables.add(VariableDefinition::new().min(0));
        let e2 = variables.add(VariableDefinition::new().min(0));
        let objective = e1 + e2;

        let mut problem = variables
            .minimise(objective)
            .using(good_lp::solvers::minilp::minilp);

        problem = problem.with(sum.eq(e1 - e2));

        for atom in self.constraint.atoms.deref() {
            let encoded = atom.encode(symbols, &vars);
            if let Some(c) = encoded {
                problem = problem.with(c);
            }
        }
        match problem.solve() {
            Ok(solution) => {
                let mut solv = vec![];
                for (index, &var) in vars.iter().enumerate() {
                    println!("_{} = {:?}", index, solution.value(var));
                    solv.push(solution.value(var));
                }
                Some(solv)
            }
            Err(ResolutionError::Infeasible) => None,
            Err(e) => panic!("{}", e),
        }
    }
}
