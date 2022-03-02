//! Test

use crate::fmt::DisplayWithSymbols;
use crate::{symbols::*, *};

use good_lp::Variable as LPVariable;
use good_lp::{Expression, ProblemVariables, Variable, VariableDefinition};
use good_lp::{ResolutionError, Solution, SolverModel};
use itertools::Itertools;

use std::ops::Mul;

use std::ops::Deref;

const EPISLON: f64 = 1. / 1000.;

impl CTerm {
    fn encode(&self, variables: &Vec<LPVariable>) -> Expression {
        match self {
            CTerm::Inj(t) => match t {
                Term::Variable(_, Typ::F) => debug_unreachable!(),
                Term::Variable(index, _) => variables[*index as usize].into(),
                Term::Constant(_, Typ::F) => panic!("constant of type F occurs in constraint"),
                Term::Constant(c, tau) => unimplemented!(
                    "free constant with index {} of type '{}' occurs in constraint",
                    c,
                    tau
                ),
                Term::Integer(value) => (*value as f64).into(),
            },
            CTerm::Add(args) => args[0].encode(variables) + args[1].encode(variables),
            CTerm::Sub(args) => args[0].encode(variables) - args[1].encode(variables),
            CTerm::Neg(args) => -args[0].encode(variables),
            CTerm::Mul(args) => args[0]
                .encode(variables)
                .mul(if let CTerm::Inj(term) = args[1] {
                    match term {
                        Term::Integer(value) => (value as f64),
                        _ => debug_unreachable!(),
                    }
                } else {
                    debug_unreachable!()
                }),
        }
    }
}

impl CAtom {
    fn encode(&self, variables: &Vec<LPVariable>) -> Option<good_lp::Constraint> {
        let left: Expression = self.left.encode(variables);
        let right: Expression = self.right.encode(variables);
        match self.predicate {
            CPredicate::Eq => Some(left.eq(right)),
            CPredicate::Le => Some(left.leq(right)),
            CPredicate::Ge => Some(left.geq(right)),
            CPredicate::Gt => Some(left.geq(right + <Expression>::from(EPISLON))),
            CPredicate::Lt => Some(left.leq(right - <Expression>::from(EPISLON))),
            _ => {
                warn!("Predicate '{}' is not implemented. The atom '{}' will be dropped. Results might be incorrect.", self.predicate, self);
                None
            }
        }
    }
}

impl Constraint {
    pub fn encode(&self, pv: &mut ProblemVariables) -> (Expression, Vec<good_lp::Constraint>) {
        let max_var = self
            .atoms
            .iter()
            .map(|a| a.max_var())
            .reduce(|x, y| default_combine(|x, y| x.max(y), x, y))
            .unwrap()
            .unwrap();
        let mut vars: Vec<LPVariable> = Vec::with_capacity(max_var as usize + 1 + 2);
        let mut sum = <Expression>::from(0.);

        for i in 0..=max_var {
            let next = pv.add(VariableDefinition::new().name(format!("_{}", i)));
            vars.push(next);
            sum += &next;
        }

        let e1 = pv.add(VariableDefinition::new().min(0));
        let e2 = pv.add(VariableDefinition::new().min(0));
        let objective = e1 + e2;

        let mut constraints = vec![];

        constraints.push(sum.eq(e1 - e2));

        for atom in self.atoms.deref() {
            let encoded = atom.encode(&vars);
            if let Some(c) = encoded {
                constraints.push(c);
            }
        }

        (objective, constraints)
    }

    pub fn satisfiable(&mut self) -> bool {
        if self.atoms.is_empty() || self.solution.is_some() {
            true
        } else {
            self.solve();
            self.solution.is_some()
        }
    }

    pub fn solve(&mut self) -> Result<(), ResolutionError> {
        let mut variables = ProblemVariables::new();
        let encoded = self.encode(&mut variables);
        match solve(variables, encoded.0, encoded.1) {
            Ok(solution) => {
                self.solution = Some(solution);
                Ok(())
            }
            Err(ResolutionError::Infeasible) => Err(ResolutionError::Infeasible),
            Err(e) => panic!("{}", e),
        }
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.atoms.is_empty()
    }

    /// An implicitly negated disjunction.
    pub(crate) fn negate(&self) -> Vec<CAtom> {
        self.atoms
            .iter()
            .flat_map(|atom| atom.negate())
            .collect_vec()
    }

    pub(crate) fn implies(&self, other: &Constraint) -> bool {
        if other.is_empty() {
            return true;
        }

        if self.is_empty() {
            return other.satisfiable();
        }

        // Make sure that the variables in `other` is a subset of the vars in `self`.
        // Otherwise we need quantifier eliminiation.
        let other_vars = other
            .atoms
            .iter()
            .flat_map(|atom| atom.var_occurrences())
            .collect_vec();
        let self_vars = self
            .atoms
            .iter()
            .flat_map(|atom| atom.var_occurrences())
            .collect_vec();

        for var in other_vars {
            if self_vars.iter().find(|&x| *x == var).is_none() {
                // This variable is in `other`, but not in `self`.
                // We would have to eliminate it, but this is not implemented.
                return false;
            }
        }

        for atom in other.negate() {
            let mut pv = ProblemVariables::new();
            let mut self_encoded = self.encode(&mut pv);
            let mut atom_encoded = atom.encode(&mut pv);
            self_encoded.1.append(&mut atom_encoded.1);
            if solve(pv, self_encoded.0 + atom_encoded.0, self_encoded.1).is_ok() {
                return true;
            }
        }

        false
    }
}

fn solve(
    variables: ProblemVariables,
    objective: Expression,
    constraints: Vec<good_lp::Constraint>,
) -> Result<good_lp::solvers::minilp::MiniLpSolution, ResolutionError> {
    let mut problem = variables
        .minimise(objective)
        .using(good_lp::solvers::minilp::minilp);

    for constraint in constraints {
        problem = problem.with(constraint);
    }

    match problem.solve() {
        Ok(solution) => Ok(solution),
        Err(ResolutionError::Infeasible) => Err(ResolutionError::Infeasible),
        Err(e) => panic!("{}", e),
    }
}

impl Clause {
    pub fn solve(&mut self) {
        let mut variables = ProblemVariables::new();
        let mut vars: Vec<LPVariable> = Vec::with_capacity(self.typs.len() + 2);
        let mut sum = <Expression>::from(0.);

        for i in 0..self.typs.len() {
            // TODO: We assume that all variables are relevant for the constraint.
            let next = variables.add(VariableDefinition::new().name(format!("_{}", i)));
            vars.push(next);
            sum += &next;
        }

        debug_assert!(self.max_var().map_or(0, |v| (v + 1) as usize) == self.typs.len());
        debug_assert!(vars.len() == self.typs.len());

        let e1 = variables.add(VariableDefinition::new().min(0));
        let e2 = variables.add(VariableDefinition::new().min(0));
        let objective = e1 + e2;

        let mut problem = variables
            .minimise(objective)
            .using(good_lp::solvers::minilp::minilp);

        problem = problem.with(sum.eq(e1 - e2));

        for atom in self.constraint.atoms.deref() {
            let encoded = atom.encode(&vars);
            if let Some(c) = encoded {
                problem = problem.with(c);
            }
        }
        match problem.solve() {
            Ok(solution) => {
                self.constraint.solution = Some(solution);
            }
            Err(ResolutionError::Infeasible) => {}
            Err(e) => panic!("{}", e),
        }
    }
}
