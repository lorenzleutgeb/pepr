//! Test

use crate::fmt::DisplayWithSymbols;
use crate::{symbols::*, *};

use good_lp::solvers::minilp::MiniLpSolution;
use good_lp::Variable as LPVariable;
use good_lp::{Expression, ProblemVariables, VariableDefinition};
use good_lp::{ResolutionError, Solution, SolverModel};
use itertools::Itertools;

use std::ops::Mul;

use std::ops::Deref;

const EPSILON: f64 = 1. / 1000.;

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
    fn encode(&self, variables: &Vec<LPVariable>) -> good_lp::Constraint {
        let left: Expression = self.left.encode(variables);
        let right: Expression = self.right.encode(variables);
        match self.predicate {
            CPredicate::Eq => left.eq(right),
            CPredicate::Le => left.leq(right),
            CPredicate::Ge => left.geq(right),
            CPredicate::Gt => left.geq(right + <Expression>::from(EPSILON)),
            CPredicate::Lt => left.leq(right - <Expression>::from(EPSILON)),

            // Inequality cannot be directly encoded.
            // This is worked around by axiomatising,
            // and by negating as '<' or '>'.
            CPredicate::Ne => debug_unreachable!(),
        }
    }
}

impl Constraint {
    pub fn encode(
        &self,
        pv: &mut ProblemVariables,
    ) -> (Expression, Vec<good_lp::Constraint>, Vec<LPVariable>) {
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
            constraints.push(atom.encode(&vars));
        }

        (objective, constraints, vars)
    }

    pub fn mut_satisfiable(&mut self) -> bool {
        if self.atoms.is_empty() || self.solution.is_some() {
            true
        } else if let Ok(solution) = self.solve() {
            self.solution = Some(solution);
            true
        } else {
            false
        }
    }

    pub fn satisfiable(&self) -> bool {
        if self.atoms.is_empty() || self.solution.is_some() {
            true
        } else {
            self.solve().is_ok()
        }
    }

    pub fn solve(&self) -> Result<MiniLpSolution, ResolutionError> {
        let mut variables = ProblemVariables::new();
        let encoded = self.encode(&mut variables);
        match solve(variables, encoded.0, encoded.1) {
            Err(ResolutionError::Infeasible) => Err(ResolutionError::Infeasible),
            Err(e) => panic!("{}", e),
            ok => ok,
        }
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.atoms.is_empty()
    }

    /// An implicitly negated disjunction.
    fn negate(&self) -> Vec<CAtom> {
        self.atoms
            .iter()
            .flat_map(|atom| atom.negate())
            .collect_vec()
    }

    pub(crate) fn implies(&self, other: &Constraint) -> bool {
        if other.is_empty() {
            true
        } else if self.is_empty() {
            other.satisfiable()
        } else if !self.contains_all_vars(other) {
            // There is a variable that occurs in `other`, but not in `self`.
            // We would have to eliminate it, but this is not implemented.
            // Conservatively return false.
            false
        } else {
            other.negate().iter().all(|atom| {
                let mut pv = ProblemVariables::new();
                let mut self_encoded = self.encode(&mut pv);
                self_encoded.1.push(atom.encode(&self_encoded.2));
                solve(pv, self_encoded.0, self_encoded.1).is_err()
            })
        }
    }

    fn contains_all_vars(&self, other: &Constraint) -> bool {
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
            if !self_vars.iter().any(|x| *x == var) {
                return false;
            }
        }

        true
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
            problem = problem.with(encoded);
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn implication() {
        let x: Term = Term::Variable(0, Typ::R);
        let y: Term = Term::Variable(1, Typ::R);
        let one: Term = Term::Integer(1);
        let two: Term = Term::Integer(2);

        let xeqy: Constraint = Constraint::new(
            vec![CAtom {
                predicate: CPredicate::Eq,
                left: CTerm::Inj(x),
                right: CTerm::Inj(y),
            }]
            .into_boxed_slice(),
        );

        let xgey: Constraint = Constraint::new(
            vec![CAtom {
                predicate: CPredicate::Ge,
                left: CTerm::Inj(x),
                right: CTerm::Inj(y),
            }]
            .into_boxed_slice(),
        );

        let xeq2: Constraint = Constraint::new(
            vec![CAtom {
                predicate: CPredicate::Eq,
                left: CTerm::Inj(x),
                right: CTerm::Inj(two),
            }]
            .into_boxed_slice(),
        );

        let xge1: Constraint = Constraint::new(
            vec![CAtom {
                predicate: CPredicate::Ge,
                left: CTerm::Inj(x),
                right: CTerm::Inj(one),
            }]
            .into_boxed_slice(),
        );

        assert_eq!(xeqy.implies(&xeqy), true);
        assert_eq!(xeqy.implies(&xgey), true);
        assert_eq!(xgey.implies(&xeqy), false);
        assert_eq!(xgey.implies(&xgey), true);
        assert_eq!(xeq2.implies(&xge1), true);
        assert_eq!(xge1.implies(&xeq2), false);
    }
}
