use log::{debug, info};

use crate::{
    fmt::DisplayWithSymbols, resolution::*, substitution::*, symbols::Term, unification::unify_at,
    Atom, CAtom, Clause, ClausePosition, Constraint, Parents, State, Typ,
};

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Result {
    Unsatisfiable,
    Unknown,
}

impl State {
    pub(crate) fn saturate(&mut self) -> Result {
        let (mut start, mut end) = (0, self.index.units.len());
        loop {
            let mut resolvents: Vec<Clause> = vec![];
            for i in start..end {
                let unit = &self.clauses[self.index.units[i]];
                if !unit.is_unit() {
                    continue;
                }

                let predicate = unit.atoms[0].predicate;
                let positive = unit.is_positive();

                let index = if positive {
                    &self.index.negative
                } else {
                    &self.index.positive
                };

                if index.len() <= predicate {
                    continue;
                }

                for j in 0..index[predicate].len() {
                    let pos = &index[predicate][j];
                    let candidate = &self.clauses[pos.clause];
                    debug_assert!(pos.literal < candidate.atoms.len());
                    let unifier = unify_at([&unit, &candidate], [0, pos.literal]);
                    if let Ok(sigma) = unifier {
                        //println!("Resolving {} and {}.", unit.display(&self.symbols), candidate.display(&self.symbols));
                        let mut resolvent =
                            resolve_with_subst([&unit, &candidate], [0, pos.literal], sigma);
                        resolvent.normalize();
                        if resolvent.constraint.satisfiable() {
                            if resolvent.is_empty() {
                                return Result::Unsatisfiable;
                            }

                            let mut subsumed = false;
                            for clause in self.clauses.iter() {
                                if clause.subsumes(&resolvent) {
                                    subsumed = true;
                                    break;
                                }
                            }

                            if subsumed {
                                continue;
                            }

                            println!("{}", resolvent.display(&self.symbols));
                            resolvents.push(resolvent);
                        }
                    }
                }
                info!("{}", resolvents.len());
            }
            //start = end;
            if self.index.units.len() == end {
                // No new unit clauses generated.
                break;
            }
            end = self.index.units.len();

            for mut resolvent in resolvents {
                self.ingest_clause(resolvent);
            }
        }

        Result::Unknown
    }

    pub fn ingest_clause(&mut self, mut clause: Clause) {
        if clause.max_var().map_or(0, |v| (v + 1) as usize) != clause.typs.len() {
            dbg!(&clause.typs.len());
            dbg!(&clause);
            dbg!(&clause.max_var());
            panic!();
        }

        clause.id = self.clauses.len();
        self.index.add(&clause);
        self.clauses.push(clause);
    }

    pub(crate) fn ingest(
        &mut self,
        catoms: Box<[CAtom]>,
        arrow: usize,
        atoms: Box<[Atom]>,
        parents: Parents,
        typs: Box<[Typ]>,
    ) {
        let id = self.clauses.len();
        let clause = Clause {
            id,
            constraint: Constraint {
                atoms: catoms,
                solution: None,
            },
            atoms,
            arrow,
            parents,
            typs,
        };

        self.index.add(&clause);
        self.clauses.push(clause);
    }
}
