use crate::Constraint;
use crate::{
    bail, debug_unreachable, fmt::DisplayWithSymbols, symbols::Term, Atom, CAtom, CPredicate,
    CTerm, Clause, ConstantData, Constants, Interner, Parents, PredicateData, Predicates, State,
    Symbols, Typ, VariableData, Variables,
};
use core::panic;

use pest::error::Error as PestError;
use pest::{
    iterators::{Pair, Pairs},
    Parser, Position, Span,
};
use std::fmt::Debug;
use string_interner::Symbol;

#[derive(pest_derive::Parser)]
#[grammar = "grammar.pest"]
pub struct FTCNFParser;

/*
fn parse_catom(symbols: &mut Symbols, pair: Pair<Rule>) {
    let problem = Problem::new(minilp::OptimizationDirection::Minimize);

    let pairs: Vec<Pair<Rule>> = pair.into_inner().into_iter().collect();

    debug_assert!(pairs.len() == 3);

    let op = match pairs[0].as_rule() {
        Rule::le => minilp::ComparisonOp::Le,
        Rule::ge => minilp::ComparisonOp::Ge,
        Rule::eq => minilp::ComparisonOp::Eq,
    };

    let left = Term::parse(symbols, clause_variables, pairs[1]);
    let right = Term::parse(symbols, clause_variables, pairs[2]);

    // If right is a constant, then everything is fine.
    problem.add_var(obj_coeff, _)
}
*/

fn parse_preamble(symbols: &mut Symbols, pair: Pair<Rule>) {
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::item => {
                parse_preamble_item(symbols, inner);
            }
            _ => debug_unreachable!(),
        }
    }
}

fn parse_preamble_item(symbols: &mut Symbols, pair: Pair<Rule>) {
    let pairs: Vec<Pair<Rule>> = pair.into_inner().into_iter().collect();

    debug_assert!(pairs.len() == 2);

    let pair_term = &pairs[0];
    let pair_typ = &pairs[1];

    let typ = match pair_typ.as_str() {
        "R" => Typ::R,
        "I" => Typ::I,
        "F" => Typ::F,
        _ => debug_unreachable!(),
    };

    match pair_term.as_rule() {
        Rule::constant => {
            let s = pair_term.as_str();
            let constant = symbols.constants.interner.get_or_intern(s);
            let index = constant.to_usize();
            if index == symbols.constants.data.len() {
                symbols.constants.data.push(ConstantData { typ })
            } else if symbols.constants.data[index].typ != typ {
                bail_from_span(
                    format!(
                        "Constant '{}' previously declared with type {}.",
                        s, symbols.constants.data[index].typ
                    ),
                    1,
                    pair_typ.as_span(),
                )
            }
        }
        Rule::variable => {
            let s = pair_term.as_str();
            let variable = symbols.variables.interner.get_or_intern(s);
            let index = variable.to_usize();
            if index == symbols.variables.data.len() {
                symbols.variables.data.push(VariableData { typ })
            } else if symbols.variables.data[index].typ != typ {
                bail_from_span(
                    format!(
                        "Variable '{}' previously declared with type {}.",
                        s, symbols.variables.data[index].typ
                    ),
                    1,
                    pair_typ.as_span(),
                )
            }
        }
        _ => debug_unreachable!(),
    }
}

impl State {
    pub fn parse_str(str: &str) -> Option<State> {
        match FTCNFParser::parse(Rule::input, str) {
            Ok(pairs) => Some(State::parse(pairs)),
            Err(_) => None,
        }
    }

    pub fn parse(pairs: Pairs<Rule>) -> State {
        let mut state = State {
            symbols: Symbols {
                predicates: Predicates {
                    interner: Interner::new(),
                    data: vec![],
                },
                constants: Constants {
                    interner: Interner::new(),
                    data: vec![],
                },
                variables: Variables {
                    interner: Interner::new(),
                    data: vec![],
                },
            },
            clauses: vec![],
        };

        for pair in pairs {
            match pair.as_rule() {
                Rule::preamble => {
                    parse_preamble(&mut state.symbols, pair);
                }
                Rule::clause | Rule::tclause => {
                    Clause::parse(&mut state, pair);
                }
                Rule::EOI | Rule::LINE_COMMENT => {}
                _ => debug_unreachable!(),
            }
        }

        state
    }
}

impl Clause {
    fn parse(state: &mut State, pair: Pair<Rule>) {
        let variables = &mut Variables {
            interner: Interner::new(),
            data: vec![],
        };
        let mut arrow: usize = 0;
        let mut atoms: Vec<Atom> = vec![];
        let mut catoms: Vec<CAtom> = vec![];

        for inner in pair.into_inner() {
            match inner.as_rule() {
                Rule::catom => catoms.push(CAtom::parse(&mut state.symbols, variables, inner)),
                Rule::arrow => {
                    arrow = atoms.len();
                }
                Rule::fatom => {
                    atoms.push(Atom::parse(&mut state.symbols, variables, inner));
                }
                _ => debug_unreachable!(),
            }
        }

        let clause = Clause {
            id: state.clauses.len(),
            constraint: Constraint {
                atoms: catoms.into_boxed_slice(),
            },
            atoms: atoms.into_boxed_slice(),
            arrow,
            parents: Parents::Input,
            typs: variables.data.iter().map(|d| d.typ).collect(),
        };

        println!("{}", clause.display(&state.symbols));

        state.clauses.push(clause);
    }
}

impl CPredicate {
    fn parse(rule: Rule) -> CPredicate {
        match rule {
            Rule::eq => CPredicate::Eq,
            Rule::ne => CPredicate::Ne,
            Rule::ge => CPredicate::Ge,
            Rule::gt => CPredicate::Gt,
            Rule::le => CPredicate::Le,
            Rule::lt => CPredicate::Lt,
            _ => debug_unreachable!(),
        }
    }
}

impl CAtom {
    fn parse(symbols: &mut Symbols, clause_variables: &mut Variables, pair: Pair<Rule>) -> CAtom {
        let mut pairs = pair.into_inner();

        CAtom {
            predicate: CPredicate::parse(pairs.next().unwrap().as_rule()),
            left: CTerm::parse(symbols, clause_variables, pairs.next().unwrap()),
            right: CTerm::parse(symbols, clause_variables, pairs.next().unwrap()),
        }
    }
}

impl CTerm {
    fn parse_op(
        symbols: &mut Symbols,
        clause_variables: &mut Variables,
        pair: Pair<Rule>,
    ) -> CTerm {
        let mut op: Option<Rule> = None;
        let mut args = vec![];
        let span = pair.as_span();
        for inner in pair.into_inner() {
            match inner.as_rule() {
                Rule::add | Rule::mul | Rule::sub => {
                    op = Some(inner.as_rule());
                }
                Rule::cterm => args.push(CTerm::parse(symbols, clause_variables, inner)),
                _ => debug_unreachable!(),
            }
        }
        let boxed = args.into_boxed_slice();

        if op == Some(Rule::sub) {
            if boxed.len() == 1 {
                CTerm::Neg(boxed.try_into().unwrap())
            } else {
                CTerm::Sub(boxed.try_into().unwrap())
            }
        } else if boxed.len() != 2 {
            todo!()
        } else if op == Some(Rule::add) {
            CTerm::Add(boxed.try_into().unwrap())
        } else if op == Some(Rule::mul) {
            if let CTerm::Inj(term) = boxed[1] {
                match term {
                    Term::Constant(_, _) => CTerm::Mul(boxed.try_into().unwrap()),
                    _ => {
                        bail_from_span(String::from("Non-Linear!"), 1, span);
                        panic!()
                    }
                }
            } else {
                // In this case, the right argument might actually evaluate to a
                // constant, e.g. *(x,+(1,1)). Flattening is not implemented.
                todo!()
            }
        } else {
            debug_unreachable!()
        }
    }

    fn parse(symbols: &mut Symbols, clause_variables: &mut Variables, pair: Pair<Rule>) -> CTerm {
        for inner in pair.into_inner() {
            match inner.as_rule() {
                Rule::term => return CTerm::Inj(Term::parse(symbols, clause_variables, inner)),
                Rule::op => return CTerm::parse_op(symbols, clause_variables, inner),
                _ => debug_unreachable!(),
            }
        }
        debug_unreachable!()
    }
}

impl Atom {
    fn parse(symbols: &mut Symbols, clause_variables: &mut Variables, pair: Pair<Rule>) -> Atom {
        // TODO(lorenz): Check types of arguments.

        let mut predicate: usize = 0;
        let mut arity: Option<usize> = Option::None;
        let mut terms: Vec<Term> = vec![];
        let pos: Position = pair.as_span().start_pos();

        for inner in pair.into_inner() {
            match inner.as_rule() {
                Rule::predicate => {
                    predicate = symbols
                        .predicates
                        .interner
                        .get_or_intern(inner.as_str())
                        .to_usize();
                    if predicate < symbols.predicates.data.len() {
                        arity = Option::Some(symbols.predicates.arity(predicate))
                    }
                }
                Rule::term => {
                    if arity.is_some() && arity.unwrap() <= terms.len() {
                        bail_from_span(
                            format!(
                                "Predicate {}/{} cannot take {} (or more) arguments.",
                                symbols.predicates.interner.resolve(predicate).unwrap(),
                                arity.unwrap(),
                                terms.len() + 1
                            ),
                            1,
                            inner.as_span(),
                        );
                    }

                    terms.push(Term::parse(symbols, clause_variables, inner))
                }
                _ => debug_unreachable!(),
            }
        }

        match arity {
            Some(arity) => {
                if arity != terms.len() {
                    bail_from_pos(
                        format!(
                            "Predicate {}/{} cannot take {} arguments.",
                            symbols.predicates.interner.resolve(predicate).unwrap(),
                            arity,
                            terms.len()
                        ),
                        1,
                        pos,
                    );
                }
            }
            None => {
                debug_assert!(predicate == symbols.predicates.data.len());
                symbols
                    .predicates
                    .data
                    .push(PredicateData { arity: terms.len() });
                // println!("{:?}", variables.i);
            }
        }

        Atom {
            predicate,
            terms: terms.into_boxed_slice(),
        }
    }
}

impl Term {
    fn parse(symbols: &mut Symbols, clause_variables: &mut Variables, pair: Pair<Rule>) -> Term {
        for inner in pair.into_inner() {
            match inner.as_rule() {
                Rule::natural0 => return Term::Integer(inner.as_str().parse().unwrap()),
                Rule::constant => {
                    // TODO: Fix type.
                    return Term::Constant(
                        symbols
                            .constants
                            .interner
                            .get_or_intern(inner.as_str())
                            .get(),
                        Typ::R,
                    );
                }
                Rule::variable => {
                    // First check variables from preamble, then fall back to clause variables.
                    let s = inner.as_str();
                    let t = match symbols.variables.interner.get(s) {
                        Some(v) => symbols.variables.data[v.to_usize()].typ,
                        None => Typ::R,
                    };
                    let v = clause_variables.interner.get_or_intern(inner.as_str());
                    if v.to_usize() == clause_variables.data.len() {
                        clause_variables.data.push(VariableData { typ: t })
                    }
                    return Term::Variable(v.get(), t);
                }
                _ => debug_unreachable!(),
            }
        }
        debug_unreachable!();
    }
}

fn bail_from_span(message: String, code: i32, span: Span) {
    bail(
        PestError::<Rule>::new_from_span(pest::error::ErrorVariant::CustomError { message }, span),
        code,
    );
}

fn bail_from_pos(message: String, code: i32, pos: Position) {
    bail(
        PestError::<Rule>::new_from_pos(pest::error::ErrorVariant::CustomError { message }, pos),
        code,
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    use pest::Parser;

    fn ok(rule: Rule, s: &str) {
        assert!(FTCNFParser::parse(rule, s).is_ok())
    }

    #[test]
    fn atom() {
        let rule: Rule = Rule::fatom;
        ok(rule, "P(a)");
        ok(rule, "P(0)");
    }

    #[test]
    fn catom() {
        let rule: Rule = Rule::catom;
        ok(rule, "=(a,0)");
        ok(rule, ">(x,y)");
    }

    #[test]
    fn clause() {
        let rule: Rule = Rule::clause;
        ok(rule, ">(x,y) ∥ P(x) → Q(y).");
    }
}
