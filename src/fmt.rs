//! Formatting all kinds of things.

use string_interner::backend::Backend;

use crate::{symbols::*, *};

pub trait DisplayWithSymbols: Sized {
    fn display<'a>(&'a self, symbols: &'a Symbols) -> DisplayWrapper<'a, Self> {
        DisplayWrapper {
            wrapped: self,
            symbols,
        }
    }
    fn fmt_internal(&self, symbols: &Symbols, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result;
}

pub struct DisplayWrapper<'a, T> {
    wrapped: &'a T,
    symbols: &'a Symbols,
}

impl<'a, T: DisplayWithSymbols> std::fmt::Display for DisplayWrapper<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.wrapped.fmt_internal(self.symbols, f)
    }
}

impl DisplayWithSymbols for Term {
    fn fmt_internal(&self, symbols: &Symbols, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Term::Variable(v, _t) => write!(f, "_{}", v),
            Term::Constant(c, _t) => write!(
                f,
                "{}",
                symbols.constants.interner.resolve(c.into()).unwrap()
            ),
            Term::Integer(i) => write!(f, "{}", i),
            _ => debug_unreachable!(),
        }
    }
}

impl DisplayWithSymbols for CTerm {
    fn fmt_internal(&self, symbols: &Symbols, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            CTerm::Inj(t) => t.fmt_internal(symbols, f),
            CTerm::Add(args) => write!(f, "+(")
                .and_then(|_| args[0].fmt_internal(symbols, f))
                .and_then(|_| write!(f, ", "))
                .and_then(|_| args[1].fmt_internal(symbols, f))
                .and_then(|_| write!(f, ")")),
            CTerm::Mul(args) => write!(f, "*(")
                .and_then(|_| args[0].fmt_internal(symbols, f))
                .and_then(|_| write!(f, ", "))
                .and_then(|_| args[1].fmt_internal(symbols, f))
                .and_then(|_| write!(f, ")")),
            CTerm::Sub(args) => write!(f, "-(")
                .and_then(|_| args[0].fmt_internal(symbols, f))
                .and_then(|_| write!(f, ", "))
                .and_then(|_| args[1].fmt_internal(symbols, f))
                .and_then(|_| write!(f, ")")),
            CTerm::Neg(args) => write!(f, "-")
                .and_then(|_| args[0].fmt_internal(symbols, f))
                .and_then(|_| write!(f, ")")),
        }
    }
}

impl DisplayWithSymbols for CAtom {
    fn fmt_internal(&self, symbols: &Symbols, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.left
            .fmt_internal(symbols, f)
            .and_then(|_| write!(f, " "))
            .and_then(|_| <CPredicate as std::fmt::Display>::fmt(&self.predicate, f))
            .and_then(|_| write!(f, " "))
            .and_then(|_| self.right.fmt_internal(symbols, f))
    }
}

impl DisplayWithSymbols for Atom {
    fn fmt_internal(&self, symbols: &Symbols, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Err(e) = write!(
            f,
            "{}(",
            symbols.predicates.interner.resolve(self.predicate).unwrap()
        ) {
            return Err(e);
        }
        for (index, term) in self.into_iter().enumerate() {
            if let Err(e) = term.fmt_internal(symbols, f) {
                return Err(e);
            }
            if index < self.terms.len() - 1 {
                if let Err(e) = write!(f, ", ") {
                    return Err(e);
                }
            }
        }
        write!(f, ")")
    }
}

impl DisplayWithSymbols for Clause {
    fn fmt_internal(&self, symbols: &Symbols, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (premises, conclusions) = self.split_at_arrow();

        let constraint_str = if self.constraint.atoms.is_empty() {
            String::from("")
        } else {
            self.constraint
                .atoms
                .iter()
                .map(|atom| atom.display(symbols).to_string())
                .collect::<Vec<String>>()
                .join(", ")
                + " ∥ "
        };

        let premise_str = premises
            .iter()
            .map(|atom| atom.display(symbols).to_string())
            .collect::<Vec<String>>()
            .join(", ");

        let conclusion_str = conclusions
            .iter()
            .map(|atom| atom.display(symbols).to_string())
            .collect::<Vec<String>>()
            .join(", ");

        let sorts = self
            .typs
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<String>>()
            .join("");

        write!(
            f,
            "{}:{}:{}:{}:{}: {}{} → {}.",
            self.id,
            self.atoms.len(),
            self.typs.len(),
            self.parents,
            sorts,
            constraint_str,
            premise_str,
            conclusion_str
        )
    }
}
