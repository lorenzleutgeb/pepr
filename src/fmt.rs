//! Formatting all kinds of things.

use string_interner::backend::Backend;

use crate::{symbols::*, *};

pub(crate) trait DisplayWithSymbols: Sized {
    fn display<'a>(&'a self, symbols: &'a Symbols) -> DisplayWrapper<'a, Self> {
        DisplayWrapper {
            wrapped: self,
            symbols,
        }
    }
    fn fmt_internal(&self, symbols: &Symbols, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result;
}

pub(crate) struct DisplayWrapper<'a, T> {
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
            Term::Variable(_, _) => self.fmt(f),
            Term::Constant(c, _) => write!(
                f,
                "{}",
                symbols
                    .constants
                    .interner
                    .resolve(SymbolU32::try_from(c).unwrap())
                    .unwrap()
            ),
            Term::Integer(i) => write!(f, "{}", i),
        }
    }
}

impl DisplayWithSymbols for CTerm {
    fn fmt_internal(&self, symbols: &Symbols, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            CTerm::Inj(t) => t.fmt_internal(symbols, f),
            CTerm::Add(t1, t2) => write!(f, "+(")
                .and_then(|_| t1.fmt_internal(symbols, f))
                .and_then(|_| write!(f, ", "))
                .and_then(|_| t2.fmt_internal(symbols, f))
                .and_then(|_| write!(f, ")")),
            CTerm::Mul(t1, t2) => write!(f, "*(")
                .and_then(|_| t1.fmt_internal(symbols, f))
                .and_then(|_| write!(f, ", "))
                .and_then(|_| t2.fmt_internal(symbols, f))
                .and_then(|_| write!(f, ")")),
            CTerm::Sub(t1, t2) => write!(f, "-(")
                .and_then(|_| t1.fmt_internal(symbols, f))
                .and_then(|_| write!(f, ", "))
                .and_then(|_| t2.fmt_internal(symbols, f))
                .and_then(|_| write!(f, ")")),
            CTerm::Neg(t) => write!(f, "-")
                .and_then(|_| t.fmt_internal(symbols, f))
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
        write!(
            f,
            "{}",
            symbols.predicates.interner.resolve(self.predicate).unwrap()
        )?;

        if !self.terms.is_empty() {
            write!(f, "(")?;
        }

        for (index, term) in self.into_iter().enumerate() {
            term.fmt_internal(symbols, f)?;
            if index < self.terms.len() - 1 {
                write!(f, ", ")?;
            }
        }

        if !self.terms.is_empty() {
            write!(f, ")")?;
        }

        Ok(())
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
            self.id, // self.id.map_or(String::from("?"), |id| id.to_string()),
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

impl Display for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        let mut result = Ok(());
        for clause in self.clauses.iter() {
            result = clause.fmt_internal(&self.symbols, f).and(writeln!(f));
            result?;
        }
        result
    }
}
