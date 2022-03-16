/// Fourier-Motzkin Elimination
///
/// See also https://github.com/Nicholas42/FourierMotzkin
use std::ops::Div;

use ::dimacs::Var;
use itertools::Itertools;

use crate::{symbols::*, *};

type Coefficient = f64;
type System = Vec<Ineq>;

// Instead of using `Default::default` or literal '0.0', prefer 'ZERO' for clarity and in case the type changes.
const ZERO: Coefficient = 0.0;

const ONE: Coefficient = 1.0;

#[derive(Debug, Clone, Copy)]
struct Mul {
    x: Variable,
    a: Coefficient,
}

impl Mul {
    fn new(a: Coefficient, x: Variable) -> Mul {
        Mul { a, x }
    }

    fn one(x: Variable) -> Mul {
        Mul { a: 1., x }
    }
}

impl Display for Mul {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.a == ONE {
            write!(f, "x{}", self.x)
        } else if self.a == ZERO {
            write!(f, "0")
        } else {
            write!(f, "{} · x{}", self.a, self.x)
        }
    }
}

impl std::ops::Mul<Coefficient> for Mul {
    type Output = Self;

    fn mul(self, rhs: Coefficient) -> Self::Output {
        Mul {
            x: self.x,
            a: self.a * rhs,
        }
    }
}

impl std::ops::Div<Coefficient> for Mul {
    type Output = Self;

    fn div(self, rhs: Coefficient) -> Self::Output {
        Mul {
            x: self.x,
            a: self.a / rhs,
        }
    }
}

#[derive(Debug, Clone)]
struct Ineq {
    xs: Vec<Mul>,
    b: Coefficient,
    // TODO: Consider adding parent to track where this inequality came from?
}

impl Display for Ineq {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.xs.is_empty() {
            write!(f, "0")?;
        } else {
            write!(f, "{}", self.xs.iter().map(ToString::to_string).join(" + "))?;
        }
        write!(f, " ≥ {}", self.b)
    }
}

fn elim(system: &System, target: Variable) -> System {
    let mut n: Vec<Ineq> = vec![];
    let mut p: Vec<Ineq> = vec![];

    // Partitioning phase. We partition the system into
    // three partitions.
    // z contains the (indices of) inequalities where target does
    // not occur.
    // n (resp. p) contains the (inidices of) inequalities where
    // target occurs negatively (resp. positively).

    let mut result: Vec<Ineq> = vec![];

    for (i, ineq) in system.iter().enumerate() {
        let a = ineq.coeff(target);
        if a > ZERO {
            p.push(system[i].norm_on(target));
        } else if a < ZERO {
            n.push(system[i].norm_on(target));
        } else if a == ZERO {
            result.push(system[i].clone());
        } else {
            // f64 has all kinds of weird values, like Infinity and NaN.
            debug_unreachable!();
        }
    }

    result.reserve(n.len() * p.len());
    for a in n {
        for b in &p {
            result.push(a.add_elim(b, target));
        }
    }

    result
}

impl Ineq {
    fn new(xs: Vec<Mul>, b: Coefficient) -> Ineq {
        // TODO: Use unstable feature 'is_sorted'?
        if let Some(violation) = xs.windows(2).find(|window| window[0].x >= window[1].x) {
            panic!(
                "ineq order violated: {} >= {}",
                violation[0].x, violation[1].x
            )
        } else {
            Ineq { xs, b }
        }
    }

    fn var_index(&self, target: Variable) -> Option<usize> {
        self.xs.binary_search_by(|m| m.x.cmp(&target)).ok()
    }

    fn contains_var(&self, target: Variable) -> bool {
        self.var_index(target).is_some()
    }

    fn coeff(&self, target: Variable) -> Coefficient {
        self.var_index(target).map_or(ZERO, |i| self.xs[i].a)
    }

    fn norm_on(&self, target: Variable) -> Ineq {
        let a = self.coeff(target);
        if a == ZERO {
            Ineq {
                xs: vec![],
                b: ZERO,
            }
        } else if a == ONE {
            self.clone()
        } else {
            self.clone().div(a.abs())
        }
    }

    fn add_elim(&self, other: &Ineq, target: Variable) -> Ineq {
        // We principally iterate over self, squeezing in variables from other.
        let mut result: Vec<Mul> = Vec::with_capacity(self.xs.len().max(other.xs.len()) - 1_usize);

        let mut j = 0;
        for i in 0..self.xs.len() {
            while j < other.xs.len() && other.xs[j].x < self.xs[i].x {
                // Variable is unique to other.
                if other.xs[j].x != target {
                    result.push(other.xs[j]);
                }
                j += 1;
            }

            if j < other.xs.len() {
                if self.xs[i].x != target {
                    if other.xs[j].x == self.xs[i].x {
                        // Sum.
                        result.push(Mul {
                            x: self.xs[i].x,
                            a: self.xs[i].a + other.xs[j].a,
                        });
                        j += 1;
                    } else {
                        // Variable is unique to self.
                        result.push(self.xs[i]);
                    }
                }
            } else if self.xs[i].x != target {
                // other is exhausted, just carry on.
                result.push(self.xs[i]);
            }
        }

        Ineq {
            xs: result,
            b: self.b + other.b,
        }
    }
}

impl std::ops::Div<Coefficient> for Ineq {
    type Output = Self;

    fn div(self, rhs: Coefficient) -> Self::Output {
        Ineq {
            xs: self.xs.iter().map(|m| *m / rhs).collect_vec(),
            b: self.b / rhs,
        }
    }
}

impl std::ops::Mul<Coefficient> for Ineq {
    type Output = Self;

    fn mul(self, rhs: Coefficient) -> Self::Output {
        Ineq {
            xs: self.xs.iter().map(|m| *m * rhs).collect_vec(),
            b: self.b * rhs,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn add_elim() {
        let a = Ineq::new(vec![Mul::new(1., 0), Mul::new(2., 2), Mul::new(3., 3)], 1.);
        let b = Ineq::new(
            vec![
                Mul::new(1., 0),
                Mul::new(1., 1),
                Mul::new(-2., 2),
                Mul::new(3., 3),
            ],
            1.,
        );
        let atob = a.add_elim(&b, 2);
        println!("{}", atob);

        let btoa = b.add_elim(&a, 2);
        println!("{}", btoa);
    }

    /// https://de.wikipedia.org/wiki/Fourier-Motzkin-Elimination#Beispiel_für_die_Fourier-Motzkin-Elimination
    #[test]
    fn wikipedia() {
        let system = vec![
            Ineq::new(vec![Mul::new(1., 0)], 1.),
            Ineq::new(vec![Mul::new(-2., 0), Mul::new(-4., 1)], -14.),
            Ineq::new(vec![Mul::new(-1., 0), Mul::new(2., 1)], 1.),
        ];

        for ineq in elim(&system, 0) {
            println!("{}", ineq);
        }
    }

    /// https://youtu.be/s-9oyoiTTl8?t=666
    #[test]
    fn youtube() {
        let system = vec![
            Ineq::new(vec![Mul::one(1), Mul::one(2)], 1.),
            Ineq::new(vec![Mul::one(1), Mul::one(2), Mul::new(2., 3)], 2.),
            Ineq::new(vec![Mul::new(2., 1), Mul::new(3., 3)], 3.),
            Ineq::new(vec![Mul::one(1), Mul::new(-4., 3)], 4.),
            Ineq::new(vec![Mul::new(-2., 1), Mul::one(2), Mul::new(-1., 3)], 5.),
        ];

        for ineq in system.iter() {
            println!("{}", ineq);
        }

        println!();

        for ineq in elim(&system, 0) {
            println!("{}", ineq);
        }

        println!();

        for ineq in elim(&system, 3) {
            println!("{}", ineq);
        }
    }
}
