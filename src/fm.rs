/// Fourier-Motzkin Elimination
///
/// See also https://github.com/Nicholas42/FourierMotzkin
use bitvec::prelude as bv;

use bitvec::vec::BitVec;
use num::traits::NumAssign;

use core::panic;
use std::ops::{Add, Div, DivAssign, Mul, Neg, Sub};

use ::dimacs::Var;
use itertools::Itertools;

use num::rational::Ratio;
use num::{BigInt, Integer, One, Signed, ToPrimitive, Zero};

use crate::{symbols::*, *};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Comparison {
    Le,
    Lt,
    Eq,
}

impl Comparison {
    fn combine(self, other: Comparison) -> Option<Self> {
        match (self, other) {
            (Comparison::Le, Comparison::Le) => Some(Comparison::Le),

            (Comparison::Lt, Comparison::Lt) => Some(Comparison::Lt),
            (Comparison::Lt, Comparison::Le) => Some(Comparison::Lt),
            (Comparison::Le, Comparison::Lt) => Some(Comparison::Lt),

            /*
            (Comparison::Le, Comparison::Eq) => Some(Comparison::Eq),
            (Comparison::Eq, Comparison::Eq) => Some(Comparison::Eq),
            (Comparison::Eq, Comparison::Le) => Some(Comparison::Eq),
            */
            (Comparison::Eq, other) => Some(other),
            (other, Comparison::Eq) => Some(other),
        }
    }
}

impl Display for Comparison {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Comparison::Le => "≤",
                Comparison::Lt => "<",
                Comparison::Eq => "=",
            }
        )
    }
}

struct System<Coefficient, Variable, Rational = Coefficient>
where
    Coefficient: Copy + Integer + Signed,
    Variable: Copy + Ord + Eq,
    Rational: Clone + Integer + From<Coefficient>,
{
    rows: Vec<Ineq<Coefficient, Variable, Rational>>,

    /// Number of variables that this system uses.
    vars: usize,
}

impl<Coefficient> System<Coefficient, usize>
where
    Coefficient: Copy + Integer + Signed,
{
    fn canonical(rows: &[&[Coefficient]]) -> Self {
        let mut result: Vec<Ineq<Coefficient, usize>> = Vec::with_capacity(rows.len());

        let columns = rows[0].len();
        let vars = columns - 1;

        for (i, row) in rows.iter().enumerate() {
            if columns != row.len() {
                panic!(
                    "in row {}: got {} columns but {} are expected",
                    i,
                    row.len(),
                    columns
                )
            } else if let Some((&b, xs)) = row.split_last() {
                let ms: Vec<Mon<Coefficient, usize>> = xs
                    .iter()
                    .enumerate()
                    .filter_map(|(x, &a)| {
                        if a.is_zero() {
                            None
                        } else {
                            Some(Mon::new(a, x))
                        }
                    })
                    .collect_vec();
                result.push(Ineq::new(ms, Comparison::Le, b, i, rows.len(), vars));
            }
        }

        System { rows: result, vars }
    }
}

impl<Coefficient, Variable> System<Coefficient, Variable>
where
    Coefficient: Copy + Integer + Signed + NumAssign,
    Variable: Copy + Ord + Eq + Into<usize>,
{
    fn new(ineqs: &[(&[Mon<Coefficient, Variable>], Comparison, Coefficient)]) -> Self {
        let vars = ineqs
            .iter()
            .map(|(ms, _, _)| ms[ms.len() - 1].x)
            .max()
            .unwrap()
            .into()
            + 1;
        System {
            rows: ineqs
                .iter()
                .enumerate()
                .map(|(i, &(xs, cmp, b))| Ineq::new(xs.to_vec(), cmp, b, i, ineqs.len(), vars))
                .collect(),
            vars,
        }
    }

    fn elim(&mut self, targets: &[Variable]) {
        let mut o: BitVec = BitVec::repeat(false, self.vars);

        for &target in targets {
            let mut n: Vec<usize> = vec![];
            let mut p: Vec<usize> = vec![];
            let mut e: Vec<usize> = vec![];

            // Partitioning phase. We partition the system into
            // three partitions.
            // z contains the (indices of) inequalities where target does
            // not occur. These are left unchanged.
            // n (resp. p) contains the (inidices of) inequalities where
            // target occurs negatively (resp. positively), i.e. those
            // that are a lower (resp. upper) bound for the target variable.

            let mut result: Vec<Ineq<Coefficient, Variable>> = vec![];

            for (i, ineq) in self.rows.iter().enumerate() {
                let a = ineq.coeff(target);
                if a.is_zero() {
                    result.push(self.rows[i].clone());
                } else if ineq.cmp == Comparison::Eq {
                    e.push(i);
                } else if a.is_positive() {
                    p.push(i);
                } else {
                    n.push(i);
                }
            }

            println!("|p| = {},  |n| = {},  |e| = {}", p.len(), n.len(), e.len());

            if p.len() + n.len() + e.len() == 0 {
                // Target variable does not occur. We're done.
                self.rows = result
            } else if p.len() + e.len() == 0 {
                // Target has no upper bounds. Just drop it.
                o.set(target.into(), true);
                result.reserve(n.len());
                for a in n {
                    result.push(self.rows[a].drop(target))
                }
                // self.vars -= 1;
                self.rows = result
            } else if n.len() + e.len() == 0 {
                // Target has no lower bounds. Just drop it.
                o.set(target.into(), true);
                result.reserve(p.len());
                for b in p {
                    result.push(self.rows[b].drop(target))
                }
                // self.vars -= 1;
                self.rows = result
            } else if !e.is_empty() {
                // Eliminate target through rewriting everywhere.

                // rw is the equation with the least number of xs, tie breaking using the absolute value of the right side.
                /*
                let &rw_index = e.iter().min_by_key(|&&m| -> i64 {
                    (i64::try_from(self.rows[m].xs.len()).unwrap() * 2_i64.pow(32)) + (self.rows[m].coeff(target) * 2_i64.pow(16)) + (self.rows[m].b.to_integer())
                }).unwrap();
                */
                let rw_index = 0;
                let rw = &self.rows[rw_index];
                // println!("Using {} to eliminate x{}", &rw, &target);

                for i in e {
                    if i == rw_index {
                        continue;
                    } else {
                        result.push(self.rows[i].substitute(rw, target));
                    }
                }

                for i in n {
                    result.push(self.rows[i].substitute(rw, target));
                }

                for i in p {
                    result.push(self.rows[i].substitute(rw, target));
                }

                self.rows = result;
            } else {
                // Exponential blow-up, off we go!
                o.set(target.into(), true);
                result.reserve(n.len() * p.len());
                for ai in n {
                    for &bi in &p {
                        let a: &Ineq<_, _, _> = &self.rows[ai];
                        let b: &Ineq<_, _, _> = &self.rows[bi];

                        let ascaled = a.mul(b.coeff(target));
                        let bscaled = b.mul(a.coeff(target).abs());

                        // TODO: Avoid eagerly computing c.
                        let mut c = ascaled.add_elim(&bscaled, target);
                        c.reduce_mut();

                        let hc = &c.history;
                        let ic = &c.implicit;
                        let ec = &c.explicit;
                        let ico = o.clone() & ic;

                        if (ico | ec).count_ones() < hc.count_ones() {
                            result.push(c);
                        } else {
                            // println!("Did not add: {}", c)
                        }
                    }
                }

                /*
                for i in off..(result.len() - 1) {
                    for j in (i+1)..result.len() {
                        let a = &result[i].history;
                        let b = &result[j].history;
                        let mut tmp = a.clone();
                        if tmp & b == tmp {
                            // The history of i is a subset of the history of j.
                        } else {
                            tmp = b.clone();
                            if tmp & a == tmp {
                                // The history of j is a subset of the history of i.
                            }
                        }
                    }
                }
                */
                self.rows = result;
                // self.vars -= 1;
            }

            /*
            println!("After eliminating x{}", target);
            for ineq in self.rows.iter() {
                println!("{}", ineq);
            }
            */
        }
    }
}

impl<Coefficient, Variable, Rational> Display for System<Coefficient, Variable, Rational>
where
    Coefficient: Copy + Integer + Signed + Display,
    Variable: Copy + Ord + Eq + Display,
    Rational: Clone + Integer + From<Coefficient> + Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for ineq in self.rows.iter() {
            writeln!(f, "{}", ineq)?
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd)]
struct Mon<Coefficient = i64, Variable = u32>
where
    Coefficient: Copy + Zero + One + Signed,
    Variable: Copy + Ord + Eq,
{
    x: Variable,
    a: Coefficient,
}

impl<Coefficient, Variable> Mon<Coefficient, Variable>
where
    Coefficient: Copy + Zero + One + Signed,
    Variable: Copy + Ord + Eq,
{
    fn new(a: Coefficient, x: Variable) -> Mon<Coefficient, Variable> {
        debug_assert!(!a.is_zero());
        Mon { a, x }
    }

    fn one(x: Variable) -> Self {
        Mon {
            a: Coefficient::one(),
            x,
        }
    }
}

impl<Coefficient, Variable> Display for Mon<Coefficient, Variable>
where
    Coefficient: Copy + Zero + One + Signed + Display,
    Variable: Copy + Ord + Eq + Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        const NO_COEFF: &str = "  ";
        const NO_VAR: &str = "  ";

        if self.a.is_zero() {
            write!(f, "{}0{}", NO_COEFF, NO_VAR)
        } else {
            if self.a.is_positive() {
                write!(f, "+")?;
            } else if self.a.is_negative() {
                write!(f, "-")?;
            }

            let abs = self.a.abs();

            if abs.is_one() {
                write!(f, "{} x{:<2}", NO_COEFF, self.x)
            } else {
                write!(f, "{:<2} x{:<2}", abs, self.x)
            }
        }
    }
}

impl<Coefficient, Variable> Mul<Coefficient> for Mon<Coefficient, Variable>
where
    Coefficient: Copy + Integer + Signed,
    Variable: Copy + Ord + Eq,
{
    type Output = Self;

    fn mul(self, rhs: Coefficient) -> Self::Output {
        Mon::new(self.a * rhs, self.x)
    }
}

impl<Coefficient, Variable> Div<Coefficient> for Mon<Coefficient, Variable>
where
    Coefficient: Copy + Integer + Signed,
    Variable: Copy + Ord + Eq,
{
    type Output = Self;

    fn div(self, rhs: Coefficient) -> Self::Output {
        Mon::new(self.a / rhs, self.x)
    }
}

impl<Coefficient, Variable> DivAssign<Coefficient> for Mon<Coefficient, Variable>
where
    Coefficient: Copy + Integer + DivAssign<Coefficient> + Signed,
    Variable: Copy + Ord + Eq,
{
    fn div_assign(&mut self, rhs: Coefficient) {
        self.a /= rhs;
    }
}

impl<Coefficient, Variable> DivAssign<Coefficient> for &mut Mon<Coefficient, Variable>
where
    Coefficient: Copy + Integer + DivAssign<Coefficient> + Signed,
    Variable: Copy + Ord + Eq,
{
    fn div_assign(&mut self, rhs: Coefficient) {
        self.a /= rhs;
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Ineq<Coefficient = i64, Variable = u32, Rational = Coefficient>
where
    Coefficient: Copy + Integer + Signed,
    Variable: Copy + Ord + Eq,
    Rational: Clone + Integer + From<Coefficient>,
{
    xs: Vec<Mon<Coefficient, Variable>>,
    cmp: Comparison,
    b: Ratio<Rational>,
    history: BitVec,
    explicit: BitVec,
    implicit: BitVec,
}

fn bv_indices(bv: &BitVec) -> String {
    bv.iter()
        .enumerate()
        .filter_map(|(i, x)| if *x { Some(i.to_string()) } else { None })
        .join(", ")
}

impl<Coefficient, Variable, Rational> Display for Ineq<Coefficient, Variable, Rational>
where
    Coefficient: Copy + Integer + Signed + Display,
    Variable: Copy + Ord + Eq + Display,
    Rational: Clone + Integer + From<Coefficient> + Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.xs.is_empty() {
            write!(f, "{:>80}", "0")?;
        } else {
            write!(
                f,
                "{:>80}",
                self.xs.iter().map(ToString::to_string).join(" ")
            )?;
        }
        write!(
            f,
            " {} {:<+20.3} ({:<10}; {}; {})",
            self.cmp,
            self.b,
            bv_indices(&self.history),
            bv_indices(&self.explicit),
            bv_indices(&self.implicit)
        )
    }
}

impl<Coefficient, Variable, Rational> Ineq<Coefficient, Variable, Rational>
where
    Coefficient: Copy + Integer + Signed,
    Variable: Copy + Ord + Eq + Into<usize>,
    Rational: Clone + From<Coefficient> + Integer,
{
    fn reduce(&mut self) -> Self {
        if let Some(gcd) = self.xs.iter().map(|m| m.a).reduce(|x, y| x.gcd(&y)) {
            self.new_partial(
                self.xs.iter().map(|m| m.div(gcd)).collect(),
                &self.b / Ratio::from_integer(gcd.into()),
            )
        } else {
            panic!("could not compute gcd")
        }
    }
}

impl<Coefficient, Variable, Rational> Ineq<Coefficient, Variable, Rational>
where
    Coefficient: Copy + Integer + Signed + DivAssign<Coefficient>,
    Variable: Copy + Ord + Eq + Into<usize>,
    Rational: Clone + From<Coefficient> + Integer + NumAssign,
{
    fn reduce_mut(&mut self) {
        if let Some(gcd) = self.xs.iter().map(|m| m.a).reduce(|x, y| x.gcd(&y)) {
            for mut m in self.xs.iter_mut() {
                m /= gcd;
            }
            self.b /= Ratio::from_integer(gcd.into());
        }
    }

    fn substitute(&self, other: &Self, target: Variable) -> Self {
        debug_assert_eq!(other.cmp, Comparison::Eq);

        let other_coeff = other.coeff(target);
        debug_assert!(!other_coeff.is_zero());

        let self_coeff = self.coeff(target);
        debug_assert!(!self_coeff.is_zero());

        // We want to ensure that if the coefficients have the same polarity,
        // other is always scaled negatively, and self is always scaled positively.

        let other_scaled = other.mul(
            if self_coeff.is_negative() || (other_coeff.is_negative() && self_coeff.is_negative()) {
                self_coeff
            } else {
                self_coeff.neg()
            },
        );

        let mut combined = self.mul(other_coeff.abs()).add_elim(&other_scaled, target);
        combined.reduce_mut();
        combined
    }
}

impl<Coefficient, Variable, Rational> Ineq<Coefficient, Variable, Rational>
where
    Coefficient: Copy + Integer + Signed,
    Variable: Copy + Ord + Eq + Into<usize>,
    Rational: Clone + From<Coefficient> + Integer,
{
    fn new_partial(&self, xs: Vec<Mon<Coefficient, Variable>>, b: Ratio<Rational>) -> Self {
        Ineq {
            xs,
            cmp: self.cmp,
            b,
            history: self.history.clone(),
            explicit: self.explicit.clone(),
            implicit: self.implicit.clone(),
        }
    }

    fn new(
        xs: Vec<Mon<Coefficient, Variable>>,
        cmp: Comparison,
        b: Rational,
        id: usize,
        n: usize,
        vars: usize,
    ) -> Self {
        debug_assert!(id <= n);
        // TODO: Use unstable feature 'is_sorted'?
        debug_assert!(xs.windows(2).all(|window| window[0].x < window[1].x));

        let mut history = BitVec::repeat(false, n);
        history.set(id, true);
        Ineq {
            xs,
            cmp,
            b: Ratio::from_integer(b),
            history,
            explicit: BitVec::repeat(false, vars),
            implicit: BitVec::repeat(false, vars),
        }
    }

    fn drop(&self, target: Variable) -> Self {
        let mut explicit = self.explicit.clone();
        explicit.set(target.into(), true);
        Ineq {
            xs: self.xs.iter().copied().filter(|m| m.x != target).collect(),
            cmp: self.cmp,
            b: self.b.clone(),
            history: self.history.clone(),
            implicit: self.implicit.clone(),
            explicit,
        }
    }

    fn max_var(&self) -> Variable {
        self.xs[self.xs.len() - 1].x
    }

    fn var_index(&self, target: Variable) -> Option<usize> {
        self.xs.binary_search_by(|m| m.x.cmp(&target)).ok()
    }

    fn contains_var(&self, target: Variable) -> bool {
        self.var_index(target).is_some()
    }

    fn coeff(&self, target: Variable) -> Coefficient {
        self.var_index(target)
            .map_or(Coefficient::zero(), |i| self.xs[i].a)
    }

    fn add_elim(&self, other: &Self, target: Variable) -> Self {
        debug_assert!((self.coeff(target) + other.coeff(target)).is_zero());

        let mut result: Vec<Mon<_, _>> =
            Vec::with_capacity(self.xs.len().max(other.xs.len()) - 1_usize);
        let mut implicit = self.implicit.clone();
        let mut explicit = self.explicit.clone();

        // Check sizes of implicit/explicit vectors.
        debug_assert!((target.into()) < explicit.len());
        debug_assert!((target.into()) < implicit.len());

        // Check that target has not been eliminated yet.
        debug_assert!(!explicit.get(target.into()).unwrap());
        debug_assert!(!implicit.get(target.into()).unwrap());

        explicit.set(target.into(), true);

        // We principally iterate over self, squeezing in variables from other.
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
                        let x = self.xs[i].x;
                        let a = self.xs[i].a + other.xs[j].a;
                        if a.is_zero() {
                            // Variable is implicitly eliminated.
                            debug_assert!(!explicit.get(x.into()).unwrap());
                            debug_assert!(!implicit.get(x.into()).unwrap());
                            implicit.set(x.into(), true);
                        } else {
                            // Variable is not eliminated.
                            result.push(Mon { x, a });
                        }
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

        #[cfg(debug_assertions)]
        {
            for &m in result.iter() {
                debug_assert!(!explicit.get(m.x.into()).unwrap());
                debug_assert!(!implicit.get(m.x.into()).unwrap());
            }
        }

        Ineq {
            xs: result,
            cmp: self.cmp.combine(other.cmp).unwrap(),
            b: self.b.clone() + &other.b,
            history: self.history.clone() | &other.history,
            explicit,
            implicit,
        }
    }
}

impl<Coefficient, Variable, Rational> Mul<Coefficient> for &Ineq<Coefficient, Variable, Rational>
where
    Coefficient: Copy + Integer + Signed,
    Variable: Copy + Ord + Eq + Into<usize>,
    Rational: Clone + Integer + From<Coefficient>,
{
    type Output = Ineq<Coefficient, Variable, Rational>;

    fn mul(self, rhs: Coefficient) -> Self::Output {
        self.new_partial(
            self.xs.iter().map(|m| *m * rhs).collect_vec(),
            self.b.clone() * Ratio::from_integer(rhs.into()),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /*
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
    */

    /// https://de.wikipedia.org/wiki/Fourier-Motzkin-Elimination#Beispiel_für_die_Fourier-Motzkin-Elimination
    #[test]
    fn wikipedia() {
        let mut system: System<i32, usize> = System::new(&[
            (&[Mon::new(1, 0)], Comparison::Le, 1),
            (&[Mon::new(-2, 0), Mon::new(-4, 1)], Comparison::Le, -14),
            (&[Mon::new(-1, 0), Mon::new(2, 1)], Comparison::Le, 1),
        ]);

        system.elim(&[0]);
        for ineq in system.rows {
            println!("{}", ineq);
        }
    }

    /// https://youtu.be/s-9oyoiTTl8?t=666
    #[test]
    fn youtube() {
        let mut system: System<i32, usize> = System::new(&[
            (&[Mon::one(1), Mon::one(2)], Comparison::Le, 1),
            (
                &[Mon::one(1), Mon::one(2), Mon::new(2, 3)],
                Comparison::Le,
                2,
            ),
            (&[Mon::new(2, 1), Mon::new(3, 3)], Comparison::Le, 3),
            (&[Mon::one(1), Mon::new(-4, 3)], Comparison::Le, 4),
            (
                &[Mon::new(-2, 1), Mon::one(2), Mon::new(-1, 3)],
                Comparison::Le,
                5,
            ),
        ]);

        for ineq in system.rows.iter() {
            println!("{}", ineq);
        }

        println!();

        system.elim(&[0]);
        for ineq in system.rows.iter() {
            println!("{}", ineq);
        }

        println!();

        system.elim(&[3]);
        for ineq in system.rows.iter() {
            println!("{}", ineq);
        }
    }

    #[test]
    fn imbert_1_8() {
        let mut system = System::canonical(&[
            &[0, 0, -1, -1, -1, 0, 0, 0, -1],
            &[1, 0, 0, 2, 0, 0, -1, 0, -2],
            &[0, 1, 0, 0, 2, 0, 0, -1, -2],
            &[0, -2, 0, 0, -3, 0, 0, 1, 1],
            &[0, 1, 0, 0, 0, 0, 0, 0, 0],
            &[0, 0, 1, 0, 0, 0, 0, 0, 0],
            &[-1, -1, 2, 0, 0, -1, 0, 0, -3],
            &[-1, -1, -2, -2, -2, 1, 1, 1, 5],
            &[0, -1, 0, 0, -1, 0, 2, 0, 0],
        ]);

        for ineq in system.rows.iter() {
            println!("{}", ineq);
        }

        println!("---");
        system.elim(&[0, 1]);
    }

    #[test]
    fn kobialka_ex_2_3_3() {
        let mut system = System::canonical(&[
            &[1, 1, 4],
            &[1, -4, 2],
            &[-1, -4, -3],
            &[-1, 2, 3],
            &[-3, 2, 4],
            &[-1, -1, 4],
        ]);

        for ineq in system.rows.iter() {
            println!("{}", ineq);
        }

        println!("---");
        system.elim(&[0]);

        for ineq in system.rows.iter() {
            println!("{}", ineq);
        }

        println!("---");
        system.elim(&[1]);

        for ineq in system.rows.iter() {
            println!("{}", ineq);
        }
    }

    #[test]
    fn with_equality() {
        let mut system: System<i32, usize> = System::new(&[
            (&[Mon::one(1), Mon::one(2)], Comparison::Eq, 1),
            (
                &[Mon::one(1), Mon::one(2), Mon::new(2, 3)],
                Comparison::Le,
                2,
            ),
            (&[Mon::new(2, 1), Mon::new(3, 3)], Comparison::Le, 3),
            (&[Mon::one(1), Mon::new(-4, 3)], Comparison::Le, 4),
            (
                &[Mon::new(-2, 1), Mon::one(2), Mon::new(-1, 3)],
                Comparison::Le,
                5,
            ),
        ]);

        println!("initial");
        for ineq in system.rows.iter() {
            println!("{}", ineq);
        }

        println!("---");

        system.elim(&[0, 3, 1]);
        println!("{}", &system);
    }
}
