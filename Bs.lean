import Std
import Std.Data.AssocList

-- Utility
def catOptions : List (Option α) -> List α := fun
  | [] => []
  | Option.none :: xs => catOptions xs
  | Option.some x :: xs => [x] ++ catOptions xs

abbrev Pred := String
abbrev Var := String
abbrev Const := String
abbrev Message := String

inductive Term where
  | var : Var -> Term
  | const : Const -> Term

def Term.free : Term -> Option Var := fun
  | Term.var v => some v
  | Term.const c => none

def Term.unify (t₁ t₂ : Term) : Sum (Var × Term) Message := match (t₁, t₂) with
  | (Term.var v₁, Term.var v₂) => Sum.inl (v₁, Term.var v₂)
  | (Term.const c, Term.var v) => Sum.inl (v, Term.const c)
  | (Term.var v, Term.const c) => Sum.inl (v, Term.const c)
  | _ => Sum.inr "Cannot unify!"

inductive Expr where
  | and  : Expr -> Expr -> Expr
  | or   : Expr -> Expr
  | not  : Expr -> Expr
  | atom : Pred -> List Term -> Expr

-- Lit Term can be a non-ground literal.
-- Lit Const is a ground literal.
inductive Lit α where
  | pos : Pred -> List α -> Lit α
  | neg : Pred -> List α -> Lit α

def Lit.pred : Lit α -> Pred := fun
  | pos p ts => p
  | neg p ts => p

def Lit.terms {α : Type}: Lit α -> List α := fun
  | pos p ts => ts
  | neg p ts => ts

def Lit.free (l : Lit Term) : List Var := catOptions $ l.terms.map Term.free

-- Negation.
def Lit.flip {α : Type} : Lit α -> Lit α := fun
  | pos p ts => Lit.neg p ts
  | neg p ts => Lit.pos p ts

-- def Lit.unify (l₁ l₂ : Lit) : Sum (List (Var × Term)) Message := List.zipWith Term.unify (Lit.terms l₁) (Lit.terms l₂)
-- def Lit.same (f : Pred -> List Term -> α) : (Lit -> α) := (λ l . match l with | Lit.pos => f l | Lit.neg => f r)

-- "Ground Literal"
abbrev GLit := Lit Const

-- A model should somehow map atoms to true/false.
abbrev Model := List GLit
abbrev Model' := Std.AssocList (Pred × GLit) Bool

abbrev Clause α := List (Lit α)

-- NOTE: I do not consider "constrained clauses" that involve both
-- background and foreground literals Λ ‖ C just yet, but only
-- clauses involving foreground literals.

-- Some way to translate an expression into CNF.
def cnf (e : Expr) : List (Clause Term) := sorry

def Clause.free : Clause Term -> List Var := fun
  | [] => []
  | l :: c' => (Lit.free l) ++ free c'

abbrev Subst := Std.AssocList Var Term

def Subst.grounding? (s : Subst) (c : Clause Term) : Bool := c.free.all s.contains

inductive Reason where
  -- "Propagation Literal" or "Propagated Literal"
  | prp : Clause Term -> Subst -> Reason
  -- "Decision Literal"
  | dec : Nat -> Reason

-- Some attempt at writing down the "state" of the calculus.
-- Question: Did I capture the trail (including "Reason") correctly?
structure State where
  trail : List ((GLit × Reason))
  init : List (Clause Term)
  learned : List (Clause Term)
  -- Question: Is this the object that defines the ordering of literals/clauses?
  b : List (Const)
  k : Nat

-- Question: I looked at "SCL Clause Learning from Simple Models" (Fiori, Weidenbach)
--  and it does not use B. How can it work without B and "Grow"?

def propagate (trail : List ((Lit Const × Reason))) : Empty := sorry
-- Head of the trail is the newest assignment.
-- Find all clauses watched by the literal that was assigned.
-- Question: How does this lookup work?
--   The clauses that are being watched consist of non-ground literals,
--   but the elements of the trail are ground literals.
--   How do we look up ground literals in a "table of" non-ground literals?
-- Try to find a new watched literal.
-- If there is none, either propagate or analyze conflict.

def grow  (s: State) (b': List Const) : State := sorry
-- Question: The paper talks about b' as a "non-empty sequence of foreground literals of background sorts".
--   I do not even understand what this means.

def compute (e : Expr) : Sum (Option Model) Message := sorry

def main : IO Unit :=
  IO.println "Hello!"