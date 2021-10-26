# pepr

## Idea

### Parsing

#### TPTP CNF

#### TPTP FOF

Requires normalizing.

#### SMT-LIB 2

Requires normalizing.

### Normalizing

 1. Abort if there is a function symbol.
 2. Bring formula in prenex normal form, renaming variables in case of clashes.
 3. Abort if there is an existential quantifier in the scope of a universal quantifier.
 4. Replace all existentially quantified variables with constants ("skolemize").
 5. Drop universal quantifiers, from now on all variables are implicitly universally quantified.
 6. Translate to CNF.

### Solving

#### Unpacked

Clauses are the addressed objects in the heap.

For every clause, store a "header":
 - length
 - maybe indices of literals?
 - other flags (ground?, future)
Then, all literals.
 
For every literal, store a "header":
 - predicate symbol (integer)
 - positive/negative
 - other flags (ground?, future)
Then, all terms.

Every term is an integer (positive for constants, negative for variables).

Binary clauses are to be stored separately.

If packing does not work (not enough bits), then resort to a separate memory region for something...

If it would be possible to consistently address the arguments of a predicate, i.e. a sequence of
terms, then it might make sense to pack the predicate symbol and its sign into one integer, and
the arguments into another integer. If there are not many predicates, this is not helpful...

#### Packed

##### Sign

We spend one bit to encode sign:
 * positive numbers represent positive literals
 * negative numbers represent negative literals

##### Predicate

All predicate symbols are known after normalization, and can be numbered.

##### Terms

The number of variables (and whether they are quantified universally or existentially)
is known after normalization, they can also be numbered.

##### Arity

The arity of all predicate symbols is known after normalization.

##### Masking

After normalization, we can define one bitmask that can be applied to any literal,
in order to find out
 * its sign
 * term on position n

##### Decision

Only pack if we can fit twice as many variables as there are in the formula
into every position of every predicate.

##### Substitutions

Just smash variable and constant bits in the same register.
By definition they will not overlap.

##### Ordering

How does the ordering look like? Maybe we can even use this trick to compare
literals/clauses directly?! If the ordering is dynamic, this does not work.

##### Example

###### Even

  1 bit for sign
 21 bits for 2097152 predicates of maximum arity 1
 21 bits for 2097152 existentially quantified variables
 21 bits for 2097152 universally quantified variables

###### Few Predicates

On a 64bit machine, we could encode:

 1 bit  for sign
 2 bits for 4 predicates of maximum arity 1
30 bits for 1073741824 existentially quantified variables
30 bits for 1073741824 universally quantified variables

###### Arity

  1 bit for sign
 13 bits for 8192 predicates of maximum arity 4
 12 bits for 4096 existentially quantified variables
 12 bits for 4096 universally quantified variables

#### Unification

Use `ena` crate?

### Results

Satisfiable, Unsatisfiable, Timeout, Unknown (formula is not in BS)

## Input

### CASC EPR Track

http://www.tptp.org/CASC/27/SelectedProblems.html
http://www.tptp.org/CASC/J9/SelectedProblems.html

### `qbf2epr`

http://fmv.jku.at/qbf2epr/
http://fmv.jku.at/papers/SeidlLonsingBiere-PAAR12.pdf

### `smv2tptp`

https://nokyotsu.com/me/tools/#effectively-propositional-logic

## Solvers

### EPR

https://www.cs.man.ac.uk/~korovink/iprover/index.html
https://vprover.github.io/index.html

### Rust CDCL

https://github.com/shnarazk/splr
https://github.com/jix/varisat (generates proofs)
https://github.com/togatoga/screwsat

## Useful Crates

### TPTP

Parses FOF and CNF: https://crates.io/crates/tptp

### SMT-LIB

Parses SMT-LIB-2: https://crates.io/crates/smt2parser
