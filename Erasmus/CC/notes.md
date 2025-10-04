# Lecture notes

From parsers

## LL1 Parsers

### First Set

- if there's a rule `A -> alpha | beta`
- `FIRST(alpha)` - set of all terminals that can be a start of any word derivable of `alpha`

### Follow Set

- if `epsilon` is a production
- keep track which terminals can follow the non-terminal
- `$` is in `FOLLOW(A)` if `A` is a rightmost syntax

for a production rule A -> aplha B gamma FOLLOW(B) contains FIRST(gamma)

### A context-free grammar is LL1 when:

- for all rules: `A -> alpha | beta`:
- `FIRST(alpha) interception FIRST(beta) = 0`
- for all rules: `A -> alpha`:
- `FIRST(alpha) interception FIRST(A) = 0`

### Refactoring grammar to be LL1

- TODO

## Table Controlled Parser

- not recursive descent
- more generic algorithm

## Recursive Descent

- easier for manual coding
- lookahead symbols for predictive parsing

## Parser Generators

- we could use token spec. to create control tables and DFAs to automate scanners
- similarly we'd use grammar spec. 