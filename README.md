# MYLET Language Interpreter

## Project Overview
This project implements an interpreter for the MYLET programming language, which is an extension of the LET language with additional features. The interpreter is written in Racket (EOPL).

## Features

### Current Functionality
- Basic arithmetic operations (`op` for addition, multiplication, division, subtraction)
- Variable binding with `let` expressions
- Conditional expressions with `if-elif-else` clauses
- Support for rational numbers with simplification
- List operations (creation, cons, multiplication, minimum)
- Procedures with multiple arguments (`proc`)
- Closures with proper lexical scoping

### Example Usage
```racket
;; Procedure with multiple arguments
let add = proc (x y) op(x, y, 1) in (add 3 4)

;; Rational number operations
let r = rational(3/2) in simpl(r)

;; List operations
let nums = cons 1 to cons 2 to cons 3 to create-new-list() in
multiplication(nums)
```

### Coming Soon
- `letrec` expression for recursive procedures
- Additional list operations
- Enhanced error handling

## File Structure
- `lang.rkt` - Grammar and parser for the MYLET language
- `data-structures.rkt` - Core data structures (environments, expressed values)
- `interp.rkt` - The interpreter implementation
- `environments.rkt` - Environment handling
- `tests.rkt` - Test cases for the interpreter
- `multi-proc-test.rkt` - Test cases focused on multi-argument procedures

## Running Tests
Execute any of the test files in a Racket environment to verify the implementation.

## License
This project is licensed under the GNU General Public License v3.0.
