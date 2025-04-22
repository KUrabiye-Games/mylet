#lang eopl
(require "lang.rkt")             ; for scan & parse
(require "interp.rkt")           ; for value-of-program

;; Define the run function
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

;; Test single-argument procedure (for backward compatibility)
(display "Testing single-argument procedure:\n")
(display (run "let f = proc (x) op(x, 2, 1) in (f 5)"))
(display "\n\n")

;; Test multiple-argument procedure
(display "Testing two-argument procedure:\n")
(display (run "let add = proc (x y) op(x, y, 1) in (add 3 4)"))
(display "\n\n")

;; Test multiple-argument procedure with more than two arguments
(display "Testing three-argument procedure:\n")
(display (run "let sum3 = proc (x y z) op(op(x, y, 1), z, 1) in (sum3 1 2 3)"))
(display "\n\n")

;; Test with nested procedure calls
(display "Testing nested procedure calls:\n")
(display (run "let add = proc (x y) op(x, y, 1) in 
              let mul = proc (x y) op(x, y, 2) in 
              (add (mul 2 3) (mul 4 5))"))
(display "\n\n")

;; Test procedure with no arguments
(display "Testing procedure with no arguments:\n")
(display (run "let constant = proc () 42 in (constant)"))
(display "\n\n")

;; Test with closures to make sure environment capture works correctly
(display "Testing closures with multiple arguments:\n")
(display (run "let x = 10 in
              let f = proc (a b) op(op(a, b, 1), x, 1) in
              let x = 5 in
              (f 1 2)"))
(display "\n\n")

;; Test higher-order functions
(display "Testing higher-order functions:\n")
(display (run "let make_adder = proc (x) proc (y) op(x, y, 1) in
              let add5 = (make_adder 5) in
              (add5 10)"))
(display "\n\n")

;; Test higher-order functions with multiple arguments
(display "Testing higher-order functions with multiple arguments:\n")
(display (run "let curry_add = proc (x) proc (y z) op(op(x, y, 1), z, 1) in
              let add5 = (curry_add 5) in
              (add5 10 20)"))
(display "\n\n")

;; Test with mixed rational and integer math
(display "Testing with rational numbers:\n")
(display (run "let divider = proc (x y) op(x, y, 3) in
              (divider 10 4)"))
(display "\n\n")

(display "Testing with rational and multiple arguments:\n")
(display (run "let rational_op = proc (r1 r2) op(r1, r2, 1) in
              (rational_op rational(3/2) rational(1/4))"))
(display "\n\n")

;; More complex example with multiple args and conditionals
(display "Testing complex function with multiple arguments and conditionals:\n")
(display (run "let max = proc (x y) 
                if zero?(-(x, y)) then y 
                elif zero?(-(y, x)) then x
                else if zero?(-(op(x, y, 4), 0)) then x else y
              in (max 8 5)"))
(display "\n\n")

;; Create a function that uses a procedure as an argument
(display "Testing with procedure as an argument:\n")
(display (run "let apply_to_5_and_10 = proc (func) (func 5 10) in
              let add = proc (x y) op(x, y, 1) in
              (apply_to_5_and_10 add)"))
(display "\n\n")

;; Test with list operations
(display "Testing with lists and multiple args:\n")
(display (run "let make_list = proc (x y z) 
                cons x to cons y to cons z to create-new-list()
               in
               let compute_list = proc (list1 list2) 
                  op(multiplication(list1), multiplication(list2), 2)
               in
               let list1 = (make_list 1 2 3) in
               let list2 = (make_list 4 5 6) in
               (compute_list list1 list2)"))
(display "\n\n")