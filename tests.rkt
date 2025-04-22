#lang eopl
(require eopl/tests/private/utils)

(require "data-structures.rkt")  ; for expval constructors
(require "lang.rkt")             ; for scan & parse
(require "interp.rkt")           ; for value-of-program

;; run : String -> ExpVal
;; Page: 71
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(define equal-answer?
  (lambda (ans correct-ans)
    ;; (equal? (eopl:error "ans " ans))))
    (equal? ans (sloppy->expval correct-ans))))

(define sloppy->expval 
  (lambda (sloppy-val)
    (cond
      ((number? sloppy-val) (num-val sloppy-val))
      ((boolean? sloppy-val) (bool-val sloppy-val))
      ;((string? sloppy-val) (str-val sloppy-val))
      ((list? sloppy-val) (list-val sloppy-val)) 
      ((pair? sloppy-val) (rational-val sloppy-val)) ; may not work if you define rational with list instead of cons
      (else
       (eopl:error 'sloppy->expval 
                   "Can't convert sloppy value to expval: ~s"
                   sloppy-val)))))

(define-syntax-rule (check-run (name str res) ...)
  (begin
    (cond [(eqv? 'res 'error)
           (check-exn always? (lambda () (run str)))]
          [else
           (check equal-answer? (run str) 'res (symbol->string 'name))])
    ...))

;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;

(check-run

 ;; simple arithmetic
 (positive-const "11" 11)
 (zero "0" 0)
 
 ;; check environment
 (env-i "i" error)
 
 ;; replace these with the values you defined
 (env-x "x" 4)
 (env-y "y" 2)
 (env-z "z" 3)
 
;; simple unbound variables
 (test-unbound-var-1 "foo" error)
 (test-unbound-var-2 "-(x,foo)" error)

;; test dynamic typechecking
 (no-bool-to-diff-1 "op(zero?(0),1, 4)" error)
 (no-bool-to-sum-2 "op(1,zero?(0), 1)" error)
 (no-int-to-if "if 1 then 2 else 3" error)
 (no-string-to-if "if 'gul' then 'sena' else 'altintas'" error)

;; simple let
 (simple-let-1 "let x = 3 in x" 3)
  
;; make sure the body and rhs get evaluated
 (eval-let-body "let x = 3 in op(x,1,4)" 2)
 (eval-let-rhs "let x = op(4,1,1) in op(x,1,4)" 4)

;; check nested let and shadowing
 (simple-nested-let "let x = 3 in let y = 4 in op(x,y, 4)" -1)
 (check-shadowing-in-body "let x = 3 in let x = 4 in x" 4)
 (check-shadowing-in-rhs "let x = 3 in let x = op(x,1,1) in x" 4)

;; op exps
 (op-test-sum "op(3, 2, 1)" 5)
 (op-test-mult "op(3, 2, 2)" 6)
 (op-test-div "op(3, 2, 3)" 3/2)
 (op-test-diff "op(3, 2, 4)" 1)

 ;; if elif else test
 (if-elif-test1  "if zero?(0) then 1 elif zero?(3) then 2 else 3" 1)
 (if-elif-test2 "if zero?(-5) then 1 elif zero?(0) then 2 else 3" 2)
 (if-elif-test3  "if zero?(0) then 1 elif zero?(0) then 2 else 3" 1)
 (if-elif-test3  "if zero?(2) then 1 elif zero?(4) then 2 else 3" 3)

;; rational number test

 (simple-rat-def "rational(5/2)"  (5 . 2))
 (simple-rat-def2 "rational(5/0)" error)

 (rat-op-test-sum1 "op(rational(5/2), rational(3/5), 1)" (31 . 10))
 (rat-op-test-sum2 "op(rational(5/2), 4, 1)" (13 . 2))
 (rat-op-test-sum3 "op(3, rational(3/5), 1)" (18 . 5))
 
 (rat-op-test-mult1 "op(rational(5/2), rational(3/5), 2)" (15 . 10))
 (rat-op-test-mult2 "op(rational(5/2), 4, 2)" (20 . 2))
 (rat-op-test-mult3 "op(3, rational(3/5), 2)" (9 . 5))
 
 (rat-op-test-div1 "op(rational(5/2), rational(3/5), 3)" (25 . 6))
 (rat-op-test-div2 "op(rational(5/2), 4, 3)" (5 . 8))
 (rat-op-test-div3 "op(3, rational(3/5), 3)" (15 . 3))

 (rat-op-test-diff1 "op(rational(5/2), rational(3/5), 5)" (19 . 10))
 (rat-op-test-diff2 "op(rational(5/2), 4, -1)" (-3 . 2))
 (rat-op-test-diff3 "op(3, rational(3/5), 10)" (12 . 5))

 (rat-op-test-simpl1 "simpl(rational(10/4))" (5 . 2))
 (rat-op-test-simpl2 "simpl(rational(10/3))" (10 . 3))
 (rat-op-test-simpl3 "simpl(rational(5/25))" (1 . 5))

 (rat-op-test-simpl-mult "simpl(op(rational(5/2), 4, 2))" (10 . 1))

 (new-list-test1 "create-new-list()" ())


 (new-list-test2 "cons 5 to create-new-list()" (5))
 (new-list-test3 "cons 6 to cons 5 to create-new-list()" (6 5))
 (new-list-test4 "cons 6 to cons 5 to create-new-list()" (6 5))

 
 (new-list-test5 "multiplication (cons 6 to cons 5 to create-new-list())" 30)
 (new-list-test6 "multiplication (cons 7 to cons 6 to cons 5 to create-new-list())" 210)
 (new-list-test7 "multiplication (create-new-list())" 0)

  
   
 (min-test1 "min (create-new-list())" -1)
 (min-test2 "min (cons 8 to cons 13 to cons 5 to create-new-list())" 5)
 (min-test3 "min (cons 2000 to cons 2000 to cons 2000 to create-new-list())" 2000)
 (min-test4 "min (cons 139 to create-new-list())" 139)  #||#


 (if-else-if-rand "if zero?(0) then 1 elif zero?(3) then 2 else 3" 1)
 (if-else-if-rand2 "if zero?(-5) then 1 elif zero?(2) then 2 elif zero?(0) then 7 else 3" 7)

 ;; Additional test cases for all features

 ;; Additional rational number tests
 (rational-zero "rational(0/1)" (0 . 1))
 (rational-negative "rational(-5/2)" (-5 . 2))
 (rational-nested-op "op(rational(1/2), op(rational(1/3), rational(1/6), 1), 1)" (36 . 36))
 (rational-complex-simpl "simpl(op(op(rational(2/3), rational(1/6), 1), rational(1/2), 2))" (5 . 12))



 ;; Additional list tests
 (list-nested-cons "cons 1 to cons 2 to cons 3 to create-new-list()" (1 2 3))
 (list-min-negative "min (cons -10 to cons 5 to cons 0 to create-new-list())" -10)
 (list-mul-one-item "multiplication (cons 42 to create-new-list())" 42)
 (list-complex-op "multiplication (cons 2 to cons op(3, 1, 4) to create-new-list())" 4)

 ;; Procedure tests
 (proc-identity "let f = proc (x) x in (f 10)" 10)
 (proc-add-one "let add1 = proc (x) op(x, 1, 1) in (add1 5)" 6)
 (proc-compose "let f = proc (x) op(x, 2, 2) in let g = proc (y) op(y, 3, 1) in (f (g 4))" 14)
 (proc-with-rational "let half = proc (x) op(x, rational(1/2), 2) in simpl((half 10))" (5 . 1))

 ;; Nested if-elif-else tests
 (if-complex-1 "if zero?(1) then 1 elif zero?(2) then 2 elif zero?(3) then 3 elif zero?(0) then 4 else 5" 4)
 (if-complex-2 "if zero?(0) then 1 elif zero?(0) then 2 elif zero?(0) then 3 else 4" 1)
 (if-complex-3 "if zero?(1) then 1 elif zero?(2) then 2 elif zero?(3) then 3 else 4" 4)
 (if-nested "if zero?(0) then if zero?(1) then 1 else 2 else 3" 2)

 ;; Diff expression tests
 (diff-simple "-(5, 3)" 2)
 (diff-negative "-(3, 5)" -2)
 (diff-rational "-(rational(5/2), rational(1/2))" (8 . 4))
 (diff-mixed "-(rational(3/2), 1)" (1 . 2))

 #|
 ;; Letrec tests
 (letrec-factorial "letrec f(n) = if zero?(n) then 1 else op(n, f(-(n,1)), 2) in f(5)" 120)
 (letrec-fibonacci "letrec fib(n) = if zero?(n) then 0 elif zero?(-(n,1)) then 1 else op(fib(-(n,1)), fib(-(n,2)), 1) in fib(6)" 8)
 (letrec-sum-to-n "letrec sum(n) = if zero?(n) then 0 else op(n, sum(-(n,1)), 1) in sum(5)" 15)
 (letrec-even-odd "letrec even(n) = if zero?(n) then 1 else odd(-(n,1)) 
                   in letrec odd(n) = if zero?(n) then 0 else even(-(n,1))
                   in even(4)" 1)
|#

 ;; Comprehensive mixed tests
 (comprehensive-1 
   "let x = 5 in 
    let f = proc(n) op(n, x, 1) in
    let x = 10 in
    (f 3)" 8)
 
 (comprehensive-2 
   "let make_adder = proc (y) proc(x) op(x, y, 1) in
    let add5 = (make_adder 5) in
    (add5 10)" 15)

 (comprehensive-3
   "let apply_twice = proc(f) proc(x) (f (f x)) in
    let add1 = proc(x) op(x, 1, 1) in
    let add2 = (apply_twice add1) in
    (add2 5)" 7)

 (comprehensive-4
   "let x = rational(1/2) in
    let y = rational(1/3) in
    let z = op(x, y, 1) in
    simpl(z)" (5 . 6))
    
 #|
 ;; This test requires recursion which is not supported yet
 (comprehensive-5
   "let make_list = proc(n) 
      if zero?(n) then
        create-new-list()
      else
        cons n to (make_list -(n,1))
    in
    min((make_list 5))" 1)
 |#

 (comprehensive-rational-list
   "let add_half = proc(x) op(x, rational(1/2), 1) in
    let xs = cons 1 to cons 2 to cons 3 to create-new-list() in
    multiplication(xs)" 6)
)