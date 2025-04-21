#lang eopl

;; interpreter for the LET language.

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

;; Custom helper function to replace andmap
(define (all-satisfy? pred lst)
  (if (null? lst)
      #t
      (and (pred (car lst)) (all-satisfy? pred (cdr lst)))))

(provide value-of-program value-of)

;; Helper function for Greatest Common Divisor (GCD)
(define (gcd a b)
  (if (= b 0)
      (abs a) ; GCD is defined for non-negative numbers, take abs for safety
      (gcd b (remainder a b))))

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
;; Page: 71
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
      
      (const-exp (num) (num-val num))
      
      (var-exp (var) (apply-env env var))
      
      (rational-exp (num1 num2)
                    (if (zero? num2)
                        (eopl:error 'value-of "Denominator cannot be zero in rational number: ~s/~s" num1 num2)
                        (rational-val (cons num1 num2))))
      
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((eval1 (expval->rational val1)))
                     (cond
                       ((number? eval1) (bool-val (zero? eval1)))
                       ((pair? eval1) (bool-val (zero? (car eval1))))
                       (else (eopl:error 'value-of "zero? requires a number or rational, got ~s" val1))))))
      
      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))
      
      (list-exp () (list-val '()))
      
      (cons-exp (exp1 lst-exp)
                (let ((val1 (value-of exp1 env))
                      (lst-val (value-of lst-exp env)))
                  (let ((num (expval->num val1))
                        (lst (expval->list lst-val)))
                    (if (and (number? num) (list? lst) (all-satisfy? number? lst))
                        (list-val (cons num lst))
                        (eopl:error 'value-of "cons requires a number and a list of numbers, got ~s and ~s" val1 lst-val)))))
      
      (mul-exp (lst-exp)
               (let ((lst-val (value-of lst-exp env)))
                 (let ((lst (expval->list lst-val)))
                   (if (list? lst)
                       (if (null? lst)
                           (num-val 0)
                           (if (all-satisfy? number? lst)
                               (num-val (apply * lst))
                               (eopl:error 'value-of "mul-exp requires a list of numbers, got ~s" lst-val)))
                       (eopl:error 'value-of "mul-exp requires a list, got ~s" lst-val)))))
      
      (min-exp (lst-exp)
               (let ((lst-val (value-of lst-exp env)))
                 (let ((lst (expval->list lst-val)))
                   (if (list? lst)
                       (if (null? lst)
                           (num-val -1)
                           (if (all-satisfy? number? lst)
                               (num-val (apply min lst))
                               (eopl:error 'value-of "min-exp requires a list of numbers, got ~s" lst-val)))
                       (eopl:error 'value-of "min-exp requires a list, got ~s" lst-val)))))
      
      (if-elif-exp (exp1 exp2 exp3 exp4 exp5)
                   (let ((test1-val (value-of exp1 env)))
                     (if (expval->bool test1-val)
                         (value-of exp2 env)
                         (let ((test2-val (value-of exp3 env)))
                           (if (expval->bool test2-val)
                               (value-of exp4 env)
                               (value-of exp5 env))))))
      
      (simpl-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (cases expval val1
                     (num-val (num) val1)
                     (rational-val (p)
                                   (let ((n (car p))
                                         (d (cdr p)))
                                     (if (zero? d)
                                         (eopl:error 'value-of "Cannot simplify rational with zero denominator: ~s/~s" n d)
                                         (let ((common (gcd n d)))
                                           (rational-val (cons (/ n common) (/ d common)))))))
                     (else (eopl:error 'value-of "simpl-exp requires a number or rational, got ~s" val1)))))
      
      (op-exp (exp1 exp2 num)
              (let ((val1 (value-of exp1 env))
                    (val2 (value-of exp2 env)))
                (let ((eval1 (expval->rational val1))
                      (eval2 (expval->rational val2)))
                  (cond
                    ;; Case 1: Both are numbers
                    ((and (number? eval1) (number? eval2))
                     (num-val 
                      (cond 
                        ((= num 1) (+ eval1 eval2))
                        ((= num 2) (* eval1 eval2))
                        ((= num 3)
                         (if (zero? eval2)
                             (eopl:error 'value-of "Division by zero: ~s / ~s" eval1 eval2)
                             (/ eval1 eval2)))
                        (else (- eval1 eval2)))))
                    
                    ;; Case 2: First is number, second is rational
                    ((and (number? eval1) (pair? eval2))
                     (let ((n2 (car eval2)) (d2 (cdr eval2)))
                       (rational-val
                        (cond 
                          ((= num 1) (cons (+ (* eval1 d2) n2) d2))
                          ((= num 2) (cons (* eval1 n2) d2))
                          ((= num 3)
                           (if (zero? n2)
                               (eopl:error 'value-of "Division by zero in rational: ~s / (~s/~s)" eval1 n2 d2)
                               (cons (* eval1 d2) n2)))
                          (else (cons (- (* eval1 d2) n2) d2))))))
                    
                    ;; Case 3: First is rational, second is number
                    ((and (pair? eval1) (number? eval2))
                     (let ((n1 (car eval1)) (d1 (cdr eval1)))
                       (rational-val
                        (cond 
                          ((= num 1) (cons (+ n1 (* eval2 d1)) d1))
                          ((= num 2) (cons (* n1 eval2) d1))
                          ((= num 3)
                           (if (zero? eval2)
                               (eopl:error 'value-of "Division by zero: (~s/~s) / ~s" n1 d1 eval2)
                               (cons n1 (* d1 eval2))))
                          (else (cons (- n1 (* eval2 d1)) d1))))))
                    
                    ;; Case 4: Both are rational
                    ((and (pair? eval1) (pair? eval2))
                     (let ((n1 (car eval1)) (d1 (cdr eval1))
                           (n2 (car eval2)) (d2 (cdr eval2)))
                       (rational-val
                        (cond 
                          ((= num 1) (cons (+ (* n1 d2) (* n2 d1)) (* d1 d2)))
                          ((= num 2) (cons (* n1 n2) (* d1 d2)))
                          ((= num 3)
                           (if (or (zero? n2) (zero? d1))
                               (eopl:error 'value-of "Division by zero in rational division: (~s/~s) / (~s/~s)" n1 d1 n2 d2)
                               (cons (* n1 d2) (* d1 n2))))
                          (else (cons (- (* n1 d2) (* n2 d1)) (* d1 d2)))))))
                    
                    (else (eopl:error 'value-of "op-exp requires numbers or rationals, got ~s and ~s" val1 val2))))))
      
      (else (eopl:error 'value-of "Unknown expression type: ~s" exp))
      )))