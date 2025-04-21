#lang eopl

;; interpreter for the LET language.  The \commentboxes are the
;; latex code for inserting the rules into the code in the book.
;; These are too complicated to put here, see the text, sorry.

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
;; Page: 71
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
;; Page: 71
(define value-of
  (lambda (exp env)
    (cases expression exp
      
      (const-exp (num) (num-val num))

      (var-exp (var) (apply-env env var))
      
      (op-exp (exp1 exp2 op)
              (let ((val1 (value-of exp1 env))
                    (val2 (value-of exp2 env)))
                  (let ((num1 (expval->rational val1))
                        (num2 (expval->rational val2)))
                      (cond 
                        ((and (number? num1) (number? num2))
                          (num-val
                            (cond 
                              ((= op 1) (+ num1 num2))
                              ((= op 2) (* num1 num2))
                              )))
                        
                        ((and (number? num1) (not (number? num2)))
                          (rational-val
                          (let ((num2top (car num2))
                                (num2bot (cdr num2)))
                            (cond 
                              ((= op 1) (cons (+ (* num1 num2bot) num2top) num2bot))
                              ((= op 2) (cons (* num1 num2top) num2bot))
                              ))))

                        ((and (number? num2) (not (number? num1)))
                          (rational-val
                          (let ((num1top (car num1))
                                (num1bot (cdr num1)))
                            (cond 
                              ((= op 1) (cons (+ (* num1bot num2) num1top) num1bot))
                              ((= op 2) (cons (* num1top num2) num1bot))
                              ))))

                        (else
                          (rational-val
                          (let ((num1top (car num1))
                                (num1bot (cdr num1))
                                (num2top (car num2))
                                (num2bot (cdr num2)))
                            (cond 
                              ((= op 1) (cons (+ (* num1top num2bot) (* num1bot num2top)) (* num1bot num2bot))) ;; add
                              ((= op 2) (cons (* num1top num2top) (* num1bot num2bot))) ;; multiply
                            ))))))))
      (zero?-exp (exp1) ; Updated zero?-exp for rationals
                 (let ((val1 (value-of exp1 env)))
                   (let ((eval1 (expval->rational val1)))
                     (cond
                       ((number? eval1) (bool-val (zero? eval1)))
                       ((pair? eval1) (bool-val (zero? (car eval1)))) ; Check if numerator is zero
                       (else (eopl:error 'value-of "zero? requires a number or rational, got ~s" val1))))))

      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))

      ;; Handle list-exp and cons-exp
      (list-exp () (list-val '())) ;; Create an empty list value

      (cons-exp (exp1 lst-exp)
                (let ((val1 (value-of exp1 env))
                      (lst-val (value-of lst-exp env)))
                  (let ((num (expval->num val1)) ; Ensure the first expression is a number
                        (lst (expval->list lst-val))) ; Ensure the second expression is a list
                    (if (and (number? num) (list? lst) (andmap number? lst)) ; Check if list contains only numbers
                        (list-val (cons num lst))
                        (eopl:error 'value-of "cons requires a number and a list of numbers, got ~s and ~s" val1 lst-val)))))

      ;; Handle mul-exp
      (mul-exp (lst-exp)
               (let ((lst-val (value-of lst-exp env)))
                 (let ((lst (expval->list lst-val)))
                   (if (list? lst)
                       (if (null? lst)
                           (num-val 0) ; Multiplication of empty list is 0
                           (if (andmap number? lst) ; Ensure all elements are numbers
                               (num-val (apply * lst))
                               (eopl:error 'value-of "mul-exp requires a list of numbers, got ~s" lst-val)))
                       (eopl:error 'value-of "mul-exp requires a list, got ~s" lst-val)))))

      ;; Handle other expressions from the image
      (rational-exp (num1 num2)
                    (if (zero? num2)
                        (eopl:error 'value-of "Denominator cannot be zero in rational number: ~s/~s" num1 num2)
                        (rational-val (cons num1 num2)))) ; Use cons to create the pair

      (op-exp (exp1 exp2 num) ; Updated op-exp based on image syntax
              (let ((val1 (value-of exp1 env))
                    (val2 (value-of exp2 env)))
                (let ((eval1 (expval->rational val1))
                      (eval2 (expval->rational val2)))
                  (cond
                    ;; Case 1: Both are numbers
                    ((and (number? eval1) (number? eval2))
                     (num-val (cond ((= num 1) (+ eval1 eval2))      ; Add
                                    ((= num 2) (* eval1 eval2))      ; Multiply
                                    (else (eopl:error 'value-of "Unknown op number: ~s" num)))))
                    ;; Case 2: First is number, second is rational
                    ((and (number? eval1) (pair? eval2))
                     (let ((n2 (car eval2)) (d2 (cdr eval2)))
                       (rational-val
                        (cond ((= num 1) (cons (+ (* eval1 d2) n2) d2)) ; Add: n1 + n2/d2 = (n1*d2 + n2)/d2
                              ((= num 2) (cons (* eval1 n2) d2))      ; Multiply: n1 * (n2/d2) = (n1*n2)/d2
                              (else (eopl:error 'value-of "Unknown op number: ~s" num))))))
                    ;; Case 3: First is rational, second is number
                    ((and (pair? eval1) (number? eval2))
                     (let ((n1 (car eval1)) (d1 (cdr eval1)))
                       (rational-val
                        (cond ((= num 1) (cons (+ n1 (* eval2 d1)) d1)) ; Add: n1/d1 + n2 = (n1 + n2*d1)/d1
                              ((= num 2) (cons (* n1 eval2) d1))      ; Multiply: (n1/d1) * n2 = (n1*n2)/d1
                              (else (eopl:error 'value-of "Unknown op number: ~s" num))))))
                    ;; Case 4: Both are rational
                    ((and (pair? eval1) (pair? eval2))
                     (let ((n1 (car eval1)) (d1 (cdr eval1))
                           (n2 (car eval2)) (d2 (cdr eval2)))
                       (rational-val
                        (cond ((= num 1) (cons (+ (* n1 d2) (* n2 d1)) (* d1 d2))) ; Add: n1/d1 + n2/d2 = (n1*d2 + n2*d1)/(d1*d2)
                              ((= num 2) (cons (* n1 n2) (* d1 d2)))      ; Multiply: (n1/d1) * (n2/d2) = (n1*n2)/(d1*d2)
                              (else (eopl:error 'value-of "Unknown op number: ~s" num))))))
                    (else (eopl:error 'value-of "op-exp requires numbers or rationals, got ~s and ~s" val1 val2)))))

      (min-exp (lst-exp) ; Added min-exp handler
               (let ((lst-val (value-of lst-exp env)))
                 (let ((lst (expval->list lst-val)))
                   (if (list? lst)
                       (if (null? lst)
                           (num-val -1) ; Return -1 for empty list
                           (if (andmap number? lst)
                               (num-val (apply min lst))
                               (eopl:error 'value-of "min-exp requires a list of numbers, got ~s" lst-val)))
                       (eopl:error 'value-of "min-exp requires a list, got ~s" lst-val)))))

      (if-elif-exp (exp1 exp2 exp3 exp4 exp5) ; Added if-elif-exp handler
                   (let ((test1-val (value-of exp1 env)))
                     (if (expval->bool test1-val)
                         (value-of exp2 env)
                         (let ((test2-val (value-of exp3 env)))
                           (if (expval->bool test2-val)
                               (value-of exp4 env)
                               (value-of exp5 env))))))

      ))))))