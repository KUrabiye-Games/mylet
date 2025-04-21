#lang eopl

;; grammar for the LET language  

(provide (all-defined-out))

;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)

    ;;
    ;; -----------------------
    ;; INSERT YOUR CODE HERE 
    (keyword ("cons") cons)
    (keyword ("to") to)
    (keyword ("multiplication") multiplication) ; Added keyword
    (keyword ("create-new-list") create-new-list) ; Added keyword
    (keyword ("zero?") zero?) ; Added keyword
    (keyword ("if") if) ; Added keyword
    (keyword ("elif") elif) ; Added keyword
    (keyword ("then") then) ; Added keyword
    (keyword ("else") else) ; Added keyword
    (keyword ("let") let) ; Added keyword
    (keyword ("in") in) ; Added keyword
    (keyword ("op") op) ; Added keyword
    (keyword ("min") min) ; Added keyword
    (keyword ("simplification") simplification) ; Added keyword for simpl-exp
    ;; -----------------------


    ;; -----------------------
  ))

(define the-grammar
  '((program (expression) a-program)
    
    (expression (number) const-exp)

    ;; Added rational-exp based on image
    (expression ("(" number "/" number ")") rational-exp)

    ;; Added op-exp based on image
    (expression ("op" "(" expression "," expression "," number ")") op-exp)

    (expression
     ("zero?" "(" expression ")")
     zero?-exp)
    
    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)   


    ;; -----------------------
    ;; INSERT YOUR CODE HERE 
    ;; Add the list-exp rule
    (expression ("create-new-list") list-exp)
    ;; Add the cons-exp rule
    (expression ("cons" expression "to" expression) cons-exp)
    ;; Add the mul-exp rule
    (expression ("multiplication" "(" expression ")") mul-exp) ; Added rule
    ;; Add the min-exp rule based on image
    (expression ("min" "(" expression ")") min-exp)
    ;; Add the if-elif-exp rule based on image
    (expression ("if" expression "then" expression "elif" expression "then" expression "else" expression) if-elif-exp)
    ;; Add the var-exp rule based on image
    (expression (identifier) var-exp)
    ;; Add the simpl-exp rule
    (expression ("simplification" "(" expression ")") simpl-exp)
    ;; -----------------------
))
)

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

