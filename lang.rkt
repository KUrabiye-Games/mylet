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

   
  ))

(define the-grammar
  '((program (expression) a-program)
    
    (expression (number) const-exp)

    ;; Added rational-exp based on image
    (expression ("(" number "/" number ")") rational-exp)

    ;; Use literal strings for keywords in grammar rules
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
    (expression ("create-new-list()") list-exp)
    ;; Add the cons-exp rule
    (expression ("cons" expression "to" expression) cons-exp)
    ;; Add the mul-exp rule
    (expression ("multiplication" "(" expression ")") mul-exp)
    ;; Add the min-exp rule based on image
    (expression ("min" "(" expression ")") min-exp)
    ;; Add the if-elif-exp rule based on image
    (expression ("if" expression "then" expression "elif" expression "then" expression "else" expression) if-elif-exp)
    ;; Add the var-exp rule based on image
    (expression (identifier) var-exp)
    ;; Add the simpl-exp rule
    (expression ("simpl" "(" expression ")") simpl-exp)
    ;; -----------------------
))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

