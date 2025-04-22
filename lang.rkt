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

    ;; Added rational-exp
    ;; We use a different syntax from the given homework
    ;; to avoid conflicts with the existing grammar
    (expression ("rational" "(" number "/" number ")") rational-exp)


    ;; Use literal strings for keywords in grammar rules
    (expression ("op" "(" expression "," expression "," number ")") op-exp)

    (expression
     ("zero?" "(" expression ")")
     zero?-exp)

    
    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)   


    ;; -----------------------
    ;; Add the list-exp rule
    (expression ("create-new-list()") list-exp)
    ;; Add the cons-exp rule
    (expression ("cons" expression "to" expression) cons-exp)
    ;; Add the mul-exp rule
    (expression ("multiplication" "(" expression ")") mul-exp)
    ;; Add the min-exp rule based on image
    (expression ("min" "(" expression ")") min-exp)
    
    ;; Remove the conflicting if-elif-exp rule and replace with a new approach
    ;; that handles if-statements with optional elif clauses
    (expression ("if" expression "then" expression (arbno "elif" expression "then" expression) "else" expression) if-exp-with-elifs)
    
    ;; Add the simpl-exp rule
    (expression ("simpl" "(" expression ")") simpl-exp)
    ;; Add support for the -(x,y) expression syntax
    (expression ("-" "(" expression "," expression ")") diff-exp)

    (expression
       ("proc" "(" identifier ")" expression)
       proc-exp)

    ;; Modified call-exp to support function call syntax
    (expression
       ("(" expression expression ")")
       call-exp)

    ;; Add the var-exp rule based on image - moved lower in the grammar to resolve conflict
    (expression (identifier) var-exp)
       
    (expression
        ("letrec"
          identifier "(" identifier ")" "=" expression
           "in" expression)
        letrec-exp)
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

