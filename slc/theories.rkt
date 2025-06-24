#lang racket/base

(require "terms.rkt")
(provide (all-defined-out))

;; A Lawvere Theory T
;; Operations are implicitly defined by their usage in axioms.
;; For stricter checking, we could add an operations signature (symbol -> arity).
(struct lawvere-theory (name axioms) #:transparent)
; axioms: (Listof (Cons TermPattern TermPattern))
; e.g., '((lhs . rhs) ...)

;; Example: (define T_Mon (lawvere-theory 'Monoid '(...axioms here...)))

;; Optional: Macro for defining theories
(define-syntax-rule (define-theory TheoryName (axioms (= lhs rhs) ...))
  (define TheoryName
    (lawvere-theory 'TheoryName
                    (list (cons #'lhs #'rhs) ...))))

;; Example with macro:
;; (define-theory Monoid-Test
;;   (axioms (= (op * (op * x y) z) (op * x (op * y z)))
;;           (= (op * e x) x)))

(module+ test
  (require rackunit)
  (require "terms.rkt") ; For term constructors in test

  (define test-ax1
    (cons (term-op '* (list (term-var 'x) (term-const 'e)))
          (term-var 'x)))
  (define T_Test (lawvere-theory 'TestTheory (list test-ax1)))

  (check-pred lawvere-theory? T_Test)
  (check-equal? (lawvere-theory-name T_Test) 'TestTheory)
)