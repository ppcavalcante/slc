#lang racket/base

(require rackunit
         rackunit/text-ui
         "../slc/theories.rkt"
         "../slc/terms.rkt") ; For constructing terms in tests

(provide theories-test-suite)

(define theories-test-suite
  (test-suite
   "Lawvere Theory Definitions"

   (test-case "Theory Construction"
     (define ax1 (cons (term-var 'x) (term-const 'e)))
     (define T1 (lawvere-theory 'MyTheory (list ax1)))
     (check-pred lawvere-theory? T1)
     (check-equal? (lawvere-theory-name T1) 'MyTheory)
     (check-equal? (length (lawvere-theory-axioms T1)) 1))
   
   ;; Add more tests when define-theory macro is used or for specific properties
   ))

(module+ main
  (run-tests theories-test-suite))