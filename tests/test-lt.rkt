#lang racket/base

(require rackunit
         rackunit/text-ui
         "../slc/lt.rkt"
         "../slc/terms.rkt" 
         "../slc/theories.rkt"
         "../slc/examples/monoid.rkt"
         racket/set
         racket/list)

(provide lt-test-suite)

;; The lt-test-suite focuses on testing the Logic Theory (LT) component
;; of the SLC theorem, which is responsible for equational reasoning
(define lt-test-suite
  (test-suite
   "Logic Theory (LT) Tests"

   (test-case "lt-terms-equal? for Monoid"
     ;; Basic variable and constant terms
     (define x (mvar 'X))
     (define y (mvar 'Y))
     (define z (mvar 'Z))
     (define e-const me)

     ;; Basic equalities from monoid axioms
     (eprintf "--- Testing lt-terms-equal? with basic monoid axioms ---\n")
     (flush-output (current-error-port))
     
     ;; Test 1: Right identity: X*e = X
     (define term-xe (m* x e-const))
     (define result1 (lt-terms-equal? term-xe x T_Mon 2))
     (eprintf "lt-terms-equal? X*e = X: ~s\n" result1)
     (check-true result1 "X*e should equal X in the monoid theory")
     
     ;; Test 2: Left identity: e*X = X
     (define term-ex (m* e-const x))
     (define result2 (lt-terms-equal? term-ex x T_Mon 2))
     (eprintf "lt-terms-equal? e*X = X: ~s\n" result2)
     (check-true result2 "e*X should equal X in the monoid theory")
     
     ;; Test 3: Associativity: (X*Y)*Z = X*(Y*Z)
     (define term-assoc-lhs (m* (m* x y) z))
     (define term-assoc-rhs (m* x (m* y z)))
     (define result3 (lt-terms-equal? term-assoc-lhs term-assoc-rhs T_Mon 2))
     (eprintf "lt-terms-equal? (X*Y)*Z = X*(Y*Z): ~s\n" result3)
     (check-true result3 "Associativity should hold in the monoid theory")
     
     ;; Test 4: Reflexivity: X = X
     (define result4 (lt-terms-equal? x x T_Mon 1))
     (eprintf "lt-terms-equal? X = X: ~s\n" result4)
     (check-true result4 "Reflexivity should hold")
     
     ;; Test 5: Non-equality: X ≠ Y
     (define result5 (lt-terms-equal? x y T_Mon 2))
     (eprintf "lt-terms-equal? X = Y: ~s\n" result5)
     (check-false result5 "X should not equal Y in the monoid theory")

     ;; Test 6: Multi-step equality: (e*X)*e = X
     (define term-exe (m* (m* e-const x) e-const))
     (define result6 (lt-terms-equal? term-exe x T_Mon 3))
     (eprintf "lt-terms-equal? (e*X)*e = X: ~s\n" result6)
     (check-true result6 "(e*X)*e should equal X in the monoid theory"))

   ;; This is a placeholder for future lt-compose tests
   ;; when the function is implemented
   (test-case "lt-compose (placeholder)"
     (check-true (procedure? lt-compose) "lt-compose should be a procedure")
     (eprintf "Note: lt-compose is not fully implemented yet.\n"))
  ))

(module+ main
  (run-tests lt-test-suite))
