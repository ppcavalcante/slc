#lang racket/base

(require rackunit
         rackunit/text-ui
         "../slc/slc-simulator.rkt"
         "../slc/terms.rkt" 
         "../slc/theories.rkt"
         "../slc/examples/monoid.rkt"
         racket/set
         racket/list)

(provide slc-simulator-test-suite)

(define slc-simulator-test-suite
  (test-suite
   "SLC Simulator (Prover+Simulator) Tests"

   (test-case "simulate-rewrites function"
     ;; Basic variable and constant terms
     (define x (mvar 'X))
     (define e-const me)
     (define term-xe (m* x e-const))
     
     ;; Test simulation depth 1
     (define rewrites-depth1 (simulate-rewrites term-xe T_Mon 1))
     (eprintf "simulate-rewrites result for X*e (depth 1):\n")
     (for ([path rewrites-depth1]
           [i (in-naturals)])
       (eprintf "  Path ~a: ~s\n" i path))
     (flush-output (current-error-port))
     
     (check-true (list? rewrites-depth1) "simulate-rewrites should return a list")
     (check-not-equal? (length rewrites-depth1) 0 "Should return at least one path")
     
     ;; Test that one of the paths contains X
     (define has-x-path 
       (for/or ([path rewrites-depth1])
         (for/or ([term path])
           (term-equal? term x))))
     (check-true has-x-path "One of the rewrites of X*e should contain X")
     
     ;; Test simulation depth 2
     (define rewrites-depth2 (simulate-rewrites term-xe T_Mon 2))
     (check-true (list? rewrites-depth2) "simulate-rewrites should return a list")
     (check-true (>= (length rewrites-depth2) (length rewrites-depth1)) 
                 "Depth 2 should have at least as many paths as depth 1"))

   (test-case "prove-equality function"
     (define x (mvar 'X))
     (define y (mvar 'Y))
     (define z (mvar 'Z))
     (define e-const me)
     
     ;; Test basic equalities
     (check-true (prove-equality x x T_Mon 1) "X = X should be provable")
     (check-true (prove-equality (m* x e-const) x T_Mon 2) "X*e = X should be provable")
     (check-true (prove-equality (m* e-const x) x T_Mon 2) "e*X = X should be provable")
     
     ;; Test complex equalities
     (check-true (prove-equality 
                  (m* (m* x y) z) 
                  (m* x (m* y z)) 
                  T_Mon 2) 
                 "(X*Y)*Z = X*(Y*Z) should be provable")
     
     ;; Test non-equalities
     (check-false (prove-equality x y T_Mon 2) "X = Y should not be provable")
     
     ;; Test multi-step equalities
     (check-true (prove-equality 
                  (m* (m* e-const x) e-const)
                  x
                  T_Mon 3)
                 "(e*X)*e = X should be provable with depth 3"))

   (test-case "prover+simulator function"
     (define x (mvar 'X))
     (define y (mvar 'Y))
     (define z (mvar 'Z))
     (define e-const me)
     
     ;; Test result for equal terms
     (define result1 (prover+simulator (m* x e-const) x T_Mon 2))
     (eprintf "prover+simulator result for X*e = X:\n")
     (eprintf "  equal? ~s\n" (prover-simulator-result-are-equal? result1))
     (eprintf "  path: ~s\n" (prover-simulator-result-path result1))
     (flush-output (current-error-port))
     
     (check-true (prover-simulator-result? result1) "Should return a result object")
     (check-true (prover-simulator-result-are-equal? result1) "X*e = X should be equal")
     (check-not-false (prover-simulator-result-path result1) "Should contain a path")
     (when (prover-simulator-result-path result1)
       (check-equal? (length (prover-simulator-result-path result1)) 2 
                    "Path from X*e to X should have length 2"))
     
     ;; Test result for non-equal terms
     (define result2 (prover+simulator x y T_Mon 2))
     (eprintf "prover+simulator result for X = Y:\n")
     (eprintf "  equal? ~s\n" (prover-simulator-result-are-equal? result2))
     (eprintf "  path: ~s\n" (prover-simulator-result-path result2))
     (flush-output (current-error-port))
     
     (check-true (prover-simulator-result? result2) "Should return a result object")
     (check-false (prover-simulator-result-are-equal? result2) "X = Y should not be equal")
     (check-false (prover-simulator-result-path result2) "Should not contain a path"))
  ))

(module+ main
  (run-tests slc-simulator-test-suite)) 