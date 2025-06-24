#lang racket/base

(require "terms.rkt"
         "theories.rkt"
         "lt.rkt"
         "ct.rkt"
         racket/set)

(provide simulate-rewrites
         prove-equality
         prover+simulator
         prover-simulator-result?
         prover-simulator-result-term1
         prover-simulator-result-term2
         prover-simulator-result-theory-name
         prover-simulator-result-are-equal?
         prover-simulator-result-path
         display-proof-path
         reachable-terms-by-depth)

;; Simulate all possible rewrites from a term up to a certain depth
;; Returns a tree of possible rewrite paths
;; simulate-rewrites : Term LawvereTheory Integer -> (Listof RewritePath)
;; where RewritePath is (Listof Term)
(define (simulate-rewrites start-term theory max-depth)
  (define visited (set start-term))
  
  ;; Helper to build rewrite paths
  (define (build-paths term current-depth)
    (if (>= current-depth max-depth)
        (list (list term)) ; Base case: return just the term itself
        (let* ([next-terms (one-step-rewrites term theory)]
               [unvisited-terms (filter (λ (t) (not (set-member? visited t))) next-terms)])
          
          ;; Mark these terms as visited
          (for ([t unvisited-terms])
            (set! visited (set-add visited t)))
          
          ;; Prepend current term to all possible continuations
          (cons (list term) ; Path of just this term
                (apply append
                       (map (λ (next-term)
                              (map (λ (path) (cons term path))
                                   (build-paths next-term (add1 current-depth))))
                            unvisited-terms))))))
  
  (build-paths start-term 0))

;; Prove whether two terms are equal in the theory
;; Returns #t if equal, #f otherwise
;; prove-equality : Term Term LawvereTheory Integer -> Boolean
(define (prove-equality term1 term2 theory max-depth)
  (lt-terms-equal? term1 term2 theory max-depth))

;; Combined prover+simulator function that demonstrates the SLC theorem
;; Returns a result object with both proof and simulation data
;; prover+simulator : Term Term LawvereTheory Integer -> ProverSimulatorResult

;; Define the result struct for prover+simulator
(struct prover-simulator-result
  (term1        ; First term
   term2        ; Second term
   theory-name  ; Name of the theory used
   are-equal?   ; Boolean: are the terms equal?
   path)        ; Path from term1 to term2, or #f if not equal
  #:transparent)

(define (prover+simulator term1 term2 theory max-depth)
  (define are-equal? (prove-equality term1 term2 theory max-depth))
  (define path (and are-equal? (find-rewrite-path term1 term2 theory max-depth)))
  
  (prover-simulator-result
   term1 
   term2
   (lawvere-theory-name theory)
   are-equal?
   path))

;; Pretty-print the proof path
(define (display-proof-path result)
  (if (not (prover-simulator-result-are-equal? result))
      (printf "No proof exists that ~a = ~a in theory ~a\n"
              (prover-simulator-result-term1 result)
              (prover-simulator-result-term2 result)
              (prover-simulator-result-theory-name result))
      (let ([path (prover-simulator-result-path result)])
        (printf "Proof that ~a = ~a in theory ~a:\n"
                (prover-simulator-result-term1 result)
                (prover-simulator-result-term2 result)
                (prover-simulator-result-theory-name result))
        (for ([step path]
              [i (in-naturals)])
          (printf "  Step ~a: ~a\n" i step)))))

;; Get unique terms reachable at each depth level
;; reachable-terms-by-depth : Term LawvereTheory Integer -> (Listof (Setof Term))
(define (reachable-terms-by-depth start-term theory max-depth)
  (define paths (simulate-rewrites start-term theory max-depth))
  
  ;; Extract terms at each depth level
  (for/list ([depth (in-range (add1 max-depth))])
    (for/set ([path paths]
              #:when (>= (length path) (add1 depth)))
      (list-ref path depth))))

(module+ test
  (require rackunit)
  (require "examples/monoid.rkt")
  
  ;; Test simulate-rewrites
  (define x (mvar 'X))
  (define e-const me)
  (define term-xe (m* x e-const))
  
  (define rewrites (simulate-rewrites term-xe T_Mon 1))
  (check-true (list? rewrites) "simulate-rewrites should return a list")
  
  ;; Test prove-equality
  (check-true (prove-equality term-xe x T_Mon 2) "X*e should equal X")
  (check-false (prove-equality x (mvar 'Y) T_Mon 2) "X should not equal Y")
  
  ;; Test prover+simulator
  (define result (prover+simulator term-xe x T_Mon 2))
  (check-true (prover-simulator-result? result) "prover+simulator should return a result object")
  (check-true (prover-simulator-result-are-equal? result) "X*e and X should be equal")
  (check-not-false (prover-simulator-result-path result) "Path should exist between X*e and X")
)

(module+ main
  (require "examples/monoid.rkt")
  
  ;; Example of using the prover+simulator with monoid terms
  (define x (mvar 'X))
  (define y (mvar 'Y))
  (define z (mvar 'Z))
  (define e-const me)
  
  ;; Test basic equality: x*e = x
  (define result1 (prover+simulator (m* x e-const) x T_Mon 2))
  (display-proof-path result1)
  (newline)
  
  ;; Test associativity: (x*y)*z = x*(y*z)
  (define result2 (prover+simulator (m* (m* x y) z) (m* x (m* y z)) T_Mon 2))
  (display-proof-path result2)
  (newline)
  
  ;; Test a non-equality: x ≠ y
  (define result3 (prover+simulator x y T_Mon 2))
  (display-proof-path result3)
  (newline)
  
  ;; Test a complex equality: (e*x)*e = x
  (define result4 (prover+simulator (m* (m* e-const x) e-const) x T_Mon 3))
  (display-proof-path result4)
  (newline)
  
  ;; Show all possible single-step rewrites from (x*e)
  (define rewrites1 (simulate-rewrites (m* x e-const) T_Mon 1))
  (printf "All possible rewrites from x*e (depth 1):\n")
  (for ([path rewrites1]
        [i (in-naturals)])
    (printf "  Path ~a: ~a\n" i path))
) 