#lang racket/base

(require "../terms.rkt"
         "../theories.rkt")
(provide mvar me m* T_Mon)

;; --- Monoid Term Constructors (optional helpers) ---
(define (mvar s) (term-var s))
(define me (term-const 'e))
(define (m* t1 t2) (term-op '* (list t1 t2)))

;; --- The Lawvere Theory for Monoids (T_Mon) ---
(define T_Mon
  (lawvere-theory
   'Monoid
   (list
    ;; Associativity: (X*Y)*Z = X*(Y*Z)
    (cons (m* (m* (mvar 'X) (mvar 'Y)) (mvar 'Z))
          (m* (mvar 'X) (m* (mvar 'Y) (mvar 'Z))))
    ;; Left identity: e*X = X
    (cons (m* me (mvar 'X))
          (mvar 'X))
    ;; Right identity: X*e = X
    (cons (m* (mvar 'X) me)
          (mvar 'X))
    )))

(module+ test
  (require rackunit)
  (check-pred lawvere-theory? T_Mon)
  (check-equal? (lawvere-theory-name T_Mon) 'Monoid)
  (check-equal? (length (lawvere-theory-axioms T_Mon)) 3)
  (displayln "Monoid theory defined."))

(module+ main ; For quick testing of this file
    (require "../ct.rkt")
    (require "../lt.rkt")
    (displayln "Testing Monoid Theory specific rewrites (manual):")
    (define x (mvar 'x))
    (define y (mvar 'y))
    (define z (mvar 'z))

    (define term1 (m* (m* x y) z)) ; (x*y)*z
    (define term2 (m* x (m* y z))) ; x*(y*z)
    (define term_xe (m* x me))    ; x*e

    (displayln "Term1 (x*y)*z:")
    (displayln term1)
    (displayln "One-step rewrites from term1:")
    (for-each displayln (one-step-rewrites term1 T_Mon))
    ; Expected: (x*(y*z)) and maybe others if axioms were more complex

    (displayln "---")
    (displayln (format "Path from (x*y)*z to x*(y*z) (depth 2): ~s"
                       (find-rewrite-path term1 term2 T_Mon 2)))
    (displayln (format "lt-terms-equal? (x*y)*z and x*(y*z): ~s"
                       (lt-terms-equal? term1 term2 T_Mon 2)))
    
    (displayln "---")
    (displayln (format "Path from x*e to x (depth 2): ~s"
                       (find-rewrite-path term_xe x T_Mon 2)))
    (displayln (format "lt-terms-equal? x*e and x: ~s"
                       (lt-terms-equal? term_xe x T_Mon 2)))

    (define term_exe (m* (m* me x) me)) ; (e*x)*e
    (displayln "---")
    (displayln (format "Path from (e*x)*e to x (depth 5): ~s"
                       (find-rewrite-path term_exe x T_Mon 5)))
    (displayln (format "lt-terms-equal? (e*x)*e and x: ~s"
                       (lt-terms-equal? term_exe x T_Mon 5)))
    )