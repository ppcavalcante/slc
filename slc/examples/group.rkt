#lang racket/base

(require "../terms.rkt"
         "../theories.rkt"
         "monoid.rkt")  ; We'll extend the monoid theory
(provide gvar ginv T_Grp)

;; --- Group Term Constructors (optional helpers) ---
(define (gvar s) (term-var s))
(define (ginv t) (term-op 'inv (list t)))  ; Inverse operator

;; --- The Lawvere Theory for Groups (T_Grp) ---
(define T_Grp
  (lawvere-theory
   'Group
   (append
    (lawvere-theory-axioms T_Mon)  ; Include all monoid axioms
    (list
     ;; Left inverse: x^-1 * x = e
     (cons (m* (ginv (gvar 'X)) (gvar 'X))
           me)
     
     ;; Right inverse: x * x^-1 = e
     (cons (m* (gvar 'X) (ginv (gvar 'X)))
           me)))))

(module+ test
  (require rackunit)
  (check-pred lawvere-theory? T_Grp)
  (check-equal? (lawvere-theory-name T_Grp) 'Group)
  
  ;; Group should have 5 axioms: 3 from Monoid + 2 inverse axioms
  (check-equal? (length (lawvere-theory-axioms T_Grp)) 5)
  
  (displayln "Group theory defined."))
