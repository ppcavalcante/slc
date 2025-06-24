#lang racket/base

(require "../terms.rkt"
         "../theories.rkt")
(provide rvar r0 r1 r+ r* r- T_Ring)

;; --- Ring Term Constructors (optional helpers) ---
(define (rvar s) (term-var s))
(define r0 (term-const '0))  ; Additive identity
(define r1 (term-const '1))  ; Multiplicative identity
(define (r+ t1 t2) (term-op '+ (list t1 t2)))
(define (r* t1 t2) (term-op '* (list t1 t2)))
(define (r- t) (term-op '- (list t)))  ; Additive inverse

;; --- The Lawvere Theory for Rings (T_Ring) ---
(define T_Ring
  (lawvere-theory
   'Ring
   (list
    ;; + associativity: (x+y)+z = x+(y+z)
    (cons (r+ (r+ (rvar 'x) (rvar 'y)) (rvar 'z))
          (r+ (rvar 'x) (r+ (rvar 'y) (rvar 'z))))
    
    ;; + commutativity: x+y = y+x
    (cons (r+ (rvar 'x) (rvar 'y))
          (r+ (rvar 'y) (rvar 'x)))
    
    ;; + identity: 0+x = x
    (cons (r+ r0 (rvar 'x))
          (rvar 'x))
    
    ;; + right identity: x+0 = x
    (cons (r+ (rvar 'x) r0)
          (rvar 'x))
    
    ;; + inverse: x+(-x) = 0
    (cons (r+ (rvar 'x) (r- (rvar 'x)))
          r0)
    
    ;; + right inverse: (-x)+x = 0
    (cons (r+ (r- (rvar 'x)) (rvar 'x))
          r0)
    
    ;; * associativity: (x*y)*z = x*(y*z)
    (cons (r* (r* (rvar 'x) (rvar 'y)) (rvar 'z))
          (r* (rvar 'x) (r* (rvar 'y) (rvar 'z))))
    
    ;; * identity: 1*x = x
    (cons (r* r1 (rvar 'x))
          (rvar 'x))
    
    ;; * right identity: x*1 = x
    (cons (r* (rvar 'x) r1)
          (rvar 'x))
    
    ;; Left distributivity: x*(y+z) = (x*y)+(x*z)
    (cons (r* (rvar 'x) (r+ (rvar 'y) (rvar 'z)))
          (r+ (r* (rvar 'x) (rvar 'y))
              (r* (rvar 'x) (rvar 'z))))
    
    ;; Right distributivity: (x+y)*z = (x*z)+(y*z)
    (cons (r* (r+ (rvar 'x) (rvar 'y)) (rvar 'z))
          (r+ (r* (rvar 'x) (rvar 'z))
              (r* (rvar 'y) (rvar 'z))))
    
    ;; Multiplicative absorption: x*0 = 0
    (cons (r* (rvar 'x) r0)
          r0)
    
    ;; Multiplicative right absorption: 0*x = 0
    (cons (r* r0 (rvar 'x))
          r0)
    )))

(module+ test
  (require rackunit)
  (check-pred lawvere-theory? T_Ring)
  (check-equal? (lawvere-theory-name T_Ring) 'Ring)
  (check-equal? (length (lawvere-theory-axioms T_Ring)) 14)
  
  (displayln "Ring theory defined.")) 