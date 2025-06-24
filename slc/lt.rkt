#lang racket/base

(require "terms.rkt"
         "theories.rkt"
         "ct.rkt" ; lt-terms-equal? will use find-rewrite-path
         racket/list) ; For first, last, etc.
(provide (all-defined-out))


;; Check if two terms are provably equal in the logic LT for a given theory T.
;; According to SLC, this is equivalent to them being connected by a rewrite path in CT.
;; lt-terms-equal? : Term Term LawvereTheory Integer [#:method Symbol] -> Boolean
(define (lt-terms-equal? term1 term2 theory max-search-depth #:method [method 'auto])
  (if (smart-find-rewrite-path term1 term2 theory max-search-depth #:method method)
      #t
      #f))

;; Fast equality check using confluent normalization (when applicable)
;; lt-confluent-equal? : Term Term LawvereTheory -> Boolean
(define (lt-confluent-equal? term1 term2 theory)
  (confluent-equal? term1 term2 theory))

;; LT Morphism representation (n -> m)
;; An m-tuple of T-terms in x1,...,xn
(struct lt-morphism (source-arity target-arity terms) #:transparent)

;; Create identity morphism for arity n
;; lt-identity : Natural -> LT-Morphism
(define (lt-identity n)
  (lt-morphism n n 
    (for/list ([i (in-range n)])
      (term-var (string->symbol (format "x~a" i))))))

;; Create variable tuple [x_0, x_1, ..., x_{n-1}]
(define (make-var-tuple n)
  (for/list ([i (in-range n)])
    (term-var (string->symbol (format "x~a" i)))))

;; Composition of LT morphisms: [s] ∘ [t] = [s_i[t_vec/x_vec]]
;; lt-compose : LT-Morphism LT-Morphism LawvereTheory -> LT-Morphism
(define (lt-compose g-morph f-morph theory)
  (unless (= (lt-morphism-source-arity g-morph) 
             (lt-morphism-target-arity f-morph))
    (error 'lt-compose "Arity mismatch: ~a -> ~a but ~a -> ~a"
           (lt-morphism-source-arity f-morph)
           (lt-morphism-target-arity f-morph)
           (lt-morphism-source-arity g-morph)
           (lt-morphism-target-arity g-morph)))
  
  (define f-terms (lt-morphism-terms f-morph))
  (define g-terms (lt-morphism-terms g-morph))
  (define intermediate-arity (lt-morphism-target-arity f-morph))
  
  ;; Create substitution map: x_i ↦ f_i for the intermediate variables
  (define substitution-map
    (for/hash ([i (in-range intermediate-arity)]
               [f-term f-terms])
      (values (string->symbol (format "x~a" i)) f-term)))
  
  ;; Apply substitution to each term in g
  (define composed-terms
    (for/list ([g-term g-terms])
      (substitute g-term substitution-map)))
  
  (lt-morphism (lt-morphism-source-arity f-morph)
               (lt-morphism-target-arity g-morph)
               composed-terms))

;; Check if two morphisms are equal in the theory
;; lt-morphism-equal? : LT-Morphism LT-Morphism LawvereTheory Integer -> Boolean
(define (lt-morphism-equal? m1 m2 theory max-depth)
  (and (= (lt-morphism-source-arity m1) (lt-morphism-source-arity m2))
       (= (lt-morphism-target-arity m1) (lt-morphism-target-arity m2))
       (andmap (λ (t1 t2) (lt-terms-equal? t1 t2 theory max-depth))
               (lt-morphism-terms m1)
               (lt-morphism-terms m2))))

;; Convert a single term to a morphism n -> 1
;; term->lt-morphism : Term Natural -> LT-Morphism
(define (term->lt-morphism term arity)
  (lt-morphism arity 1 (list term)))

;; Convert a morphism 1 -> 1 back to a term
;; lt-morphism->term : LT-Morphism -> Term
(define (lt-morphism->term morph)
  (unless (and (= (lt-morphism-source-arity morph) 1)
               (= (lt-morphism-target-arity morph) 1)
               (= (length (lt-morphism-terms morph)) 1))
    (error 'lt-morphism->term "Not a 1->1 morphism: ~a" morph))
  (first (lt-morphism-terms morph)))

(module+ test
  (require rackunit)
  
  ;; Test identity morphism
  (define id2 (lt-identity 2))
  (check-equal? (lt-morphism-source-arity id2) 2)
  (check-equal? (lt-morphism-target-arity id2) 2)
  (check-equal? (length (lt-morphism-terms id2)) 2)
  
  ;; Test basic morphism composition (without requiring specific theory)
  ;; f: 1 -> 2, f(x) = [x, x]
  (define f (lt-morphism 1 2 (list (term-var 'x0) (term-var 'x0))))
  ;; g: 2 -> 1, g(x,y) = [x]  (projection)
  (define g (lt-morphism 2 1 (list (term-var 'x0))))
  
  ;; g ∘ f should be: 1 -> 1, (g ∘ f)(x) = [x]
  (define gf (lt-compose g f #f)) ; No theory needed for this test
  (check-equal? (lt-morphism-source-arity gf) 1)
  (check-equal? (lt-morphism-target-arity gf) 1)
  
  ;; The composed term should be x0
  (define actual-term (first (lt-morphism-terms gf)))
  (check-true (term-equal? actual-term (term-var 'x0)))
  
  (check-true (procedure? lt-terms-equal?))
  (displayln "LT tests with morphism composition passed."))