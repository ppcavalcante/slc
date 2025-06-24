#lang racket/base

(require "terms.rkt"
         "theories.rkt"
         "ct.rkt"
         "lt.rkt"
         "examples/monoid.rkt"
         racket/list)

(provide benchmark-search-methods
         demo-performance-improvements)

;; Simple timing function
(define (time-it thunk description)
  (printf "~a: " description)
  (define start-time (current-inexact-milliseconds))
  (define result (thunk))
  (define end-time (current-inexact-milliseconds))
  (printf "~a ms\n" (- end-time start-time))
  result)

;; Create a complex term for testing
(define (make-complex-term depth)
  (if (= depth 0)
      (mvar 'x)
      (m* (make-complex-term (- depth 1))
          (if (even? depth) me (mvar 'y)))))

;; Benchmark different search methods
(define (benchmark-search-methods term1 term2 theory max-depth)
  (printf "Benchmarking search from:\n  ~a\nto:\n  ~a\n" term1 term2)
  (printf "Max depth: ~a\n\n" max-depth)
  
  ;; Clear caches for fair comparison
  (clear-caches!)
  
  ;; Test BFS
  (printf "BFS method:\n")
  (define bfs-result
    (time-it (lambda () (smart-find-rewrite-path term1 term2 theory max-depth #:method 'bfs)) "BFS method"))
  (printf "Result: ~a\n\n" (if bfs-result "Found path" "No path"))
  
  ;; Test IDS  
  (printf "Iterative Deepening Search:\n")
  (define ids-result
    (time-it (lambda () (smart-find-rewrite-path term1 term2 theory max-depth #:method 'ids)) "Iterative Deepening Search"))
  (printf "Result: ~a\n\n" (if ids-result "Found path" "No path"))
  
  ;; Test confluent method (if applicable)
  (printf "Confluent normalization:\n")
  (define confluent-result
    (time-it (lambda () (smart-find-rewrite-path term1 term2 theory max-depth #:method 'confluent)) "Confluent normalization"))
  (printf "Result: ~a\n\n" (if confluent-result "Equal via normalization" "Not equal"))
  
  ;; Test auto method
  (printf "Auto-selected method:\n")
  (define auto-result
    (time-it (lambda () (smart-find-rewrite-path term1 term2 theory max-depth #:method 'auto)) "Auto-selected method"))
  (printf "Result: ~a\n\n" (if auto-result "Found path" "No path"))
  
  (values bfs-result ids-result confluent-result auto-result))

;; Demonstrate caching benefits
(define (demo-caching-benefits term theory)
  (printf "Demonstrating caching benefits for one-step-rewrites:\n")
  (printf "Term: ~a\n\n" term)
  
  ;; Clear cache
  (clear-caches!)
  
  ;; First call (uncached)
  (printf "First call (uncached):\n")
  (define result1 (time-it (lambda () (one-step-rewrites term theory)) "First call (uncached)"))
  
  ;; Second call (cached)
  (printf "Second call (cached):\n")
  (define result2 (time-it (lambda () (one-step-rewrites term theory)) "Second call (cached)"))
  
  (printf "Results identical: ~a\n" (equal? result1 result2))
  (printf "Number of one-step rewrites: ~a\n\n" (length result1)))

;; Main performance demonstration
(define (demo-performance-improvements)
  (printf "=== SLC Performance Optimizations Demo ===\n\n")
  
  ;; Simple test case
  (printf "1. Simple equality test (X*e = X):\n")
  (define simple-lhs (m* (mvar 'X) me))
  (define simple-rhs (mvar 'X))
  (benchmark-search-methods simple-lhs simple-rhs T_Mon 3)
  
  ;; More complex test case  
  (printf "2. Complex term test:\n")
  (define complex1 (m* (m* (mvar 'x) me) (m* me (mvar 'y))))
  (define complex2 (m* (mvar 'x) (mvar 'y)))
  (benchmark-search-methods complex1 complex2 T_Mon 5)
  
  ;; Demonstrate caching
  (printf "3. Caching demonstration:\n")
  (demo-caching-benefits complex1 T_Mon)
  
  ;; Demonstrate normalization
  (printf "4. Term normalization demo:\n")
  (define unnormalized (m* (m* me (mvar 'x)) me))
  (printf "Original term: ~a\n" unnormalized)
  (define normalized (time-it (lambda () (normalize-term unnormalized T_Mon)) "Term normalization"))
  (printf "Normalized: ~a\n" normalized)
  (printf "Equal via confluent check: ~a\n\n" 
          (confluent-equal? unnormalized (mvar 'x) T_Mon))
  
  (printf "Demo complete!\n"))

(module+ main
  (demo-performance-improvements)) 