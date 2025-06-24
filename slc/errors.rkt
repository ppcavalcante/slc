#lang racket/base

(require racket/string)

(provide (all-defined-out))

;; Base SLC error type
(struct slc-error exn:fail () #:transparent)

;; Specific error types for different failure modes
(struct parse-error slc-error (input-string) #:transparent)
(struct theory-error slc-error (theory-name) #:transparent)
(struct arity-mismatch slc-error (expected actual operation) #:transparent)
(struct unification-error slc-error (term1 term2 reason) #:transparent)
(struct rewrite-error slc-error (term theory max-depth) #:transparent)

;; Helper functions for creating structured errors
(define (make-parse-error input-string message)
  (parse-error message (current-continuation-marks) input-string))

(define (make-theory-error theory-name message)
  (theory-error message (current-continuation-marks) theory-name))

(define (make-arity-mismatch expected actual operation message)
  (arity-mismatch message (current-continuation-marks) expected actual operation))

(define (make-unification-error term1 term2 reason message)
  (unification-error message (current-continuation-marks) term1 term2 reason))

(define (make-rewrite-error term theory max-depth message)
  (rewrite-error message (current-continuation-marks) term theory max-depth))

;; Pretty printing for errors
(define (format-slc-error err)
  (cond
    [(parse-error? err)
     (format "Parse Error: ~a\nInput: ~a" 
             (exn-message err) 
             (parse-error-input-string err))]
    [(theory-error? err)
     (format "Theory Error: ~a\nTheory: ~a" 
             (exn-message err) 
             (theory-error-theory-name err))]
    [(arity-mismatch? err)
     (format "Arity Mismatch: ~a\nExpected: ~a, Got: ~a, Operation: ~a" 
             (exn-message err)
             (arity-mismatch-expected err)
             (arity-mismatch-actual err)
             (arity-mismatch-operation err))]
    [(unification-error? err)
     (format "Unification Error: ~a\nTerm1: ~a\nTerm2: ~a\nReason: ~a" 
             (exn-message err)
             (unification-error-term1 err)
             (unification-error-term2 err)
             (unification-error-reason err))]
    [(rewrite-error? err)
     (format "Rewrite Error: ~a\nTerm: ~a\nTheory: ~a\nMax Depth: ~a" 
             (exn-message err)
             (rewrite-error-term err)
             (rewrite-error-theory err)
             (rewrite-error-max-depth err))]
    [else
     (format "SLC Error: ~a" (exn-message err))]))

(module+ test
  (require rackunit)
  
  ;; Test error creation and formatting
  (define test-parse-err (make-parse-error "bad input" "Invalid syntax"))
  (check-true (parse-error? test-parse-err))
  (check-true (string-contains? (format-slc-error test-parse-err) "Parse Error"))
  
  (displayln "Error handling tests passed.")) 