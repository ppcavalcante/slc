#lang info

(define collection "slc")
(define version "0.1.0-alpha")

;; Runtime dependencies
(define deps
  '("base"
    "rackunit-lib"))

;; Build-time dependencies  
(define build-deps
  '("scribble-lib"
    "racket-doc"))

;; Documentation configuration
(define scribblings 
  '(("scribblings/slc.scrbl" ())))

;; Test files
(define test-omit-paths '("examples"))