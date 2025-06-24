#lang racket/base

(require rackunit
         rackunit/text-ui
         "test-terms.rkt"
         "test-theories.rkt"
         "test-ct.rkt"       
         "test-lt.rkt"
         "test-slc-simulator.rkt")

;; Run all test modules
(run-tests 
 (test-suite
  "SLC Racket All Tests"
  (test-suite "Terms Tests" terms-test-suite)
  (test-suite "Theories Tests" theories-test-suite)
  (test-suite "CT Tests" ct-test-suite)
  (test-suite "LT Tests" lt-test-suite)
  (test-suite "SLC Simulator Tests" slc-simulator-test-suite)
  ))