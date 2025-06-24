#lang racket/base

(require "terms.rkt"
         "theories.rkt"
         "ct.rkt"
         "lt.rkt")

(provide (all-from-out "terms.rkt")
         (all-from-out "theories.rkt")
         (all-from-out "ct.rkt")
         (all-from-out "lt.rkt"))

(module+ main
  (displayln "slc/core.rkt loaded (usually for providing modules)"))