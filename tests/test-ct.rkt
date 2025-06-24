#lang racket/base

(require rackunit
         rackunit/text-ui
         "../slc/ct.rkt"
         "../slc/terms.rkt" 
         "../slc/theories.rkt"
         "../slc/examples/monoid.rkt"
         racket/set
         racket/list)

(provide ct-test-suite)

;; Helper: compare rules (cons cells of terms) structurally
(define (rule-equal? r1 r2)
  (and (term-equal? (car r1) (car r2))
       (term-equal? (cdr r1) (cdr r2))))

;; Helper for comparing lists of terms/rules where order doesn't matter
(define (list-set-equal?/cmp lst1 lst2 item-cmp) 
  (eprintf "--- Inside list-set-equal?/cmp ---\n")
  (eprintf "lst1 (~a items): ~s\n" (length lst1) lst1)
  (eprintf "lst2 (~a items): ~s\n" (length lst2) lst2)
  (flush-output (current-error-port))

  (let* ([len-match (= (length lst1) (length lst2))]
         [all-l1-in-l2 (andmap (lambda (item1)
                                 (let ([m (member item1 lst2 item-cmp)])
                                   (eprintf "  item1 ~s in lst2? (member returned: ~s) -> ~s\n" item1 m (if m #t #f))
                                   (not (not m))))
                               lst1)]
         [all-l2-in-l1 (andmap (lambda (item2)
                                 (let ([m (member item2 lst1 item-cmp)])
                                   (eprintf "  item2 ~s in lst1? (member returned: ~s) -> ~s\n" item2 m (if m #t #f))
                                   (not (not m))))
                               lst2)]
         [final-result (and len-match all-l1-in-l2 all-l2-in-l1)])
    (eprintf "len-match: ~s\n" len-match)
    (eprintf "all-l1-in-l2 (truthy?): ~s\n" (if all-l1-in-l2 #t #f))
    (eprintf "all-l2-in-l1 (truthy?): ~s\n" (if all-l2-in-l1 #t #f))
    (eprintf "FINAL list-set-equal?/cmp RESULT: ~s\n" final-result)
    (flush-output (current-error-port))
    final-result))

(define ct-test-suite
  (test-suite
   "Computation Category (CT) Tests"

   (test-case "get-rewrite-rules for Monoid"
     (define rules (get-rewrite-rules T_Mon))
     (check-equal? (length rules) 6 "Monoid theory should yield 6 rewrite rules")

     (define x (mvar 'X)) 
     (define e-const me)
     
     ;; Since we now use fresh variables in axioms, we need to check that the 
     ;; semantic content is preserved rather than exact syntactic matches
     (eprintf "--- Debugging get-rewrite-rules ---\n")
     (eprintf "Rules list:\n")
     (for-each (lambda (r) (eprintf "  ~s\n" r)) rules)
     
     ;; Check that we have rules of the right form by looking for patterns
     ;; that match X*e -> X (where vars may be renamed)
     (define has-xe-to-x-rule?
       (for/or ([rule rules])
         (and (term-op? (car rule))
              (eq? (term-op-op-sym (car rule)) '*)
              (= (length (term-op-args (car rule))) 2)
              (term-const? (cadr (term-op-args (car rule))))
              (eq? (term-const-name (cadr (term-op-args (car rule)))) 'e)
              (term-var? (cdr rule)))))
     
     (define has-x-to-xe-rule?
       (for/or ([rule rules])
         (and (term-var? (car rule))
              (term-op? (cdr rule))
              (eq? (term-op-op-sym (cdr rule)) '*)
              (= (length (term-op-args (cdr rule))) 2)
              (term-const? (cadr (term-op-args (cdr rule))))
              (eq? (term-const-name (cadr (term-op-args (cdr rule)))) 'e))))
     
     (eprintf "Has X*e->X pattern rule? ~s\n" has-xe-to-x-rule?)
     (eprintf "Has X->X*e pattern rule? ~s\n" has-x-to-xe-rule?)
     (flush-output (current-error-port))
     
     (check-true has-xe-to-x-rule? "Should have a rule of form X*e -> X")
     (check-true has-x-to-xe-rule? "Should have a rule of form X -> X*e"))

   (test-case "one-step-rewrites for Monoid"
     (define x (mvar 'X)) 
     (define y (mvar 'Y))
     (define z (mvar 'Z))
     (define e-const me)

     ;; Test 1: (X*e) contains X among its one-step rewrites
     (define term-xe (m* x e-const))
     (define actual-rewrites1 (one-step-rewrites term-xe T_Mon))
     (eprintf "--- Debugging one-step-rewrites for (X*e) ---\n")
     (eprintf "Target term to find in actual-rewrites1: ~s\n" x)
     (eprintf "Actual one-step-rewrites for (X*e): ~s\n" actual-rewrites1)
     (define member-check-result (member x actual-rewrites1 term-equal?))
     (eprintf "Result of (member x actual-rewrites1 term-equal?): ~s\n" member-check-result)
     (flush-output (current-error-port))
     (check-not-false member-check-result  ; <<< CHANGED TO check-not-false
                 "X should be a one-step-rewrite of (X*e)")

     ;; Test 2: x rewrites to exactly (x*e) and (e*x)
     (define actual-rewrites2 (one-step-rewrites x T_Mon))
     (define expected-rewrites2 (list (m* x e-const) (m* e-const x)))
     (eprintf "--- Debugging one-step-rewrites for X (Test 2) ---\n")
     (eprintf "Actual for X: ~s\n" actual-rewrites2)
     (eprintf "Expected for X: ~s\n" expected-rewrites2)
     (flush-output (current-error-port))
     (let ([set-eq-result (list-set-equal?/cmp actual-rewrites2 expected-rewrites2 term-equal?)])
       (eprintf "Result of list-set-equal?/cmp for Test 2: ~s\n" set-eq-result)
       (flush-output (current-error-port))
       (check-true set-eq-result "one-step-rewrites for X (exact set)"))

     ;; Test 3: ((X*Y)*Z) should have X*(Y*Z) among its one-step rewrites
     (define term-assoc-lhs (m* (m* x y) z))
     (define term-assoc-rhs (m* x (m* y z)))
     (define actual-rewrites3 (one-step-rewrites term-assoc-lhs T_Mon))
     (eprintf "--- Debugging one-step-rewrites for ((X*Y)*Z) ---\n")
     (eprintf "Target term to find in actual-rewrites3: ~s\n" term-assoc-rhs)
     (eprintf "Actual one-step-rewrites for ((X*Y)*Z): ~s\n" actual-rewrites3)
     (define member-check-result3 (member term-assoc-rhs actual-rewrites3 term-equal?))
     (eprintf "Result of (member term-assoc-rhs actual-rewrites3 term-equal?): ~s\n" member-check-result3)
     (flush-output (current-error-port))
     (check-not-false member-check-result3 ; <<< CHANGED TO check-not-false
                 "X*(Y*Z) should be a one-step-rewrite of ((X*Y)*Z)")
     
     ;; Test 4: (e*X)*e should have X*e and e*X among its one-step rewrites
     (define term-exe (m* (m* e-const x) e-const))
     (define actual-rewrites4 (one-step-rewrites term-exe T_Mon))
     (define expected-rewrite-A (m* x e-const))
     (define expected-rewrite-B (m* e-const x))
     (eprintf "--- Debugging one-step-rewrites for (e*X)*e ---\n")
     (eprintf "Target A to find in actual-rewrites4: ~s\n" expected-rewrite-A)
     (eprintf "Target B to find in actual-rewrites4: ~s\n" expected-rewrite-B)
     (eprintf "Actual one-step-rewrites for (e*X)*e: ~s\n" actual-rewrites4)
     (define member-check-A (member expected-rewrite-A actual-rewrites4 term-equal?))
     (define member-check-B (member expected-rewrite-B actual-rewrites4 term-equal?))
     (eprintf "Result of (member A actual-rewrites4 term-equal?): ~s\n" member-check-A)
     (eprintf "Result of (member B actual-rewrites4 term-equal?): ~s\n" member-check-B)
     (flush-output (current-error-port))
     (check-not-false member-check-A "X*e should be a one-step-rewrite of (e*X)*e") ; <<< CHANGED
     (check-not-false member-check-B "e*X should be a one-step-rewrite of (e*X)*e")) ; <<< CHANGED

   (test-case "find-rewrite-path for Monoid"
    ;; ... (rest of this test case remains the same, but its success depends on one-step-rewrites)
     (define x (mvar 'X))
     (define y (mvar 'Y))
     (define z (mvar 'Z))
     (define e-const me)

     (eprintf "--- Debugging find-rewrite-path ---\n")
     (flush-output (current-error-port))

     ;; Path 1: (X*e) to X
     (define term-xe (m* x e-const))
     (define path1 (find-rewrite-path term-xe x T_Mon 2))
     (eprintf "Path1 (X*e -> X): ~s\n" path1) 
     (eprintf "Path1 is a list?: ~s\n" (list? path1))
     (eprintf "Path1 non-false?: ~s\n" (if path1 #t #f))
     (flush-output (current-error-port))
     (check-not-false path1 "Path should exist from (X*e) to X")
     (when path1 (check-equal? (length path1) 2 "Path (X*e) -> X should be length 2"))
     (when path1 (check-true (term-equal? (first path1) term-xe)))
     (when path1 (check-true (term-equal? (last path1) x)))

     ;; Path 2: ((X*Y)*Z) to (X*(Y*Z))
     (define term-assoc-lhs (m* (m* x y) z))
     (define term-assoc-rhs (m* x (m* y z)))
     (define path2 (find-rewrite-path term-assoc-lhs term-assoc-rhs T_Mon 2))
     (eprintf "Path2 (assoc): ~s\n" path2) 
     (eprintf "Path2 is a list?: ~s\n" (list? path2))
     (eprintf "Path2 non-false?: ~s\n" (if path2 #t #f))
     (flush-output (current-error-port))
     (check-not-false path2 "Path should exist for associativity")
     (when path2 (check-equal? (length path2) 2 "Path for assoc should be length 2"))

     ;; Path 3: (e*X)*e to X
     (define term-exe (m* (m* e-const x) e-const))
     (define path3 (find-rewrite-path term-exe x T_Mon 3))
     (eprintf "Path3 ((e*X)*e -> X): ~s\n" path3) 
     (eprintf "Path3 is a list?: ~s\n" (list? path3))
     (eprintf "Path3 non-false?: ~s\n" (if path3 #t #f))
     (flush-output (current-error-port))
     (check-not-false path3 "Path should exist from (e*X)*e to X")
     (when path3 (check-equal? (length path3) 3 "Path (e*X)*e -> X*e -> X should be length 3"))
     (when path3 (check-true (term-equal? (last path3) x)))
     
     ;; Path 4: No path from X to Y
     (define path4-result (find-rewrite-path x y T_Mon 2))
     (eprintf "Path4 (X -> Y): ~s\n" path4-result) 
     (eprintf "Path4 is a list?: ~s\n" (list? path4-result))
     (eprintf "Path4 non-false?: ~s\n" (if path4-result #t #f))
     (flush-output (current-error-port))
     (check-false path4-result "No path should exist from X to Y")

     ;; Path 5: Start = End
     (define path5 (find-rewrite-path x x T_Mon 1))
     (eprintf "Path5 (X -> X): ~s\n" path5) 
     (eprintf "Path5 is a list?: ~s\n" (list? path5))
     (eprintf "Path5 non-false?: ~s\n" (if path5 #t #f))
     (flush-output (current-error-port))
     (check-not-false path5 "Path from X to X should exist")
     (when path5 (check-equal? (length path5) 1 "Path from X to X is just (list X)"))
     (when path5 (check-true (term-equal? (first path5) x)))
     )
  ))

(module+ main
  (run-tests ct-test-suite))