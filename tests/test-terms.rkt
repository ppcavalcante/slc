#lang racket/base

(require rackunit
         rackunit/text-ui
         "../slc/terms.rkt"
         racket/set)

(provide terms-test-suite)

;; Helper: compare lists as sets (order doesn't matter)
(define (list-sorts-eq? l1 l2)
  (equal? (sort l1 symbol<?) (sort l2 symbol<?)))

;; Helper: compare lists of lists as sets (for subterm paths)
(define (list-set=? l1 l2)
  (and (= (length l1) (length l2))
       (andmap (λ (item) (member item l2 equal?)) l1)
       (andmap (λ (item) (member item l1 equal?)) l2)))

(define (list-of-lists-set=? l1 l2)
  (equal? (list->set l1) (list->set l2)))

(define terms-test-suite
  (test-suite
   "Term Representation and Utilities"

   (test-case "Term Construction and Predicates"
     (define x (term-var 'x))
     (define e (term-const 'e))
     (define t_op (term-op '* (list x e)))
     (check-true (term-var? x))
     (check-true (term-const? e))
     (check-true (term-op? t_op))
     (check-true (term? x))
     (check-false (term? 'foo)))

   (test-case "Term Equality"
     (define x1 (term-var 'x))
     (define x2 (term-var 'x))
     (define y (term-var 'y))
     (define e1 (term-const 'e))
     (define e2 (term-const 'e))
     (define op1 (term-op '* (list x1 e1)))
     (define op2 (term-op '* (list x2 e2)))
     (define op3 (term-op '+ (list x1 e1)))
     (check-true (term-equal? x1 x2))
     (check-false (term-equal? x1 y))
     (check-true (term-equal? e1 e2))
     (check-true (term-equal? op1 op2))
     (check-false (term-equal? op1 op3))
     (check-false (term-equal? op1 (term-op '* (list x1 y)))))

   (test-case "Free Variables"
     (define x (term-var 'x))
     (define y (term-var 'y))
     (define e (term-const 'e))
     (define t1 (term-op '* (list x y)))
     (define t2 (term-op '+ (list x (term-op '* (list y e)))))
     (define t3 (term-op 'f (list x x y)))
     (check-true (list-sorts-eq? (free-vars x) '(x)))
     (check-true (list-sorts-eq? (free-vars e) '()))
     (check-true (list-sorts-eq? (free-vars t1) '(x y)))
     (check-true (list-sorts-eq? (free-vars t2) '(x y)))
     (check-true (list-sorts-eq? (free-vars t3) '(x y))))

   (test-case "Substitution"
     (define x (term-var 'x))
     (define y (term-var 'y))
     (define z (term-var 'z))
     (define a (term-const 'a))
     (define b (term-const 'b))
     (define t (term-op 'f (list x (term-op 'g (list y a)) z)))
     (check-equal? (substitute t (hash 'x b))
                   (term-op 'f (list b (term-op 'g (list y a)) z)))
     (check-equal? (substitute t (hash 'y b 'z x))
                   (term-op 'f (list x (term-op 'g (list b a)) x)))
     (check-equal? (substitute t (hash 'w b)) t)
     (check-equal? (substitute x (hash 'x (term-op '+ (list y z))))
                   (term-op '+ (list y z))))

   (test-case "Find All Subterms With Paths"
     (define x (term-var 'x))
     (define e (term-const 'e))
     (define t-simple (term-op '* (list x e)))
     (define t-nested (term-op 'f (list x (term-op 'g (list e x)) x)))
     (define expected-simple
       (list (list t-simple '()) (list x '(0)) (list e '(1))))
     (define expected-nested
       (list (list t-nested '())
             (list x '(0))
             (list (term-op 'g (list e x)) '(1))
             (list e '(1 0))
             (list x '(1 1))
             (list x '(2))))
     (check-true (list-of-lists-set=? (find-all-subterms-with-paths t-simple) expected-simple))
     (check-true (list-of-lists-set=? (find-all-subterms-with-paths t-nested) expected-nested)))

   (test-case "Replace Subterm At"
     (define x (term-var 'x))
     (define y (term-var 'y))
     (define e (term-const 'e))
     (define new-term (term-const 'NEW))
     (define t (term-op '* (list x (term-op '+ (list y e)))))
     (check-equal? (replace-subterm-at t '() new-term) new-term)
     (check-equal? (replace-subterm-at t '(0) new-term)
                   (term-op '* (list new-term (term-op '+ (list y e)))))
     (check-equal? (replace-subterm-at t '(1) new-term)
                   (term-op '* (list x new-term)))
     (check-equal? (replace-subterm-at t '(1 0) new-term)
                   (term-op '* (list x (term-op '+ (list new-term e)))))
     (check-equal? (replace-subterm-at t '(1 1) new-term)
                   (term-op '* (list x (term-op '+ (list y new-term)))))
     (check-exn exn:fail? (λ () (replace-subterm-at t '(2) e)))
     (check-exn exn:fail? (λ () (replace-subterm-at t '(0 0) e)))
     (check-exn exn:fail? (λ () (replace-subterm-at x '(0) e))))
  ))

(module+ main
  (run-tests terms-test-suite))