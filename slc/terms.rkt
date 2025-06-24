#lang racket/base

(require racket/match) 
(require racket/list) ;

(provide (all-defined-out)) ; For now, provide everything. Refine later.

;; --- Term Representation ---
(struct term-var (name) #:transparent)      ; e.g., (term-var 'x)
(struct term-const (name) #:transparent)    ; e.g., (term-const 'e)
(struct term-op (op-sym args) #:transparent) ; e.g., (term-op '* (list (term-var 'x) (term-var 'y)))
                                             ; op-sym is a symbol like '*

;; Type predicate for any term
(define (term? x)
  (or (term-var? x)
      (term-const? x)
      (term-op? x)))

;; --- Basic Term Functions (Stubs) ---

;; Syntactic equality (deep equality of structs)
(define (term-equal? t1 t2)
  (cond
    [(and (term-var? t1) (term-var? t2))
     (equal? (term-var-name t1) (term-var-name t2))]
    [(and (term-const? t1) (term-const? t2))
     (equal? (term-const-name t1) (term-const-name t2))]
    [(and (term-op? t1) (term-op? t2))
     (and (equal? (term-op-op-sym t1) (term-op-op-sym t2))
          (= (length (term-op-args t1)) (length (term-op-args t2)))
          (andmap term-equal? (term-op-args t1) (term-op-args t2)))]
    [else #f]))

;; Get free variables in a term
;; free-vars : Term -> (Listof Symbol)
(define (free-vars term)
  (match term
    [(term-var s) (list s)]
    [(term-const _) '()]
    [(term-op _ args) (remove-duplicates (append-map free-vars args))])) ; <-- Change here

;; Substitute variables in a term according to a binding map
;; substitute : Term (Hash Symbol Term) -> Term
;; bindings: (hash 'x replacement-term-for-x ...)
(define (substitute term bindings)
  (match term
    [(term-var s) (hash-ref bindings s term)] ; If var not in bindings, return var itself
    [(term-const c) term]
    [(term-op op-sym args)
     (term-op op-sym (map (λ (arg) (substitute arg bindings)) args))]))

;; Enhanced term matching with occurs check and better error handling
;; term-match? : TermPattern Term (Hash Symbol Term) -> (U (Hash Symbol Term) #f)
(define (term-match? pattern term current-bindings)
  (cond
    [(term-equal? pattern term) 
     ;; If pattern and term are structurally identical, they match
     current-bindings]
    
    [(term-var? pattern) ; Meta-variable in pattern
     (let ([var-name (term-var-name pattern)])
       (cond
         [(hash-has-key? current-bindings var-name)
          ;; Variable already bound - check consistency
          (if (term-equal? (hash-ref current-bindings var-name) term)
              current-bindings
              #f)] ; Bound to something else, mismatch
         [(occurs-check? var-name term)
          ;; Occurs check failed - would create infinite term
          #f]
         [else
          ;; New binding
          (hash-set current-bindings var-name term)]))]
    
    [(and (term-const? pattern) (term-const? term))
     (if (equal? (term-const-name pattern) (term-const-name term))
         current-bindings
         #f)]
    
    [(and (term-op? pattern) (term-op? term))
     (if (and (equal? (term-op-op-sym pattern) (term-op-op-sym term))
              (= (length (term-op-args pattern)) (length (term-op-args term))))
         ;; Match arguments left-to-right
         (let loop ([p-args (term-op-args pattern)]
                    [t-args (term-op-args term)]
                    [bindings current-bindings])
           (cond
             [(null? p-args) bindings] ; All arguments matched
             [(not bindings) #f]       ; Previous match failed
             [else
              (let ([new-bindings (term-match? (first p-args) (first t-args) bindings)])
                (loop (rest p-args) (rest t-args) new-bindings))]))
         #f)] ; Operator or arity mismatch
    
    [else #f])) ; Type mismatch

;; Occurs check: does variable occur in term?
;; occurs-check? : Symbol Term -> Boolean
(define (occurs-check? var term)
  (cond
    [(term-var? term) (eq? var (term-var-name term))]
    [(term-const? term) #f]
    [(term-op? term) (ormap (lambda (arg) (occurs-check? var arg)) (term-op-args term))]))

;; Unification: find most general unifier of two terms
;; unify : Term Term -> (U (Hash Symbol Term) #f)
(define (unify term1 term2)
  (unify-with-bindings term1 term2 (hash)))

;; unify-with-bindings : Term Term (Hash Symbol Term) -> (U (Hash Symbol Term) #f)
(define (unify-with-bindings term1 term2 bindings)
  ;; Apply current bindings first
  (define t1 (substitute term1 bindings))
  (define t2 (substitute term2 bindings))
  
  (cond
    [(term-equal? t1 t2) bindings]
    
    [(term-var? t1)
     (cond
       [(occurs-check? (term-var-name t1) t2) #f]
       [else (hash-set bindings (term-var-name t1) t2)])]
    
    [(term-var? t2)
     (cond
       [(occurs-check? (term-var-name t2) t1) #f]
       [else (hash-set bindings (term-var-name t2) t1)])]
    
    [(and (term-op? t1) (term-op? t2)
          (equal? (term-op-op-sym t1) (term-op-op-sym t2))
          (= (length (term-op-args t1)) (length (term-op-args t2))))
     ;; Unify arguments pairwise
     (let loop ([args1 (term-op-args t1)]
                [args2 (term-op-args t2)]
                [current-bindings bindings])
       (cond
         [(null? args1) current-bindings]
         [(not current-bindings) #f]
         [else
          (let ([new-bindings (unify-with-bindings (first args1) (first args2) current-bindings)])
            (loop (rest args1) (rest args2) new-bindings))]))]
    
    [else #f])) ; Cannot unify

;; Apply bindings (from a match) to instantiate a pattern term
;; apply-bindings : TermPattern (Hash Symbol Term) -> Term
(define (apply-bindings pattern-term bindings)
  (substitute pattern-term bindings)) ; Substitute works for this if pattern vars are distinct


;; --- Helper for subterm traversal ---
;; (Placeholder for now - Zippers or path-based access would be better)
;; find-all-subterms-with-paths : Term -> (Listof (List Term (Listof Integer)))
(define (find-all-subterms-with-paths term)
  ;; TODO: Implement this
  ;; Returns list of (subterm path-to-subterm)
  ;; Path could be like '() for root, '(0) for first arg, '(0 1) for first arg of first arg
  (define (collect term current-path)
    (cons (list term current-path)
          (match term
            [(term-op _ args)
             (apply append
                    (for/list ([arg args] [i (in-naturals)])
                      (collect arg (append current-path (list i)))))]
            [_ '()])))
  (collect term '()))

;; replace-subterm-at : Term (Listof Integer) Term -> Term
(define (replace-subterm-at term path new-subterm)
  ;; TODO: Implement this
  (if (null? path)
      new-subterm
      (match term
        [(term-op op-sym args)
         (let ([idx (first path)])
           (if (< idx (length args))
               (term-op op-sym
                        (append (take args idx)
                                (list (replace-subterm-at (list-ref args idx) (rest path) new-subterm))
                                (drop args (+ idx 1))))
               (error 'replace-subterm-at "Invalid path ~a for term ~a" path term)))]
        [_ (error 'replace-subterm-at "Cannot descend into non-op term ~a with path ~a" term path)])))

(module+ test
  (require rackunit)
  (define x (term-var 'x))
  (define y (term-var 'y))
  (define e (term-const 'e))
  (define t1 (term-op '* (list x y)))
  (define t2 (term-op '* (list x y)))
  (define t3 (term-op '* (list x e)))

  (check-true (term? x))
  (check-true (term-equal? t1 t2))
  (check-false (term-equal? t1 t3))

  (check-equal? (free-vars x) '(x))
  (check-equal? (free-vars e) '())
  ;; Consider simplifying or removing this next line if list-set=? is primarily for tests/test-terms.rkt
  ;; For example, just check the length or presence of 'x:
  (check-equal? (length (free-vars t3)) 1)
  (check-true (member 'x (free-vars t3)))
  ;; OR, if you want to keep the spirit, use sets:
  ; (require racket/set) ; Would need this at the top of module+ test or file
  ; (check-equal? (list->set (free-vars t3)) (list->set '(x)))


  (check-equal? (substitute t3 (hash 'x e)) (term-op '* (list e e)))
  (check-equal? (substitute t3 (hash 'z (term-const 'a))) t3) ; No 'z in t3

  (displayln "Term tests (within terms.rkt) passed (basic).") ; Modified message
)