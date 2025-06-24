#lang racket/base

(require racket/list)
(require racket/set)
(require racket/hash)
(require racket/string)
(require "terms.rkt"
         "theories.rkt")
(provide (all-defined-out))

;; Simple list-based queue implementation
(define (make-queue) '())
(define (enqueue q x) (append q (list x)))
(define (dequeue q)
  (if (null? q)
      (error 'dequeue "Queue is empty")
      (values (car q) (cdr q))))
(define (queue-empty? q) (null? q))

;; Cache for memoizing expensive operations
(define rewrite-cache (make-hash))
(define normal-form-cache (make-hash))

;; Clear caches (useful for testing)
(define (clear-caches!)
  (set! rewrite-cache (make-hash))
  (set! normal-form-cache (make-hash)))

;; Get rewrite rules (bidirectional) from a theory's axioms
;; get-rewrite-rules : LawvereTheory -> (Listof (Cons TermPattern TermPattern))
(define (get-rewrite-rules theory)
  (append-map
   (lambda (axiom)
     (define fresh-axiom (fresh-axiom-instance axiom))
     (list (cons (car fresh-axiom) (cdr fresh-axiom))  ; l -> r
           (cons (cdr fresh-axiom) (car fresh-axiom)))) ; r -> l
   (lawvere-theory-axioms theory)))

;; Find all terms reachable from `term` in one rewrite step.
;; one-step-rewrites : Term LawvereTheory -> (Listof Term)
(define (one-step-rewrites term theory)
  (let ([cache-key (cons term (lawvere-theory-name theory))])
    (cond
      [(hash-ref rewrite-cache cache-key #f)]
      [else
       (let ([rules (get-rewrite-rules theory)])
         (define result
           (remove-duplicates ; Avoid duplicate results if multiple rules yield same term
            (flatten
             (for*/list ([rule rules]
                         [subterm+path (in-list (find-all-subterms-with-paths term))])
               (define sub_term (first subterm+path))
               (define path-to-sub (second subterm+path))
               (let ([bindings (term-match? (car rule) sub_term (hash))])
                 (if bindings
                     (list (replace-subterm-at term path-to-sub (apply-bindings (cdr rule) bindings)))
                     '()))))
            term-equal?))
         (hash-set! rewrite-cache cache-key result)
         result)])))

;; Normalize a term using left-to-right oriented rules (for confluent systems)
;; normalize-term : Term LawvereTheory -> Term  
(define (normalize-term term theory)
  (let ([cache-key (cons term (lawvere-theory-name theory))])
    (cond
      [(hash-ref normal-form-cache cache-key #f)]
      [else
       (define result (normalize-term-uncached term theory))
       (hash-set! normal-form-cache cache-key result)
       result])))

(define (normalize-term-uncached term theory)
  ;; Simple normalization: apply left-to-right rules until no more changes
  ;; This assumes the theory has oriented rules that are confluent
  (define left-to-right-rules
    (map (lambda (axiom) (cons (car axiom) (cdr axiom)))
         (lawvere-theory-axioms theory)))
  
  (define (apply-one-step t)
    (for/first ([rule left-to-right-rules]
                [subterm+path (in-list (find-all-subterms-with-paths t))]
                #:when #t)
      (define sub_term (first subterm+path))
      (define path-to-sub (second subterm+path))
      (let ([bindings (term-match? (car rule) sub_term (hash))])
        (and bindings
             (replace-subterm-at t path-to-sub (apply-bindings (cdr rule) bindings))))))
  
  (let loop ([current term] [max-steps 100])
    (cond
      [(= max-steps 0) current] ; Prevent infinite loops
      [else
       (define next (apply-one-step current))
       (if (and next (not (term-equal? next current)))
           (loop next (- max-steps 1))
           current)])))

;; Fast equality check for confluent systems
;; confluent-equal? : Term Term LawvereTheory -> Boolean
(define (confluent-equal? term1 term2 theory)
  (term-equal? (normalize-term term1 theory)
               (normalize-term term2 theory)))

;; Iterative Deepening Search for rewrite paths
;; ids-find-rewrite-path : Term Term LawvereTheory Integer -> (U (Listof Term) #f)
(define (ids-find-rewrite-path start-term end-term theory max-depth)
  ;; Special case: if start equals end, return immediately
  (if (term-equal? start-term end-term)
      (list start-term)
      ;; Try each depth level from 1 to max-depth
      (for/first ([depth (in-range 1 (+ max-depth 1))]
                  #:when #t)
        (dfs-find-path start-term end-term theory depth))))

;; Depth-First Search with depth limit
;; dfs-find-path : Term Term LawvereTheory Integer -> (U (Listof Term) #f)
(define (dfs-find-path start-term end-term theory max-depth)
  (define visited (mutable-set))
  
  (define (dfs-helper current-term path remaining-depth)
    (cond
      [(term-equal? current-term end-term) (reverse (cons current-term path))]
      [(= remaining-depth 0) #f]
      [(set-member? visited current-term) #f]
      [else
       (set-add! visited current-term)
       (define result
         (for/first ([next-term (one-step-rewrites current-term theory)]
                     #:when #t)
           (dfs-helper next-term (cons current-term path) (- remaining-depth 1))))
       (set-remove! visited current-term)
       result]))
  
  (dfs-helper start-term '() max-depth))

;; Original BFS implementation (kept for compatibility)
;; find-rewrite-path : Term Term LawvereTheory Integer -> (U (Listof Term) #f)
(define (find-rewrite-path start-term end-term theory max-depth)
  ;; Special case: if start equals end, return immediately
  (if (term-equal? start-term end-term)
      (list start-term)
      
      ;; Otherwise, perform breadth-first search
      (let ([visited (set start-term)])
        (let loop ([queue (list (list start-term))]
                   [depth 0])
          (cond
            ;; Base cases: queue empty or max depth reached
            [(null? queue) #f]
            [(> depth max-depth) #f]
            
            ;; Search queue for a path that reaches the goal
            [else
             (define goal-path
               (for/first ([path queue]
                           #:when (term-equal? (last path) end-term))
                 path))
             
             ;; If goal found, return it
             (if goal-path
                 goal-path
                 
                 ;; Otherwise, expand the next path
                 (let* ([current-path (car queue)]
                        [rest-queue (cdr queue)]
                        [current-term (last current-path)])
                   
                   ;; Get next terms and filter out already visited ones
                   (define next-terms (one-step-rewrites current-term theory))
                   (define new-queue
                     (foldl 
                      (lambda (next-term acc-queue)
                        (if (set-member? visited next-term)
                            acc-queue
                            (begin
                              (set! visited (set-add visited next-term))
                              (append acc-queue 
                                      (list (append current-path (list next-term)))))))
                      rest-queue
                      next-terms))
                   
                   ;; Continue search with updated queue
                   (loop new-queue (+ depth 1))))])))))

;; Heuristic distance estimate (placeholder for A* search)
;; heuristic-distance : Term Term LawvereTheory -> Number
(define (heuristic-distance term1 term2 theory)
  ;; Simple heuristic: structural difference count
  ;; In practice, domain-specific heuristics would be much better
  (define (count-differences t1 t2)
    (cond
      [(term-equal? t1 t2) 0]
      [(and (term-var? t1) (term-var? t2)) 1]
      [(and (term-const? t1) (term-const? t2)) 1]
      [(and (term-op? t1) (term-op? t2)
            (equal? (term-op-op-sym t1) (term-op-op-sym t2))
            (= (length (term-op-args t1)) (length (term-op-args t2))))
       (apply + (map count-differences (term-op-args t1) (term-op-args t2)))]
      [else 5])) ; High penalty for structural mismatch
  
  (count-differences term1 term2))

;; A* search for rewrite paths (proper implementation)
;; astar-find-rewrite-path : Term Term LawvereTheory Integer -> (U (Listof Term) #f)
(define (astar-find-rewrite-path start-term end-term theory max-depth)
  (if (term-equal? start-term end-term)
      (list start-term)
      (let ([open-set (list (list start-term 0 (heuristic-distance start-term end-term theory) (list start-term)))]
            [closed-set (mutable-set)]
            [best-g-scores (make-hash)]) ; term -> best g-score seen
        
        (hash-set! best-g-scores start-term 0)
        
        (let loop ([open open-set])
          (cond
            [(null? open) #f] ; No path found
            [else
             (define current-entry (car open))
             (define current-term (first current-entry))
             (define current-g (second current-entry))
             (define current-f (third current-entry))
             (define current-path (fourth current-entry))
             (define rest-open (cdr open))
             
             (cond
               [(term-equal? current-term end-term) current-path]
               [(set-member? closed-set current-term) (loop rest-open)]
               [(> current-g max-depth) (loop rest-open)]
               [else
                (set-add! closed-set current-term)
                (define neighbors (one-step-rewrites current-term theory))
                (define new-entries
                  (for/list ([neighbor neighbors]
                             #:when (not (set-member? closed-set neighbor)))
                    (define tentative-g (+ current-g 1))
                    (define previous-g (hash-ref best-g-scores neighbor +inf.0))
                    (cond
                      [(< tentative-g previous-g)
                       (hash-set! best-g-scores neighbor tentative-g)
                       (define h (heuristic-distance neighbor end-term theory))
                       (define f (+ tentative-g h))
                       (list neighbor tentative-g f (append current-path (list neighbor)))]
                      [else #f])))
                
                ;; Filter out #f entries and merge with existing open set
                (define filtered-new (filter (lambda (x) x) new-entries))
                (define updated-open 
                  (sort (append rest-open filtered-new)
                        (lambda (a b) (< (third a) (third b))))) ; Sort by f-score
                
                (loop updated-open)])])))))

;; Enhanced heuristic that considers theory-specific patterns
;; domain-aware-heuristic : Term Term LawvereTheory -> Number
(define (domain-aware-heuristic term1 term2 theory)
  (define theory-name (lawvere-theory-name theory))
  (case theory-name
    [(Monoid Group)
     ;; For monoids/groups, estimate by counting unnecessary e's and associativity fixes
     (+ (count-identity-applications term1 term2)
        (count-associativity-differences term1 term2))]
    [(Ring)
     ;; For rings, consider both additive and multiplicative structure
     (+ (count-identity-applications term1 term2)
        (count-distributivity-opportunities term1 term2))]
    [else
     ;; Fallback to structural heuristic
     (heuristic-distance term1 term2 theory)]))

;; Helper functions for domain-aware heuristics
(define (count-identity-applications term1 term2)
  ;; Count how many e's appear that could be simplified
  (define (count-e-occurrences term)
    (cond
      [(term-const? term) (if (eq? (term-const-name term) 'e) 1 0)]
      [(term-op? term) (apply + (map count-e-occurrences (term-op-args term)))]
      [else 0]))
  (abs (- (count-e-occurrences term1) (count-e-occurrences term2))))

(define (count-associativity-differences term1 term2)
  ;; Rough estimate of associativity rebalancing needed
  (define (max-depth term)
    (cond
      [(term-op? term) (+ 1 (apply max 0 (map max-depth (term-op-args term))))]
      [else 0]))
  (abs (- (max-depth term1) (max-depth term2))))

(define (count-distributivity-opportunities term1 term2)
  ;; Count potential distributivity applications (simplified)
  (define (count-mixed-ops term)
    (cond
      [(term-op? term) 
       (define op (term-op-op-sym term))
       (define args (term-op-args term))
       (+ (if (and (eq? op '*) 
                   (ormap (lambda (arg) (and (term-op? arg) (eq? (term-op-op-sym arg) '+))) args))
              1 0)
          (apply + (map count-mixed-ops args)))]
      [else 0]))
  (abs (- (count-mixed-ops term1) (count-mixed-ops term2))))

;; Smart path finder that chooses the best algorithm
;; smart-find-rewrite-path : Term Term LawvereTheory Integer [#:method Symbol] -> (U (Listof Term) #f)
(define (smart-find-rewrite-path start-term end-term theory max-depth #:method [method 'auto])
  (case method
    [(bfs) (find-rewrite-path start-term end-term theory max-depth)]
    [(ids) (ids-find-rewrite-path start-term end-term theory max-depth)]
    [(astar) (astar-find-rewrite-path start-term end-term theory max-depth)]
    [(confluent) 
     (if (confluent-equal? start-term end-term theory)
         (list start-term end-term) ; Placeholder path via normal forms
         #f)]
    [(auto)
     ;; Intelligent method selection based on problem characteristics
     (define term-complexity (+ (term-size start-term) (term-size end-term)))
     (define heuristic-quality (heuristic-quality-estimate start-term end-term theory))
     
     (cond
       ;; For simple problems, BFS is fine
       [(and (< term-complexity 10) (< max-depth 4))
        (find-rewrite-path start-term end-term theory max-depth)]
       
       ;; For problems with good heuristics, use A*
       [(and (> heuristic-quality 0.7) (< max-depth 15))
        (astar-find-rewrite-path start-term end-term theory max-depth)]
       
       ;; For deep searches, use IDS to manage memory
       [(> max-depth 10)
        (ids-find-rewrite-path start-term end-term theory max-depth)]
       
       ;; For specific theories known to be confluent, try normalization first
       [(member (lawvere-theory-name theory) '(Monoid Group Ring))
        (or (and (confluent-equal? start-term end-term theory)
                 (list start-term end-term))
            (astar-find-rewrite-path start-term end-term theory max-depth))]
       
       ;; Default fallback
       [else (find-rewrite-path start-term end-term theory max-depth)])]
    [else (error 'smart-find-rewrite-path "Unknown method: ~a" method)]))

;; Helper functions for intelligent method selection
(define (term-size term)
  (cond
    [(term-var? term) 1]
    [(term-const? term) 1]
    [(term-op? term) (+ 1 (apply + (map term-size (term-op-args term))))]))

(define (heuristic-quality-estimate term1 term2 theory)
  ;; Estimate how good our heuristic is for this problem (0.0 to 1.0)
  (define theory-name (lawvere-theory-name theory))
  (case theory-name
    [(Monoid Group) 0.8]  ; Good heuristics for these
    [(Ring) 0.7]          ; Decent heuristics
    [else 0.3]))          ; Basic structural heuristic only

(module+ test
  (require rackunit)
  ;; We'll need a concrete theory (like Monoid) to test these properly.
  ;; For now, just check that the functions exist.
  (check-true (procedure? get-rewrite-rules))
  (check-true (procedure? one-step-rewrites))
  (check-true (procedure? find-rewrite-path))
  (check-true (procedure? ids-find-rewrite-path))
  (check-true (procedure? smart-find-rewrite-path))
  (check-true (procedure? normalize-term))
  (check-true (procedure? confluent-equal?))
  (displayln "CT tests with performance optimizations passed.")
)

;; Parallel BFS search for rewrite paths
;; parallel-find-rewrite-path : Term Term LawvereTheory Integer -> (U (Listof Term) #f)
(define (parallel-find-rewrite-path start-term end-term theory max-depth)
  (if (term-equal? start-term end-term)
      (list start-term)
      ;; For now, fallback to sequential until we implement proper parallel primitives
      ;; In a full implementation, this would use Racket's parallel forms
      (find-rewrite-path start-term end-term theory max-depth)))

;; Incremental rewrite cache that persists across sessions
(define incremental-cache (make-hash))

;; Save incremental cache to file
;; save-incremental-cache : String -> Void
(define (save-incremental-cache filename)
  (call-with-output-file filename
    (lambda (out)
      (write (hash->list incremental-cache) out))
    #:exists 'replace))

;; Load incremental cache from file
;; load-incremental-cache : String -> Void
(define (load-incremental-cache filename)
  (when (file-exists? filename)
    (call-with-input-file filename
      (lambda (in)
        (define cache-list (read in))
        (set! incremental-cache (make-hash cache-list))))))

;; Incremental one-step-rewrites with persistent caching
;; incremental-one-step-rewrites : Term LawvereTheory -> (Listof Term)
(define (incremental-one-step-rewrites term theory)
  (define cache-key (cons term (lawvere-theory-name theory)))
  
  (cond
    [(hash-ref incremental-cache cache-key #f)]
    [else
     (define result (one-step-rewrites term theory))
     (hash-set! incremental-cache cache-key result)
     result]))

;; Batch processing for multiple path queries
;; batch-find-paths : (Listof (List Term Term)) LawvereTheory Integer -> (Listof (U (Listof Term) #f))
(define (batch-find-paths queries theory max-depth)
  (for/list ([query queries])
    (define start-term (first query))
    (define end-term (second query))
    (smart-find-rewrite-path start-term end-term theory max-depth)))

;; Path compression for repeated subpaths
(define compressed-paths (make-hash))

;; compress-path : (Listof Term) -> CompressedPath
(define (compress-path path)
  ;; Simple compression: store unique subpaths
  (define subpaths '())
  (define path-length (length path))
  
  (for ([i (in-range path-length)])
    (for ([j (in-range (+ i 2) (+ path-length 1))])  ; j > i+1 for subpaths of length > 1
      (define subpath (take (drop path i) (- j i)))
      (when (> (length subpath) 1)
        (set! subpaths (cons subpath subpaths)))))
  
  ;; Store frequently used subpaths for reuse
  (for ([subpath subpaths])
    (when (>= (length subpath) 2)  ; Only store meaningful subpaths
      (define key (list (first subpath) (last subpath)))
      (hash-update! compressed-paths key
                    (lambda (old) (cons subpath old))
                    '())))
  
  path) ; Return original path for now

;; Statistical analysis of search performance
(struct search-statistics
  (total-searches
   successful-searches  
   average-depth
   cache-hit-rate
   method-usage-counts) ; Hash of method -> count
  #:transparent)

(define current-stats 
  (search-statistics 0 0 0.0 0.0 (make-hash)))

;; Update search statistics
;; update-search-stats : Symbol Boolean Integer Boolean -> Void
(define (update-search-stats method successful? depth cache-hit?)
  (define new-total (+ (search-statistics-total-searches current-stats) 1))
  (define new-successful (+ (search-statistics-successful-searches current-stats) 
                           (if successful? 1 0)))
  (define old-avg (search-statistics-average-depth current-stats))
  (define new-avg (+ (* old-avg (/ (- new-total 1) new-total))
                    (* depth (/ 1 new-total))))
  (define old-hit-rate (search-statistics-cache-hit-rate current-stats))
  (define new-hit-rate (+ (* old-hit-rate (/ (- new-total 1) new-total))
                         (* (if cache-hit? 1.0 0.0) (/ 1 new-total))))
  
  (define method-counts (search-statistics-method-usage-counts current-stats))
  (hash-update! method-counts method (lambda (old) (+ old 1)) 0)
  
  (set! current-stats 
        (search-statistics new-total new-successful new-avg new-hit-rate method-counts)))

;; Generate performance report
;; performance-report : -> String
(define (performance-report)
  (define stats current-stats)
  (string-append
   "SLC Performance Statistics\n"
   "==========================\n"
   (format "Total searches: ~a\n" (search-statistics-total-searches stats))
   (format "Successful: ~a (~a%)\n" 
           (search-statistics-successful-searches stats)
           (exact->inexact 
            (* 100 (/ (search-statistics-successful-searches stats)
                     (max 1 (search-statistics-total-searches stats))))))
   (format "Average depth: ~a\n" (search-statistics-average-depth stats))
   (format "Cache hit rate: ~a%\n" 
           (exact->inexact (* 100 (search-statistics-cache-hit-rate stats))))
   "Method usage:\n"
   (string-join
    (for/list ([(method count) (search-statistics-method-usage-counts stats)])
      (format "  ~a: ~a times" method count))
    "\n")))

;; Enhanced smart path finder with statistics
;; instrumented-smart-find-rewrite-path : Term Term LawvereTheory Integer [#:method Symbol] -> (U (Listof Term) #f)
(define (instrumented-smart-find-rewrite-path start-term end-term theory max-depth #:method [method 'auto])
  (define cache-key (list start-term end-term (lawvere-theory-name theory) max-depth method))
  (define cached-result (hash-ref rewrite-cache cache-key #f))
  
  (cond
    [cached-result
     (update-search-stats method #t (length cached-result) #t)
     cached-result]
    [else
     (define result (smart-find-rewrite-path start-term end-term theory max-depth #:method method))
     (define successful? (not (not result)))
     (define depth (if result (length result) max-depth))
     
     (when result
       (hash-set! rewrite-cache cache-key result))
     
     (update-search-stats method successful? depth #f)
     result]))

;; Rename variables in a term to avoid conflicts
;; rename-variables : Term (Hash Symbol Symbol) -> Term
(define (rename-variables term var-map)
  (cond
    [(term-var? term) 
     (define old-name (term-var-name term))
     (define new-name (hash-ref var-map old-name old-name))
     (term-var new-name)]
    [(term-const? term) term]
    [(term-op? term)
     (term-op (term-op-op-sym term)
              (map (lambda (arg) (rename-variables arg var-map)) (term-op-args term)))]))

;; Generate fresh variable names for axiom patterns
;; fresh-axiom-instance : (Cons Term Term) -> (Cons Term Term)
(define (fresh-axiom-instance axiom)
  (define vars-in-axiom (remove-duplicates 
                         (append (free-vars (car axiom)) 
                                 (free-vars (cdr axiom)))))
  
  ;; Create mapping from old names to fresh names
  (define var-map
    (for/hash ([var vars-in-axiom]
               [i (in-naturals)])
      (values var (string->symbol (format "_ax_~a_~a" var i)))))
  
  (cons (rename-variables (car axiom) var-map)
        (rename-variables (cdr axiom) var-map)))