#lang racket/base

(require "terms.rkt"
         "theories.rkt"
         "lt.rkt"
         "ct.rkt"
         "slc-simulator.rkt"
         "theory-analysis.rkt"
         "parser.rkt"
         "errors.rkt"
         "proofs.rkt"
         racket/set
         racket/string
         racket/match
         racket/list)

;; ----- Global state for the interactive REPL -----
(define current-theory (make-parameter #f))

;; ----- Basic term conversion functions -----

;; Use the improved parser from parser.rkt
(define (parse-term-sexp str)
  (parse-term str))

;; Use the improved formatter from parser.rkt
(define (format-term term)
  (format-term-pretty term))

;; Format a rewrite path
(define (format-path path)
  (string-join 
   (for/list ([term path]
              [i (in-naturals)])
     (format "  Step ~a: ~a" i (format-term term)))
   "\n"))

;; ----- Command handlers -----

;; Load a predefined theory
(define (cmd-use-theory name)
  (cond
    [(equal? name "Monoid")
     (define T_Mon (dynamic-require '"slc/examples/monoid.rkt" 'T_Mon))
     (current-theory T_Mon)
     "Using Monoid theory"]
    [(equal? name "Group")
     (define T_Grp (dynamic-require '"slc/examples/group.rkt" 'T_Grp))
     (current-theory T_Grp)
     "Using Group theory"]
    [(equal? name "Ring")
     (define T_Ring (dynamic-require '"slc/examples/ring.rkt" 'T_Ring))
     (current-theory T_Ring)
     "Using Ring theory"]
    [else
     (format "Theory not found: ~a (available: Monoid, Group, Ring)" name)]))

;; Define a new theory
(define (cmd-define-theory name axioms)
  (define axiom-pairs
    (for/list ([axiom axioms])
      (match (string-split axiom "=")
        [(list lhs rhs)
         (cons (parse-term-sexp (string-trim lhs))
               (parse-term-sexp (string-trim rhs)))]
        [_ (error 'define-theory "Invalid axiom format: ~a" axiom)])))
  
  (define theory (lawvere-theory (string->symbol name) axiom-pairs))
  (current-theory theory)
  
  (format "Defined theory ~a with ~a axiom~a"
          name
          (length axiom-pairs)
          (if (= (length axiom-pairs) 1) "" "s")))

;; Prove an equality with method selection
(define (cmd-prove lhs rhs depth-str method-str)
  (unless (current-theory)
    (error 'prove "No theory selected. Use 'use-theory' first."))
  
  (define depth 
    (cond
      [(number? depth-str) depth-str]
      [(and (string? depth-str) (string->number depth-str)) (string->number depth-str)]
      [else 5]))  ; Default depth
  
  (define method
    (cond
      [(and (string? method-str) (member method-str '("bfs" "ids" "confluent" "auto")))
       (string->symbol method-str)]
      [else 'auto]))  ; Default method
  
  (define lhs-term (parse-term-sexp lhs))
  (define rhs-term (parse-term-sexp rhs))
  
  (define result (prover+simulator lhs-term rhs-term (current-theory) depth))
  
  (if (prover-simulator-result-are-equal? result)
      (format "✓ Proved: ~a = ~a (method: ~a)\nProof path:\n~a" 
              (format-term lhs-term) 
              (format-term rhs-term)
              method
              (format-path (prover-simulator-result-path result)))
      (format "✗ Could not prove: ~a = ~a (within depth ~a, method: ~a)" 
              (format-term lhs-term) 
              (format-term rhs-term)
              depth
              method)))

;; Benchmark different proof methods
(define (cmd-benchmark lhs rhs depth-str)
  (unless (current-theory)
    (error 'benchmark "No theory selected. Use 'use-theory' first."))
  
  (define depth 
    (cond
      [(number? depth-str) depth-str]
      [(and (string? depth-str) (string->number depth-str)) (string->number depth-str)]
      [else 5]))
  
  (define lhs-term (parse-term-sexp lhs))
  (define rhs-term (parse-term-sexp rhs))
  
  (clear-caches!) ; Clear caches for fair comparison
  
  (define methods '(bfs ids confluent auto))
  (define results
    (for/list ([method methods])
      (define start-time (current-inexact-milliseconds))
      (define path (smart-find-rewrite-path lhs-term rhs-term (current-theory) depth #:method method))
      (define end-time (current-inexact-milliseconds))
      (list method path (- end-time start-time))))
  
  (string-join
   (cons (format "Benchmarking: ~a = ~a (depth ~a)\n"
                 (format-term lhs-term) (format-term rhs-term) depth)
         (for/list ([result results])
           (match result
             [(list method path time)
              (format "  ~a: ~a (~a ms)" 
                     method 
                     (if path "✓ Found path" "✗ No path")
                     time)])))
   "\n"))

;; Normalize a term
(define (cmd-normalize term)
  (unless (current-theory)
    (error 'normalize "No theory selected. Use 'use-theory' first."))
  
  (define term-val (parse-term-sexp term))
  (define normalized (normalize-term term-val (current-theory)))
  
  (format "Original: ~a\nNormalized: ~a\nEqual? ~a"
          (format-term term-val)
          (format-term normalized)
          (term-equal? term-val normalized)))

;; Clear performance caches
(define (cmd-clear-cache)
  (clear-caches!)
  "Performance caches cleared.")

;; Simulate rewrites from a term
(define (cmd-simulate term depth show-states?)
  (unless (current-theory)
    (error 'simulate "No theory selected. Use 'use-theory' first."))
  
  (define depth-val (if depth (string->number depth) 3))
  (define term-val (parse-term-sexp term))
  
  (if show-states?
      (let ([states-by-depth (reachable-terms-by-depth term-val (current-theory) depth-val)])
        (string-join
         (for/list ([states states-by-depth]
                    [depth (in-naturals)])
           (format "Depth ~a (~a term~a):\n~a"
                   depth
                   (set-count states)
                   (if (= (set-count states) 1) "" "s")
                   (string-join
                    (for/list ([term (in-set states)])
                      (format "  ~a" (format-term term)))
                    "\n")))
         "\n\n"))
      (let ([paths (simulate-rewrites term-val (current-theory) depth-val)])
        (format "Simulating rewrites from ~a (depth ~a):\n\n~a"
                (format-term term-val)
                depth-val
                (string-join
                 (for/list ([path paths]
                            [i (in-naturals)])
                   (format "Path ~a:\n~a" i (format-path path)))
                 "\n\n")))))

;; Show one-step rewrites
(define (cmd-onestep term)
  (unless (current-theory)
    (error 'onestep "No theory selected. Use 'use-theory' first."))
  
  (define term-val (parse-term-sexp term))
  (define next-terms (one-step-rewrites term-val (current-theory)))
  
  (format "One-step rewrites from ~a:\n~a"
          (format-term term-val)
          (string-join
           (for/list ([next-term next-terms]
                      [i (in-naturals)])
             (format "  ~a: ~a" i (format-term next-term)))
           "\n")))

;; Handle specific Group theory axiom checks
(define (check-group-axiom)
  (unless (current-theory)
    (error 'check "No theory selected. Use 'use-theory' first."))
  
  ;; Create the terms for the left inverse axiom: inv(X) * X = e
  (define X (term-var 'X))
  (define inv-X (term-op 'inv (list X)))
  (define inv-X-times-X (term-op '* (list inv-X X)))
  (define e (term-const 'e))
  
  ;; Check if the axiom holds
  (define result (prover+simulator inv-X-times-X e (current-theory) 3))
  
  (if (prover-simulator-result-are-equal? result)
      (format "✓ Proved: (inv X) * X = e\nProof path:\n~a" 
              (format-path (prover-simulator-result-path result)))
      (format "✗ Could not prove: (inv X) * X = e (within depth 3)")))

;; Analyze the current theory
(define (cmd-analyze-theory)
  (unless (current-theory)
    (error 'analyze "No theory selected. Use 'use-theory' first."))
  
  (define analysis (analyze-theory (current-theory)))
  (analysis-report analysis))

;; Test unification between two terms
(define (cmd-unify term1-str term2-str)
  (define term1 (parse-term-sexp term1-str))
  (define term2 (parse-term-sexp term2-str))
  (define unifier (unify term1 term2))
  
  (if unifier
      (format "✓ Unification successful:\n~a"
              (string-join
               (for/list ([(var term) unifier])
                 (format "  ~a ↦ ~a" var (format-term term)))
               "\n"))
      (format "✗ Terms cannot be unified:\n  ~a\n  ~a" 
              (format-term term1) (format-term term2))))

;; Build a proof step by step using equational logic
(define (cmd-build-proof lhs-str rhs-str)
  (unless (current-theory)
    (error 'build-proof "No theory selected. Use 'use-theory' first."))
  
  (define lhs-term (parse-term-sexp lhs-str))
  (define rhs-term (parse-term-sexp rhs-str))
  
  (printf "Building L_T proof for: ~a = ~a\n" 
          (format-term lhs-term) (format-term rhs-term))
  
  ;; Use the C_T computation path to construct L_T proof
  (define path (find-rewrite-path lhs-term rhs-term (current-theory) 5))
  
  (if path
      (let ([steps (- (length path) 1)])
        (format "✓ Proof found using ~a equational steps:\n~a"
                steps
                (string-join
                 (for/list ([step path]
                            [i (in-naturals)])
                   (cond
                     [(= i 0) (format "  Given: ~a" (format-term step))]
                     [(= i (- (length path) 1)) (format "  Conclude: ~a" (format-term step))]
                     [else (format "  Step ~a: ~a" i (format-term step))]))
                 "\n")))
      (format "✗ Could not build proof for ~a = ~a within search depth"
              (format-term lhs-term)
              (format-term rhs-term))))

;; Format proof steps for display
(define (format-proof-steps proof-builder)
  (if (not proof-builder)
      "No proof steps"
      (string-join
       (for/list ([step (proof-builder-steps proof-builder)]
                  [i (in-naturals)])
         (match step
           [(proof-step rule premises conclusion)
            (format "  Step ~a: ~a ~a ⊢ ~a = ~a"
                   (+ i 1)
                   rule
                   (if (null? premises) "" (format "(using ~a premises)" (length premises)))
                   (format-term (car conclusion))
                   (format-term (cdr conclusion)))]))
       "\n")))

;; Show performance statistics
(define (cmd-show-stats)
  (performance-report))

;; Save persistent cache
(define (cmd-save-cache filename)
  (save-incremental-cache filename)
  (format "Cache saved to ~a" filename))

;; Load persistent cache  
(define (cmd-load-cache filename)
  (load-incremental-cache filename)
  (format "Cache loaded from ~a" filename))

;; Batch prove multiple equalities
(define (cmd-batch-prove equations-file depth-str)
  (unless (current-theory)
    (error 'batch-prove "No theory selected. Use 'use-theory' first."))
  
  (define depth 
    (cond
      [(number? depth-str) depth-str]
      [(and (string? depth-str) (string->number depth-str)) (string->number depth-str)]
      [else 5]))
  
  (unless (file-exists? equations-file)
    (error 'batch-prove "File not found: ~a" equations-file))
  
  (define equations
    (call-with-input-file equations-file
      (lambda (in)
        (for/list ([line (in-lines in)]
                   #:when (and (not (string=? line ""))
                              (string-contains? line "=")))
          (define parts (string-split line "="))
          (list (string-trim (first parts))
                (string-trim (second parts)))))))
  
  (define results
    (for/list ([eq equations]
               [i (in-naturals)])
      (define lhs-term (parse-term-sexp (first eq)))
      (define rhs-term (parse-term-sexp (second eq)))
      (define result (prover+simulator lhs-term rhs-term (current-theory) depth))
      (list i (first eq) (second eq) (prover-simulator-result-are-equal? result))))
  
  (string-join
   (cons (format "Batch proving ~a equations from ~a:\n" (length equations) equations-file)
         (for/list ([result results])
           (match result
             [(list i lhs rhs success?)
              (format "  ~a. ~a = ~a: ~a" 
                     (+ i 1) lhs rhs 
                     (if success? "✓" "✗"))])))
   "\n"))

;; Test all available search methods on current query
(define (cmd-compare-methods lhs rhs depth-str)
  (unless (current-theory)
    (error 'compare-methods "No theory selected. Use 'use-theory' first."))
  
  (define depth 
    (cond
      [(number? depth-str) depth-str]
      [(and (string? depth-str) (string->number depth-str)) (string->number depth-str)]
      [else 5]))
  
  (define lhs-term (parse-term-sexp lhs))
  (define rhs-term (parse-term-sexp rhs))
  
  (clear-caches!) ; Clear caches for fair comparison
  
  (define methods '(bfs ids astar confluent))
  (define results
    (for/list ([method methods])
      (define start-time (current-inexact-milliseconds))
      (define path (smart-find-rewrite-path lhs-term rhs-term (current-theory) depth #:method method))
      (define end-time (current-inexact-milliseconds))
      (list method path (- end-time start-time))))
  
  (string-join
   (cons (format "Method comparison: ~a = ~a (depth ~a)\n"
                 (format-term lhs-term) (format-term rhs-term) depth)
         (for/list ([result results])
           (match result
             [(list method path time)
              (format "  ~a: ~a (~a ms, ~a steps)" 
                     method 
                     (if path "✓" "✗")
                     time
                     (if path (length path) "N/A"))])))
   "\n"))

;; Generate a theory completion report
(define (cmd-completion-report)
  (unless (current-theory)
    (error 'completion "No theory selected. Use 'use-theory' first."))
  
  (define analysis (analyze-theory (current-theory)))
  (string-append
   "Theory Completion Analysis\n"
   "==========================\n"
   (format "Theory: ~a\n" (theory-analysis-name analysis))
   (format "Detected properties: ~a\n" (theory-analysis-properties analysis))
   (format "Confluence status: ~a\n" (theory-analysis-confluence-status analysis))
   "\nSuggested optimizations:\n"
   (string-join
    (map (lambda (opt) (format "• ~a" opt))
         (theory-analysis-suggested-optimizations analysis))
    "\n")))

;; Export theory to different formats
(define (cmd-export-theory format filename)
  (unless (current-theory)
    (error 'export "No theory selected. Use 'use-theory' first."))
  
  (define theory (current-theory))
  (define content
    (case (string->symbol format)
      [(sexp)
       (format "~s" theory)]
      [(prolog)
       (export-to-prolog theory)]
      [(tptp)
       (export-to-tptp theory)]
      [else 
       (error 'export "Unknown format: ~a" format)]))
  
  (call-with-output-file filename
    (lambda (out)
      (display content out))
    #:exists 'replace)
  
  (format "Theory exported to ~a in ~a format" filename format))

;; Helper functions for export
(define (export-to-prolog theory)
  ;; Simplified Prolog export
  (string-join
   (for/list ([axiom (lawvere-theory-axioms theory)])
     (format "equal(~a, ~a)." 
             (term->prolog (car axiom))
             (term->prolog (cdr axiom))))
   "\n"))

(define (export-to-tptp theory)
  ;; Simplified TPTP export
  (string-join
   (for/list ([axiom (lawvere-theory-axioms theory)]
              [i (in-naturals)])
     (format "fof(axiom_~a, axiom, ~a = ~a)." 
             i
             (term->tptp (car axiom))
             (term->tptp (cdr axiom))))
   "\n"))

(define (term->prolog term)
  ;; Convert term to Prolog syntax
  (match term
    [(term-var name) (symbol->string name)]
    [(term-const name) (symbol->string name)]
    [(term-op op args)
     (format "~a(~a)" op (string-join (map term->prolog args) ", "))]))

(define (term->tptp term)
  ;; Convert term to TPTP syntax  
  (match term
    [(term-var name) (string-upcase (symbol->string name))]
    [(term-const name) (symbol->string name)]
    [(term-op op args)
     (format "~a(~a)" op (string-join (map term->tptp args) ","))]))

;; ----- REPL loop -----

;; Process a single command
(define (process-command line)
  (define parts (string-split (string-trim line)))
  
  (if (null? parts)
      "Empty command"
      (let ([cmd (car parts)]
            [args (cdr parts)])
        (with-handlers 
            ([slc-error? (λ (e) (format-slc-error e))]
             [exn:fail? (λ (e) (format "Error: ~a" (exn-message e)))])
          (cond
            [(equal? cmd "help")
             "Available commands:
  Core Theory Commands:
    define-theory NAME AXIOM1 AXIOM2 ...  - Define a new theory
    use-theory NAME                       - Use a predefined theory (Monoid, Group, Ring)
    analyze                               - Analyze current theory properties
    
  Proof Commands:
    prove LHS = RHS [DEPTH] [METHOD]      - Prove an equality
    build-proof LHS RHS                  - Build step-by-step proof in L_T
    benchmark LHS = RHS [DEPTH]           - Benchmark different proof methods
    compare-methods LHS RHS [--depth=N]   - Compare all search methods
    batch-prove FILE [--depth=N]         - Prove multiple equations from file
    
  Term Operations:
    normalize TERM                        - Normalize a term
    simulate TERM [--depth=N] [--states] - Simulate rewrites from a term
    onestep TERM                         - Show one-step rewrites
    unify TERM1 TERM2                    - Test unification of two terms
    
  Performance & Caching:
    clear-cache                          - Clear performance caches
    show-stats                           - Show performance statistics
    save-cache FILENAME                  - Save cache to file
    load-cache FILENAME                  - Load cache from file
    
  Analysis & Export:
    completion-report                    - Generate theory completion analysis
    export-theory FORMAT FILENAME       - Export theory (formats: sexp, prolog, tptp)
    
  Special Commands:
    check-group                          - Check group axioms
    exit                                 - Exit the REPL"]
            
            [(equal? cmd "use-theory")
             (if (null? args)
                 "Error: use-theory requires a theory name"
                 (cmd-use-theory (car args)))]
            
            [(equal? cmd "define-theory")
             (if (< (length args) 2)
                 "Error: define-theory requires a name and at least one axiom"
                 (cmd-define-theory (car args) (cdr args)))]
            
            [(equal? cmd "prove")
             (let ([args-str (string-join args " ")])
               ;; Find the position of the = sign
               (define eq-pos 
                 (for/first ([c (in-string args-str)]
                             [i (in-naturals)]
                             #:when (equal? c #\=))
                   i))
               (if (not eq-pos)
                   "Error: prove requires LHS = RHS [DEPTH] [METHOD]"
                   (let* ([lhs (substring args-str 0 eq-pos)]
                          [rhs-and-depth (substring args-str (+ eq-pos 1))]
                          ;; Extract depth if present
                          [depth-match (regexp-match #rx"(.*)([0-9]+)$" (string-trim rhs-and-depth))]
                          [rhs (if depth-match
                                    (string-trim (cadr depth-match))
                                    (string-trim rhs-and-depth))]
                          [depth (if depth-match
                                     (string->number (caddr depth-match))
                                     #f)]
                          [method-arg (findf (λ (a) (regexp-match? #rx"^--method=" a)) args)]
                          [method (and method-arg (regexp-replace #rx"^--method=" method-arg ""))])
                     (cmd-prove lhs rhs depth method))))]
            
            [(equal? cmd "benchmark")
             (let ([args-str (string-join args " ")])
               ;; Find the position of the = sign
               (define eq-pos 
                 (for/first ([c (in-string args-str)]
                             [i (in-naturals)]
                             #:when (equal? c #\=))
                   i))
               (if (not eq-pos)
                   "Error: benchmark requires LHS = RHS [DEPTH]"
                   (let* ([lhs (substring args-str 0 eq-pos)]
                          [rhs-and-depth (substring args-str (+ eq-pos 1))]
                          ;; Extract depth if present
                          [depth-match (regexp-match #rx"(.*)([0-9]+)$" (string-trim rhs-and-depth))]
                          [rhs (if depth-match
                                    (string-trim (cadr depth-match))
                                    (string-trim rhs-and-depth))]
                          [depth (if depth-match
                                     (string->number (caddr depth-match))
                                     #f)])
                     (cmd-benchmark lhs rhs depth))))]
            
            [(equal? cmd "normalize")
             (if (null? args)
                 "Error: normalize requires a term"
                 (cmd-normalize (string-join args " ")))]
            
            [(equal? cmd "clear-cache")
             (cmd-clear-cache)]
            
            [(equal? cmd "simulate")
             (if (null? args)
                 "Error: simulate requires a term"
                 (let* ([non-flag-args (filter (λ (a) (not (string-prefix? a "--"))) args)]
                        [term (string-join non-flag-args " ")]
                        [depth-arg (findf (λ (a) (regexp-match? #rx"^--depth=" a)) args)]
                        [depth (and depth-arg (regexp-replace #rx"^--depth=" depth-arg ""))]
                        [show-states? (member "--states" args)])
                   (cmd-simulate term depth show-states?)))]
            
            [(equal? cmd "onestep")
             (if (null? args)
                 "Error: onestep requires a term"
                 (cmd-onestep (string-join args " ")))]
            
            [(equal? cmd "exit")
             "exit"]
            
            [(equal? cmd "check-group")
             (check-group-axiom)]
            
            [(equal? cmd "analyze")
             "Analysis feature temporarily disabled - coming soon!"]
            
            [(equal? cmd "unify")
             "Unification feature coming soon!"]
            
            [(equal? cmd "show-stats")
             "Performance statistics feature coming soon!"]
            
            [(equal? cmd "save-cache")
             "Cache save feature coming soon!"]
            
            [(equal? cmd "load-cache")
             "Cache load feature coming soon!"]
            
            [(equal? cmd "batch-prove")
             "Batch prove feature coming soon!"]
            
            [(equal? cmd "compare-methods")
             "Method comparison feature coming soon!"]
            
            [(equal? cmd "completion-report")
             "Completion report feature coming soon!"]
            
            [(equal? cmd "export-theory")
             "Theory export feature coming soon!"]
            
            [(equal? cmd "build-proof")
             (if (< (length args) 2)
                 "Error: build-proof requires LHS RHS"
                 ;; Simple approach: take first arg as LHS, rest as RHS
                 (cmd-build-proof (first args) (string-join (rest args) " ")))]
            
            [else (format "Unknown command: ~a" cmd)])))))

;; Main REPL loop
(define (run-repl)
  (display "SLC Interactive REPL\n")
  (display "Type 'help' for available commands\n")
  
  (let loop ()
    (display "\nslc> ")
    (flush-output)
    
    (define input (read-line))
    (when (and input (not (eof-object? input)))
      (define result (process-command input))
      
      (unless (equal? result "exit")
        (displayln result)
        (loop)))))

(module+ main
  (run-repl)) 