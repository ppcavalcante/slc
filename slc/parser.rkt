#lang racket/base

(require "terms.rkt"
         "errors.rkt"
         racket/match
         racket/string
         racket/list)

(provide parse-term
         parse-equation
         parse-axiom-list
         format-term-pretty)

;; Main parser function that handles both infix and prefix notation
;; parse-term : String -> Term
(define (parse-term input-str)
  (with-handlers 
    ([exn:fail:read? (λ (e) 
                       (raise (make-parse-error input-str 
                                               (format "Invalid S-expression: ~a" (exn-message e)))))]
     [exn:fail? (λ (e) 
                  (raise (make-parse-error input-str 
                                          (format "Parse error: ~a" (exn-message e)))))])
    
    (define str-trimmed (string-trim input-str))
    
    ;; Simple strategy: always try infix conversion first, fall back to direct parsing
    (with-handlers
      ([exn:fail? (λ (e)
                    ;; If infix conversion fails, try direct S-expression parsing
                    (define sexp (read (open-input-string str-trimmed)))
                    (sexp->term sexp))])
      
      ;; Try infix conversion
      (define cleaned-input (preprocess-infix str-trimmed))
      (define sexp (read (open-input-string cleaned-input)))
      (sexp->term sexp))))

;; Check if a string contains infix operators that need conversion
(define (contains-infix-operators? str)
  (or (string-contains? str " * ")
      (string-contains? str " + ")
      (string-contains? str " - ")))

;; Preprocess infix notation like "x * y" -> "(* x y)"
;; preprocess-infix : String -> String
(define (preprocess-infix str)
  (define str-cleaned (string-trim str))
  
  ;; Handle different cases
  (cond
    ;; If it looks like it's already in prefix form (starts with '(' and has operator as first symbol)
    [(and (string-prefix? str-cleaned "(")
          (string-suffix? str-cleaned ")")
          (let ([tokens (string-split (substring str-cleaned 1 (- (string-length str-cleaned) 1)))])
            (and (not (null? tokens))
                 (member (first tokens) '("*" "+" "-" "inv")))))
     str-cleaned]
    
    ;; Remove outer parentheses if they just group an infix expression
    [(and (string-prefix? str-cleaned "(") 
          (string-suffix? str-cleaned ")")
          (balanced-parens? (substring str-cleaned 1 (- (string-length str-cleaned) 1)))
          (let ([inner (substring str-cleaned 1 (- (string-length str-cleaned) 1))])
            (or (string-contains? inner " * ") (string-contains? inner " + "))))
           (preprocess-infix (substring str-cleaned 1 (- (string-length str-cleaned) 1)))]
    
    ;; Convert infix multiplication to prefix
    [(string-contains? str-cleaned " * ")
     (convert-infix-to-prefix str-cleaned "*")]
    
    ;; Convert infix addition to prefix
    [(string-contains? str-cleaned " + ")
     (convert-infix-to-prefix str-cleaned "+")]
    
    ;; Already processed or simple term
    [else str-cleaned]))

;; Check if parentheses are balanced in a string
(define (balanced-parens? str)
  (define (count-parens s depth)
    (cond
      [(string=? s "") (= depth 0)]
      [(string-prefix? s "(") (count-parens (substring s 1) (+ depth 1))]
      [(string-prefix? s ")") 
       (if (= depth 0) #f (count-parens (substring s 1) (- depth 1)))]
      [else (count-parens (substring s 1) depth)]))
  (count-parens str 0))

;; Tokenize respecting function calls and parentheses
(define (tokenize-expression str)
  (define tokens '())
  (define current-token "")
  (define paren-depth 0)
  
  (define (add-token! token)
    (when (not (string=? token ""))
      (set! tokens (append tokens (list (string-trim token))))
      (set! current-token "")))
  
  (for ([char (in-string str)])
    (case char
      [(#\()
       (set! current-token (string-append current-token (string char)))
       (set! paren-depth (+ paren-depth 1))]
      [(#\))
       (set! current-token (string-append current-token (string char)))
       (set! paren-depth (- paren-depth 1))]
      [(#\space)
       (if (= paren-depth 0)
           (add-token! current-token)
           (set! current-token (string-append current-token (string char))))]
      [else
       (set! current-token (string-append current-token (string char)))]))
  
  (add-token! current-token)
  tokens)

;; Convert infix expressions to prefix, handling nested parentheses
(define (convert-infix-to-prefix str op-str)
  (define op-symbol (string->symbol op-str))
  (define tokens (tokenize-expression str))
  
  ;; Find the main operator token (not inside function calls)
  (define main-op-pos
    (for/first ([token tokens]
                [i (in-naturals)]
                #:when (string=? token op-str))
      i))
  
  (if main-op-pos
      (let ([lhs-tokens (take tokens main-op-pos)]
            [rhs-tokens (drop tokens (+ main-op-pos 1))])
        (format "(~a ~a ~a)" 
                op-symbol 
                (preprocess-infix (string-join lhs-tokens " ")) 
                (preprocess-infix (string-join rhs-tokens " "))))
      str))

;; Convert S-expression to internal term representation
;; sexp->term : Any -> Term
(define (sexp->term sexp)
  (match sexp
    ;; Variables (symbols that aren't constants)
    [(? symbol? s) 
     (case s
       [(e) (term-const 'e)]      ; Monoid identity
       [(0) (term-const '0)]      ; Ring additive identity
       [(1) (term-const '1)]      ; Ring multiplicative identity
       [else (term-var s)])]
    
    ;; Numbers -> constants
    [(? number? n)
     (cond
       [(= n 0) (term-const '0)]
       [(= n 1) (term-const '1)]
       [else (term-const n)])]
    
    ;; Binary operations
    [(list '* arg1 arg2)
     (term-op '* (list (sexp->term arg1) (sexp->term arg2)))]
    
    [(list '+ arg1 arg2)
     (term-op '+ (list (sexp->term arg1) (sexp->term arg2)))]
    
    ;; Unary operations
    [(list 'inv arg)
     (term-op 'inv (list (sexp->term arg)))]
    
    [(list '- arg)
     (term-op '- (list (sexp->term arg)))]
    
    ;; General n-ary operations
    [(list (? symbol? op) args ...)
     (cond
       [(null? args) (term-const op)]  ; Nullary operation as constant
       [else (term-op op (map sexp->term args))])]
    
    ;; Strings (parse recursively)
    [(? string? s)
     (sexp->term (read (open-input-string s)))]
    
    [else
     (raise (make-parse-error (format "~a" sexp) 
                             (format "Unsupported term syntax: ~a" sexp)))]))

;; Parse equation from string like "lhs = rhs"
;; parse-equation : String -> (Cons Term Term)
(define (parse-equation equation-str)
  (define parts (string-split equation-str "="))
  (when (not (= (length parts) 2))
    (raise (make-parse-error equation-str "Equation must have exactly one '=' sign")))
  
  (define lhs-str (string-trim (first parts)))
  (define rhs-str (string-trim (second parts)))
  
  (cons (parse-term lhs-str) (parse-term rhs-str)))

;; Parse list of axioms from strings
;; parse-axiom-list : (Listof String) -> (Listof (Cons Term Term))
(define (parse-axiom-list axiom-strings)
  (map parse-equation axiom-strings))

;; Pretty-print terms with infix notation where appropriate
;; format-term-pretty : Term -> String
(define (format-term-pretty term)
  (match term
    [(term-var name) (symbol->string name)]
    [(term-const name) 
     (cond
       [(symbol? name) (symbol->string name)]
       [(number? name) (number->string name)]
       [else (format "~a" name)])]
    
    ;; Special formatting for binary operations
    [(term-op '* (list t1 t2))
     (format "(~a * ~a)" (format-term-pretty t1) (format-term-pretty t2))]
    
    [(term-op '+ (list t1 t2))
     (format "(~a + ~a)" (format-term-pretty t1) (format-term-pretty t2))]
    
    ;; Unary operations
    [(term-op 'inv (list t))
     (format "inv(~a)" (format-term-pretty t))]
    
    [(term-op '- (list t))
     (format "-(~a)" (format-term-pretty t))]
    
    ;; General operations
    [(term-op op args)
     (format "~a(~a)" op (string-join (map format-term-pretty args) ", "))]
    
    [else (format "~a" term)]))

(module+ test
  (require rackunit)
  
  ;; Test basic parsing
  (check-equal? (parse-term "x") (term-var 'x))
  (check-equal? (parse-term "e") (term-const 'e))
  (check-equal? (parse-term "0") (term-const '0))
  
  ;; Test infix parsing
  (check-equal? (parse-term "x * y") 
                (term-op '* (list (term-var 'x) (term-var 'y))))
  
  (check-equal? (parse-term "(x * y)") 
                (term-op '* (list (term-var 'x) (term-var 'y))))
  
  ;; Test prefix parsing
  (check-equal? (parse-term "(* x y)") 
                (term-op '* (list (term-var 'x) (term-var 'y))))
  
  ;; Test equation parsing
  (define eq (parse-equation "x * e = x"))
  (check-equal? (car eq) (term-op '* (list (term-var 'x) (term-const 'e))))
  (check-equal? (cdr eq) (term-var 'x))
  
  ;; Test pretty formatting
  (check-equal? (format-term-pretty (term-op '* (list (term-var 'x) (term-var 'y))))
                "(x * y)")
  
  (displayln "Parser tests passed.")) 