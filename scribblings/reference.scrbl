#lang scribble/manual
@require[scribble/core scribble/decode]
@require[@for-label[racket/base racket/contract]]

@title{Implementation Reference}

This section provides detailed documentation for all modules and functions in the SLC implementation.
It serves as a reference for researchers wanting to understand the codebase or developers extending the system.

@; ============================================================================
@section{Core Interface}

@subsection{Unified Interface}
@defmodule[slc/core]

The core module provides unified access to all SLC functionality.

@defproc[(slc-prove [term1 term?] [term2 term?] [theory lawvere-theory?] [#:depth depth exact-nonnegative-integer? 5] [#:method method symbol? 'auto]) (or/c #f proof-result?)]{
  Main SLC proving function. Attempts to prove equality between terms using the specified theory.
  Returns a proof result if successful, @racket[#f] otherwise.
}

@defproc[(slc-simulate [term term?] [theory lawvere-theory?] [#:depth depth exact-nonnegative-integer? 5]) (listof (listof term?))]{
  Simulates all possible rewrite paths from the given term up to the specified depth.
}

@; ============================================================================
@section{Terms and Theories}

@subsection{Term Representation}
@defmodule[slc/terms]

@defstruct[term-var ([name symbol?])]{
  Represents a variable in a term.
  
  @racketblock[
    (term-var 'x)  ; Variable x
    (term-var 'y)  ; Variable y
  ]
}

@defstruct[term-const ([value (or/c symbol? number? string?)])]{
  Represents a constant (identity elements, literals, etc.).
  
  @racketblock[
    (term-const 'e)    ; Identity element
    (term-const 0)     ; Numeric constant
    (term-const "foo") ; String constant
  ]
}

@defstruct[term-op ([op-sym symbol?] [args (listof term?)])]{
  Represents an operation applied to arguments.
  
  @racketblock[
    (term-op '* (list (term-var 'x) (term-var 'y)))  ; x * y
    (term-op '+ (list (term-const 0) (term-var 'z))) ; 0 + z
  ]
}

@defproc[(term? [v any/c]) boolean?]{
  Predicate for recognizing term structures.
}

@defproc[(term-equal? [t1 term?] [t2 term?]) boolean?]{
  Tests syntactic equality between terms (ignoring variable renaming).
}

@defproc[(free-vars [term term?]) (listof symbol?)]{
  Returns the list of free variables appearing in a term.
}

@defproc[(substitute [term term?] [substitutions (hash/c symbol? term?)]) term?]{
  Performs variable substitution in a term according to the given substitution map.
}

@defproc[(rename-vars [term term?] [renaming (hash/c symbol? symbol?)]) term?]{
  Renames variables in a term according to the given renaming map.
}

@defproc[(term-size [term term?]) exact-nonnegative-integer?]{
  Returns the size of a term (number of nodes in its tree representation).
}

@subsection{Lawvere Theories}
@defmodule[slc/theories]

@defstruct[lawvere-theory ([name symbol?] [axioms (listof (cons/c term? term?))])]{
  Represents a Lawvere theory with a name and a list of axioms (equations).
  Each axiom is a pair @racket[(cons lhs rhs)] representing the equation @racket[lhs = rhs].
}

@defproc[(make-theory [name symbol?] [axiom-list (listof (cons/c term? term?))]) lawvere-theory?]{
  Constructs a new Lawvere theory with the given name and axioms.
}

@defproc[(theory-axioms [theory lawvere-theory?]) (listof (cons/c term? term?))]{
  Returns the list of axioms for a theory.
}

@defproc[(theory-name [theory lawvere-theory?]) symbol?]{
  Returns the name of a theory.
}

@; ============================================================================
@section{Categories}

@subsection{Logic Category L_T}
@defmodule[slc/lt]

The logic category captures equational proofs as morphisms.

@defproc[(lt-prove-equality [term1 term?] [term2 term?] [theory lawvere-theory?] [max-depth exact-nonnegative-integer?]) (or/c #f proof-object?)]{
  Attempts to prove that two terms are equal in the logic category L_T.
  Returns a proof object if successful, @racket[#f] otherwise.
}

@defproc[(lt-build-proof [term1 term?] [term2 term?] [theory lawvere-theory?]) (or/c #f (listof proof-step?))]{
  Builds a step-by-step proof showing how to derive the equality.
}

@defstruct[proof-object ([steps (listof proof-step?)] [valid? boolean?])]{
  Represents a formal proof in L_T with a sequence of proof steps.
}

@defstruct[proof-step ([rule symbol?] [premises (listof term?)] [conclusion term?])]{
  Represents a single step in a proof, including the rule applied and the terms involved.
}

@subsection{Computation Category C_T}
@defmodule[slc/ct]

The computation category captures rewrite paths as morphisms.

@defproc[(ct-find-path [start-term term?] [end-term term?] [theory lawvere-theory?] [max-depth exact-nonnegative-integer?] [#:method method symbol? 'auto]) (or/c #f (listof term?))]{
  Finds a rewrite path from start-term to end-term in the computation category C_T.
  Methods include @racket['bfs], @racket['ids], @racket['astar], @racket['confluent], and @racket['auto].
}

@defproc[(one-step-rewrites [term term?] [theory lawvere-theory?]) (listof term?)]{
  Returns all terms reachable from the given term in exactly one rewrite step.
}

@defproc[(normalize-term [term term?] [theory lawvere-theory?]) term?]{
  Attempts to normalize a term to its canonical form (if the theory is confluent).
}

@defproc[(ct-reachable-terms [term term?] [theory lawvere-theory?] [max-depth exact-nonnegative-integer?]) (set/c term?)]{
  Returns the set of all terms reachable from the given term within max-depth steps.
}

@defproc[(smart-search [start-term term?] [end-term term?] [theory lawvere-theory?] [max-depth exact-nonnegative-integer?] [#:method method symbol?]) search-result?]{
  Advanced search with multiple strategies and performance optimizations.
}

@; ============================================================================
@section{SLC Integration}

@subsection{Core SLC Implementation}
@defmodule[slc/slc-simulator]

This module embodies the SLC theorem by integrating logic and computation categories.

@defproc[(prover+simulator [term1 term?] [term2 term?] [theory lawvere-theory?] [max-depth exact-nonnegative-integer?] [#:method method symbol? 'auto]) slc-result?]{
  Core SLC function: proves equality in L_T by finding rewrite paths in C_T.
  This function embodies the central insight of the SLC theorem.
}

@defproc[(simulate-rewrites [term term?] [theory lawvere-theory?] [max-depth exact-nonnegative-integer?]) simulation-result?]{
  Simulates all possible rewrite sequences from a given term, showing the computation space.
}

@defproc[(reachable-terms-by-depth [term term?] [theory lawvere-theory?] [max-depth exact-nonnegative-integer?]) (listof (set/c term?))]{
  Returns a list where each element is the set of terms reachable at that depth level.
}

@defstruct[slc-result ([proved? boolean?] [method symbol?] [path (or/c #f (listof term?))] [proof (or/c #f proof-object?)] [time-ms exact-nonnegative-integer?])]{
  Result structure for SLC proving attempts, containing both logical and computational evidence.
}

@defstruct[simulation-result ([paths (listof (listof term?))] [states-by-depth (listof (set/c term?))] [total-states exact-nonnegative-integer?])]{
  Result structure for simulation runs, showing the exploration of the computation space.
}

@; ============================================================================
@section{Theory Analysis}

@defmodule[slc/theory-analysis]

Tools for analyzing properties and structure of Lawvere theories.

@defproc[(analyze-theory [theory lawvere-theory?]) theory-analysis?]{
  Performs comprehensive analysis of a theory's properties.
}

@defproc[(check-confluence [theory lawvere-theory?] [#:sample-size size exact-nonnegative-integer? 100]) confluence-result?]{
  Checks if a theory appears to be confluent by testing sample terms.
}

@defproc[(estimate-complexity [theory lawvere-theory?]) complexity-estimate?]{
  Estimates the computational complexity characteristics of the theory.
}

@defproc[(suggest-optimizations [theory lawvere-theory?]) (listof optimization-suggestion?)]{
  Suggests optimizations based on theory structure analysis.
}

@defstruct[theory-analysis ([properties (listof symbol?)] [confluence confluence-result?] [complexity complexity-estimate?] [optimizations (listof optimization-suggestion?)])]{
  Comprehensive analysis result for a theory.
}

@; ============================================================================
@section{Parsing and Formatting}

@defmodule[slc/parser]

@defproc[(parse-term [input string?]) (or/c term? parse-error?)]{
  Parses a string representation into a term structure.
  Supports both infix and prefix notation.
  
  @racketblock[
    (parse-term "x * y")           ; Infix
    (parse-term "(* x y)")         ; Prefix
    (parse-term "x * (y + z)")     ; Nested with precedence
  ]
}

@defproc[(parse-equation [input string?]) (or/c (cons/c term? term?) parse-error?)]{
  Parses a string equation into a pair of terms.
  
  @racketblock[
    (parse-equation "x * e = x")
    (parse-equation "(x * y) * z = x * (y * z)")
  ]
}

@defproc[(format-term [term term?] [#:style style symbol? 'infix]) string?]{
  Formats a term as a string. Styles include @racket['infix], @racket['prefix], and @racket['pretty].
}

@defproc[(format-equation [lhs term?] [rhs term?] [#:style style symbol? 'infix]) string?]{
  Formats an equation as a string.
}

@defstruct[parse-error ([message string?] [position exact-nonnegative-integer?])]{
  Error structure for parsing failures.
}

@; ============================================================================
@section{Proof Objects}

@defmodule[slc/proofs]

Structured representation and manipulation of proofs.

@defproc[(make-proof [steps (listof proof-step?)]) proof-object?]{
  Constructs a proof object from a sequence of steps.
}

@defproc[(proof-valid? [proof proof-object?] [theory lawvere-theory?]) boolean?]{
  Checks if a proof is valid according to the given theory.
}

@defproc[(proof-length [proof proof-object?]) exact-nonnegative-integer?]{
  Returns the number of steps in a proof.
}

@defproc[(compose-proofs [proof1 proof-object?] [proof2 proof-object?]) proof-object?]{
  Composes two proofs into a single proof (categorical composition).
}

@defproc[(format-proof [proof proof-object?] [#:style style symbol? 'detailed]) string?]{
  Formats a proof for display. Styles include @racket['detailed], @racket['compact], and @racket['latex].
}

@; ============================================================================
@section{Performance and Benchmarking}

@defmodule[slc/performance-demo]

Tools for performance analysis and benchmarking.

@defproc[(benchmark-methods [term1 term?] [term2 term?] [theory lawvere-theory?] [max-depth exact-nonnegative-integer?]) benchmark-result?]{
  Compares the performance of different search methods on the same problem.
}

@defproc[(performance-profile [term term?] [theory lawvere-theory?] [max-depth exact-nonnegative-integer?]) profile-result?]{
  Profiles the performance characteristics of operations on a specific term.
}

@defproc[(clear-all-caches!) void?]{
  Clears all internal performance caches to ensure clean benchmarking.
}

@defproc[(get-cache-statistics) cache-stats?]{
  Returns statistics about cache usage and hit rates.
}

@defstruct[benchmark-result ([methods (listof method-result?)] [best-method symbol?] [total-time-ms exact-nonnegative-integer?])]{
  Result of comparing multiple methods on the same problem.
}

@defstruct[method-result ([method symbol?] [success? boolean?] [time-ms exact-nonnegative-integer?] [path-length exact-nonnegative-integer?])]{
  Result for a single method in a benchmark comparison.
}

@; ============================================================================
@section{Error Handling}

@defmodule[slc/errors]

Structured error handling for the SLC system.

@defstruct[slc-error ([type symbol?] [message string?] [context any/c])]{
  Base error structure for SLC-specific errors.
}

@defstruct[(parse-error slc-error) ([position exact-nonnegative-integer?])]{
  Error in parsing terms or equations.
}

@defstruct[(theory-error slc-error) ([theory lawvere-theory?])]{
  Error related to theory operations.
}

@defstruct[(proof-error slc-error) ([step-number exact-nonnegative-integer?])]{
  Error in proof construction or validation.
}

@defproc[(slc-error? [v any/c]) boolean?]{
  Predicate for recognizing SLC errors.
}

@defproc[(format-error [error slc-error?]) string?]{
  Formats an error for display to users.
}

@; ============================================================================
@section{Example Theories}

@subsection{Monoids}
@defmodule[slc/examples/monoid]

@defthing[T_Monoid lawvere-theory?]{
  Complete monoid theory with associativity and identity laws.
}

@defproc[(mvar [name symbol?]) term-var?]{
  Convenience constructor for monoid variables.
}

@defthing[me term-const?]{
  The monoid identity element @racket[e].
}

@defproc[(m* [t1 term?] [t2 term?]) term-op?]{
  Monoid multiplication operation.
}

@subsection{Groups}
@defmodule[slc/examples/group]

@defthing[T_Group lawvere-theory?]{
  Complete group theory extending monoids with inverse operations.
}

@defproc[(ginv [t term?]) term-op?]{
  Group inverse operation.
}

@defthing[ge term-const?]{
  Group identity element.
}

@defproc[(g* [t1 term?] [t2 term?]) term-op?]{
  Group multiplication operation.
}

@subsection{Rings}
@defmodule[slc/examples/ring]

@defthing[T_Ring lawvere-theory?]{
  Complete ring theory with addition, multiplication, and distributivity laws.
}

@defproc[(r+ [t1 term?] [t2 term?]) term-op?]{
  Ring addition operation.
}

@defproc[(r* [t1 term?] [t2 term?]) term-op?]{
  Ring multiplication operation.
}

@defthing[rzero term-const?]{
  Additive identity (zero) in the ring.
}

@defthing[rone term-const?]{
  Multiplicative identity (one) in the ring.
}

@defproc[(rneg [t term?]) term-op?]{
  Additive inverse (negation) operation.
}

@; ============================================================================
@section{Interactive Interface}

@defmodule[slc/interactive]

The interactive REPL provides a command-line interface for exploring SLC implementations.

@defproc[(start-slc-repl) void?]{
  Starts the SLC interactive REPL with full command support.
}

@defproc[(process-command [input string?] [state repl-state?]) repl-state?]{
  Processes a single command and returns the updated REPL state.
}

@defstruct[repl-state ([current-theory (or/c #f lawvere-theory?)] [cache-enabled? boolean?] [default-depth exact-nonnegative-integer?])]{
  Maintains the state of the interactive REPL session.
}

The REPL supports the following command categories:

@itemlist[
  @item{@bold{Theory Management}: @racket[use-theory], @racket[define-theory], @racket[analyze]}
  @item{@bold{Proof Commands}: @racket[prove], @racket[benchmark], @racket[normalize]}
  @item{@bold{Simulation}: @racket[simulate], @racket[onestep], @racket[clear-cache]}
  @item{@bold{Utilities}: @racket[help], @racket[exit]}
] 