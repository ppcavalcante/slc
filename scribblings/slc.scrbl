#lang scribble/manual
@require[scribble/core scribble/decode scribble/bnf]
@require[@for-label[racket/base racket/contract]]

@title{SLC: Structure-Logic-Computation Theorem Implementation}
@author{Pedro Cavalcante}

@defmodule[slc]

This document describes the Racket implementation of the Structure-Logic-Computation (SLC) theorem,
which extends the classical Curry-Howard-Lambek correspondence. The SLC theorem shows that 
any Lawvere theory @italic{T} canonically generates both a logic category @italic{L@subscript{T}} and 
a computation category @italic{C@subscript{T}}, with deep structural relationships between them.

@table-of-contents[]

@; ============================================================================
@section{Introduction}

The Curry-Howard correspondence links proofs with programs and propositions with types.
Lambek embedded this insight in category theory, yielding the Curry-Howard-Lambek (CHL) correspondence.
The @bold{Structure-Logic-Computation (SLC) theorem} extends this foundation:

@nested[#:style 'inset]{
@bold{SLC Theorem}: Every logic and every computation arise from—and are constrained by—an 
underlying algebraic structure. Any Lawvere theory @italic{T} canonically generates:
@itemlist[
  @item{An equational-logic category @italic{L@subscript{T}} capturing proofs}
  @item{A rewrite category @italic{C@subscript{T}} capturing computations}
]
These categories are sound and complete with respect to the models of @italic{T}.
}

@subsection{Why CHL Alone Is Not Enough}

The CHL correspondence presupposes a @italic{fixed} calculus (typically the simply-typed λ-calculus)
and thus hard-wires proof theory to one particular notion of program. The SLC theorem drops one 
level down: given @italic{only} the raw algebra of the domain, it @italic{derives} both the logic 
of proofs and the machine of executions.

@subsection{The SLC Triangle}

@verbatim{
        L_T (Logic/Proofs)
       ╱    ╲
      P      M  
     ╱        ╲
    C_T ────────→ Models (Set)
 (Computation/Rewrites)
}

Where:
@itemlist[
  @item{@italic{T} = Lawvere theory (pure algebra: operations + equations)}
  @item{@italic{L@subscript{T}} = Logic category (equational proofs)}
  @item{@italic{C@subscript{T}} = Computation category (rewrite paths)}
  @item{@italic{P, M} = Canonical functors connecting computation, logic, and semantics}
]

@; ============================================================================
@section{Quick Start}

@subsection{Installation}
@margin-note{See @secref["installation"] for detailed installation instructions.}

@verbatim{
# Prerequisites: Install Racket 8.0+ from https://download.racket-lang.org/releases/

# Clone the repository
git clone https://github.com/ppcavalcante/slc.git
cd slc

# Test the installation
racket tests/main-tests.rkt

# Start the interactive REPL
racket slc/interactive.rkt
}

@subsection{Interactive Example}

@verbatim{
slc> use-theory Monoid
Using Monoid theory

slc> prove x * e = x
✓ Proved: (x * e) = x (method: auto)
Proof path:
  Step 0: (x * e)
  Step 1: x

slc> simulate (e * x) * e --depth=3
Simulating rewrites from ((e * x) * e) (depth 3):

Path 0:
  Step 0: ((e * x) * e)
  Step 1: (x * e)
  Step 2: x

slc> benchmark x * (y * z) = (x * y) * z
Benchmarking: (x * (y * z)) = ((x * y) * z) (depth 5)

  bfs: ✓ Found path (12 ms)
  ids: ✓ Found path (8 ms)  
  confluent: ✓ Found path (4 ms)
  auto: ✓ Found path (4 ms)
}

@; ============================================================================
@section{Theoretical Background}

@subsection{Lawvere Theories}

A @deftech{Lawvere theory} @italic{T} consists of:
@itemlist[
  @item{A set of @italic{operations} with specified arities}
  @item{A set of @italic{equations} (axioms) relating terms built from these operations}
]

For example, the theory of monoids has:
@itemlist[
  @item{Operation: @racket[*] (binary), @racket[e] (nullary)}  
  @item{Equations: @racket[(x * y) * z = x * (y * z)], @racket[x * e = x], @racket[e * x = x]}
]

@subsection{The Logic Category L_T}

Given a Lawvere theory @italic{T}, the logic category @italic{L@subscript{T}} has:
@itemlist[
  @item{@bold{Objects}: Natural numbers (representing variable contexts)}
  @item{@bold{Morphisms}: Equivalence classes of term equations provable from @italic{T}}
  @item{@bold{Composition}: Categorical composition of proofs}
]

@subsection{The Computation Category C_T}

The computation category @italic{C@subscript{T}} has:
@itemlist[
  @item{@bold{Objects}: Terms built from operations in @italic{T}}
  @item{@bold{Morphisms}: Sequences of rewrite steps using equations from @italic{T}}
  @item{@bold{Composition}: Path concatenation in the rewrite graph}
]

@; ============================================================================
@section{Architecture Overview}

The SLC implementation is organized into several core modules:

@subsection{Core Theory Modules}
@itemlist[
  @item{@racketmodname[slc/theories] - Lawvere theory definitions}
  @item{@racketmodname[slc/terms] - Term representation and operations}
  @item{@racketmodname[slc/lt] - Logic category @italic{L@subscript{T}} implementation}
  @item{@racketmodname[slc/ct] - Computation category @italic{C@subscript{T}} with advanced search}
]

@subsection{SLC Core}
@itemlist[
  @item{@racketmodname[slc/slc-simulator] - Core SLC prover+simulator embodying the theorem}
  @item{@racketmodname[slc/core] - Unified module exports (main API)}
]

@subsection{Implementation Support}
@itemlist[
  @item{@racketmodname[slc/parser] - Term parsing and formatting}
  @item{@racketmodname[slc/proofs] - Proof object manipulation}
  @item{@racketmodname[slc/theory-analysis] - Theory analysis tools}
  @item{@racketmodname[slc/performance-demo] - Benchmarking tools}
  @item{@racketmodname[slc/errors] - Error handling}
]

@subsection{User Interface}
@itemlist[
  @item{@racketmodname[slc/interactive] - Interactive REPL (main entry point)}
]

@subsection{Example Theories}
@itemlist[
  @item{@racketmodname[slc/examples/monoid] - Monoid theory with associativity + identity}
  @item{@racketmodname[slc/examples/group] - Group theory extending monoids with inverses}
  @item{@racketmodname[slc/examples/ring] - Ring theory with addition and multiplication}
]

@; ============================================================================
@section{The SLC Implementation}

@subsection{Core SLC Function}

The heart of the SLC implementation is the @racket[prover+simulator] function,
which embodies the SLC theorem by proving equality in @italic{L@subscript{T}} via 
rewrite paths in @italic{C@subscript{T}}.

@subsection{Search Algorithms}

The implementation includes multiple search strategies for exploring @italic{C@subscript{T}}:

@itemlist[
  @item{@bold{BFS}: Systematic exploration for shortest paths}
  @item{@bold{Iterative Deepening}: Memory-efficient deep searches}
  @item{@bold{A* Search}: Goal-directed search with heuristics}
  @item{@bold{Confluent Normalization}: Fast equality via normal forms}
  @item{@bold{Auto Method Selection}: Automatic algorithm choice based on problem characteristics}
]

@; ============================================================================
@section{Interactive Commands}

The SLC REPL provides a comprehensive command set for exploring theories:

@subsection{Theory Management}
@itemlist[
  @item{@racket[use-theory] @nonterm{NAME} - Load predefined theory (Monoid, Group, Ring)}
  @item{@racket[define-theory] @nonterm{NAME} @nonterm{AXIOM1} ... - Define custom theory}
  @item{@racket[analyze] - Analyze current theory properties}
]

@subsection{Proof and Computation}
@itemlist[
  @item{@racket[prove] @nonterm{LHS} = @nonterm{RHS} [@nonterm{depth}] [@nonterm{method}] - Prove equality using SLC}
  @item{@racket[benchmark] @nonterm{LHS} = @nonterm{RHS} [@nonterm{depth}] - Compare search methods}
  @item{@racket[normalize] @nonterm{TERM} - Normalize term using current theory}
]

@subsection{Simulation}
@itemlist[
  @item{@racket[simulate] @nonterm{TERM} [--depth=@nonterm{N}] [--states] - Explore rewrite paths from term}
  @item{@racket[onestep] @nonterm{TERM} - Show one-step rewrites}
  @item{@racket[clear-cache] - Clear performance caches}
]

@; ============================================================================
@section{Examples and Applications}

@subsection{Monoid Properties}

@verbatim{
slc> use-theory Monoid
slc> prove (x * y) * z = x * (y * z)
✓ Proved: ((x * y) * z) = (x * (y * z)) (method: auto)

slc> prove e * x = x * e  
✓ Proved: (e * x) = (x * e) (method: auto)
}

@subsection{Group Inverse Laws}

@verbatim{
slc> use-theory Group
slc> prove x * ginv(x) = e
✓ Proved: (x * (ginv x)) = e (method: confluent)

slc> simulate x * ginv(x) --depth=2
Simulating rewrites from (x * (ginv x)) (depth 2):

Path 0:
  Step 0: (x * (ginv x))
  Step 1: e
}

@subsection{Ring Distributivity}

@verbatim{
slc> use-theory Ring
slc> prove x * (y + z) = (x * y) + (x * z)
✓ Proved: (x * (y + z)) = ((x * y) + (x * z)) (method: auto)
}

@; ============================================================================
@section{Applications}

The SLC implementation enables several practical applications:

@subsection{Security Analysis}
Model privilege escalation as violations of monotonicity laws in security lattices.

@subsection{Language Design}
Generate both type systems (@italic{L@subscript{T}}) and interpreters (@italic{C@subscript{T}}) 
from algebraic effect signatures.

@subsection{Formal Verification}
Prove program equivalence in @italic{L@subscript{T}}, guaranteeing trace equivalence in @italic{C@subscript{T}}.

@subsection{Automated Reasoning}
Use the prover+simulator combination for both proof search and counterexample generation.

@; ============================================================================
@section{Performance and Optimization}

The implementation includes several performance optimizations:

@itemlist[
  @item{@bold{Multi-level Caching}: Rewrite cache + normal form cache}
  @item{@bold{Smart Method Selection}: Automatic optimal algorithm choice}
  @item{@bold{Confluent Fast Path}: O(n) equality checking for confluent theories}
  @item{@bold{Heuristic Search}: Domain-aware A* heuristics}
]

@; ============================================================================
@section{Future Enhancements}

Two major enhancements are planned that would significantly extend the system's capabilities:

@subsection{Complete L_T ↔ C_T Bridge}
Currently: ✅ @bold{L_T → C_T} (proofs convert to rewrite paths)
Planned: 🚧 @bold{C_T → L_T} (rewrite paths back to formal proofs)

This would demonstrate the true bidirectional equivalence of the SLC theorem by automatically 
reconstructing formal proof objects from computational rewrite paths.

@subsection{Knuth-Bendix Completion}
Planned: 🚧 @bold{Automatic theory completion} for confluence

Transform non-confluent theories into confluent ones, enabling:
@itemlist[
  @item{O(n) equality checking via normalization}
  @item{Unique canonical forms for all terms}
  @item{Massive performance improvements for large theories}
]

@; ============================================================================
@include-section["installation.scrbl"]
@include-section["reference.scrbl"]

@; ============================================================================
@section{Research Context}

This implementation accompanies the paper "Extending Curry-Howard-Lambek: The Structure-Logic-Computation (SLC) Theorem" which provides the complete mathematical development of the SLC theorem, rigorous proofs, and detailed examples.

@subsection{Related Work}
The SLC theorem builds on:
@itemlist[
  @item{Curry-Howard correspondence (Howard, 1980)}
  @item{Lambek's categorical logic (Lambek, 1969)}
  @item{Lawvere theories (Lawvere, 1963)}
  @item{Term rewriting systems (Baader & Nipkow, 1998)}
]

@subsection{Citation}
If you use this implementation in your research, please cite both the software and the accompanying paper. 