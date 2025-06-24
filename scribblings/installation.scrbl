#lang scribble/manual
@require[scribble/core scribble/decode]

@title{Installation Guide}

@section{Install Racket}

Download and install Racket 8.0 or later from:
@url{https://download.racket-lang.org/releases/}

Follow the installation instructions for your operating system. After installation,
ensure the @code{racket} command is available in your PATH.

@section{Setup SLC}

@itemlist[#:style 'ordered
  @item{Clone this repository: @code{git clone <your-repo-url>}}
  @item{Navigate to the directory: @code{cd slc}}
  @item{Test the installation: @code{racket tests/main-tests.rkt}}
  @item{Start the interactive REPL: @code{racket slc/interactive.rkt}}
]

@section{Quick Test}

In the SLC REPL, try:
@verbatim{
slc> use-theory Monoid
slc> prove x * e = x
slc> help
}

@section{Troubleshooting}

@bold{Command not found}: Ensure Racket's bin directory is in your PATH.

@bold{Module errors}: Try @code{raco setup slc} to rebuild compiled files. 