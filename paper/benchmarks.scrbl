#lang scribble/base

@require["common.rkt"]

@title[#:tag "sec:bm"]{The Benchmark Programs}

For our evaluation of Typed Racket, we chose a suite of twelve programs.
They are representative of actual user code yet small enough so that an
exhaustive exploration of the performance lattice remains tractable.

@section{Overview}

The table in @figure-ref{fig:bm} lists and summarizes our twelve benchmark
programs.  For each, we give an approximate measure of the program's size,
a diagram of its module structure, and a worst-case measure of the contracts
created and checked at runtime.

Size is measured by the number of modules and lines of code (LOC) in a program.
Crucially, the number of modules also determines the number of gradually-typed
configurations to be run when testing the benchmark, as a program with @math{n} modules
can be gradually-typed in @exact{$2^n$} possible configurations.
Lines of code is less important for evaluating macro-level gradual typing,
but gives a sense of the overall complexity of each benchmark.
Moreover, the Type Annotations LOC numbers are an upper bound on the annotations required
at any stage of gradual typing because each typed module in our experiment
fully annotates its import statements.

The column labeled ``Other LOC'' measures the additional infrastructure required
to run each project for all typed-untyped configurations. This count includes
project-wide type definitions, typed interfaces to untyped libraries, and
any so-called type adaptor modules (see below).

The module structure graphs show a dot for each module in the program.
An arrow is drawn from module A to module B when module A imports definitions
from module B.
When one of these modules is typed and the other untyped, the imported definitions
are wrapped with a contract to ensure type soundness. To give a sense of how
``expensive'' the contracts at each boundary are, we color
arrows to match the absolute number of times contracts at a given boundary
are checked. These numbers are independent from the actual configurations.

The colors fail to show the cost of checking data structures
imported from another library or factored through an adaptor module.
For example, the @tt{kcfa} graph has many thin black edges because the modules
only share data definitions. The column labeled ``Adaptors & Libraries''
reports the proportion of observed contract checks due to adaptor modules and
libraries.


@;;; to address this, we TODO added a counter to each contract and ran the prog.


@;; FIXME: remove figure caption rule
@figure*["fig:bm" "The software characteristics of the benchmarks"
@exact|{
\newcommand{\yespycket}{$\CIRCLE$}
\newcommand{\maybepycket}{$\RIGHTcircle$}
\newcommand{\nopycket}{$\Circle$}
\newcommand{\twoline}[2]{\parbox[s]{1.44cm}{\flushright\hfill #1\newline#2}}
\begin{tabular}[t]{lrrrrll}
\toprule
Project name          & \# Modules & \twoline{Untyped}{LOC} & \twoline{Type Ann.}{LOC} & \twoline{Other}{LOC} & Module structure  & \% Adaptors \& Libraries      \\
\midrule
\tt{sieve}            & 2          & 35          & 17            & 0         & \pict{sieve}      & 0  \\
\tt{morse-code}       & 4          & 216         & 29            & 0         & \pict{morsecode}  & 0  \\
\tt{mbta}             & 4          & 369         & 77            & 89        & \pict{mbta}       & 79 \\
\tt{zo-traversal}     & 5          & 1404        & 285           & 214       & \pict{zordoz}     & 99 \\
\tt{suffixtree}       & 6          & 545         & 125           & 40        & \pict{suffixtree} & 97 \\
\tt{lnm}              & 6          & 501         & 120           & 62        & \pict{lnm}        & 46 \\
\tt{kcfa}             & 7          & 248         & 47            & 141       & \pict{kcfa}       & 99 \\
\tt{snake}            & 8          & 161         & 50            & 27        & \pict{snake}      & 93 \\
\tt{tetris}           & 9          & 305         & 71            & 38        & \pict{tetris}     & 99 \\
\tt{synth}            & 10         & 837         & 142           & 33        & \pict{synth}      & 47 \\
\tt{gregor}           & 13         & 996         & 164           & 103       & \pict{gregor}     & 78 \\
\tt{quad}             & 16         & 6722        & 300           & 241       & \pict{quad}       & 45 \\
\bottomrule
\end{tabular}

\vspace{0.1cm}

\newcommand{\blackmbf}[1]{\color{black}{\mathbf{#1}}}
\begin{tikzpicture}
  \draw [green!48!white, line width=6] (0,0) -- node[below] {$\blackmbf{<10}$} (2.5,0);
  \draw [yellow!45!orange, line width=6] (2.5,0) -- node[below] {$\blackmbf{<1,000}$} (5,0);
  \draw [blue!43!white, line width=6] (5,0) -- node[below] {$\blackmbf{<100,000}$} (7.5,0);
  \draw [purple!64!white, line width=6] (7.5,0) -- node[below] {$\blackmbf{<1,000,000}$} (10,0);
  \draw [red!87!black, line width=6] (10,0) -- node[below] {$\blackmbf{< 1 \mbox{ billion}}$} (12,0);
\end{tikzpicture}
}|
]


@section{Adaptor Modules}

When an untyped data structure crosses a boundary into typed code,
Typed Racket generates a new type definition for the structure based on annotations in the
importing module.
If two typed modules import the same untyped data structure, then each import
will generate a unique type definition.
Even if the typed modules use identical type annotations, the generated types
are incompatible.

These generative type definitions pose a challenge when testing all gradually-typed configurations of a program.
Some configurations need to be refactored to ensure that typed code sharing untyped
data references a common set of type definitions.
We perform this refactoring uniformly by introducing so-called @emph{type adaptor modules}.
An adaptor is a collection of type annotations.
Typed modules reference these annotations rather than declaring their own.
In the context of our method, adaptors are available as library files to all configurations
in a performance lattice.


@section{Program Descriptions}

This section briefly describes each benchmark, noting the dependencies and required adaptor
modules.  Unless otherwise noted, the benchmarks rely
only on core Racket libraries and do not use adaptor modules.
We credit program authors in parentheses; except for @tt{sieve}, all programs
are independently useful.

@parag{Sieve (Ben Greenman)}
This program finds prime numbers using the Sieve of Eratosthenes and is our
smallest benchmark. It consists of two modules: a streams library and the
Sieve implementation.
We wrote this benchmark to illustrate the pitfalls of sound gradual typing.

@parag{Morse code (John Clements & Neil Van Dyke)}
This script is adapted from a morse code training program.@note{@url["http://github.com/jbclements/morse-code-trainer"]}
The original program plays a morse code audio clip, 
reads the keyboard for user input, and scores the input based on its
Levenshtein distance from the correct answer.
Our benchmark setup generates morse code strings and runs the
Levenshtein algorithm on a list of frequently-used English words.

@parag{MBTA (Matthias Felleisen)}
The @tt{mbta} program builds a representation of Boston's public transit system
and answers reachability queries.
It relies on an untyped graph library.
The original program was implemented with a server thread that
responded to queries asynchronously.
We instead measure a synchronous version of the program to ensure compatibility
with Racket's stack-based profiling tools.

@parag{ZO Traversal (Ben Greenman)}
This tool is used for exploring and counting the frequency of
Racket bytecode structures.
It operates on the Racket compiler's untyped zo data structures.
Since these data structures are not natively supported in Typed Racket, even the
completely typed program incurs some dynamic overhead.

@parag{Suffixtree (Danny Yoo)}
This library implements a longest common substring algorithm
using Ukkonen's suffix tree algorithm. While the library has
minimal external dependencies, it calls for one adaptor module for the
algorithm's internal data structures.

@parag{L-NM (Ben Greenman)}
This script analyzes the measurements included in this paper
and generates figures 4 and 5. @; @figure-ref{fig:lnm1} and @figure-ref:lnm2}.
Most of this benchmark's running time is spent generating figures using Typed Racket's @tt{plot} library, so the @emph{untyped} version of this progam is noticably less performant.
This program relies on an untyped image rendering library and uses two adaptor modules.

@parag{K-CFA (Matt Might)}
The @tt{kcfa} program implements a simple control flow analysis for a
lambda calculus.
The language definitions and analysis are spread across seven modules, four of
which require adaptors because they introduce new datatypes.

@parag{Snake (David Van Horn)} This program is based on a contract verification
benchmark@note{@url["http://github.com/philnguyen/soft-contract"]} by
Nguyễn @|etal|@~cite[nthvh-icfp-2014].  It implements a game where a growing
and moving snake tries to eat apples while avoiding walls and its own tail.
Our benchmark runs a pre-recorded history of moves altering
the game state and does not display a GUI.  We use one adaptor module to
represent the game datatypes, but otherwise the program is self-contained.

@parag{Tetris (David Van Horn)}
This program is taken from the same benchmark suite as @tt{snake}@~cite[nthvh-icfp-2014]
and implements the eponymous game.
Like @tt{snake}, the benchmark runs a pre-recorded set of moves. Using it here requires
one adaptor module.

@parag{Synth (Vincent St-Amour & Neil Toronto)}
The @tt{synth} benchmark@note{@url["http://github.com/stamourv/synth"]}
is a sound synthesis example from St-Amour @|etal|'s work on
feature-specific profiling@~cite[saf-cc-2015].
The program consists of nine modules, half of which are from Typed Racket's array library.
In order to run these library modules in all typed-untyped configurations we create an adaptor module
for the underlying array data structure.

@parag{Gregor (Jon Zeppieri)}
This benchmark consists of thirteen modules and stress-tests a date and time library.
The original library uses a
library for ad-hoc polymorphism that is not supported by Typed Racket.
Our adaptation instead uses a mono-typed variant of this code and removes
the string parsing component.
The benchmark uses two adaptor modules and relies on a small, untyped library for
acquiring data on local times.

@parag{Quad (Matthew Butterick)}
This project implements a type-setting library.
It depends on an external constraint satisfaction solver
library (to divide lines of text across multiple columns) and uses two adaptor modules.
