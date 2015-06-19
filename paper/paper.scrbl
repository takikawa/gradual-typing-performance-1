#lang scribble/sigplan

@(require "common.rkt")

@authorinfo["Asumu Takikawa" "Northeastern University" "asumu@ccs.neu.edu"]
@;authorinfo["Spenser Bauman" "Indiana University" "sabauma@cs.indiana.edu"]
@authorinfo["Daniel Feltey" "Northeastern University" "dfeltey@ccs.neu.edu"]
@authorinfo["Ben Greenman" "Northeastern University" "types@ccs.neu.edu"]
@authorinfo["Max S. New" "Northeastern University" "maxsnew@ccs.neu.edu"]
@;authorinfo["Sam Tobin-Hochstadt" "Indiana University" "samth@cs.indiana.edu"]
@authorinfo["Jan Vitek" "Northeastern University" "j.vitek@ccs.neu.edu"]
@authorinfo["Matthias Felleisen" "Northeastern University" "matthias@ccs.neu.edu"]

@title{Is Sound Gradual Typing Dead?}

@abstract{
Gradual typing promises to solve a practical issue: combining the benefits of
static and dynamic type-checking into a single language.
Languages designed to address this issue must therefore give evidence that
they are practical for everyday use.
This evidence has been missing in recent proposals to add sound, static
typed checking to mainstream, dynamically-typed languages.

We establish basic principles for the performance evaluation of gradual type
systems and study the performance characteristics of one sound, gradually-typed
language.
Our results indicate that the run-time checks preserving type soundness
frequently add an order of magnitude slowdown to real programs, raising the
question of whether type soundness is incompatible with the gradual
typing promise.
}

@include-section{intro.scrbl}
@include-section{framework.scrbl}
@include-section{benchmarks.scrbl}
@include-section{typed-racket.scrbl}
@;include-section{pycket.scrbl}
@include-section{death.scrbl}
@include-section{related.scrbl}
@include-section{conclusion.scrbl}

@generate-bibliography[]
