#lang typed/racket/base

(provide
  (struct-out meaning))

;; -----------------------------------------------------------------------------

(require
  benchmark-util
  "set-value-adapted.rkt"
  "arrow-types-adapted.rkt")
(require/typed/check "arrow-meaning.rkt"
  [#:struct meaning
    ([proc : Proc-Arrow]
     [bot* : Bot*-Arrow]
     [pre* : Pre*-Arrow]
     [idx  : Idx-Arrow])])

;; =============================================================================
