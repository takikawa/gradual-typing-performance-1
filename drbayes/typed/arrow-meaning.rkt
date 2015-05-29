#lang typed/racket/base

(provide
  (struct-out meaning))

;; -----------------------------------------------------------------------------

(require
  "set-value-adapted.rkt"
  "arrow-types-adapted.rkt"
)

;; =============================================================================

(struct meaning
  ([proc : (-> Value Value)]
   [bot* : Bot*-Arrow]
   [pre* : Pre*-Arrow]
   [idx : Idx-Arrow])
  #:transparent)
