#lang typed/racket/base

(provide
  (struct-out store-set-sample)
  (struct-out store-sample))

;; -----------------------------------------------------------------------------

(require
  (only-in "set/store.rkt"
    Store)
  (only-in "set/store-set.rkt"
    Empty-Store-Set
    Nonempty-Store-Set)
  (only-in "flonum/probability.rkt"
    Prob)
  (only-in "search-tree.rkt"
    Search-Tree)
)

;; =============================================================================

(struct: store-set-sample ([set : Nonempty-Store-Set] [numer : Prob] [denom : Prob])
  #:transparent)

(struct: store-sample ([s : Store] [numer : Prob] [denom : Prob])
  #:transparent)

