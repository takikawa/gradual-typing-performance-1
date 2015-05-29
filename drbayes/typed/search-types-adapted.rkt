#lang typed/racket/base

(provide
  (struct-out store-set-sample)
  (struct-out store-sample)
  Store-Search-Tree)

;; -----------------------------------------------------------------------------

(require
  benchmark-util
  ;; -- TODO adapt or something
  (only-in "flonum/probability.rkt"
    Prob)
  (only-in "set/store.rkt"
    Store)
  (only-in "set/store-set.rkt"
    Empty-Store-Set
    Nonempty-Store-Set)
  "search-tree-adapted.rkt"
)
(require/typed/check "search-types.rkt"
  [#:struct store-set-sample
    ([set : Nonempty-Store-Set]
     [numer : Prob]
     [enom : Prob])]
  [#:struct store-sample
    ([s : Store] [numer : Prob] [denom : Prob])])

;; =============================================================================

(define-type Store-Search-Tree (Search-Tree Nonempty-Store-Set Empty-Store-Set))
