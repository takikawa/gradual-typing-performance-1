#lang typed/racket/base

(provide
 Store-Search-Tree
 (struct-out search-fail)
 (struct-out search-node)
 (struct-out search-succ)
 sample-search-tree
 )

;; -----------------------------------------------------------------------------

(require
 benchmark-util
 "set-store-set-adapted.rkt"
 "flonum-probability-adapted.rkt"
 )

(require/typed/check "search-tree.rkt"
  [#:struct search-succ
    ([value : Nonempty-Store-Set] [prob : Prob])]
  [#:struct search-fail
            ([value : Empty-Store-Set])]
  [#:struct search-node
            ([left : (Promise Store-Search-Tree)]
             [right : (Promise Store-Search-Tree)]
             [prob-left : Prob]
             [name : Symbol])]
  [sample-search-tree (-> Prob Store-Search-Tree
                                     (Values Prob (U Nonempty-Store-Set Empty-Store-Set)
                                             Prob Store-Search-Tree))]
  )

;; =============================================================================

(define-type Store-Search-Tree (U search-succ search-fail search-node))
