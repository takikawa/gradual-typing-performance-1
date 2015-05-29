#lang typed/racket/base

(provide
 Empty-Store-Set
 Nonempty-Store-Set
 Store-Set
 Plain-Store-Set
 Full-Store-Set
 empty-store-set
 empty-store-set?
 stores
 stores?
 store-set-random-measure
 store-set-random-proj
 store-set-branch-proj
 store-set-random-unproj
 store-set-branch-unproj
 store-set-realize
 store-set?
 store-set-intersect
)

;; -----------------------------------------------------------------------------

(require
  benchmark-util
  "set-types-adapted.rkt"
)
(require/typed/check "set-store-set.rkt"
  [#:struct Base-Store-Set Base-Bot-Basic ()]
  [#:struct Full-Store-Set Base-Store-Set ()]
  [#:struct Empty-Store-Set Base-Store-Set ()]
  [#:struct Plain-Store-Set
    ([random : Nonempty-Prob-Set]
     [branch : Nonempty-Bool-Set]
     [left   : Nonempty-Store-Set]
     [right  : Nonempty-Store-Set])]
  [stores (-> Full-Store-Set)]
  [store-set-realize (-> Nonempty-Store-Set Store)]
  [store-set-random-proj Boolean]
  [store-set-branch-proj Boolean]
  [store-set-random-unproj (-> Store-Set Store-Index Prob-Set Store-Set)]
  [store-set-branch-unproj (-> Store-Set Store-Index Bool-Set Store-Set)]
  [store-set-intersect
   (case-> (-> Store-Set Nonfull-Store-Set Nonfull-Store-Set)
                               (-> Nonfull-Store-Set Store-Set Nonfull-Store-Set)
                               (-> Store-Set Store-Set Store-Set))]
  [store-set-random-measure (-> Store-Set Prob)])

;; =============================================================================

(define empty-store-set? Empty-Store-Set?)
(define empty-store-set (Empty-Store-Set))
(define store-set? Base-Store-Set?)
(define stores? Full-Store-Set?)

(define-type Nonempty-Store-Set (U Plain-Store-Set Full-Store-Set))
(define-type Nonfull-Store-Set (U Plain-Store-Set Empty-Store-Set))
(define-type Store-Set (U Plain-Store-Set Empty-Store-Set Full-Store-Set))
