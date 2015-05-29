#lang typed/racket/base

(provide
  Empty-Bool-Set
  Nonempty-Bool-Set
  Bool-Set
  Plain-Bool-Set
  Full-Bool-Set
  ;; ---
  bools
  trues
  falses
  empty-bool-set
  bools?
  trues?
  empty-bool-set?
  bool-set?
  bool-set-intersect
)

;; -----------------------------------------------------------------------------

(require
  benchmark-util
  "set-types-adapted.rkt")
(require/typed/check "set-bool-set.rkt"
  [#:struct (Base-Bool-Set Base-Bot-Basic) ()]
  [#:struct (Empty-Bool-Set Base-Bool-Set) ()]
  [#:struct (Full-Bool-Set Base-Bool-Set) ()]
  [#:struct (True-Bool-Set Base-Bool-Set) ()]
  [#:struct (False-Bool-Set Base-Bool-Set) ()]
  [empty-bool-set Empty-Bool-Set]
  [bool-set-intersect (case-> (-> Bool-Set Nonfull-Bool-Set Nonfull-Bool-Set)
                              (-> Nonfull-Bool-Set Bool-Set Nonfull-Bool-Set)
                              (-> Bool-Set Bool-Set Bool-Set))]
)

;; =============================================================================

(define-type Plain-Bool-Set (U True-Bool-Set False-Bool-Set))
(define-type Nonfull-Bool-Set (U Plain-Bool-Set Empty-Bool-Set))
(define-type Nonempty-Bool-Set (U Plain-Bool-Set Full-Bool-Set))
(define-type Bool-Set (U Plain-Bool-Set Empty-Bool-Set Full-Bool-Set))

(define empty-bool-set? Empty-Bool-Set?)
(define bools (Full-Bool-Set))
(define trues (True-Bool-Set))
(define falses (False-Bool-Set))
(define bools? Full-Bool-Set?)
(define trues? True-Bool-Set?)
(define bool-set? Base-Bool-Set?)
