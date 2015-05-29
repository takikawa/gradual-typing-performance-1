#lang typed/racket/base

(provide
  (struct-out Store)
)

;; -----------------------------------------------------------------------------

(require
  "flonum-probability-adapted.rkt"
  (only-in "set-bottom-adapted.rkt"
    Bottom)
)

;; =============================================================================

(struct Store ([random : (U (Promise (U Prob Bad-Prob)) (U Prob Bad-Prob))]
               [branch : (U (Promise (U Boolean Bottom)) (U Boolean Bottom))]
               [left   : (U (Promise Store) Store)]
               [right  : (U (Promise Store) Store)])
  #:transparent)
