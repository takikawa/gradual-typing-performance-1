#lang typed/racket/base

(provide
  (struct-out Store)
  store?
)

;; -----------------------------------------------------------------------------

(require
  benchmark-util
  "flonum-probability-adapted.rkt"
  (only-in "set-bottom-adapted.rkt"
    Bottom))
(require/typed/check "set-store.rkt"
  [#:struct Store
    ([random : (Maybe-Promise (U Prob Bad-Prob))]
     [branch : (Maybe-Promise (U Boolean Bottom))]
     [left   : (Maybe-Promise Store)]
     [right  : (Maybe-Promise Store)])])

;; =============================================================================

(define store? Store?)
