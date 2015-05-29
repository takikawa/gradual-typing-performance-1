#lang typed/racket/base

(provide
 Empty-Set
 Universe
 empty-set
 empty-set?
 universe
 universe?)

;; -----------------------------------------------------------------------------

(require benchmark-util)
(require/typed/check "set-extremal-set.rkt"
  [#:struct Empty-Set ()]
  [#:struct Universe ()])

;; =============================================================================

(define universe (Universe))
(define universe? Universe?)
(define empty-set (Empty-Set))
(define empty-set? Empty-Set?)
