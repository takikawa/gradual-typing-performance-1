#lang typed/racket/base

(provide
  (rename-out [-Bottom Bottom])
  bottom
  bottom?
  ;bottom-message
)
;; -----------------------------------------------------------------------------

(require
  benchmark-util)
(require/typed/check "set-bottom.rkt"
  [#:struct Bottom
    ([message : (Promise String)])])

;; =============================================================================

(define-type -Bottom Bottom)
(define bottom Bottom)
(define bottom? Bottom?)
(define bottom-message Bottom-message)
