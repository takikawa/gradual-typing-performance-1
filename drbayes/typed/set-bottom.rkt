#lang typed/racket/base

(provide
  (struct-out Bottom))

;; -----------------------------------------------------------------------------
;; =============================================================================

(struct Bottom ([message : (Promise String)]) #:transparent)
