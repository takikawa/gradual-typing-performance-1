#lang typed/racket/base

(provide
  (struct-out Empty-Set)
  (struct-out Universe))

;; -----------------------------------------------------------------------------
;; =============================================================================

(struct Empty-Set ())
(struct Universe ())
