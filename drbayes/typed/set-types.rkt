#lang typed/racket/base

(provide
  (struct-out Base-Value)
  (struct-out Base-Set)
  (struct-out Base-Bot-Set)
  (struct-out Base-Top-Set)
  (struct-out Base-Bot-Entry)
  (struct-out Base-Top-Entry)
  (struct-out Base-Bot-Basic)
)

;; -----------------------------------------------------------------------------
;; =============================================================================

(struct: Base-Value () #:transparent)
(struct: Base-Set () #:transparent)
(struct: Base-Bot-Set Base-Set () #:transparent)
(struct: Base-Top-Set Base-Set () #:transparent)
(struct: Base-Bot-Entry Base-Bot-Set () #:transparent)
(struct: Base-Top-Entry Base-Top-Set () #:transparent)
(struct: Base-Bot-Basic Base-Bot-Entry () #:transparent)
