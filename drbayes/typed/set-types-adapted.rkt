#lang typed/racket/base

(provide
  (struct-out Base-Value)
  (struct-out Base-Bot-Basic)
  (struct-out Base-Bot-Entry)
  (struct-out Base-Top-Entry)
  (struct-out Base-Bot-Set)
  (struct-out Base-Top-Set)
  ;; ---
  Tag
  ;; ---
  bot-set?
  bot-basic?
  bot-entry?
  top-entry?
)

;; -----------------------------------------------------------------------------

(require
  benchmark-util)
(require/typed/check "set-types.rkt"
  [#:struct Base-Value ()]
  [#:struct Base-Set ()]
  [#:struct (Base-Bot-Set Base-Set) ()]
  [#:struct (Base-Top-Set Base-Set) ()]
  [#:struct (Base-Bot-Entry Base-Bot-Set) ()]
  [#:struct (Base-Top-Entry Base-Top-Set) ()]
  [#:struct (Base-Bot-Basic Base-Bot-Entry) ()]
)

;; =============================================================================

(define-type Tag Symbol)

(define bot-set? Base-Bot-Set?)
(define bot-basic? Base-Bot-Basic?)
(define bot-entry? Base-Bot-Entry?)
(define top-entry? Base-Top-Entry?)
