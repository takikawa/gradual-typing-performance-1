#lang typed/racket/base

(provide
  j0
  left
  right)

;; -----------------------------------------------------------------------------
;; =============================================================================

(: j0 (Listof Boolean))
(define j0 '())

(: left (-> (Listof Boolean) (Listof Boolean)))
(define (left j) (cons #t j))

(: right (-> (Listof Boolean) (Listof Boolean)))
(define (right j) (cons #f j))
