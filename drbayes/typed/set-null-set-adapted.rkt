#lang typed/racket/base

(provide
 (struct-out Empty-Null-Set)
 (struct-out Full-Null-Set)
 nulls
 nulls?
 null-set?
 null-set-intersect
 empty-null-set?
 )

;; -----------------------------------------------------------------------------

(require
 "set-types-adapted.rkt"
 )

;; =============================================================================

(struct Base-Null-Set Base-Bot-Basic () #:transparent)
(define null-set? Base-Null-Set?)

(struct Empty-Null-Set Base-Null-Set ()) ;empty-null-set
(define empty-null-set? Empty-Null-Set?)
(struct Full-Null-Set  Base-Null-Set ()) ;nulls)
(define nulls (Full-Null-Set))
(define nulls? Full-Null-Set?)

;; -----------------------------------------------------------------------------

(define-type Null-Set (U Empty-Null-Set Full-Null-Set))

;; -----------------------------------------------------------------------------

(: null-set-intersect (case-> (-> Null-Set Empty-Null-Set Empty-Null-Set)
                              (-> Empty-Null-Set Null-Set Empty-Null-Set)
                              (-> Null-Set Null-Set Null-Set)))
(define (null-set-intersect A B)
  (if (nulls? A) B A))
