#lang typed/racket/base

(provide
  (struct-out tagged-value)
  (struct-out Base-Value)
  value?
  value-list-ref
)

;; -----------------------------------------------------------------------------

(require
 racket/promise
  "set-types-adapted.rkt"
  "flonum-probability-adapted.rkt"
  "set-store-adapted.rkt"
  "set-bottom-adapted.rkt"
  )

;; =============================================================================

(struct: tagged-value Base-Value
  ([tag : Symbol] [value : Value]) #:transparent)

(: value? (Value -> Boolean))
(define (value? v)
  (cond [(flonum? v)        (< -inf.0 v +inf.0)]
        [(prob? v)          #t]
        [(boolean? v)       #t]
        [(null? v)          #t]
        [(pair? v)          (and (value? (car v)) (value? (cdr v)))]
        [(store? v)         #t]
        [(tagged-value? v)  (value? (tagged-value-value v))]))

;; -----------------------------------------------------------------------------

(define-type Value (Rec Value (U Flonum
                                 Prob
                                 Boolean
                                 Null
                                 (Pair Value Value)
                                 tagged-value
                                 Store)))

(define-type Maybe-Value (U Value Bottom))

;; -----------------------------------------------------------------------------

(: value-list-ref (Value Natural -> Maybe-Value))
(define (value-list-ref orig-v j)
  (let loop ([v orig-v] [j j])
    (cond [(null? v)
           (bottom (delay (format "value-list-ref: index out of range; given ~e and ~e" orig-v j)))]
          [(not (pair? v))
           (bottom (delay (format "value-list-ref: expected list; given ~e" orig-v)))]
          [(zero? j)  (car v)]
          [else       (loop (cdr v) (- j 1))])))
