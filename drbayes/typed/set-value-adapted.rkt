#lang typed/racket/base

(provide
  Maybe-Value
  Value
  value?
  (struct-out tagged-value) ;; for 'language-functions.rkt'
  value-list-ref
)
;; -----------------------------------------------------------------------------

(require
  benchmark-util
  "flonum-probability-adapted.rkt"
  "set-bottom-adapted.rkt"
  "set-store-adapted.rkt")
(require/typed/check "set-value.rkt"
  [#:struct Base-Value ()]
  [#:struct (tagged-value Base-Value)
    ([tag : Symbol]
     [value : Value])]
  [value? (-> Any Boolean)]
  [value-list-ref (Value Natural -> Maybe-Value)])

;; =============================================================================

(define-type Value (Rec Value (U Flonum
                                 Prob
                                 Boolean
                                 Null
                                 (Pair Value Value)
                                 tagged-value
                                 Store)))

(define-type Maybe-Value (U Value Bottom))
