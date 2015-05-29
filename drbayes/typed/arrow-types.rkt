#lang typed/racket/base

(provide
  (struct-out random-index)
  (struct-out ifte*-index)
  (struct-out bot*-arrow)
  (struct-out bot-wrapper)
  (struct-out pre*-arrow)
  (struct-out pre-wrapper)
  (struct-out Empty-Pre-Mapping)
  (struct-out nonempty-pre-mapping))

;; -----------------------------------------------------------------------------

(require
 benchmark-util
 "set-prob-set-adapted.rkt"
 "set-store-set-adapted.rkt"
 "set-union-adapted.rkt"
 "set-value-adapted.rkt"
 "set-bottom-adapted.rkt"
 )

(define-type Store-Index (Listof Boolean))

;; =============================================================================
;; Indexes

(define-type Interval-Splitter (-> Nonempty-Prob-Interval (Listof Nonempty-Prob-Interval)))

(struct: random-index ([index : Store-Index]
                       [split : (U #f Positive-Integer Interval-Splitter)])
  #:transparent)

(struct: ifte*-index ([index : Store-Index]
                      [true  : (Promise Indexes)]
                      [false : (Promise Indexes)])
  #:transparent)

(define-type Indexes (Listof (U random-index ifte*-index)))

;; ===================================================================================================
;; Preimage mappings

(struct Empty-Pre-Mapping ())

(struct: nonempty-pre-mapping ([range : Nonempty-Set]
                               [fun : (-> Nonempty-Set (Values Set Boolean))])
  #:transparent)

(define-type Pre-Mapping (U Empty-Pre-Mapping nonempty-pre-mapping))

;; ===================================================================================================
;; Bottom* and Preimage* arrow

(define-type Bot-Arrow (-> Value Maybe-Value))
(define-type Pre-Arrow (-> Nonempty-Set Pre-Mapping))

(struct: bot*-arrow ([arrow : Bot-Arrow]) #:transparent)
(struct: bot-wrapper ([arrow : Bot-Arrow] [arrow* : Bot-Arrow]) #:transparent)

(struct: pre*-arrow ([arrow : Pre-Arrow]) #:transparent)
(struct: pre-wrapper ([arrow : Pre-Arrow] [arrow* : Pre-Arrow]) #:transparent)
