#lang typed/racket/base

(provide
 Indexes
 Interval-Splitter
 Bot-Arrow
 Pre-Arrow
 Bot*-Arrow
 Pre*-Arrow
 Idx-Arrow
 (struct-out ifte*-index)
 (struct-out random-index)
 (struct-out bot-wrapper)
 (struct-out bot*-arrow)
 (struct-out pre-wrapper)
 (struct-out pre*-arrow)
 (struct-out nonempty-pre-mapping)
 ;; ---
 empty-pre-mapping
 empty-pre-mapping?
 ;; ---
 Proc-Arrow
 Pre-Arrow
 Pre-Mapping
 Empty-Pre-Mapping
 Refiner
 )

;; -----------------------------------------------------------------------------

(require
  benchmark-util
  "set-union-adapted.rkt"
  "set-value-adapted.rkt"
  "set-store-set-adapted.rkt"
  (only-in "set-prob-set-adapted.rkt"
    Nonempty-Prob-Interval)
)
(require/typed/check "arrow-types.rkt"
  [#:struct random-index
    ([index : Store-Index]
     [split : (U #f Positive-Integer Interval-Splitter)])]
  [#:struct ifte*-index
    ([index : Store-Index]
     [true : (Promise Indexes)]
     [false : (Promise Indexes)])]
  [#:struct bot*-arrow ([arrow : Bot-Arrow])]
  [#:struct bot-wrapper ([arrow : Bot-Arrow] [arrow* : Bot-Arrow])]
  [#:struct pre*-arrow ([arrow : Pre-Arrow])]
  [#:struct pre-wrapper ([arrow : Pre-Arrow] [arrow* : Pre-Arrow])]
  [#:struct Empty-Pre-Mapping ()]
  [#:struct nonempty-pre-mapping
    ([range : Nonempty-Set]
     [fun : (-> Nonempty-Set (Values Set Boolean))])]
  [make-preimage-refiner (-> Pre-Arrow Nonempty-Set Refiner)]
  )

(define-type Store-Index (Listof Boolean))

;; =============================================================================

(define empty-pre-mapping (Empty-Pre-Mapping))
(define empty-pre-mapping? Empty-Pre-Mapping?)

;; -----------------------------------------------------------------------------

(define-type Interval-Splitter (-> Nonempty-Prob-Interval (Listof Nonempty-Prob-Interval)))

(define-type Indexes (Listof (U random-index ifte*-index)))

(define-type Pre-Mapping (U Empty-Pre-Mapping nonempty-pre-mapping))

(define-type Bot-Arrow (-> Value Maybe-Value))
(define-type Pre-Arrow (-> Nonempty-Set Pre-Mapping))

(define-type Bot*-Arrow (U bot-wrapper bot*-arrow))
(define-type Pre*-Arrow (U pre-wrapper pre*-arrow))
(define-type Idx-Arrow (-> Store-Index Indexes))

;; -----------------------------------------------------------------------------
;; --- From 'refine.rkt', in here to avoid making another adaptor

(define-type Refiner (-> Store-Set (Values Store-Set Boolean)))

;; -----------------------------------------------------------------------------
;; --- from 'proc-arrow.rkt'

(define-type Proc-Arrow (Value -> Value))
