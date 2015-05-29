#lang typed/racket/base

(provide
 make-preimage-refiner)

;; -----------------------------------------------------------------------------

(require
 benchmark-util
 "arrow-types-adapted.rkt"
 "set-store-set-adapted.rkt"
 "set-extremal-set-adapted.rkt"
 "set-union-adapted.rkt"
 "set-null-set-adapted.rkt"
 "set-types-adapted.rkt"
 )
(require/typed/check "arrow-prob-arrows.rkt"
  [run/pre (-> Pre-Arrow Set Pre-Mapping)])

;; =============================================================================

(: make-preimage-refiner (-> Pre-Arrow Nonempty-Set Refiner))
(define ((make-preimage-refiner h B) S)
  (cond [(empty-store-set? S)  (values empty-store-set #t)]
        [else
         (define-values (SN exact?) (preimage/pre (run/pre h (set-pair S nulls)) B))
         (let-values ([(S N)  (set-projs SN)])
           (cond [(not (or (empty-set? N) (nulls? N)))
                  (raise-result-error 'preimage-refiner "(U Empty-Set Full-Null-Set)" N)]
                 [else
                  (values (set-take-stores S) exact?)]))]))

;; -----------------------------------------------------------------------------
;; From 'preimage-mapping.rkt'

(: preimage/pre (-> Pre-Mapping Set (Values Set Boolean)))
(define (preimage/pre h B)
  (if (empty-pre-mapping? h)
      (values empty-set #t)
      (let ([B  (set-intersect B (nonempty-pre-mapping-range h))])
        (if (empty-set? B)
            (values empty-set #t)
            ((nonempty-pre-mapping-fun h) B)))))
