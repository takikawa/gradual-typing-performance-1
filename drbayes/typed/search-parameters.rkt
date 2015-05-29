#lang typed/racket/base

(provide
  drbayes-refinement-prob-min
  drbayes-sample-max-splits
  drbayes-refinement-max-splits
  drbayes-refinement-sample?
  drbayes-refinement-search?)

;; -----------------------------------------------------------------------------

(require
  benchmark-util
  racket/flonum
  (only-in "set-prob-set-adapted.rkt"
    probs
    Nonempty-Prob-Set
    prob-set-member?)
  "flonum-probability-adapted.rkt"
)

;; =============================================================================

(: ->prob (-> Nonempty-Prob-Set (-> (U Flonum Prob) Prob)))
(define ((->prob X) x)
  (define p (if (prob? x) x (flonum->prob x)))
  (cond [(and (prob? p) (prob-set-member? X p))  p]
        [else  (raise-argument-error '->
                                     (format "Flonum or Prob in ~v" X)
                                     x)]))

;; ===================================================================================================
;; General parameters

(: drbayes-refinement-search? (Parameterof Boolean))
(: drbayes-refinement-sample? (Parameterof Boolean))
(: drbayes-refinement-max-splits (Parameterof Natural))
(: drbayes-refinement-prob-min (Parameterof (U Flonum Prob) Prob))

(define drbayes-refinement-search? (make-parameter #t))
(define drbayes-refinement-sample? (make-parameter #t))
(define drbayes-refinement-max-splits (make-parameter 0))
(define drbayes-refinement-prob-min (make-parameter prob-0 (->prob probs)))

;; =============================================================================
;; Sampler parameters

(: drbayes-sample-max-splits (Parameterof Natural))

(define drbayes-sample-max-splits (make-parameter 3))
