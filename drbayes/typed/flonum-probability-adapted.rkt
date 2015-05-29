#lang typed/racket/base

(provide
  (struct-out Prob) ;; TODO need to struct-out?
  Bad-Prob
  (rename-out [Prob? prob?])
  prob-0
  prob-1
  prob-0?
  prob-1?
  prob1-
  flonum->prob
  prob->flonum
  prob=
  prob<
  prob<=
  prob-quotient->flonum
  prob+
  prob*
  prob-
  prob/
  prob-random
  prob-random-index
  prob-midpoint
  prob-boolean
  prob-min
)

;; -----------------------------------------------------------------------------

(require
  benchmark-util)
(require/typed/check "flonum-probability.rkt"
  [#:struct Prob
    ([value : Flonum])]
  [#:struct Bad-Prob ()]
  [prob-0 Prob]
  [prob-1 Prob]
  [prob1- Prob]
  [prob-0? (-> Any Boolean)]
  [prob-1? (-> Any Boolean)]
  [flonum->prob (-> Flonum (U Bad-Prob Prob))]
  [prob->flonum (-> Prob Flonum)]
  [prob-quotient->flonum (-> Prob Prob Nonnegative-Flonum)]
  [prob*  (-> Prob Prob Prob)]
  [prob-  (-> Prob Prob Prob)]
  [prob/  (-> Prob Prob Prob)]
  [prob+  (-> Prob Prob Prob)]
  [prob-boolean (-> Prob Prob)]
  [prob-random (-> Prob Prob Prob)]
  [prob-midpoint (-> Prob Prob Prob)]
  [prob-min (-> Prob Prob Prob)]
  [prob= (-> Prob Prob Boolean)]
  [prob-random-index (-> (Listof Prob) Index)]
  [prob< (-> Prob Prob Boolean)]
  [prob< (-> Prob Prob Boolean)])

;; =============================================================================
