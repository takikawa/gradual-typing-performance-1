#lang typed/racket/base

(provide
 prob-normalize-first)

;; -----------------------------------------------------------------------------

(require
 "flonum-probability-adapted.rkt")

;; =============================================================================

(define prob-0.5 (assert (flonum->prob 0.5) prob?))

(: make-prob-normalize-first (-> (-> Prob Prob (U Prob Bad-Prob))
                                 (-> Prob Prob (U Prob Bad-Prob))
                                 (-> Prob Prob)
                                 (-> Prob Prob)
                                 (-> Prob Prob Prob)))
;; Returns p1/(p1+p2)
(define ((make-prob-normalize-first prob+ prob/ prob-reduce1 prob-reduce2) p1 p2)
  (cond [(prob-0? p1)  prob-0]
        [(prob-0? p2)  prob-1]
        [else
         (let loop ([p1 p1] [p2 p2])
           (define p (prob+ p1 p2))
           (cond [(prob? p)
                  (define q (prob/ p1 p))
                  (if (prob? q) q prob-1)]
                 [else
                  (loop (prob-reduce1 p1)
                        (prob-reduce2 p2))]))]))

(define prob-normalize-first
  (make-prob-normalize-first
   prob+
   prob/
   (λ (p) (prob* p prob-0.5))
   (λ (p) (prob* p prob-0.5))))
