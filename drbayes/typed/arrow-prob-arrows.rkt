#lang typed/racket/base

(provide
 run/bot*
 run/pre*
 run/pre
)

;; -----------------------------------------------------------------------------

(require
 racket/match
 "arrow-types-adapted.rkt"
 "set-extremal-set-adapted.rkt"
 "set-union-adapted.rkt"
)

;; =============================================================================

(: run/bot* (Bot*-Arrow -> Bot-Arrow))
(define (run/bot* k)
  (if (bot-wrapper? k)
      (bot-wrapper-arrow* k)
      (bot*-arrow-arrow k)))

(: run/pre* (Pre*-Arrow -> Pre-Arrow))
(define (run/pre* k)
  (if (pre-wrapper? k)
      (pre-wrapper-arrow* k)
      (pre*-arrow-arrow k)))

;; TODO filter caselam
(: run/pre (case-> (-> Pre-Arrow Empty-Set Empty-Pre-Mapping)
                   (-> Pre-Arrow Set Pre-Mapping)
                   (-> Pre-Arrow Empty-Set Boolean Empty-Pre-Mapping)
                   (-> Pre-Arrow Set Boolean Pre-Mapping)))
(define (run/pre h A [exact? #t])
  (if (empty-set? A)
      empty-pre-mapping
      (if exact?
          (h A)
          (let ([h  (h A)])
            (cond [(empty-pre-mapping? h)  empty-pre-mapping]
                  [else  (match-define (nonempty-pre-mapping Y p) h)
                         (nonempty-pre-mapping
                          Y (λ (B)
                              (define-values (A _) (p B))
                              (values A #f)))])))))
