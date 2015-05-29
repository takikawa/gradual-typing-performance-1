#lang typed/racket/base

(provide
 neg/proc
 -/idx
 -/pre*
 -/bot*
 -/proc
 list-ref/bot*
 +/bot*
 null/bot*
 &&&/pre*
 >>>/pre*
 list-ref/pre*
 +/pre*
 null/pre*
 &&&/idx
 >>>/idx
 list-ref/idx
 +/idx
 null/idx
 &&&/bot*
 >>>/bot*
 ;; ---
 apply/proc
 ;; -- implemented
 const/proc
 list-ref/proc
 >>>/proc
 &&&/proc
 +/proc
 null/proc
 )

;; -----------------------------------------------------------------------------

(require
 racket/promise
 racket/flonum
 racket/match
 "arrow-types-adapted.rkt"
 "set-bottom-adapted.rkt"
 "set-value-adapted.rkt"
 )

;; =============================================================================

(: const/bot (-> Value Bot-Arrow))
(define ((const/bot b) a) b)

(define const/proc (λ: ([b : Value]) (lower/proc (const/bot b))))

(: lower/proc (Bot-Arrow -> Proc-Arrow))
(define ((lower/proc f) a)
  (define b (f a))
  (if (bottom? b) (error 'drbayes "bottom errors") b))

(define list-ref/proc (λ: ([j : Natural]) (lower/proc (list-ref/bot j))))

;; -----------------------------------------------------------------------------

(: >>>/proc (Proc-Arrow Proc-Arrow -> Proc-Arrow))
(define ((>>>/proc f1 f2) a)
  (f2 (f1 a)))

(: &&&/proc (Proc-Arrow Proc-Arrow -> Proc-Arrow))
(define ((&&&/proc f1 f2) a)
  (cons (f1 a) (f2 a)))

(: apply/proc (Proc-Arrow (Listof Proc-Arrow) -> Proc-Arrow))
(define (apply/proc body args)
  (error "apply/proc not implemented"))
  ;; ((list/proc (apply list/proc args)) . >>>/proc . body))

(: list-ref/bot (-> Natural Bot-Arrow))
(define ((list-ref/bot n) a) (value-list-ref a n))

;; (: list-ref/pre (-> Natural Pre-Arrow))
;; (define (list-ref/pre n) (make-pre-arrow/memo (λ (A) (pre-mapping
;;                                                       (set-proj A n)
;;                                                       (exact-pre-mapping-fun (set-unproj A n))))))

(define (-/proc)
  (error "-/proc not implemented"))

(define (-/bot*)
  (error "-/proc not implemented"))

(define (+/proc)
  (error "+/proc not implemented"))
  ;; (lower/proc (+/bot)))
;; (define (-/proc) (lower/proc (-/bot)))
;; (define (*/proc) (lower/proc (*/bot)))
;; (define (//proc) (lower/proc (//bot)))

(define null/proc (const/proc null))

(: &&&/bot* (Bot*-Arrow Bot*-Arrow -> Bot*-Arrow))
(define (&&&/bot* k1 k2)
  (error "&&&/bot* not implemented"))
  ;; (cond [(and (bot-wrapper? k1) (bot-wrapper? k2))
  ;;        (η/bot* (&&&/bot (bot-wrapper-arrow k1) (bot-wrapper-arrow k2)))]
  ;;       [else
  ;;        (bot*-arrow
  ;;         (&&&/bot (>>>/bot (first/bot (store-left/bot)) (run/bot* k1))
  ;;                  (>>>/bot (first/bot (store-right/bot)) (run/bot* k2))))]))

(: >>>/bot* (Bot*-Arrow Bot*-Arrow -> Bot*-Arrow))
(define (>>>/bot* k1 k2)
  (error ">>>/bot* not implemented"))
  ;; (cond [(and (bot-wrapper? k1) (bot-wrapper? k2))
  ;;        (η/bot* (>>>/bot (bot-wrapper-arrow k1) (bot-wrapper-arrow k2)))]
  ;;       [else
  ;;        (bot*-arrow
  ;;         (>>>/bot (&&&/bot (>>>/bot (fst/bot) (store-right/bot))
  ;;                           (>>>/bot (first/bot (store-left/bot)) (run/bot* k1)))
  ;;                  (run/bot* k2)))]))

(define list-ref/bot* (λ: ([j : Natural])
                          (error "list-ref/bot* not implemented")))
                          ;; (η/bot* (list-ref/bot j))))

(define (null/bot*)
  (error "null/bot* not implemented"))

(define (&&&/pre*)
  (error "NOT IMPLEMETNED"))

(define (>>>/pre*)
  (error "NOT IMPLEMETNED"))

(define (+/pre*)
  (error "NOT IMPLEMETNED"))

(define (null/pre*)
  (error "NOT IMPLEMETNED"))

(define (list-ref/pre*)
  (error "NOT IMPLEMETNED"))

(define (&&&/idx)
  (error "NOT IMPLEMETNED"))

(define (>>>/idx)
  (error "NOT IMPLEMETNED"))

(define (list-ref/idx)
  (error "NOT IMPLEMETNED"))

(define (+/idx)
  (error "NOT IMPLEMETNED"))

(define (null/idx)
  (error "NOT IMPLEMETNED"))

(define (+/bot*)
  (error "+/bot* not implemented"))
  ;; (η/bot* (+/bot)))

(define (-/pre*)
  (error "NOPE"))
(define (-/idx)
  (error "NOPE"))
(define (neg/proc)
  (error "NOPE"))
(define (neg/bot*)
  (error "NOPE"))
