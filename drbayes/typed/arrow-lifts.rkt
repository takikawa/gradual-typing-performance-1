#lang typed/racket/base

(provide
 +/bot)

;; -----------------------------------------------------------------------------

(require
 racket/flonum
 "arrow-trijection.rkt"
 )

;; =============================================================================
;; real-lifts.rkt
(define-values (+/bot +/pre) (strict-monotone2d/prim '+ fl+ trij-add))

;; -----------------------------------------------------------------------------
;; from 'make-real-lift.rkt'

(: strict-monotone/pre (-> bijection (-> Pre-Arrow)))
(define (strict-monotone/pre f)
  (define img (bijection-image f))
  (define pre (bijection-preimage f))
  (λ ()
    (define fun (make-pre-mapping-fun/memo))
    (make-pre-arrow/memo
     (λ (A)
       (define-values (B B-exact?) (img A))
       (cond [(empty-set? B)  empty-pre-mapping]
             [else
              (define h (pre A))
              (nonempty-pre-mapping
               B (fun (λ (B)
                        (define-values (A A-exact?) (h B))
                        (values A A-exact?))))])))))

(: strict-monotone2d/prim (-> Symbol (-> Flonum Flonum Value) trijection
                              (Values (-> Bot-Arrow) (-> Pre-Arrow))))
(define (strict-monotone2d/prim name f/proc f)
  (values (real2d/bot name (trijection-domain1 f) (trijection-domain2 f) (trijection-range f) f/proc)
          (strict-monotone2d/pre f)))
