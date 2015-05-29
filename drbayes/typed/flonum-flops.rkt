#lang typed/racket/base

(provide
  fllog-random
  fllog/error
  fllog1p/error)

;; -----------------------------------------------------------------------------

(require
  racket/performance-hint
  (only-in math/flonum
    fllog
    fl2log
    fl2log1p
    lg-
    lg+)
)

;; =============================================================================

(begin-encourage-inline
  (: fllog/error   (-> Flonum (Values Flonum Flonum)))
  (define (fllog/error x)   (fl2log x 0.0))

  (: fllog1p/error (-> Flonum (Values Flonum Flonum)))
  (define (fllog1p/error x) (fl2log1p x 0.0))
)

(: fllog-random (-> Flonum Flonum Flonum))
(define (fllog-random a b)
  (let ([a  (min a b)]
        [b  (max a b)])
    (let loop ()
      (define x (lg+ a (+ (fllog (random)) (lg- b a))))
      (if (<= a x b) x (loop)))))
