#lang typed/racket/base

(provide
 trij-add
 )

;; -----------------------------------------------------------------------------

(require
 math/flonum
 "set-real-set-adapted.rkt"
 )

;; =============================================================================

(struct: trijection ([inc1? : Boolean]
                     [inc2? : Boolean]
                     [domain1 : Nonempty-Real-Interval]
                     [domain2 : Nonempty-Real-Interval]
                     [range : Nonempty-Real-Interval]
                     [fc/rndd : (-> Flonum Flonum Flonum)]
                     [fc/rndu : (-> Flonum Flonum Flonum)]
                     [fa/rndd : (-> Flonum Flonum Flonum)]
                     [fa/rndu : (-> Flonum Flonum Flonum)]
                     [fb/rndd : (-> Flonum Flonum Flonum)]
                     [fb/rndu : (-> Flonum Flonum Flonum)])
  #:transparent)

;; -----------------------------------------------------------------------------

(define trij-add
  (trijection #t #t reals reals reals
              fl+/rndd fl+/rndu
              flrev-/rndd flrev-/rndu
              fl-/rndd fl-/rndu))
