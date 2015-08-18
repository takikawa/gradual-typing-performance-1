#lang typed/racket/base

(require "../../scripts/app-trace.rkt")

(require "data-adapted.rkt")

(define (main)
  (define y (+ 1 (car xs)))
  (define z (f (list 1 2 3)))
  (set-box! b -2)
  (set-box! b 42))
  ;(void))

(main)
