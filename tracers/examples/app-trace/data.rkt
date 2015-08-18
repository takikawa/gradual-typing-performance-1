#lang racket/base

(require "../../scripts/app-trace.rkt")

(define xs (list 1 2 3))
(define f (lambda (x) (car x)))
(define b (box 12))

(provide/trace xs f b)
