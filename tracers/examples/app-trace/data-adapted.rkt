#lang typed/racket/base

(require/typed "data.rkt"
  [xs (Listof Natural)]
  [f (-> (Listof Index) Index)]
  [b (Boxof Integer)])

(require "../../scripts/app-trace.rkt")
(provide/trace xs f b)
