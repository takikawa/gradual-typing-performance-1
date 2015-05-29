#lang typed/racket/base

(provide
  (struct-out search-succ)
  (struct-out search-fail)
  (struct-out search-node)
  sample-search-tree
)

;; -----------------------------------------------------------------------------

(require
 racket/list
 racket/match
 racket/promise
 benchmark-util
 "set-store-set-adapted.rkt"
 "flonum-probability-adapted.rkt")
(require/typed/check "search-utils.rkt"
  [prob-normalize-first (-> Prob Prob Prob)])

;; =============================================================================

(struct search-succ ([value : Nonempty-Store-Set] [prob : Prob]) #:transparent)
(struct search-fail ([value : Empty-Store-Set]) #:transparent)
(struct search-node ([left : (Promise Store-Search-Tree)]
                           [right : (Promise Store-Search-Tree)]
                           [prob-left : Prob]
                           [name : Symbol])
  #:transparent)

(define-type Store-Search-Tree (U search-succ search-fail search-node))

;; -----------------------------------------------------------------------------

(define: search-stats : (HashTable Symbol Natural)  (make-hasheq empty))

(: increment-search-stat (Symbol -> Void))
(define (increment-search-stat name)
  (hash-set! search-stats name (+ 1 (hash-ref search-stats name (λ () 0)))))

(: adjusted-node (-> Prob
                     (Promise Store-Search-Tree)
                     (Promise Store-Search-Tree)
                     Prob
                     Prob
                     Symbol
                     (Values Prob Store-Search-Tree)))
(define (adjusted-node pt cl cr pl pr name)
  (cond [(or (prob-0? pl) (prob-1? pr))  (values (prob* pt pr) (force cr))]
        [(or (prob-0? pr) (prob-1? pl))  (values (prob* pt pl) (force cl))]
        [else
         (values (let ([pt  (prob+ (prob* pt pl) (prob* pt pr))])
                   (if (prob? pt) pt prob-1))
                 (search-node cl cr (prob-normalize-first pl pr) name))]))

(: sample-search-tree (-> Prob Store-Search-Tree
                                     (Values Prob (U Nonempty-Store-Set Empty-Store-Set)
                                             Prob Store-Search-Tree)))
(define (sample-search-tree pt t)
  (match t
    [(search-succ x px)
     ;; Removes zero-probability leaves without altering other leaf probabilities:
     ;(values pt x pt t)
     ;; Convergence properties not known, but it seems to work well, and it's necessary when
     ;; drbayes-always-terminate? is #t
     (values pt x (prob-min px pt) t)
     ]
    [(search-fail x)  (values pt x prob-0 t)]
    [(search-node cl cr pl name)
     (increment-search-stat name)
     (define pr (prob1- pl))
     (define left? (prob-boolean pl))
     (let*-values ([(cl cr pl pr)  (if left? (values cl cr pl pr) (values cr cl pr pl))]
                   [(px x pt* cl*)  (sample-search-tree (prob* pt pl) (force cl))]
                   [(pl*)  (prob/ pt* pt)]
                   [(pl*)  (if (prob? pl*) pl* prob-1)]
                   [(cl*)  (delay cl*)]
                   [(_)  (force cl*)]
                   [(pt t)  (if left?
                                (adjusted-node pt cl* cr pl* pr name)
                                (adjusted-node pt cr cl* pr pl* name))])
       (values px x pt t))]))
