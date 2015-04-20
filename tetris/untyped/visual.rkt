#lang racket

(require 2htdp/image
         "data.rkt"
         "consts.rkt"
         "world.rkt"
         "aux.rkt")

(provide
 world->image
 blocks->image
 block->image
 place-block
 world0)

;; Visualize whirled peas
;; World -> Scene
(define (world->image w)
  (place-image (blocks->image (append (tetra-blocks (world-tetra w))
                                      (append (ghost-blocks w)
                                              (world-blocks w))))
               (/ (* board-width block-size) 2)
               (/ (* board-height block-size) 2)
               (empty-scene (* board-width block-size)
                            (* board-height block-size))))

;; BSet -> Scene
(define (blocks->image bs)
  (foldr (λ (b img)
             (cond [(<= 0 (block-y b)) (place-block b img)]
                   [else img]))
           (empty-scene (add1 (* board-width block-size)) 
                        (add1 (* board-height block-size)))
           bs))

;; Visualizes a block.
;; Block -> Image
(define (block->image b)
  (overlay 
   (rectangle (add1 block-size) (add1 block-size) 'solid (block-color b))
   (rectangle (add1 block-size) (add1 block-size) 'outline 'black)))

;; Block Scene -> Scene
(define (place-block b scene)
  (place-image (block->image b)
               (+ (* (block-x b) block-size) (/ block-size 2))
               (+ (* (block-y b) block-size) (/ block-size 2))
               scene))

(define (world0)
  (world (list-pick-random tetras) empty))
