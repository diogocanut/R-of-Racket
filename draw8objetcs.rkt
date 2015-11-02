;this code draw 8 images on a background defined by user and make the move.
;put your background and image V

#lang racket
(require 2htdp/universe 2htdp/image)
(define WIDTH 600)
(define HEIGHT 600)

(define ET (scale 0.4 <INSERT IMAGE HERE>))
(define bg-scene <INSERT BACKGROUND IMAGE HERE>)

(define (add-state-3 current-state)
(if (>= current-state 580)
    0
    (+ current-state 3)))



(define (draw-scene current-state)
  (place-image ET (/ WIDTH 2) current-state
  (place-image ET current-state current-state
  (place-image ET (- WIDTH current-state) current-state
  (place-image ET (/ WIDTH 2) (- HEIGHT current-state)
  (place-image ET (- WIDTH current-state) (- HEIGHT current-state)
  (place-image ET current-state (- HEIGHT current-state)
  (place-image ET current-state (/ HEIGHT 2)
   (place-image ET (- WIDTH current-state) (/ HEIGHT 2)
  (place-image bg-scene (/ WIDTH 2) (/ HEIGHT 2)            
               (empty-scene WIDTH HEIGHT)))))))))))

(big-bang 0
          (on-tick add-state-3)
          (to-draw draw-scene))
