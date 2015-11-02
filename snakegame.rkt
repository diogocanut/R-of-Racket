;compile then type start-snake to start.
;the images used aren't include.


#lang racket

(require 2htdp/universe 2htdp/image)



;imagens;
(define GOO-IMG (bitmap "goo.png"))
(define SEG-IMG (bitmap "snakecorpse.png"))
(define HEAD-IMG (bitmap "snakehead.png" ))
(define EXTRA-GOO (bitmap "gooextra.png"))
(define HEAD-LEFT-IMG HEAD-IMG)
(define HEAD-DOWN-IMG (rotate 90 HEAD-LEFT-IMG))
(define HEAD-RIGHT-IMG (flip-horizontal HEAD-LEFT-IMG))
(define HEAD-UP-IMG (flip-vertical HEAD-DOWN-IMG))


;definicoes

(define SEG-SIZE 15)
(define SIZE 15)
(define WIDTH-PX (* SEG-SIZE 15))
(define HEIGHT-PX (* SEG-SIZE 15))
(define TICK-RATE 0.1)
(define EXPIRATION-TIME 150)
(define MT-SCENE (empty-scene WIDTH-PX HEIGHT-PX))



;data;

(struct pit (snake goos)#:transparent)
(struct snake (dir segs)#:transparent)
(struct posn (x y)#:transparent)
(struct goo (loc expire type)#:transparent)


;next-pit;
(define (next-pit w)
(define snake (pit-snake w))
(define goos (pit-goos w))
(define goo-to-eat (can-eat snake goos))
(if goo-to-eat
(pit (grow-n snake goo-to-eat) (age-goo (eat goos goo-to-eat)))
(pit (slither snake) (age-goo goos))))



;can-eat;
(define (can-eat snake goos)
(cond [(empty? goos) #f]
[else (if (close? (snake-head snake) (first goos))
(first goos)
(can-eat snake (rest goos)))]))
(define (close? s g)
(posn=? s (goo-loc g)))
(define (eat goos goo-to-eat)
  (cons (fresh-goo) (remove goo-to-eat goos)))



;slither;
(define (slither sn)
(snake (snake-dir sn)
(cons (next-head sn) (all-but-last (snake-segs sn)))))

;all-but-last;
(define (all-but-last segs)
(cond [(empty? (rest segs)) empty]
[else (cons (first segs) (all-but-last (rest segs)))]))

;next-head;
(define (next-head sn)
(define head (snake-head sn))
(define dir (snake-dir sn))
(cond [(string=? dir "up") (posn-move head 0 -1)]
[(string=? dir "down") (posn-move head 0 1)]
[(string=? dir "left") (posn-move head -1 0)]
[(string=? dir "right") (posn-move head 1 0)]))

(define (grow-n snake goo)
  (if (= (goo-type goo) 1) (grow snake) (grow (grow snake))))




;posn;
(define (posn-move p dx dy)
(posn (+ (posn-x p) dx)
(+ (posn-y p) dy)))


(define (decay g)
  (goo (goo-loc g) (sub1 (goo-expire g)) (goo-type g)))


;goo;
(define (age-goo goos)
(rot (renew goos)))
(define (rot goos)
(cond [(empty? goos) empty]
[else (cons (decay (first goos)) (rot (rest goos)))]))



(define (renew goos)
(cond [(empty? goos) empty]
[(rotten? (first goos))
(cons (fresh-goo) (renew (rest goos)))]
[else
(cons (first goos) (renew (rest goos)))]))

(define (rotten? g)
(zero? (goo-expire g)))


(define (fresh-goo)
(goo (posn (add1 (random (sub1 (/ WIDTH-PX SEG-SIZE))))
(add1 (random (sub1(/ WIDTH-PX SEG-SIZE)))))
EXPIRATION-TIME
(if (= (random 3) 0) 2 1)))

(define (grow sn)
  (snake (snake-dir sn)
         (cons (next-head sn) (snake-segs sn))))





;direcao;
(define (direct-snake w ke)
(cond [(dir? ke) (world-change-dir w ke)]
[else w]))

(define (dir? x)
(or (key=? x "up")
(key=? x "down")
(key=? x "left")
(key=? x "right")))

;opposite-dir;
(define (opposite-dir? d1 d2)
(cond [(string=? d1 "up") (string=? d2 "down")]
[(string=? d1 "down") (string=? d2 "up")]
[(string=? d1 "left") (string=? d2 "right")]
[(string=? d1 "right") (string=? d2 "left")]))

;world-change-dir;
(define (world-change-dir w d)
(define the-snake (pit-snake w))
(cond [(and (opposite-dir? (snake-dir the-snake) d)
(cons? (rest (snake-segs the-snake))))
(stop-with w)]
[else
(pit (snake-change-dir the-snake d) (pit-goos w))]))

;;cenario
(define (render-pit w)
(snake+scene (pit-snake w)
(goo-list+scene (pit-goos w) MT-SCENE)))

(define (snake+scene snake scene)
(define snake-body-scene
(img-list+scene (snake-body snake) SEG-IMG scene))
(define dir (snake-dir snake))
(img+scene (snake-head snake)
(cond [(string=? "up" dir) HEAD-UP-IMG]
[(string=? "down" dir) HEAD-DOWN-IMG]
[(string=? "left" dir) HEAD-LEFT-IMG]
[(string=? "right" dir) HEAD-RIGHT-IMG])
snake-body-scene))

(define (img-list+scene posns img scene)
(cond [(empty? posns) scene]
[else (img+scene
(first posns)
img
(img-list+scene (rest posns) img scene))]))

(define (img+scene posn img scene)
(place-image img
(* (posn-x posn) SEG-SIZE)
(* (posn-y posn) SEG-SIZE)
scene))

(define (img-goo goo)
(cond [(= (goo-type goo) 1) GOO-IMG]
  [else EXTRA-GOO]))

(define (goo-list+scene goos scene)
(cond [(empty? goos) scene]
[else (goo-list+scene (rest goos)
                      (img+scene (goo-loc (first goos)) (img-goo (first goos)) scene))]))

;end-game
(define (dead? w)
(define snake (pit-snake w))
	 (or (self-colliding? snake) (wall-colliding? snake)))

(define (render-end w)
(overlay (text "FIM DE JOGO" 25 "red")
(render-eat w)))

(define (render-eat w)
  (place-image/align
   (text (number->string (n-eat (snake-segs (pit-snake w)))) 15 "blue")
   195 (- (/ HEIGHT-PX 2) 30) "left" "center"
   (place-image/align
    (text "macas comidas:" 15 "blue") 80 (- (/ HEIGHT-PX 2) 30) "left" "center" (render-pit w))))
  
  (define (n-eat lst)
    (cond [(empty? (rest lst)) 0]
          [else (+ (n-eat (cdr lst)) 1)]))

;bater em si propria
(define (self-colliding? snake)
(cons? (member (snake-head snake) (snake-body snake))))
;parede
(define (wall-colliding? snake)
(define x (posn-x (snake-head snake)))
(define y (posn-y (snake-head snake)))
(or (= 0 x) (= x SIZE)
(= 0 y) (= y SIZE)))
  
;auxiliares;
(define (posn=? p1 p2)
(and (= (posn-x p1) (posn-x p2))
(= (posn-y p1) (posn-y p2))))

(define (snake-head sn)
(first (snake-segs sn)))
(define (snake-body sn)
(rest (snake-segs sn)))
(define (snake-tail sn)
(last (snake-segs sn)))
(define (snake-change-dir sn d)
(snake d (snake-segs sn)))

;random-goo;
(define (random-goo x goos)
  (cond [(= x 0) goos]
        [else (random-goo (sub1 x) (cons (fresh-goo) goos))]))



;big-bang;
(define (start-snake)
(big-bang (pit (snake "right" (list (posn 1 1)))
(random-goo (random (sub1 5)) (list (fresh-goo))))
(on-tick next-pit TICK-RATE)
(on-key direct-snake)
(to-draw render-pit)
(stop-when dead? render-end)))
