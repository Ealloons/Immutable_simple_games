#lang racket

(require 2htdp/universe 2htdp/image)



(struct posn (x y))

(define one (posn -1 -1))
(define two (posn 0 -1))
(define three (posn 1 -1))
(define four (posn -1 0))
(define five (posn 0 0))
(define six (posn 1 0))
(define seven (posn -1 1))
(define eight (posn 0 1))
(define nine (posn 1 1))

(define (inverse b)
  (posn (- (posn-y b)) (posn-x b)))
(define (sum-posn p1 p2)
  (posn (+ (posn-x p1)
           (posn-x p2))
        (+ (posn-y p1)
           (posn-y p2))))
(define (=posn? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

;; ㅗ blcok
(define block1
  (list two four five six))

;; ㅣ block
(define block2
  (list two five eight))

;; ㄱ block
(define block3
  (list one two three six nine))

(define (rotate block)
  (map inverse block))

;;
(struct pit (block posn))
(struct world (pit board))

;;constant
(define IMG (square 5 "outline" "red"))
(define WALL (square 5 "outline" "blue"))
(define SIZE 5)
(define WIDTH 500)
(define HEIGHT 500)
(define BOARD-WIDTH 10)
(define BOARD-HEIGHT 50)
(define BLOCK-LIST (list block1 block2 block3))
(define MT-SCENE (empty-scene WIDTH HEIGHT))
(define BOARD '())
                    
;; render
(define (img-list+scene posns img scene)
  (cond [(empty? posns) scene]
        [else (img+scene
               (first posns)
               img
               (img-list+scene (rest posns) img scene))]))
(define (img+scene posn img scene)
  (place-image img
               (* (posn-x posn) SIZE)
               (* (posn-y posn) SIZE)
               scene))
(define (render-block w)
  (define p (world-pit w))
  (define b (world-board w))
  (img-list+scene b WALL (img-list+scene (real p) IMG MT-SCENE)))

;; on-tick
(define (move w)
  (define p (world-pit w))
  (define b (world-board w))
  (define movement
    (pit (pit-block p) (sum-posn (pit-posn p) (posn 0 1))))
  (if (or (crash-wall? (real movement))
          (crash-board? (real movement) b))
      (world (pit (random-block) (posn (/ BOARD-WIDTH 2) 0)) (satisfy (append b (real p)))) 
      (world movement (satisfy b))))
(define (satisfy b)
  (define (filtering x)
    (filter (lambda (k) (= (posn-y k) x)) b))
  (define (checker a)
    (if (>= (length a) (- BOARD-WIDTH 1))
        '()
        a))
  (define (iter n)
    (if (= 0 n)
        '()
        (append (checker (filtering n)) (iter (- n 1)))))
  (iter BOARD-HEIGHT))
                
;; on-key
(define (direct w ke)
  (cond [(legal? ke) (world-change w ke)]
        [else w]))
(define (legal? k)
  (or (key=? k "left")
      (key=? k "right")
      (key=? k "down")
      (key=? k "up")))

(define (world-change w ke)
  (define p (world-pit w))
  (define b (world-board w))
  (world (force-move p ke b) b))

(define (force-move p ke b)
  (define po (pit-posn p))
  (define bl (pit-block p))
  (define movement
    (cond [(string=? ke "left") (pit bl (sum-posn po
                                                (posn -1 0)))]
          [(string=? ke "right") (pit bl (sum-posn po
                                                 (posn 1 0)))]
          [(string=? ke "down") (pit bl po)]
          [(string=? ke "up") (pit (rotate bl) po)]))
  (if (or (crash-wall? (real movement))
          (crash-board? (real movement) b))
      p
      movement))

;; predicate
(define (crash-wall? posns)
  (define (wall p)
    (or (= (posn-x p) 0)
        (= (posn-x p) BOARD-WIDTH)
        (= (posn-y p) BOARD-HEIGHT)))
  (ormap wall posns))
(define (crash-board? posns board)
  (ormap (lambda (x)
           (ormap (lambda (y)
                    (=posn? x y)) board)) posns))
;; Auxiliray
(define (random-block)
  (list-ref BLOCK-LIST (random 3)))
(define (fresh-block)
  (world (pit (random-block) (posn (/ BOARD-WIDTH 2) 0)) BOARD))
(define (real x)
  (define p (pit-posn x))
  (define b (pit-block x))
  (map (lambda (pp) (sum-posn p pp)) b))

(big-bang (fresh-block)
  (on-tick move)
  (on-key direct)
  (to-draw render-block))