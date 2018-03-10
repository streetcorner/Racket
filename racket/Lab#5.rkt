#|Author: Wang Yumeng|#
#lang racket
(require graphics/turtles)
(turtles)

#|
(move-offset)
;;(clear)
|#
;;excs #1
(define (repeat n f)
      (when (> n 0)
          (f)
          (repeat (- n 1) f)))
;;(repeat 4 (lambda () (displayln "hello!")))

;;excs #2
(define (square n)
  (repeat 4
          (lambda () (draw n) (turn 90))))
;;(square 100)

(define (rectangle n)
  (repeat 4
          (lambda ()
            (draw (* 2 n))
            (turn 90)
            (draw n)
            (turn 90))))
;;(rectangle 100)

(define (triangle n)
  (repeat 3
          (lambda ()
            (draw n)
            (turn 120))))
;;(triangle 100)

;;excs 3
(define (circle n)
  (repeat 360
          (lambda ()
            (draw n)
            (turn 1))))
;;(circle 8)

;;excs #4
(define (star f l n a)
  (when (> n 0 )
            (f l)
            (turn a)
            (star f l (- n 1) a)))
;;(star (lambda (n)(draw n)(move (- n))) 100 5 45)
;;(star square 100 6 -20)

;;excs #5
(define (spiral f l dl lmt a)
  (when (> l lmt)
    (f l)
    (turn a)
    (spiral f (- l dl) dl lmt a)))
#|
(turn 90)
(spiral square 200 5 1 -5)

(turn 90)
(move-offset 0 -150)
(spiral circle 4 0.1 0 -5)
|#

;;excs #6
(define (koch n l)
  (cond
    [(= n 1) (draw l)]
    [else
     (koch (- n 1) (/ l 3))
     (turn 60)
     (koch (- n 1) (/ l 3))
     (turn -120)
     (koch (- n 1) (/ l 3))
     (turn 60)
     (koch (- n 1) (/ l 3))]))
;;(koch 6 300)

(define (snowflake n l)
  (repeat 3 (lambda ()
              (koch n l)
              (turn -120))))
;;(snowflake 3 200)
;;(snowflake 5 300)

;;excs #7
(define (tree l r lmt)
     (turn 90)
     (draw l)
  
     (when (> (/ l r) lmt)
       (turn 30)
       (turn -90)
       (tree (/ l r) r lmt)
       
       (move (-(/ l r)))
       (turn -60)
       (turn -90)
       (tree (/ l r) r lmt)
       
       (move (-(/ l r)))
       (turn 30)))
;;(tree 80 2 1) 
;;(tree 200 2 50)
;;(tree 20 2 5)
;;(tree 200 2 40) 

;;excs #8
(define (sierpinski n l)
  (turn 90)
  (cond
    [(= n 1)
    (repeat 3 (lambda ()
                (draw l)
                (turn -120)))]
    [else (repeat 3 (lambda ()
                     (turn -90)
                     (sierpinski (- n 1) (/ l 2))
                     (move l)
                     (turn -120)))]))
;;(sierpinski 1 300)










    
          
    
    