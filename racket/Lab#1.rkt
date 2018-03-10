#lang racket
;;excs #1
(define (factorial N)
  (cond [( = N 0) 1]
        [else (* N (factorial (- N 1)))]))

;;excs #2
(define (fibonacci n)
  (cond [(< n 2) n]
        [else (+ (fibonacci (- n 1))
                 (fibonacci (- n 2)))]))
 
;;excs #3
(define (delta x y z)
  (- (* y y) (* 4 (* x z))))

(define (solve x y z)
  (cond[(> (delta x y z) 0) (list (- 0(/ (- y (sqrt (delta x y z))) (* 2 x))) 
                                 (- 0(/ (+ y (sqrt (delta x y z))) (* 2 x))))]      
       [(= (delta x y z) 0) (list (- 0 (* 2 y) x))]
       [else (list )]))
  
;;excs #4
(define (solve2 x y z)
  (let ([delta (- (* y y) (* 4 (* x z)))])
    (cond
       [(> delta 0) (list (- 0(/ (- y (sqrt delta)) (* 2 x))) 
                          (- 0(/ (+ y (sqrt delta)) (* 2 x))))]      
       [(= delta 0) (list (- 0 (/ y (* 2 x))))]
       [else (list )])))
  

;;excs #5
(define (sum-square1 n)
  (cond
    [(= n 1) 1]
    [else (+ (sum-square1 (- n 1)) (* n n))]))

(define (sum-cube1 n)
  (cond
    [(= n 1) 1]
    [else (+ (sum-cube1 (- n 1)) (* n (* n n)))]))

;;excs #6
(define (sigma1 f n)
  (cond
      [(= n 1) 1]
      [else (+ (sigma1 f (- n 1)) (f n))]))


;;excs #7

(define ((sigma2 f) n)
  (cond
      [(= n 1) 1]
      [else (+ ((sigma2 f) (- n 1)) (f n))]))
  
;;excs #8
(define ((derivative1 f) a h)
  (/ (- (f (+ a h)) (f a)) h))

;;excs #9
(define ((derivative2 f h) a)
  (/ (- (f (+ a h)) (f a)) h))