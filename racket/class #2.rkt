#lang racket
(require dyoo-while-loop)

;;check empty
(empty? '(()))
;;a normal liist
first
rest
(define l '(a b c d e))
(first l)
(rest l)
;;add z
(cons 'z l)
;;change the first elem of the list
(cons 3(rest l))

;;check list
(list? l)
(list? (rest l))
;;first != list, it is a elem.
(list? (first l))

;;append
(append '(x y) '(1 2 3))

(define ll '(a b c d e (a s d)))
(length ll)

(define b 4)
(- b 1)
(- b 1)

;;map
;; (map (square '(2 3 6)))
;;==(4 9 36)

;;max

;;apply
(define t '(1 2 5))
  (apply + t)

;;while
(define (repeat x t)
  (cond
    [(= t 0) '()]
    [else
     (cons x (repeat x (- t 1)))]))

(define (repeat2 x n)
  (letrec ((rec (lambda (x n r)
           (cond
             [(= n 0) r]
           [else (rec x (- n 1) (cons x r))]))))
     (rec x n '())))