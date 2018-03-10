#lang racket
(require dyoo-while-loop)

;;excs #1
(define (repeat1 x t)
  (cond
    [(= t 0) '()]
    [else
     (cons x (repeat1 x (- t 1)))]))

(define (repeat2 x n)
  (letrec ((rec (lambda (x n r)
           (cond
             [(= n 0) r]
           [else (rec x (- n 1) (cons x r))]))))
     (rec x n '())))
;;excs #2
(define (factorial2 n)
  (letrec ((rec (lambda (n r)
                  (cond
                    [(= n 1) r]
                    [else (rec (- n 1) (* n r))]))))
    (rec n 1)))

;;excs #3
(define (fibonacci2 n)
  (letrec ((rec (lambda (n r1 r2)
                  (cond
                    [(= n 0) r1]
                    [(= n 1) r2]
                    [else (rec (- n 1) r2 (+ r1 r2))]))))
      (rec n 0 1)))

;;excs #4
(define (my-reverse2 x)
  (letrec ((rec (lambda (x r)
             (cond
               [(empty? x) r]
               [else (rec (rest x) (cons (first x) r))]))))
    (rec x '())))

(define (my-reverse3 x)
  (lambda (x)
    (let ((r '()))
      (while (not empty? x)
      (set! r (cons (first x) r))
      (set! r (rest x))))))
;;excs #5

 (define (my-range2 a b)
  (letrec ((rec (lambda (a b r)
                  (cond
                    [(>= a b) r]
                    [else (rec (+ a 1) b (cons a r))]))))
    (my-reverse2 (rec a b '()))))

;;(define (my-range2 a b)
  ;;(my-reverse2 (my-range a b)))

;;;excs #6
