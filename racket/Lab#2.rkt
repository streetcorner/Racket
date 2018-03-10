#lang racket
;;递归
;;excs #1
(define (my-length1 li)
  (cond
    [(empty? li) 0]
    [else (+ (my-length1 (rest li)) 1)]))

;;exce #2
(define (my-range a b)
  (cond
    [(< a b) (cond [(= (+ 1 a) b) (list a)]
                   [else (cons a (my-range (+ a 1) b))])]
    [else '()]))

;;exce #3
(define (atoms l)
  (cond
    [(empty? l) 0]
    [else (cond
         [(list? l) (+ (atoms (first l)) (atoms (rest l)))]
         [else 1])]))
;;直接检测l是否是list
;;exce #4
(define (my-reverse1 l)
  (cond
    [(empty? l) '() ]
    [else (append (my-reverse1 (rest l)) (list (first l)))]))

;;exce #5
(define (full-reverse l)
  (cond
    [(empty? l) '() ]
    [else (cond
       [(list? (first l)) (append (full-reverse (rest l)) (list (full-reverse (first l))))]
       [else (append (full-reverse (rest l)) (list (first l)))])]))
;;exce #6
(define (depth l)
  (cond
    [(empty? l) 1]
    [else (cond
            [(list? l) (+ 1 (apply max (map depth l)))]
            [else 0])]))
;;exce #7
(define (my-filter f l)
  (cond
    [(empty? l) '() ]
    [else (cond
         [(f (first l)) (cons (first l) (my-filter f (rest l)))]
         [else (my-filter f (rest l))])]))