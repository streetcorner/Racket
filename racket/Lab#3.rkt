;;Author:Wang Yumeng
#lang racket
(require dyoo-while-loop)

;;;excs #6
(define (my-filter2 f l)
  (letrec ((rec (lambda (f l r)
                  (cond
                    [(empty? l) r]
                    [else (cond
                            [(f (first l)) (rec f (rest l)(cons (first l) r))]
                            [else (rec f (rest l) r)])]))))
    (reverse (rec f l '()))))
;;excs #7
(define (zip l1 l2)
  (cond
    [(and (empty? l1) (empty? l2)) '()]
    [(and (empty? l1) (not (empty? l2))) (append (list (first l2)) (zip l1 (rest l2)))]
    [(and (empty? l2) (not (empty? l1))) (append (list (first l1)) (zip (rest l1) l2))]
    [else (append (cons (first l1) (list (first l2))) (zip (rest l1) (rest l2)))]))

(define (zip1 l1 l2)
  (let ((r '()))
    (while (and (not (null? l1)) (not (null? l2)))
             (set! r (append r (list (first l1)) (list (first l2))))
             (set! l1 (rest l1))
             (set! l2 (rest l2)))    
      (cond [(not (null? l1)) (append r l1)]
            [(not (null? l2)) (append r l2)]
            [else r])))
    

(define (zip2 l1 l2)
  (letrec ((rec (lambda (l1 l2 r)
             (cond
               [(empty? l1) (append r l2)]
               [(empty? l2) (append r l1)]
               [else (rec (rest l1) (rest l2) (append r (list (first l1)) (list (first l2))))]))))
    (rec l1 l2 '())))

;;excs #8
(define (unzip l)
  (letrec ((rec (lambda (l l1 l2)
                       (cond
                         [(empty? l) (list l1 l2)]
                         [else (cond
                                 [(> (length l) 1) (rec (rest (rest l)) (append l1 (list (first l))) (append l2 (list (first (rest l)))))]
                                 [else (rec (rest l) (append l1 (list (first l))) l2)])]))))
    (rec l '() '())))
;;

;;excs #9

(define (repeat x n)
  (letrec ((rec (lambda (x n r)
           (cond
             [(= n 0) r]
           [else (rec x (- n 1) (cons x r))]))))
     (rec x n '())))

(define (expand-list1 l)
  (let ([r '()])
    (while (not (empty? l))
          (cond
            [(list? (first l))
                     (set! r (append r (repeat (first (first l)) (last (first l)))))]
            [else (set! r (append r (list (first l))))])
            (set! l (rest l)))
    r))

(define (expand-list2 l)
  (letrec ((rec (lambda (l r)
                  (cond
                    [(empty l) r]
                    [else (cond
                            [(list? (first l)) (rec (rest l) (append r (repeat (first (first l)) (last (first l)))))]
                            [else (rec (rest l) (append r (list (first l))))])]))))
    (rec l '())))
  

  
  

                               
               
               
                    
                   

    
