#lang racket
;;WANG YUMENG
(require dyoo-while-loop)

;;exercise 1

(define (even li)
  (let ([r '()]
        [cot 0])
     (while (not (empty? li))
            (cond
              [(even? cot)
               (set! r (cons (first li) r))
               (set! li (rest li))
               (set! cot (+ cot 1))]                                  
              [else
               (set! li (rest li))
               (set! cot (+ cot 1))]))
    (reverse r)))

(define (odd li)
  (let ([r '()]
        [cot 0])
     (while (not (empty? li))
            (cond
              [(odd? cot)
               (set! r (cons (first li) r))
               (set! li (rest li))
               (set! cot (+ cot 1))]                                  
              [else
               (set! li (rest li))
               (set! cot (+ cot 1))]))
    (reverse r)))

;;exercise 2
(define (even-tr li)
  (letrec ((rec (lambda (li r cot)          
     (cond
       [(empty? li) r]
       [else (cond
              [(even? cot)
               (rec (rest li) (cons (first li) r) (+ cot 1))]                              
              [else
               (rec (rest li) r (+ cot 1))])]))))
    (reverse (rec li '() 0))))

;;exercise 3
(define remove-sublist
  (lambda (li)          
    (cond [(empty? li) '()]
             [(list? (car li)) (append (remove-sublist (car li)) (remove-sublist (cdr li)))]
             [else (append (list (car li)) (remove-sublist (cdr li)))])))

;;exercise 4
 ;;(define (same-structure l1 l2)
 (define same-structure
  (lambda (l1 l2)
    (cond [(and (null? l1) (null? l2)) #t]
          [(not (eq? (length l1) (length l2))) #f]
          [(and (list? (car l1)) (list? (car l2))) (and (same-structure (car l1) (car l2)) (same-structure (cdr l1) (cdr l2)))]
          [(and (not (list? (car l1))) (not (list? (car l2)))) (same-structure (cdr l1) (cdr l2))]
          [else #f])))

;; exercise 5
(define compose1
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

(define compose2
  (lambda (f g)
    (lambda x
      (f (apply g x)))))
;; exercise 6
(define memo
  (lambda (f)
    (let ([res 0])
      (lambda x
        (when (> (length x) 0)
            (set! res (apply f x)))
        res))))
                     
 ;; exercise 7
(define-syntax mdisplay
  (lambda (stx)
    (let ([datum (cdr (syntax->datum stx))])
      (datum->syntax stx
                     `(begin ,@(map (lambda (expr)
                                    `(display ,expr))
                                    datum))))))
;;exercise 8
(define-syntax mdisplayln
  (lambda (stx)
    (let ([datum (cdr (syntax->datum stx))])
      (datum->syntax stx
                     `(begin ,@(map (lambda (expr)
                                    `(display ,expr))
                                    datum)
                                    (displayln ""))))))

  