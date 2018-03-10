#lang racket

;;(parsing-tree (make-tree "powersupply.txt"))
(define (make-tree filepath)

  (letrec ([input (open-input-file filepath)]
    [rec (lambda (input)
           (cond
             [(equal? (read-line input) "Q:") (list (read-line input) (rec input) (rec input))]
             [else (list (read-line input))]))])
    (rec input)))

(define (parsing-tree tree)
  (cond
      [(= (length tree) 1)
       (displayln (first tree))]
      [else (displayln (first tree))
            (let ([line (read-line)])
              (cond
                 [(equal? line "yes") (parsing-tree (first (rest tree)))]
                 [(equal? line "no") (parsing-tree (last tree))]
                 [else
                  (displayln "Wrong answer, please input yes or no, try again!")
                  (parsing-tree tree)]))]))
