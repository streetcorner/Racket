#lang racket

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
       (begin
         (display "is ")
         (display first tree)
         (displayln " your object?"))
       (let ([result (read-line)])
         (cond
           [(equal? result "yes") (displayln "Got it!!!")]
           [(equal? result "no")
            (displayln "What is your object?")
            (let [(ans read-line)]
              (displayln "Give a question to distinguish between my object and your object:")
              
            ]
           [else (displayln "Wrong answer, please input yes or no, try again!")
                  (parsing-tree tree)]))]
      [else (displayln (first tree))
            (let ([line (read-line)])
              (cond
                 [(equal? line "yes") (parsing-tree (first (rest tree)))]
                 [(equal? line "no") (parsing-tree (last tree))]
                 [else
                  (displayln "Wrong answer, please input yes or no, try again!")
                  (parsing-tree tree)]))]))
;;(parsing-tree (make-tree "animals.txt"))