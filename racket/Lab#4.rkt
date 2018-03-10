#lang racket
;;(define t1 (readtree "powersupply.txt" ))
;;(diagnose t1)
;;(yesnogame "powersupply.txt" )

;;function
;;read-line
;;display
;;display



;;(let ((input (open-input-file filepath)
;;             (...readline input)

;;(let ((output (open-input-file filepath)
;;   

(define tree '("Are you hungry?" ("Meat?" ("Chicken") ("Tomatoes?" ("Tomatoes") ("Potatoes"))) ("Are you thirsty?" ("Water") ("None"))))

(define (parsing-tree tree)
  (cond
      [(= (length tree) 1) (displayln (first tree))]
      [else (displayln (first tree))
            (let ([line (read-line)])
              (cond
                 [(equal? line "yes") (parsing-tree (first (rest tree)))]
                 [(equal? line "no") (parsing-tree (last tree))]
                 [else
                  (displayln "Wrong answer, try again!")
                  (parsing-tree tree)]))]))

(parsing-tree tree)
