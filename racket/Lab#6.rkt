;;Author:Wang Yumeng

#lang racket
(require dyoo-while-loop)
;;micro function
;;car=head
;;cdr=tail
;;cadr=the head of the tail
;;cdar

;;excs #1
(define-syntax sub!
  (lambda (stx)
    (let [(datum (cdr (syntax->datum stx)))]
      (datum->syntax stx
                     `(set! ,(car datum) (- ,(car datum) ,(cadr datum)))))))

;;excs #2
(define-syntax def-echo1
  (lambda (stx)
    (let [(datum (cdr (syntax->datum stx)))]
      (datum->syntax stx
                     `(define ,(car datum)
                              ,@(map (lambda (param)
                                         `(displayln ,param))
                                (cdar datum))
                        ,@(cdr datum))))))

;;excs #3
(define-syntax def-echo2
  (lambda (stx)
    (let [(datum (cdr (syntax->datum stx)))]
      (datum->syntax stx
                     `(define ,(car datum)
                              ,@(map (lambda (param)
                                         `(begin
                                            (display ',param)
                                            (display "=")
                                            (displayln ,param)))
                                (cdar datum))
                        ,@(cdr datum))))))

;;excs #4
(define-syntax def-doc
  (lambda (stx)
    (let [(datum (cdr (syntax->datum stx)))]
      (datum->syntax stx
                     `(define ,(car datum)
                        (cond
                          [(equal? ,@(cdar datum) '**doc**) (displayln ,(cadr datum))]
                          [else ,@(cdr datum)]))))))


;;excs #5
(define-syntax for-loop
  (lambda (stx)
    (let [(datum (cdr (syntax->datum stx)))]
      (datum->syntax stx
                     `(let [(,(car datum) ,(cadr datum))]
                        (while  ,(caddr datum)
                          ,@(cdr datum)
                          (set! ,(car datum) ,(cadddr datum))))))))
                          

;;excs #6
(define-syntax def-type1
   (lambda (stx)
    (let* [(datum (cdr (syntax->datum stx)))
          (judge (cdar datum))]
      (datum->syntax stx
                     `(define ,(caar datum)
                        (lambda (,(caar judge))
                          (cond
                            [(,@(cdar judge) ,(caar judge)) ,@(cdr datum)]
                            [else (displayln "; bad argument [,bt for context]")])))))))














                        
                     