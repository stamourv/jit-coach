#lang racket

;; misc. functions

(provide (all-defined-out))

(define separator (make-string 80 #\-))
(define (print-separator) (displayln separator))

(define (all-the-same? l)
  (cond [(empty? l) #t]
        [else
         (define head (first l))
         (andmap (lambda (x) (equal? x head)) (rest l))]))
