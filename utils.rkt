#lang racket

;; misc. functions

(provide (all-defined-out))

(define separator (make-string 80 #\-))
(define (print-separator) (displayln separator))
