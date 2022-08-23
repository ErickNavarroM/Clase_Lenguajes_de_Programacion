#lang plait

(define (is-odd? x)
  (if (zero? x)
      #f
      (is-even? (- x 1))))
 
(define (is-even? x)
  (if (zero? x)
      #t
      (is-odd? (- x 1))))
