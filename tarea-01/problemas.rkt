#lang racket

;; Escribe aquí tus soluciones <   >

;; 1
(define (countdown n)
  (if (< n 0)
      '()
      (list* n (countdown (- n 1)))))

; Test 10 5 1 0 -1 -15

;; 2
(define (insertL a b ls)
  (if (empty? ls)
      '()
      (if (equal? (first ls) a)
          (append (list b a) (insertL a b (rest ls)))
          (append (list (first ls)) (insertL a b (rest ls))))))
               
;; 3
(define (remv-1st a ls)
  (if (empty? ls)
      '()
      (if (eqv? (first ls) a)
          (rest ls)
          (list* (first ls) (remv-1st a (rest ls))))))
; Test () (x) (a) (x a) (a x) (a a x a) (a x a x x)

;; 4
(define (map proc ls)
  (if (empty? ls)
      '()
      (cons (proc (first ls)) (map proc (rest ls)))))

;; 5
(define (filter pred ls)
  (if (empty? ls)
      '()
      (if (pred (first ls))
          (list* (first ls) (filter pred (rest ls)))
          (filter pred (rest ls)))))

;; 6
(define (zip ls1 ls2)
  (if (or (empty? ls1) (empty? ls2))
      '()
      (list* (cons (first ls1) (first ls2)) (zip (rest ls1) (rest ls2)))))

;; 7
(define (list-index-ofv sym ls)
  (if (empty? ls)
      -1
      (if (eqv? (first ls) sym)
          0
          (+ 1 (list-index-ofv sym (rest ls))))))

 ; homogenizar nombramientos

;; 8
(define (append ls1 ls2)
  (if (empty? ls1)
      ls2
      (cons (first ls1) (append (rest ls1) ls2))))
 

;; 9
(define (reverse ls)
  (if (empty? ls)
      '()
      (append (reverse (rest ls)) (list (first ls)))))

;; 10
(define (repeat ls n)
  (if (equal? n 1)
      ls
      (list* ls (repeat ls (- n 1)))))
; corregir presentación de salida (repeat '(a b c d e) 3)

;; 11
(define (same-lists* ls1 ls2)
  (if (eqv? ls1 ls2) #t #f))

;; 13
(define (binary->natural ls)
  (if (empty? ls)
      0
      (+ (* (first ls) (expt 2 0)) (* 2 (binary->natural (rest ls))))))

;; (binary->natural '(0 0 1 0 1 1 0 0))

;; 14
(define (div dividendo divisor)
  (cond
    [(equal? dividendo 0) 0]
    [(< dividendo 0) (error "División no exacta.")]
    [(> dividendo 0) (+ 1 (div (- dividendo divisor) divisor))]))

;; 15
(define (append-map proc ls)
  (if (empty? ls)
      '()
      (append (proc (first ls)) (append-map proc (rest ls)))))

;; 16
(define (set-difference ls1 ls2)
  (if (empty? ls1)
      '()
      (if (ormap (lambda (n) (eq? n (first ls1))) ls2)
          (set-difference (rest ls1) ls2)
          (cons (first ls1) (set-difference (rest ls1) ls2)))))
;(set-difference '(1 2 3 4) '(1 9 3 5 6))
;(set-difference '(1 2 3 4 11 9) '(1 9 3 5 6))
;(set-difference '(1 2 3 4) '())
;(set-difference '() '(1 9 3 5 6))
;(set-difference '() '())

;; 17
(define (foldr proc init ls)
  (if (empty? ls)
      init
      (proc (first ls) (foldr proc init (rest ls)))))
  

;; 18
(define (powerset ls)
  (if (empty? ls)
      (list ls)
      (let ([x (powerset (rest ls))])
        (append (map (lambda (n) (cons (first ls) n)) x) x))))
      ;(append (map (lambda (n) (cons (first ls) n)) (powerset (rest ls))) (powerset (rest ls)))))
      ;(append (map (lambda (n) (cons 3 n)) ls) ls)))
      ;(map (lambda (n) (cons (first ls) n)) (powerset (rest ls)))))
      ;(map (lambda (n) (cons 3 n)) ls)))
      ;(cons (list (first ls)) (list (rest ls))))) ;a
      ;(cons (list 42 (first ls)) (powerset (rest ls)))))
; (powerset '((2 1) (2) (1) ()))

;; 19
(define (cartesian-product ls)
  (if (or (empty? (first ls)) (empty? (second ls)))
      '()
      (append (map (lambda (n) (list (first (first ls)) n)) (second ls)) (cartesian-product (list (rest (first ls)) (second ls))))))

#|(define (cartesian-product ls1 ls2)
  (if (or (empty? ls1)) (empty? ls2))
      '()
      (append (map (lambda (n) (list (first ls1) n)) ls2) (cartesian-product (rest ls1) ls2)))
(define (cartesian-product ls)
  (if (or (empty? (car ls)) (empty? (cdr ls)))
      '()
      (append (map (lambda (n) (list (first (car ls)) n)) (cdr ls)) (cartesian-product (list (rest (car ls)) (cdr ls))))))|#
;; 20 (foldr)
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#|(define (foldr proc init ls)
  (if (empty? ls)
      init
      (proc (first ls) (foldr proc init (rest ls)))))
|#

(define (insertL-fr a b ls)
  (foldr (lambda (x y)
           (if (equal? x a)
               (cons b (cons a y))
               (cons x y)))
           null
           ls))

(define (filter-fr pred ls)
  (foldr (lambda (x y)
           (if (pred x)
               (cons x y)
               y))
         null
         ls))

(define (map-fr proc ls)
  (foldr (lambda (x y)
           (cons (proc x) y))
         null
         ls))
#|
(define (map proc ls)
  (if (empty? ls)
      '()
      (cons (proc (first ls)) (map proc (rest ls)))))

(define (append-fr ))

(define (reverse-fr ))

(define (insertL-fr ))

(define (insertL-fr ))

(define (insertL-fr ))

(define (insertL-fr ))|#

;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; 21
#|#(define (snowball n)
  (if (equal? n 0)
      0
      (if (equal? 0 (modulo n 2))
          (snowball (/ n 2))
          (snowball (+ (* 3 n) 1)))))|#
 
(define snowball
    (letrec
        ((odd-case
          (lambda (fix-odd)
            (lambda (x)
              (cond
                ((and (exact-integer? x) (positive? x) (odd? x))
                 (snowball (add1 (* x 3))))
                (else (fix-odd x))))))
         (even-case
          (lambda (fix-even)
            (lambda (x)
              (cond
                ((and (exact-integer? x) (positive? x) (even? x))
                 (snowball (/ x 2)))
                (else (fix-even x))))))
         (one-case
          (lambda (fix-one)
            (lambda (x)
              (cond
                ((zero? (sub1 x)) 1)
                (else (fix-one x))))))
         (base
          (lambda (x)
            (snowball (error 'error "Invalid value ~s~n" x)))))
      (one-case (odd-case (even-case base)))))

(provide (all-defined-out))
