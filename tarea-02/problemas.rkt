
#lang racket
(require racket/trace
         pict
         racket/draw)

;; Problema 2
(define (unit-string? x)
  (and (string? x)
       (= (string-length x) 1)))

(define (unit-string-list? x)
  (or (null? x)
      (and (pair? x)
           (string? (first x))
           (= (string-length (first x)) 1)
           (unit-string-list? (rest x)))))

(define (explode s)
  (unless (string? s)
    (error 'explode "esperaba una cadena, pero recibí: ~e" s))
  (map string (string->list s)))

(define (implode ls)
  (unless (unit-string-list? ls)
    (error 'implode "esperaba una lista de cadenas unitarias, pero recibí: ~e" ls))
  (apply string-append ls))
      
      
;; Problema 3
;; take : (listof Symbol?) Number? -> (listof String)
(define (take ls n)
  (if (or (empty? ls) (= n 0))
      '()
      (cons (first ls) (take (rest ls) (sub1 n)))))

;; drop : (listof Symbol?) Number? -> (listof String)
(define (drop ls n)
  (if (or (empty? ls) (= n 0))
      ls
      (drop (rest ls) (sub1 n))))

;; Problema 4
(define (bundle2 s n)
  (cond
    [(null? s) null]
    [else
     (cons (implode (take s n))
           (bundle2 (drop s n) n))]))

;; Problema 6
;; list->chunks : (listof Symbol?) Number? -> (listof String)
(define (list->chunks ls n)
  (if (empty? ls)
      '()
      (cons (take ls n) (list->chunks (drop ls n) n))))

(define (bundle3 ls n)
  (map implode (list->chunks ls n)))

;; Problema 7
;; partition : (listof Symbol?) Number? -> (listof String)
(define (partition s n)
  (if (empty? s)
      #t
      (if (<= (string-length s) n)
          (cons (substring s 0 (string-length s)) '())
          (cons (substring s 0 n) (partition (substring s n (string-length s)) n)))))

;; Problema 8
;; isort : (listof Number?) Boolean? -> (listof Number)
(define (isort ls asc?)
  (let ([x (if (empty? ls)
      null
      (insert (first ls)
              (isort (rest ls) #f)))])
    (if (equal? asc? #t)
        (reverse x)
        x)))

(define (insert n ls)
  (cond
    [(empty? ls) (list n)]
    [(>= n (first ls)) (cons n ls)]
    [else (cons (first ls) (insert n (rest ls)))]))

;; Problema 10
(define (quicksort ls)
  (cond
    [(empty? ls) null]
    [else
     (define pivot (first ls))
     (append (quicksort (smallers ls pivot))
             (equals ls pivot)
             (quicksort (largers ls pivot)))]))

;; smallers : (listof Number?) Number? -> (listof Number)
(define (smallers ls pivot)
  (if (empty? ls)
      '()
      (if (< (first ls) pivot)
          (cons (first ls) (smallers (rest ls) pivot))
          (smallers (rest ls) pivot))))

;; largers : (listof Number?) Number? -> (listof Number)
(define (largers ls pivot)
  (if (empty? ls)
      '()
      (if (> (first ls) pivot)
          (cons (first ls) (largers (rest ls) pivot))
          (largers (rest ls) pivot))))

;; Problema 11
;; equals : (listof Number?) Number? -> (listof Number)
(define (equals ls pivot)
  (if (empty? ls)
      '()
      (if (= (first ls) pivot)
          (cons (first ls) (equals (rest ls) pivot))
          (equals (rest ls) pivot))))

;; Problema 12
;; qsort : (listof Number?) Boolean? -> (listof Number)
(define (qsort ls asc?)
  (let ([x (cond
    [(empty? ls) null]
    [else
     (define pivot (first ls))
     (append (quicksort (smallers ls pivot))
             (equals ls pivot)
             (quicksort (largers ls pivot)))])])
    (if (equal? asc? #t)
        x
        (reverse x))))

;; Problema 13
;; bothsort : (listof Number?) Boolean? -> (listof Number)
(define (bothsort ls asc?)
  (if (> (length ls) 1200)
      (qsort ls asc?)
      (isort ls asc?)))

;; Problema 14
;; smallers2 : (listof Number?) Number? -> (listof Number)
(define (smallers2 ls pivot)
  (filter (lambda (n) (< n pivot)) ls))

;; largers2 : (listof Number?) Number? -> (listof Number)
(define (largers2 ls pivot)
  (filter (lambda (n) (> n pivot)) ls))

;; Problema 15
;; quicksort2 : (listof Number?) -> (listof Number)
(define (quicksort2 ls)
  (cond
    [(empty? ls) null]
    [else
     (define pivot (first ls))
     (append (quicksort (filter (lambda (n) (< n pivot)) ls))
             (filter (lambda (n) (= n pivot)) ls)
             (quicksort (filter (lambda (n) (> n pivot)) ls)))]))

;; Problema 17
(define (bundle4 s n)
  (cond
    [(= n 0) (error "Argumento no válido.")]
    [(null? s) null]
    [else
     (cons (implode (take s n))
           (bundle4 (drop s n) n))]))

;; Problema 18

(define (smallers3 l n)
  (cond
    [(empty? l) '()]
    [else (if (<= (first l) n)
              (cons (first l) (smallers (rest l) n))
              (smallers (rest l) n))]))

;; Problema 19

(define (gcd-structural n m)
  (define (find-largest-divisor k)
    (cond [(= k 1) 1]
          [(= (remainder n k) (remainder m k) 0) k]
          [else (find-largest-divisor (- k 1))]))
  (find-largest-divisor (min n m)))

;; Problema 20

(define (gcd-generative n m)
  (define (find-largest-divisor max min)
    (if (= min 0)
        max
        (find-largest-divisor min (remainder max min))))
  (find-largest-divisor (max n m) (min n m)))

;; Problema 23
;; spki-carpet : Number? -> Pict
(define (spki-carpet side)
  (cond [(<= side 4) (square side 1 "red")]
        [else
         (define half (spki-carpet (/ side 3)))
         (vc-append (hc-append half half half) (hb-append (vc-append half half) half (vc-append half half)))]))

;; square : Number? Number? String? -> Pict
(define (square side width color)
  (define w side)
  (define h side)
  (define (draw-it ctx dx dy)
    (define prev-pen (send ctx get-pen))
    (define path (new dc-path%))
    (send ctx set-pen (new pen% [width width] [color color]))
    (send path move-to 0 0)
    (send path line-to w 0)
    (send path line-to w h)
    (send path line-to 0 h)
    (send path close)
    (send path move-to (/ w 3) (/ h 3))
    (send path line-to (* 2 (/ w 3)) (/ h 3))
    (send path line-to (* 2 (/ w 3)) (* 2 (/ h 3)))
    (send path line-to (/ w 3) (* 2 (/ h 3)))
    (send path close)
    (send ctx draw-path path dx dy)
    (send ctx set-pen prev-pen))
  (dc draw-it w h))

(provide (all-defined-out))