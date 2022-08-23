#lang racket

(require rackunit
         rackunit/text-ui
         "problemas.rkt")

(define-test-suite pruebas
  ;; 1
  (test-case "countdown"
    (check-equal? (countdown 0)
                  '(0))
    (check-equal? (countdown 1)
                  '(1 0))
    (check-equal? (countdown 5)
                  '(5 4 3 2 1 0))
    (check-equal? (countdown 10)
                  '(10 9 8 7 6 5 4 3 2 1 0))
    (check-equal? (countdown -1)
                  '())
    (check-equal? (countdown -15)
                  '())))

;; 2
  (test-case "insertL"
    (check-equal? (insertL 'x 'y '())
                  '())
    (check-equal? (insertL 'x 'y '(x z z x y x))
                  '(y x z z y x y y x))
    (check-equal? (insertL 'x 'y '(z y y z z z))
                  '(z y y z z z))
    (check-equal? (insertL 'x 'y '(x x x x x x))
                  '(y x y x y x y x y x y x)))

;; 3
  (test-case "remv-1st"
    (check-equal? (remv-1st 'x '())
                  '())
    (check-equal? (remv-1st 'x '(x y))
                  '(y))
    (check-equal? (remv-1st 'x '(y x))
                  '(y))
    (check-equal? (remv-1st 'x '(x y z x))
                  '(y z x))
    (check-equal? (remv-1st 'x '(z y x y))
                  '(z y y))
    (check-equal? (remv-1st 'y '(y x z y x))
                  '(x z y x))
    (check-equal? (remv-1st 'z '(a b c))
                  '(a b c)))

;; 4
  (test-case "map"
    (check-equal? (map add1 '())
                  '())
    (check-equal? (map sub1 '(1 2 3 4))
                  '(0 1 2 3))
    (check-equal? (map add1 '(1 2 3 4))
                  '(2 3 4 5))
    (check-equal? (map abs '(-11 2 -1 0 4 9))
                  '(11 2 1 0 4 9))
    (check-equal? (map even? '(4 3 16 67 5 6))
                  '(#t #f #t #f #f #t)))

;; 5
  (test-case "filter"
    (check-equal? (filter even? '())
                  '())
    (check-equal? (filter even? '(1 2 3 4 5 6))
                  '(2 4 6))
    (check-equal? (filter odd? '(1 2 3 4 5 6))
                  '(1 3 5))
    (check-equal? (filter positive? '(0 1 -2 -3 4 -5 6))
                  '(1 4 6))
    (check-equal? (filter list? '((cons 1 '(2)) 3 '(4) 5 '(6 7) 8 9))
                  '((cons 1 '(2)) '(4) '(6 7))))

;; 6
  (test-case "zip"
    (check-equal? (zip '(1 2 3) '())
                  '())
    (check-equal? (zip '() '(a b c))
                  '())
    (check-equal? (zip '(1 2 3) '(a b c))
                  '((1 . a) (2 . b) (3 . c)))
    (check-equal? (zip '(1 2 3 4 5 6) '(a b c))
                  '((1 . a) (2 . b) (3 . c)))
    (check-equal? (zip '(1 2 3) '(a b c d e f))
                  '((1 . a) (2 . b) (3 . c))))

;; 7
  (test-case "list-index-ofv"
    (check-eqv? (list-index-ofv 'x '()) -1)
    (check-eqv? (list-index-ofv 'x '(x y z y)) 0)
    (check-eqv? (list-index-ofv 'x '(y z x y)) 2)
    (check-eqv? (list-index-ofv 'x '(y z z y y x)) 5)
    (check-eqv? (list-index-ofv 'x '(y z x x y x)) 2))

;; 8
  (test-case "append"
    (check-equal? (append '() '(1 2 3))
                  '(1 2 3))
    (check-equal? (append '(a b c) '())
                  '(a b c))
    (check-equal? (append '(a b c) '(1 2 3))
                  '(a b c 1 2 3))
    (check-equal? (append '(42 120) '(7 8 9))
                  '(42 120 7 8 9))
    (check-equal? (append '(a b c) '(cat dog))
                  '(a b c cat dog)))

;; 9
  (test-case "reverse"
    (check-equal? (reverse '())
                  '())
    (check-equal? (reverse '(a x))
                  '(x a))
    (check-equal? (reverse '(a 3 x))
                  '(x 3 a))
    (check-equal? (reverse '(a a x x))
                  '(x x a a))
    (check-equal? (reverse '(a g u a m a r i n a))
                  '(a n i r a m a u g a)))

;; 10
  (test-case "repeat"
    (check-equal? (repeat '(1 2 3) 0)
                  '())
    (check-equal? (repeat '(1 2 3) 1)
                  '(1 2 3))
    (check-equal? (repeat '(a b c d) 3)
                  '(a b c d a b c d a b c d))
    (check-equal? (repeat '(cat dog) 7)
                  '(cat dog cat dog cat dog cat dog cat dog cat dog cat dog)))

;; 11
  (test-case "same-lists*"
    (check-true (same-lists* '() '()))
    (check-true (same-lists* '(1 2 3 4 5) '(1 2 3 4 5)))
    (check-false (same-lists* '(1 2 3 4) '(1 2 3 4 5)))
    (check-false (same-lists* '(a (b c) d) '(a (b) c d)))
    (check-true (same-lists* '((a) b (c d) d) '((a) b (c d) d))))

;; 12
  (test-case "12"
    (check-equal? (cons (cons 'w (cons 'x '())) (cons 'y (cons (cons 'z '()) '()))) '((w x) y (z))))

;; 13
  (test-case "binary->natural"
    (check-eqv? (binary->natural '()) 0)
    (check-eqv? (binary->natural '(0 0 1)) 4)
    (check-eqv? (binary->natural '(0 0 1 1)) 12)
    (check-eqv? (binary->natural '(1 1 1 1)) 15)
    (check-eqv? (binary->natural '(1 0 1 0 1)) 21)
    (check-eqv? (binary->natural '(1 1 0 0 0 0 1)) 67)
    (check-eqv? (binary->natural '(1 1 1 1 1 1 1 1 1 1 1 1 1)) 8191))

;; 14
  (test-case "div"
    (check-eqv? (div 120 1) 120)
    (check-eqv? (div 25 5) 5)
    (check-eqv? (div 42 6) 7)
    (check-eqv? (div 36 6) 6))

;; 15
  (test-case "append-map"
    (check-equal? (append-map sub1 '())
                  '())
    (check-equal? (append-map add1 '(7 5 3))
                  '(8 6 4))
    (check-equal? (append-map range '(1 2 3 4))
                  '(0 0 1 0 1 2 0 1 2 3))
    (check-equal? (append-map countdown (countdown 5))
                  '(5 4 3 2 1 0 4 3 2 1 0 3 2 1 0 2 1 0 1 0 0)))

;; 16
  (test-case "set-difference"
    (check-equal? (set-difference '() '(2 6 4 8))
                  '())
    (check-equal? (set-difference '(1 3 5) '())
                  '(1 3 5))
    (check-equal? (set-difference '(1 3 5) '(2 4 6))
                  '(1 3 5))
    (check-equal? (set-difference '(1 2 3 4 5) '(2 6 4 8))
                  '(1 3 5)))

;; 17
  (test-case "foldr"
    (check-equal? (foldr cons '() '(1 2 3 4))
                  '(1 2 3 4))
    (check-eqv? (foldr + 0 '(1 2 3 4))
                10)
    (check-eqv? (foldr * 1 '(1 2 3 4))
                24)
    (check-eqv? (foldr (lambda (m n) (sub1 (* 3 m n))) 1 '(1 2 3))
                140))
  
  (test-case "powerset"
    (check-equal? (powerset '(3 2 1))
                  '((3 2 1) (3 2) (3 1) (3) (2 1) (2) (1) ()))
    (check-equal? (powerset '())
                  '(()))
    (check-equal? (powerset '(a b))
                  '((a b) (a) (b) ())))
  
  (test-case "cartesian-product"
    (check-equal? (cartesian-product '((5 4) (3 2 1)))
                  '((5 3) (5 2) (5 1) (4 3) (4 2) (4 1))))
  
  (test-case "snowball"
    (check-eqv? (snowball 12) 1)
    (check-eqv? (snowball 120) 1)
    (check-eqv? (snowball 9999) 1))
  
 ; (test-case "snowball"
  ;  (let ((ns (make-base-namespace)))
   ;   (check-equal? (eval quine ns) quine)
   ;   (check-equal? (eval (eval quine ns) ns) quine))))

(run-tests pruebas 'verbose)
