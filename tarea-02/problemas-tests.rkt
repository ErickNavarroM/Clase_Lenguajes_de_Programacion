#lang racket

(require rackunit
         rackunit/text-ui
         "problemas.rkt")

(define-test-suite pruebas
;; Problema 3
  (test-case "take"
    (check-equal? (take '(1 2 3 4 5 6) 3) '(1 2 3))
    (check-equal? (take '(1 2 3 4 5 6) 6) '(1 2 3 4 5 6))
    (check-equal? (take '(1 2 3 4 5 6) 0) '()))

  (test-case "drop"
    (check-equal? (drop '(1 2 3 4 5 6) 3) '(4 5 6))
    (check-equal? (drop '(1 2 3 4 5 6) 7) '())
    (check-equal? (drop '(1 2 3 4 5 6) 0) '(1 2 3 4 5 6)))

;; Problema 4
  (test-case "bundle2"
    (check-equal? (bundle2 (explode "abcdefg") 3) '("abc" "def" "g"))
    (check-equal? (bundle2 (explode "abcdefg") 8) '("abcdefg"))
    (check-equal? (bundle2 (explode "abcdefg") 1) '("a" "b" "c" "d" "e" "f" "g")))

;; Problema 6
  (test-case "list->chunks"
    (check-equal? (list->chunks (explode "abcdefg") 2) '(("a" "b") ("c" "d") ("e" "f") ("g")))
    (check-equal? (list->chunks (explode "abcdefg") 5) '(("a" "b" "c" "d" "e") ("f" "g")))
    (check-equal? (list->chunks (explode "abcdefg") 1) '(("a") ("b") ("c") ("d") ("e") ("f") ("g"))))

;; Problema 4
  (test-case "bundle3"
    (check-equal? (bundle3 (explode "abcdefg") 3) '("abc" "def" "g"))
    (check-equal? (bundle3 (explode "abcdefg") 8) '("abcdefg"))
    (check-equal? (bundle3 (explode "abcdefg") 1) '("a" "b" "c" "d" "e" "f" "g")))
  
;; Problema 7
   (test-case "partition"
    (check-equal? (partition "abcdefg" 3) '("abc" "def" "g"))
    (check-equal? (partition "abcdefg" 7) '("abcdefg"))
    (check-equal? (partition "abcdefg" 2) '("ab" "cd" "ef" "g")))

;; Problema 8
   (test-case "isort"
    (check-equal? (isort '() #f) '())
    (check-equal? (isort '(3 7 5 11 0) #t) '(0 3 5 7 11))
    (check-equal? (isort '(3 7 5 11 0) #f) '(11 7 5 3 0)))

;; Problema 10
   (test-case "smallers"
    (check-equal? (smallers '(1 2 3 4 5 6) 4) '(1 2 3))
    (check-equal? (smallers '(45 83 10 2 0 57) 50) '(45 10 2 0))
    (check-equal? (smallers '(1 2 3 4 4 5 6) 5) '(1 2 3 4 4)))

   (test-case "largers"
    (check-equal? (largers '(1 2 3 4 5 6) 4) '(5 6))
    (check-equal? (largers '(45 83 10 2 0 57) 50) '(83 57))
    (check-equal? (largers '(1 2 3 4 4 5 6) 5) '(6)))

;; Problema 11
   (test-case "equals"
    (check-equal? (equals '(1 2 3 4 5 6) 4) '(4))
    (check-equal? (equals '(3 9 5 0 5 9 5) 5) '(5 5 5))
    (check-equal? (equals '(1 2 3 4 5 6) 0) '()))

;; Problema 12
   (test-case "qsort"
    (check-equal? (qsort '() #f) '())
    (check-equal? (qsort '(3 7 5 11 0) #t) '(0 3 5 7 11))
    (check-equal? (qsort '(3 7 5 11 0) #f) '(11 7 5 3 0)))
   
;; Problema 13
   (test-case "bothsort"
    (check-equal? (bothsort '() #f) '())
    (check-equal? (bothsort '(3 7 5 11 0) #t) '(0 3 5 7 11))
    (check-equal? (bothsort '(3 7 5 11 0) #f) '(11 7 5 3 0)))

;; Problema 14
   (test-case "smallers2"
    (check-equal? (smallers2 '(1 2 3 4 5 6) 4) '(1 2 3))
    (check-equal? (smallers2 '(45 83 10 2 0 57) 50) '(45 10 2 0))
    (check-equal? (smallers2 '(1 2 3 4 4 5 6) 5) '(1 2 3 4 4)))

   (test-case "largers2"
    (check-equal? (largers2 '(1 2 3 4 5 6) 4) '(5 6))
    (check-equal? (largers2 '(45 83 10 2 0 57) 50) '(83 57))
    (check-equal? (largers2 '(1 2 3 4 4 5 6) 5) '(6)))

;; Problema 15
   (test-case "quicksort2"
    (check-equal? (quicksort2 '()) '())
    (check-equal? (quicksort2 '(3 7 5 11 0)) '(0 3 5 7 11))
    (check-equal? (quicksort2 '(56 55 3 0 45)) '(0 3 45 55 56)))

;; Problema 17
  (test-case "bundle4"
    (check-equal? (bundle4 (explode "abcdefg") 3) '("abc" "def" "g"))
    (check-equal? (bundle4 (explode "abcdefg") 8) '("abcdefg"))
    (check-equal? (bundle4 (explode "abcdefg") 1) '("a" "b" "c" "d" "e" "f" "g")))
  )
(run-tests pruebas 'verbose)