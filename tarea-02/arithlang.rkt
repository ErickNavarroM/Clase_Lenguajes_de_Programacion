#lang plait

;; Problema 1
(define-type ArithC
  (numC [n : Number])
  (plusC [p : Expr] [q : Expr])
  (multC [p : Expr] [q : Expr])
  (condC [p : Expr] [q : Expr] [r : Expr]))

(define-type ArithS
  (numS [n : Number])
  (plusS [p : Expr] [q : Expr])
  (multS [p : Expr] [q : Expr])
  (bminusS [p : Expr] [q : Expr])
  (uminusS [p : Expr])
  (negS [p : Expr])
  (disjS [p : Expr] [q : Expr])
  (conjS [p : Expr] [q : Expr])
  (condS [p : Expr] [q : Expr] [r : Expr]))

(define (interp [c : ArithC]) : Number
  (type-case ArithC c
    [(numC n) n]
    [(plusC p q) (+ p q)]
    [(multC p q) (* p q)]
    [(condC p q r) (if p q r)]))

(define (desugar [s : ArithS]) : ArithC
  (type-case ArithS s
    [(numS n) (numC n)]
    [(plusS p q) (plusC p q)]
    [(multS p q) (multC p q)]
    [(bminusS p q) (plusC p (uminusS q))]
    [(uminusS p) (plusC p )]
    [(negS p) (condC p #f #t)]
    [(disjS p q) (condC p #t )]
    [(conjS p q) (condC )]
    [(condS p q r) (condC p q r)]))

(define (parse [s : S-Exp]) : ArithS
  (cond [(s-exp-number? s) (numC (s-exp->number s))]
        [(s-exp-list? s)
         (let ([ls (s-exp->list s)])
           (case (s-exp->symbol (first ls))
             [(+) (plusS (parse (second ls)) (parse (third ls)))]
             [(*) (multS (parse (second ls)) (parse (third ls)))]
             [(-) (bminusS (parse (second ls)) (parse (third ls)))]
             [(-) (uminusS (parse (second ls)))]
             [(not) (negS (parse (second ls)))]
             [(or) (disjS (parse (second ls)) (parse (third ls)))]
             [(and) (conjS (parse (second ls)) (parse (third ls)))]
             [(if) (condS (parse (second ls)) (parse (third ls)) (parse (fourth ls)))]
             [else (error 'parse ”operación aritmética malformada”)]))]
        [else (error 'parse ”expresión aritmética malformada”)]))

(define (interp) 6)

(define (eval [input : S-Exp]) : Number
  (interp (desugar (parse input))))

;; Problema 2

;; Problema 3

;; Problema 4

;; Problema 5

;; Problema 6

;; Problema 7

;; Problema 8

;; Problema 9

;; Problema 10

;; Problema 11

;; Problema 12

;; Problema 13

;; Problema 14

;; Problema 15

;; Problema 16

;; Problema 17

;; Problema 18

;; Problema 19

;; Problema 20

;; Problema 21

;; Problema 22

;; Problema 23
