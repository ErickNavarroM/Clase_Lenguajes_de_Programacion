#lang plait

;; Problema 1
(define-type Expr
  (num [n : Number])
  (plusC [m : Expr][n : Expr])
  (multC [m : Expr][n : Expr]))

(define-type ArithC
  ()
  

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

(define (desugar) 5)

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
