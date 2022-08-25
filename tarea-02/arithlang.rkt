#lang plait

;; Problema 1
(define (parse [s : S-Exp]) : ArithC
  (cond [(s-exp-number? s) (numC (s-exp->number s))]
        [(s-exp-list? s)
         (let ([ls (s-exp->list s)])
           (case (s-exp->symbol (first ls))
             [(+) (plusC (parse (second ls)) (parse (third ls)))]
             [(*) (multC (parse (second ls)) (parse (third ls)))]
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
