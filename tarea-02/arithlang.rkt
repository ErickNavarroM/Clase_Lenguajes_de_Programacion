
#lang plait

;; Problema 1
(define (interp [c : ArithC]) : Number
  (type-case ArithC c
    [(numC n) n]
    [(plusC n m) (+ (interp n) (interp m))]
    [(multC n m) (* (interp n) (interp m))]))

(define-type ArithC
  (numC [n : Number])
  (plusC [n : ArithC] [m : ArithC])
  (multC [n : ArithC] [m : ArithC]))

(define (desugar [s : ArithS]) : ArithC
  (type-case ArithS s
    [(numS n) (numC n)]
    [(plusS n m) (plusC (desugar n) (desugar m))]
    [(multS n m) (multC (desugar n) (desugar m))]
    [(bminusS n m) (plusC (desugar n) (multC (numC -1) (desugar m)))]
    [(uminusS n) (multC (numC -1) (desugar n))]))

(define-type ArithS
  (numS [n : Number])
  (plusS [n : ArithS] [m : ArithS])
  (multS [n : ArithS] [m : ArithS])
  (bminusS [n : ArithS] [m : ArithS])
  (uminusS [n : ArithS]))

(define (parse [s : S-Exp]) : ArithS
  (cond [(s-exp-number? s) (numS (s-exp->number s))]
        [(s-exp-list? s)
         (let ([ls (s-exp->list s)])
           (if (equal? (length ls) 2)
               (uminusS (parse (second ls)))
               (case (s-exp->symbol (first ls))
                 [(+) (plusS (parse (second ls)) (parse (third ls)))]
                 [(*) (multS (parse (second ls)) (parse (third ls)))]
                 [(-) (bminusS (parse (second ls)) (parse (third ls)))]
                 [else (error 'parse "operación aritmética malformada")])))]
        [else (error 'parse "expresión aritmética malformada")]))

(define (eval [input : S-Exp]) : Number
  (interp (desugar (parse input))))
