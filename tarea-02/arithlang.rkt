
#lang plait

;; Problema 1
<<<<<<< HEAD
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
=======
(define (interp [c : ArithC]) : Number
  (type-case ArithC c
    [(numC n) n]
    [(plusC n m) (+ (interp n) (interp m))]
    [(multC n m) (* (interp n) (interp m))]))

(define-type ArithC
  (numC [n : Number])
  (plusC [n : ArithC] [m : ArithC])
  (multC [n : ArithC] [m : ArithC]))
>>>>>>> workprocess

(define (desugar [s : ArithS]) : ArithC
  (type-case ArithS s
    [(numS n) (numC n)]
<<<<<<< HEAD
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
=======
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
>>>>>>> workprocess

(define (eval [input : S-Exp]) : Number
  (interp (desugar (parse input))))
