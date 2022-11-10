;;; -*- mode: racket; coding: utf-8 -*-
;;; 
;;; `7MMF'      `7MM"""YMM  `YMM'   `MP' 
;;;   MM          MM    `7    VMb.  ,P   
;;;   MM          MM   d       `MM.M'    
;;;   MM          MMmmMM         MMb     
;;;   MM      ,   MM   Y  ,    ,M'`Mb.   
;;;   MM     ,M   MM     ,M   ,P   `MM.  
;;; .JMMmmmmMMM .JMMmmmmMMM .MM:.  .:MMa.
;;; 
;;; Lexical Analyzer for an arithmetic language

#lang racket/base

(require "lex-gen.rkt")

(module+ test
  (require rackunit
           rackunit/text-ui)

  (port-count-lines-enabled #t))

(struct token (name value beg end)
  #:transparent)

(define reg-nat
  (reg-repeat 1 +inf.0 (char-set "0123456789")))

(define reg-float
  (reg-conc reg-nat "." reg-nat))

(define reg-almost-float
  (reg-union (reg-conc "." reg-nat)
             (reg-conc reg-nat ".")))

(define (lex-numeric name)
  (lambda (src lexeme beg end)
    (token name (string->number lexeme) beg end)))

(define (lex-almost-float src lexeme beg end)

  (error 'arith-parse
         (string-append
          "malformed input at line ~v column ~v: unexpected ~v\n"
          "  maybe you forgot a digit before or after the dot?")
         (pos-line beg)
         (pos-col beg)
         lexeme))

(define reg-op
  (char-set "+-*^/%"))

(define (lex-op src lexeme beg end)
  (token 'op (string->symbol lexeme) beg end))

(define reg-ignore
  (reg-repeat 1 +inf.0 just-whitespace))

(define (lex-ignore src lexeme beg end)
  (lex-arith src))

(define (lex-delimiter name)
  (lambda (src lexeme beg end)
    (token name #f beg end)))

;(define reg-let
;  (reg-conc "l" (reg-conc "e" "t")))

(define (lex-func name)
  (lambda (src lexeme beg end)
    (token 'id name beg end)))

(define reg-var
  (reg-repeat 0 +inf.0 (reg-union (char-range "a" "z")
                                  (char-range "A" "Z"))))

(define (lex-var src lexeme beg end)
  (token 'var (string->symbol lexeme) beg end))

(define reg-bool
  (reg-union "#t" "#f"))

(define (lex-bool name)
  (lambda (src lexeme beg end)
    (token 'bool name beg end)))

(define lex-arith
  (make-lexer
   'arith
   (lex-rule reg-nat (lex-numeric 'nat))
   (lex-rule reg-float (lex-numeric 'float))
   (lex-rule "#t" (lex-bool '#t))
   (lex-rule "#f" (lex-bool '#f))
   (lex-rule reg-op lex-op)
   ;; procedures
   (lex-rule "zero?" (lex-func 'zero?))
   (lex-rule "if" (lex-func 'if))
   (lex-rule "then" (lex-func 'then))
   (lex-rule "else" (lex-func 'else))
   (lex-rule "let" (lex-func 'let))
   (lex-rule "in" (lex-func 'in))
   ;; variables
   (lex-rule reg-var lex-var)
   ;; delimiters
   (lex-rule "(" (lex-delimiter 'opar))
   (lex-rule ")" (lex-delimiter 'cpar))
   ;; ignorables
   (lex-rule reg-ignore lex-ignore)
   ;; common errors
   (lex-rule reg-almost-float lex-almost-float)))

(define (lex-arith* src)
  (define t (lex-arith src))
  (if (eof-object? t)
      null
      (cons t (lex-arith* src))))

(define (lex str)
    (map (lambda (t)
           (list (token-name t) (token-value t)))
         (lex-arith* (open-input-string str))))

(module+ test
  (run-tests
   (test-suite
    "arith lex test"
    
    (check-equal? (lex "0") '((nat  0)))
    (check-equal? (lex "000") '((nat 0)))
    (check-equal? (lex "025") '((nat 25)))
    (check-equal? (lex "86420") '((nat 86420)))
    (check-equal? (lex "3.141592") '((float 3.141592)))
    (check-equal? (lex "0.0") '((float 0.0)))
    (check-equal? (lex "1+2") '((nat 1) (op +) (nat 2)))
    (check-equal? (lex "123-321") '((nat 123) (op -) (nat 321)))
    
    (check-equal? (lex "1*2.0*3/4.0")
                  '((nat 1)
                    (op *)
                    (float 2.0)
                    (op *)
                    (nat 3)
                    (op /)
                    (float 4.0)))
    
    (check-equal? (lex "42%2+1-5")
                  '((nat 42)
                    (op %)
                    (nat 2)
                    (op +)
                    (nat 1)
                    (op -)
                    (nat 5)))
    
    (check-equal? (lex "         ") '())
    (check-equal? (lex "    666 ") '((nat 666)))
    (check-equal? (lex "(2.33)") '((opar #f) (float 2.33) (cpar #f)))
    
    (check-equal? (lex " (4 * (5.0 + 3) - 1) % 7^2 ")
                  '((opar #f)
                    (nat 4)
                    (op *)
                    (opar #f)
                    (float 5.0)
                    (op +)
                    (nat 3)
                    (cpar #f)
                    (op -)
                    (nat 1)
                    (cpar #f)
                    (op %)
                    (nat 7)
                    (op ^)
                    (nat 2)))
    
    (check-equal? (lex "let leT lEt lET Let LeT LEt LET")
                  '((id let)
                    (var leT)
                    (var lEt)
                    (var lET)
                    (var Let)
                    (var LeT)
                    (var LEt)
                    (var LET)))
    (check-equal? (lex "let (x 5) in (+ x 3)")
                  '((id let)
                    (opar #f)
                    (var x)
                    (nat 5)
                    (cpar #f)
                    (id in)
                    (opar #f)
                    (op +)
                    (var x)
                    (nat 3)
                    (cpar #f)))
    (check-equal? (lex "if (zero? 4) then #t else #f")
                  '((id if)
                    (opar #f)
                    (id zero?)
                    (nat 4)
                    (cpar #f)
                    (id then)
                    (bool #t)
                    (id else)
                    (bool #f)))

    (check-exn #rx"malformed input"
               (lambda ()
                 (lex "  2 + .1")))

    (check-exn #rx"line 2 column 6"
               (lambda ()
                 (lex "  2 + \n28.1-(30. % 2)"))))))