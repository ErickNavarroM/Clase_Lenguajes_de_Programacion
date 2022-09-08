#lang plait

(require "arithlang.rkt")
; ` > <

;Num Tests
(test (eval `8) 8)

(test (eval `-9) -9)

;Plus Tests
(test (eval `(+ 5 8)) 13)

(test (eval `(+ -30 7)) -23)

(test (eval `(+ -6 -11)) -17)

;Mult Tests
(test (eval `(* 4 3)) 12)

(test (eval `(* -3 11)) -33)

(test (eval `(* 0 127)) 0)

;BMinus Tests
(test (eval `(- 7 4)) 3)

(test (eval `(- -23 9)) -32)

(test (eval `(- 6 -13)) 19)

;UMinus Tests
(test (eval `(- 37)) -37)

(test (eval `(- 15)) -15)

(test (eval `(- -8)) 8)