#lang plait

(print-only-errors #t)
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; PARSE SECTION
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(define (parse-number in)
  (numS (s-exp->number in)))

(define (parse-string in)
  (strS (s-exp->string in)))

(define (parse-id in)
  (idS (s-exp->symbol in)))

(define (parse-if in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 4)
        (ifS (parse (second inlst))
             (parse (third inlst))
             (parse (fourth inlst)))
        (error 'parse "cantidad incorrecta de argumentos para if"))))

(define (parse-and in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (andS (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para and"))))

(define (parse-or in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (orS (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para or"))))

(define (parse-+ in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (binopS (plusO) (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para +"))))

(define (parse-++ in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (binopS (appendO) (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para ++"))))

(define (parse-num= in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (binopS (numeqO) (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para num="))))

(define (parse-str= in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (binopS (streqO) (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para str="))))

(define (parse-fun in)
  (cond
    [(s-exp-match? `{fun SYMBOL ANY ...} in)
     (let ([inlst (s-exp->list in)])
       (if (equal? (length inlst) 3)
           (funS (s-exp->symbol (second inlst)) (parse (third inlst)))
           (error 'parse "funciones deben tener solo un cuerpo")))]
    [(s-exp-match? `{fun ANY ...} in)
     (error 'parse "parametros a función deben ser símbolos")]))

(define (parse-let in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (letS
         (s-exp->symbol (first (s-exp->list (second inlst))))
         (parse (second (s-exp->list (second inlst))))
         (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para let"))))

(define (parse-app in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 2)
        (appS (parse (first inlst)) (parse (second inlst)))
        (error 'parse "cantidad incorrecta de argumentos en aplicación de funciones"))))

;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; INTERPRET SECTIONS
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(define (parse [in : S-Exp]) : ExprS
  (cond
    [(s-exp-number? in)                        (parse-number in)]
    [(s-exp-string? in)                        (parse-string in)]
    [(s-exp-match? `true in)                   (boolS #t)]
    [(s-exp-match? `false in)                  (boolS #f)]
    [(s-exp-match? `{if ANY ANY ANY} in)       (parse-if in)]
    [(s-exp-match? `{and ANY ANY} in)          (parse-and in)]
    [(s-exp-match? `{or ANY ANY} in)           (parse-or in)]
    [(s-exp-match? `{+ ANY ANY} in)            (parse-+ in)]
    [(s-exp-match? `{++ ANY ANY} in)           (parse-++ in)]
    [(s-exp-match? `{num= ANY ANY} in)             (parse-num= in)]
    [(s-exp-match? `{str= ANY ANY} in)             (parse-str= in)]
    [(s-exp-match? `{fun SYMBOL ANY} in)          (parse-fun in)]
    [(s-exp-match? `{let {SYMBOL ANY} ANY} in) (parse-let in)]
    [(s-exp-match? `{ANY ANY} in)              (parse-app in)]
    [(s-exp-symbol? in)                        (parse-id in)]
    [else (error-parse in)]))


(define (desugar [e : ExprS]) : ExprC
  (type-case ExprS e
    [(numS n) (numC n)]
    [(strS n) (strC n)]
    [(boolS b) (boolC b)]
    [(idS name) (idC name)]
    [(binopS op e1 e2) (binopC op (desugar e1) (desugar e2))]
    [(orS b1 b2) (ifC (desugar b1) (boolC #t) (desugar b2) 'or)]
    [(andS b1 b2) (ifC (desugar b1) (desugar b2) (boolC #f) 'and)]
    [(ifS b e1 e2) (ifC (desugar b) (desugar e1) (desugar e2) 'if)]
    [(letS name value body) (appC (desugar (funS name body)) (desugar value))]
    [(funS name body) (funC name (desugar body))]
    [(appS func arg) (appC (desugar func) (desugar arg))]))

(define (interp [expr : ExprC]) : Value
  (interp-env expr empty))

(define (interp-env [e : ExprC] [env : Environment]) : Value
  (type-case ExprC e
    [(numC n) (numV n)]
    [(strC n) (strV n)]
    [(boolC b) (boolV b)]
    [(idC name) (check-env name env)]
    [(binopC op e1 e2)
     (let ([v1 (interp-env e1 env)]
           [v2 (interp-env e2 env)])
       (type-case Operator op
         [(plusO)
          (cond [(not (numV? v1)) (error-binop '+ v1)]
                [(not (numV? v2)) (error-binop '+ v2)]
                [else (numV (+ (numV-n v1) (numV-n v2)))])]
         [(appendO)
          (cond [(not (strV? v1)) (error-binop '++ v1)]
                [(not (strV? v2)) (error-binop '++ v2)]
                [else (strV (string-append (strV-s v1) (strV-s v2)))])]
         [(numeqO)
          (cond [(not (numV? v1)) (error-binop 'num= v1)]
                [(not (numV? v2)) (error-binop 'num= v2)]
                [else (boolV (= (numV-n v1) (numV-n v2)))])]
         [(streqO)
          (cond [(not (strV? v1)) (error-binop 'str= v1)]
                [(not (strV? v2)) (error-binop 'str= v2)]
                [else (boolV (string=? (strV-s v1) (strV-s v2)))])]))]
    [(ifC e1 e2 e3 op)
     (let ([v1 (interp-env e1 env)])
       (case op
         [(if)
          (cond [(not (boolV? v1)) (error-bool 'if v1)]
                [(boolV-b v1) (interp-env e2 env)]
                [else (interp-env e3 env)])]
         [(or)
          (cond [(not (boolV? v1)) (error-bool 'or v1)]
                [(boolV-b v1) (interp-env e2 env)]
                [(not (boolV? (interp-env e3 env))) (error-bool 'or e3)]
                [else (interp-env e3 env)])]
         [(and)
          (cond [(not (boolV? v1)) (error-bool 'or v1)]
                [(not (boolV-b v1)) (interp-env e3 env)]
                [(not (boolV? (interp-env e2 env))) (error-bool 'or e2)]
                [else (interp-env e2 env)])]))]
    [(funC name body) (funV name body env)]
    [(appC function argument)
     (let ([fun (interp-env function env)])
       (cond [(not (funV? fun))
              (error-app fun)]
             [else
              (interp-env (funV-body fun)
                   (add-env (funV-name fun)
                            (interp-env argument env)
                            (funV-env fun)))]))]))

       
(define (eval [expr : S-Exp]) : Value
  (interp (desugar (parse expr))))
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; ENVIRONMENT
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(define-type Binding
  (binding [name : Symbol]
           [value : Value]))

(define-type-alias Environment (Listof Binding))

(define (check-env [name : Symbol] [env : Environment])
  (if (empty? env)
      (error-id name)
      (if (eq? name (binding-name (first env)))
          (binding-value (first env))
          (check-env name (rest env)))))

(define (add-env name value env)
  (cons (binding name value) env))
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; TYPES OF ERRORS
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(define (error-parse expr)
  (error 'parse (string-append "La entrada es una expresión malformada: " (to-string expr))))

(define (error-bool op expr)
  (error op (string-append "Argumento no es un valor booleano: " (to-string expr))))

(define (error-id expr)
  (error 'id (string-append "El identificador no está enlazado: " (to-string expr))))

(define (error-binop op expr)
  (error op
         (string-append "Expresión con argumento incorrecto: "
                        (string-append (to-string op)
                                       (string-append ": " (to-string expr))))))

(define (error-app expr)
  (error 'app (string-append "Argumento no es una función: " (to-string expr))))
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; TYPES OF EXPRESIONS
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(define-type Operator
  (plusO)
  (appendO)
  (numeqO)
  (streqO))

(define-type ExprS
  (numS [n : Number])
  (boolS [b : Boolean])
  (idS [name : Symbol])
  (strS [s : String])
  (binopS [op : Operator]
          [e1 : ExprS]
          [e2 : ExprS])
  (orS [b1 : ExprS]
       [b2 : ExprS])
  (andS [b1 : ExprS]
        [b2 : ExprS])
  (ifS [b : ExprS]
       [e1 : ExprS]
       [e2 : ExprS])
  (letS [name : Symbol]
        [value : ExprS]
        [body : ExprS])
  (funS [name : Symbol]
        [body : ExprS])
  (appS [func : ExprS]
        [arg : ExprS]))

(define-type ExprC
  (numC [n : Number])
  (boolC [b : Boolean])
  (strC [s : String])
  (idC [name : Symbol])
  (binopC [op : Operator]
          [l : ExprC]
          [r : ExprC])
  (ifC [a : ExprC]
       [b : ExprC]
       [c : ExprC]
       [op : Symbol])
  (funC [name : Symbol]
        [body : ExprC])
  (appC [func : ExprC]
        [arg : ExprC]))

(define-type Value
  (numV [n : Number])
  (boolV [b : Boolean])
  (strV [s : String])
  (idV [name : Symbol])
  (funV [name : Symbol]
        [body : ExprC]
        [env : Environment]))

;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
