#lang typed/racket
(require typed/rackunit)

;; SHEQ4
;; Status check here



;; Data definitions

;; Value - Numbers and Booleans
(define-type Value (U Real Boolean String CloV PrimV))

;; CloV - Closures
(struct CloV ([params : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)

;; PrimV - Primitive Value types
(struct PrimV ([op : Symbol] [num-args : Natural] [func : (-> (Listof Value) Value)]) #:transparent)

;; LamC - Lambdas
(struct LamC ([args : (Listof Symbol)] [body : ExprC]) #:transparent)

;; Binding : pair of a Symbol and a Value
(struct Binding ([name : Symbol] [val : Value]) #:transparent)

;; Env : a list of Bindings
(define-type Env (Listof Binding))

;; ExprC type : NumC, BinOpC, IfC, IdC, AppC
(define-type ExprC (U NumC IfC IdC AppC LamC))

;; NumC : a Real
(struct NumC ([n : Real]) #:transparent)

;; IdC : a symbol representing an ID
(struct IdC ([name : Symbol]) #:transparent)

;; IfC : an if statement of ExprC, and ExprC's to act on if true or false
(struct IfC ([v : ExprC] [iftrue : ExprC] [iffalse : ExprC]) #:transparent)

;; AppC : a name (symbol) with a list of ExprC's
(struct AppC ([expr : ExprC] [args : (Listof ExprC)]) #:transparent)

;; FundefC : a function definition
(struct FundefC ([name : Symbol] [args : (Listof Symbol)] [body : ExprC]) #:transparent)

;; primv+ takes a list of Values, returns Real
(define (primv+ [args : (Listof Value)]) : Real
  (match args
    [(list a b)
     (cond
       [(and (real? a) (real? b)) (+ a b)]
       [else (error 'primv+ "SHEQ: PrimV + expected 2 numbers, got ~a" args)])]
    [_ (error 'primv+ "SHEQ: Incorrect number of arguments")]))

(check-equal? (primv+ (list 8 9)) 17)
(check-exn #rx"SHEQ: PrimV \\+ expected 2 numbers, got" (lambda () (primv+ (list 8 #t))))
(check-exn #rx"SHEQ: Incorrect number of arguments" (lambda () (primv+ (list 8 23 3 2))))

;; primv* takes a list of Values, returns Real
(define (primv* [args : (Listof Value)]) : Real
  (match args
    [(list a b)
     (cond
       [(and (real? a) (real? b)) (* a b)]
       [else (error 'primv* "SHEQ: PrimV * expected 2 numbers, got ~a" args)])]
    [_ (error 'primv* "SHEQ: Incorrect number of arguments")]))

(check-equal? (primv* (list 8 4)) 32)
(check-exn #rx"SHEQ: PrimV \\* expected 2 numbers, got" (lambda () (primv* (list #f #t))))
(check-exn #rx"SHEQ: Incorrect number of arguments" (lambda () (primv* (list 2))))

;; primv/ takes a list of Values, returns Real
(define (primv/ [args : (Listof Value)]) : Real
  (match args
    [(list a b)
     (cond
       [(and (real? a) (real? b) (not (equal? b 0))) (/ a b)]
       [(and (real? a) (real? b) (equal? b 0))
        (error 'primv/ "SHEQ: Divide by zero error")]
       [else (error 'primv/ "SHEQ: PrimV / expected 2 numbers, got ~a" args)])]
    [_ (error 'primv/ "SHEQ: Incorrect number of arguments")]))

(check-equal? (primv/ (list 33 11)) 3)
(check-exn #rx"SHEQ: PrimV \\/ expected 2 numbers, got" (lambda () (primv/ (list #f #t))))
(check-exn #rx"SHEQ: Divide by zero error" (lambda () (primv/ (list 3 0)))) 
(check-exn #rx"SHEQ: Incorrect number of arguments" (lambda () (primv/ (list 21 2 3))))

;; primv- takes a list of Values, returns Real
(define (primv- [args : (Listof Value)]) : Real
  (match args
    [(list a b)
     (cond
       [(and (real? a) (real? b)) (- a b)]
       [else (error 'primv- "SHEQ: PrimV - expected 2 numbers, got ~a" args)])]
    [_ (error 'primv- "SHEQ: Incorrect number of arguments")]))

(check-equal? (primv- (list 33 11)) 22)
(check-exn #rx"SHEQ: PrimV \\- expected 2 numbers, got" (lambda () (primv- (list #f #t))))
(check-exn #rx"SHEQ: Incorrect number of arguments" (lambda () (primv- (list 9 3 2 1 3))))

;; primv<= takes a list of Values, returns Boolean if arg1 <= arg2
(define (primv<= [args : (Listof Value)]) : Boolean
  (match args
    [(list a b)
     (cond
       [(and (real? a) (real? b)) (<= a b)]
       [else (error 'primv<= "SHEQ: PrimV <= expected 2 numbers, got ~a" args)])]
    [_ (error 'primv- "SHEQ: Incorrect number of arguments")]))

(check-equal? (primv<= (list 3 11)) #t)
(check-equal? (primv<= (list 3 -11)) #f)
(check-exn #rx"SHEQ: PrimV \\<= expected 2 numbers, got" (lambda () (primv<= (list #f #t))))
(check-exn #rx"SHEQ: Incorrect number of arguments" (lambda () (primv<= (list 3))))

;; primvequal takes a list of Values, returns Boolean if arg1 == arg2 or false if either is PrimV/CloV
(define (primvequal [args : (Listof Value)]) : Boolean
  (match args
    [(list a b)
     (cond [(or (CloV? a) (CloV? b) (PrimV? a) (PrimV? b)) #f]
           
           [else (equal? a b)])]
    [_ (error 'primvequal "SHEQ: Incorrect number of arguments")]))

(check-equal? (primvequal (list 9 9)) #t)
(check-equal? (primvequal (list #f #f)) #t)
(check-equal? (primvequal (list "hi" "hi")) #t)
(check-equal? (primvequal (list 3 #f)) #f)
(check-equal? (primvequal (list (CloV '(x) (NumC 1) '()) (CloV '(x) (NumC 1) '()))) #f)
(check-equal? (primvequal (list (PrimV '- 2 primv-) (PrimV '- 2 primv-))) #f)
(check-exn #rx"SHEQ: Incorrect number of arguments" (lambda () (primvequal (list 3))))

;; Top level environment
(: top-env Env)
(define top-env (list
                 (Binding 'true #t)
                 (Binding 'false #f)
                 (Binding '+ (PrimV '+ 2 primv+))
                 (Binding '- (PrimV '- 2 primv-))
                 (Binding '* (PrimV '* 2 primv*))
                 (Binding '/ (PrimV '/ 2 primv/))
                 (Binding '<= (PrimV '<= 2 primv<=))
                 (Binding 'equal? (PrimV 'equal? 2 primvequal))))

;; ---- Keywords & Internal Functions

;; a list of key-words
(define reserved-keywords '(if lambda let = in end :))


;; ---- Interpreters ----

;; top-interp - called to parse and evaluate the S-exp, return Real
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))

;; num-value - takes a Value, if Real, returns Real
(define (num-value [v : Value]) : Real
  (if (real? v) v
      (error 'num-value "SHEQ: Expected a Real, got ~a" v)))

(check-equal? (num-value 8) 8)
(check-exn #rx"SHEQ: Expected a" (lambda () (num-value #f)))

;; serialize - takes a Value and returns a serialized String
(define (serialize [v : Value]) : String
  (match v
    [(? real? r) (~v r)]
    [(? boolean? b) (if b
                        "true"
                        "false")]
    [(CloV _ _ _) "#<procedure>"]
    [(PrimV _ _ _) "#<primop>"]))

(check-equal? (serialize '32) "32")
(check-equal? (serialize #f) "false")
(check-equal? (serialize #t) "true")
(check-equal? (serialize (CloV '(x) (NumC 34) top-env)) "#<procedure>")
(check-equal? (serialize (PrimV '<= 2 primv<=)) "#<primop>")

;;
#; (define (interp-args [args : (Listof ExprC)] [env : Env] [bindings : (Listof Binding)]) : (Listof Binding)
     (match args
       ['() '()]
       [(cons arg r) (cons (Binding arg (interp arg)))]
       [_ (error 'interp-args "SHEQ: idk yet")]))



;; get-binding-val takes a symbol and enviornment, performs a lookup and returns an ExprC if found
(define (get-binding-val [s : Symbol] [env : Env]) : Value
  (match env
    ['() (error 'get-binding "SHEQ: An unbound identifier ~a" s)]
    [(cons (Binding name val) r)
     (if (equal? s name)
         val
         (get-binding-val s r))]))

(check-equal? (get-binding-val 'sym (list (Binding 'sym 5))) 5)
(check-exn #rx"SHEQ: An unbound identifier" (lambda () (get-binding-val 'sym '())))

;; interp - takes the complete AST (ExprC) with a list of FundefC, returning a Real
(define (interp [e : ExprC] [env : Env]) : Value
  ; template
  #;(match e
      [numc -> number]
      [ifc -> eval if expr]
      [binop -> eval binop]
      [application -> interp folded function]
      [idc -> throw error at unbound value])

  ; body
  (match e
    [(NumC n) n]
    [(IfC v if-t if-f)
     (define test-val (interp v env))
     (cond
       [(boolean? test-val)
        (if test-val
            (interp if-t env)
            (interp if-f env))]
       [else (error 'interp "SHEQ: If expected boolean test, got ~a" test-val)])]
    [(LamC params body) (CloV params body env)]
    [(AppC lam args)
     (define f-val (interp lam env))
     (define arg-vals
       (for/list : (Listof Value) ([a args])
         (interp a env)))
     (cond
       [(CloV? f-val)
        (define new-env
          (append (map Binding (CloV-params f-val) arg-vals)
                  (CloV-env f-val)))
        (interp (CloV-body f-val) new-env)]
       [(PrimV? f-val)
        (if (equal? (PrimV-num-args f-val) (length arg-vals))
            ((PrimV-func f-val) arg-vals)
            (error 'interp "SHEQ: Incorrect number of arguments to ~a, expected ~a but got ~a"
                   (PrimV-op f-val) (PrimV-num-args f-val) (length arg-vals)))]
       [else
        (error 'interp "SHEQ: Attempted to apply non function value ~a" f-val)])]
                     
    [(IdC id) (get-binding-val id env)]))




;; reserved-symbol? - Determines if a given symbol is in the reserved keywords
;; (+, -, /, *, def, ifleq0?, :) 
(define (reserved-symbol? [s : Symbol]) : Boolean
  (if (memq s reserved-keywords)
      #t
      #f))


;; ---- Parsers ---- 
;; parse - takes a S-exp and returns concrete syntax in ExprC format
(define (parse [e : Sexp]) : ExprC
  ; template
  #;(match e
      [number -> numc]
      [not reserved symbol -> idc]
      ; interestingly, we can wrap all out reserved functions into applications
      ; all reserved functions follow syntax {<id> <expr> ...}
      ; we will use a helper function to navigate these reserved functions EXCEPT def
      ; -> +, -, *, /, ifleq0?
      [application -> check reserved functions, validate arg count]
      [else -> throw unknown error])
  ; body
  (match e
    [(? real? n) (NumC n)]
    [(? symbol? name)
     (if (reserved-symbol? name)
         (error 'parse "SHEQ: Syntax error, unexpected reserved keyword, got ~e" name)
         (IdC name))]
    [(list 'if v iftrue iffalse)
     (IfC (parse v) (parse iftrue) (parse  iffalse))]
    [(list 'lambda (list (? symbol? params) ...) ': body)
     (LamC (cast params (Listof Symbol)) (parse body))]
    
    [(list f args ...)
     (AppC (parse f) (for/list : (Listof ExprC) ([a args]) (parse a)))]
    [other (error 'parse "SHEQ: Syntax error, got ~e" other)]))

(check-equal? (parse '{if {<= 3 90} 3 90}) (IfC (AppC (IdC '<=) (list (NumC 3) (NumC 90))) (NumC 3) (NumC 90)))

;; ---- Helper functions ----


;; distinct-args? - returns true if every symbol in args is distinct 
(define (distinct-args? [args : (Listof Symbol)]) : Boolean
  (not (check-duplicates args)))


;; create-env 
(define (create-env [args : (Listof Symbol)] [vals : (Listof Value)] [env : Env]) : Env
  (match* (args vals)
    [('() '()) env]
    [('() _) (error "SHEQ: too many values were passed in application ~a ~a" args vals)]
    [(_ '()) (error "SHEQ: too few values were passed in application ~a ~a" args vals)]
    [((cons fa ra) (cons fv rv))
     (create-env ra rv (cons (Binding fa fv) env))]))

(check-equal? (create-env (list 'a) (list 5) (list (Binding 'random 314)))
              (list (Binding 'a 5) (Binding 'random 314)))

;; ---- Tests

;; Large test
; The program calculates two areas using two different functions, and then compares them.
;; The result is the result of the comparison
(define prog '{
               {def square (x) : {* x x}}
               {def area (w h) : {* w h}}
               {def gt (v1 v2 t f) : {ifleq0? {- v2 v1} t f}}
               {def main () : {gt {square 4} {area 4 3} 0 1}}
               })
#; (check-equal? (top-interp prog) 0)

;; ---- top-interp Tests ----
; (top-interp '{def main () : {+ 1 {* 2 2}}})
#;(check-equal? (top-interp '{
                              {def main () : {+ 1 {* 2 2}}}
                              }) 5)

#;(check-equal? (top-interp '{
                              {def main () : {* 2 {/ 1 2}}}}) 1)

#;(check-equal? (top-interp '{{def main () : {ifleq0? 1 10 -10}}}) -10)

#;(check-equal? (top-interp '{{def main() : {ifleq0? -1 10 -10}}}) 10)

;; divide by zero error test case (from handin)
#;(check-exn #rx"SHEQ: Divide by zero error"
             (lambda () (top-interp
                         '{{def ignoreit (x) : {+ 7 15}} {def main () : {ignoreit {/ 52 (+ 0 0)}}}})))

#;(check-exn #rx"SHEQ:"
             (lambda () (top-interp '{{def f (x) : {+ x 2}} {def main () : {f 1 2 3}}})))

;; top-interp error check empty
#; (check-exn #rx"SHEQ: Syntax error, got"
              (lambda () (top-interp '{ {def main () : {}}})))

;; ---- interp tests ----
(check-equal? (interp (IdC 'true) top-env) #t)
(check-equal? (interp (NumC 89) top-env) 89)
(check-equal? (interp (AppC (IdC '+) (list (NumC 8)
                                           (AppC (IdC '*) (list (NumC 2) (NumC 3))))) top-env)  14) 
(check-equal? (interp (AppC (IdC 'main) '()) (list (Binding 'main (CloV '() (NumC 5) '())))) 5)
(check-equal? (interp (AppC (IdC 'someFunction) (list (NumC 3)))
                      (list (Binding 'someFunction
                                     (CloV '(x)
                                           (AppC (IdC '*) (list (NumC 10) (IdC 'x)))
                                           top-env)))) 30)

(check-equal? (interp (AppC (IdC '<=) (list (NumC 9) (NumC 10))) top-env) #t)
(check-equal? (interp (AppC (IdC 'equal?) (list (NumC 9) (NumC 10))) top-env) #f)
(check-equal? (interp (AppC (IdC 'equal?) (list (NumC 9) (NumC 10))) top-env) #f)
(check-equal? (interp (IfC (AppC (IdC '<=) (list (NumC 5) (NumC 2))) (NumC 1) (NumC -1)) top-env) -1)


;; ---- interp error check ---- 
(check-exn #rx"SHEQ: An unbound identifier" (lambda () (interp (IdC 'x) '())))
(check-exn #rx"SHEQ: PrimV \\+ expected 2 numbers" (lambda () (interp (AppC (IdC '+) (list (IdC '-) (NumC 4))) top-env)))
(check-exn #rx"SHEQ: Divide by zero error" (lambda () (interp (AppC (IdC '/) (list (NumC 5) (NumC 0))) top-env)))
(check-exn #rx"SHEQ: If expected boolean test" (lambda () (interp (parse '{if 32 23 32}) top-env))) 


;; ---- Recursion Test ----
(define reProg '{
                 {def minusTil0 (x) : {ifleq0? x x {minusTil0  {- x 10}}}}
              
                 {def main () : {minusTil0 15}}
                 })

#; (check-equal? (interp-fns (list
                              (FundefC 'minusTil0
                                       '(x)
                                       (IfC (IdC 'x)
                                            (IdC 'x)
                                            (AppC 'minusTil0 (list (BinOpC '- (IdC 'x) (NumC 10))))))
                              (FundefC 'main '() (AppC 'minusTil0 (list (NumC 1001)))))) -9)


;; ---- interp-bin-op tests ----
;(check-equal? (interp-bin-op '* (NumC 2) (NumC 5) '()) 10)

;; throws Divide by zero error
;(check-exn #rx"SHEQ: Divide by zero error" (lambda () (interp-bin-op '/ (NumC 43) (NumC 0) '())))


;; ---- parse Tests ----
(check-equal? (parse '{(lambda (x) : {+ x 1}) 5})
              (AppC (LamC '(x) (AppC (IdC '+) (list (IdC 'x) (NumC 1)))) (list (NumC 5))))
(check-equal? (parse '{+ 5 12}) (AppC (IdC '+) (list (NumC 5) (NumC 12))))
(check-equal? (parse '{applyThis 5 12}) (AppC (IdC 'applyThis) (list (NumC 5) (NumC 12))))
(check-equal? (parse 'double) (IdC 'double))
(check-equal? (parse '{double x 2}) (AppC (IdC 'double) (list (IdC 'x) (NumC 2))))
(check-equal? (parse '{ifleq0? 5 x y}) (AppC (IdC 'ifleq0?) (list (NumC 5) (IdC 'x) (IdC 'y))))

(check-equal? (interp (AppC (LamC '(x) (AppC (IdC '+) (list (IdC 'x) (NumC 1))))
                            (list (NumC 5))) top-env) 6)

(check-equal? (interp (IfC (AppC (IdC 'equal?) (list (NumC 81) (NumC 81))) (IdC 'true) (IdC 'false)) top-env) #t)

(check-exn #rx"SHEQ: Incorrect number of arguments"
           (lambda ()
             (interp (AppC (LamC '(x)
                                 (AppC (IdC '+) (list (IdC 'x) (NumC 1) (NumC 2))))
                           (list (NumC 5))) top-env)))
(check-exn #rx"SHEQ: Attempted to apply non function value"
           (lambda ()
             (interp (AppC (NumC 9) (list (NumC 12))) top-env)))

; parse error checking
(check-exn #rx"SHEQ: Syntax error, got"
           (lambda () (parse '{})))

(check-exn #rx"SHEQ: Syntax error, unexpected reserved keyword, got"
           (lambda () (parse '{let 2})))

(check-exn #rx"SHEQ: Syntax error, unexpected reserved keyword, got"
           (lambda () (parse '{end 3 4 3 2})))

(check-exn #rx"SHEQ: Syntax error, unexpected reserved keyword, got" (lambda () (parse '=)))




;; ---- Helper Tests ----

;; distinct-args? tests
(check-equal? (distinct-args? '(x y z)) #t)
(check-equal? (distinct-args? '(x y x)) #f)

;; reserved-symbol tests
(check-equal? (reserved-symbol? 'lambda) #t)
(check-equal? (reserved-symbol? '+++) #f)
