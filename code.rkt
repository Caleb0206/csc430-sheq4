#lang typed/racket
(require typed/rackunit)

;; SHEQ4
;; Status check here



;; Data definitions

;; Value - Numbers and Booleans
(define-type Value (U Real Boolean CloV))

;; Closure
(struct CloV ([params : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)

;; LamC
(struct LamC ([args : (Listof Symbol)] [body : ExprC]) #:transparent)

;; Binding : pair of a Symbol and a Value
(struct Binding ([name : Symbol] [val : Value]) #:transparent)

;; Env : a list of Bindings
(define-type Env (Listof Binding))

;; ExprC type : NumC, BinOpC, IfC, IdC, AppC
(define-type ExprC (U NumC BinOpC IfC IdC AppC LamC))

;; NumC : a Real
(struct NumC ([n : Real]) #:transparent)

;; IdC : a symbol representing an ID
(struct IdC ([name : Symbol]) #:transparent)

;; BinOpC : a binary operation with left and right ExprC's
(struct BinOpC ([op : Symbol] [l : ExprC] [r : ExprC]) #:transparent)

;; IfC : an if statement of ExprC, and ExprC's to act on if true or false
(struct IfC ([v : ExprC] [iftrue : ExprC] [iffalse : ExprC]) #:transparent)

;; AppC : a name (symbol) with a list of ExprC's
(struct AppC ([expr : ExprC] [args : (Listof ExprC)]) #:transparent)

;; FundefC : a function definition
(struct FundefC ([name : Symbol] [args : (Listof Symbol)] [body : ExprC]) #:transparent)

;; Top level environment
(: top-env Env)
(define top-env (list
                 (Binding 'true #t)
                 (Binding 'false #f)))

;; ---- Keywords & Internal Functions
;; Binary Ops - a hash table of the binary operations
(define bin-ops (hash
                 '+ +
                 '* *
                 '/ /
                 '- -))
;; a list of key-words: + * - / ifleq0? def :
;(define reserved-keywords '(+ * - / ifleq0? def :))
(define reserved-keywords '(if lambda let = in end :))


;; ---- Interpreters ----

;; top-interp - called to parse and evaluate the S-exp, return Real
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))

;; num-value - takes a Value, if Real, returns Real
(define (num-value [v : Value]) : Real
  (if (real? v) v
      (error "SHEQ: Expected a Real, got ~a" v)))

(check-equal? (num-value 8) 8)
(check-exn #rx"SHEQ: Expected a" (lambda () (num-value #f)))

;; serialize
(define (serialize [v : Value]) : String
  (match v
    [(? real? r) (~v r)]
    [(? boolean? b) (if b
                        "true"
                        "false")]
    [(CloV _ _ _) "#<procedure>"]))

(check-equal? (serialize '32) "32")
(check-equal? (serialize #f) "false")
(check-equal? (serialize #t) "true")

;;
#; (define (interp-args [args : (Listof ExprC)] [env : Env] [bindings : (Listof Binding)]) : (Listof Binding)
  (match args
    ['() '()]
    [(cons arg r) (cons (Binding arg (interp arg)))]
    [_ (error 'interp-args "SHEQ: idk yet")]))
;; interp-bin-op - takes a binary operator symbol, ExprC left and right, list of FundefC, to return a Real
(define (interp-bin-op [s : Symbol] [l : ExprC] [r : ExprC] [env : Env]) : Value
  ; retrieve function from hashtable
  (define func (hash-ref bin-ops s))
  (define interped-r (num-value (interp r env)))
  (define interped-l (num-value (interp l env)))
  ; check division for divide by zero
  (cond 
    [(eq? s '/)
     ; preemptively interpret right side for potential 0
     ; if right is 0, throw a divide by 0 error
     (if (eq? interped-r 0)
         (error 'interp-bin-op "SHEQ: Divide by zero error")
         (func interped-l interped-r))]
    [else (func interped-l interped-r)]))


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
    [(IfC v l r) (if (<= (num-value (interp v env)) 0) (interp l env) (interp r env))]
    [(BinOpC s l r) (interp-bin-op s l r env)]
    [(LamC params body) (CloV params body env)]
    [(AppC lam args)
     (define f-val (interp lam env))
     (cond
       [(CloV? f-val)
        (define arg-vals
          (for/list : (Listof Value) ([a args])
            (interp a env)))
        (define new-env
          (append (map Binding (CloV-params f-val) arg-vals)
                  (CloV-env f-val)))
        (interp (CloV-body f-val) new-env)]
       [else
        (error 'interp "SHEQ: Attempted to apply non function value ~a" f-val)])]
                     
    [(IdC id) (define val (get-binding-val id env))
              (if (FundefC? val)
                  (error 'interp "SHEQ: Cannot use Fundef as Id, got ~a" val)
                  val)]))

(check-equal? (interp (AppC (LamC '(x) (BinOpC '+ (IdC 'x) (NumC 1)))
              (list (NumC 5))) top-env) 6)









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
    [(list 'lambda (list (? symbol? params) ...) ': body)
     (LamC (cast params (Listof Symbol)) (parse body))]
    
    [(list f args ...)
         (AppC (parse f) (for/list : (Listof ExprC) ([a args]) (parse a)))]
    [other (error 'parse "SHEQ: Syntax error, got ~e" other)]))

(check-equal? (parse '{(lambda (x) : {+ x 1}) 5})
              (AppC (LamC '(x) (AppC (IdC '+) (list (IdC 'x) (NumC 1)))) (list (NumC 5))))

;; parse-rsvfn - takes an S-exp and returns an exprc of the reserved function
;; throws an error if the applied function is improperly formatted
(define (parse-rsvfn [s : Sexp]) : ExprC 
  ; template
  #;(match s
      [binop -> parse left and right]
      [ifleq0? -> parse v, left and right]
      [else -> throw error (like using def or : in an application)])
  ; body
  (match s
    [(list (and (? symbol? op) (? binop-symbol?)) l r) (BinOpC op (parse l) (parse r))]
    [(list 'ifleq0? v t f) (IfC (parse v) (parse t) (parse f))]
    [other (error 'parse-rsvfn "SHEQ: Syntax error, unexpected reserved keyword, got ~e" other)]))



;; ---- Helper functions ----


;; reserved-symbol? - Determines if a given symbol is in the reserved keywords
;; (+, -, /, *, def, ifleq0?, :)
(define (reserved-symbol? [s : Symbol]) : Boolean
  (if (memq s reserved-keywords)
      #t
      #f))

;; binop-symbol? - takes a symbol and returns true if it is a binop symbol
(define (binop-symbol? [s : Symbol]) : Boolean
  (hash-has-key? bin-ops s))

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
#; (check-exn #rx"SHEQ: Divide by zero error"
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
#; (check-equal? (interp (AppC 'someFunction (list (NumC 3)))
                      (list (Binding 'someFunction (FundefC 'someFunction '(x) (BinOpC '* (NumC 10) (IdC 'x)))))) 30)

;; ---- interp error check - unbound id's ----
; (check-exn #rx"SHEQ: An unbound identifier" (lambda () (interp (IdC 'x) '())))



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
(check-equal? (interp-bin-op '* (NumC 2) (NumC 5) '()) 10)

;; throws Divide by zero error
(check-exn #rx"SHEQ: Divide by zero error" (lambda () (interp-bin-op '/ (NumC 43) (NumC 0) '())))


;; ---- parse Tests ----
(check-equal? (parse '{+ 5 12}) (AppC (IdC '+) (list (NumC 5) (NumC 12))))
(check-equal? (parse '{applyThis 5 12}) (AppC (IdC 'applyThis) (list (NumC 5) (NumC 12))))
(check-equal? (parse 'double) (IdC 'double))
(check-equal? (parse '{double x 2}) (AppC (IdC 'double) (list (IdC 'x) (NumC 2))))
(check-equal? (parse '{ifleq0? 5 x y}) (AppC (IdC 'ifleq0?) (list (NumC 5) (IdC 'x) (IdC 'y))))

; parse error checking
(check-exn #rx"SHEQ: Syntax error, got"
           (lambda () (parse '{})))

#;(check-exn #rx"SHEQ: Syntax error, unexpected reserved keyword, got"
           (lambda () (parse '{- 11 22 33 4 5})))

#;(check-exn #rx"SHEQ: Syntax error, got"
           (lambda () (parse '{+ - 4})))

#;(check-exn #rx"SHEQ: Syntax error, got"
           (lambda () (parse '{+ 93 /})))

;; parse ifleq0? error checking 
#;(check-exn #rx"SHEQ: Syntax error, unexpected reserved keyword, got"
           (lambda () (parse '{ifleq0?})))

(check-exn #rx"SHEQ: Syntax error, unexpected reserved keyword, got"
           (lambda () (parse '{let 2})))

(check-exn #rx"SHEQ: Syntax error, unexpected reserved keyword, got"
           (lambda () (parse '{end 3 4 3 2})))

(check-exn #rx"SHEQ: Syntax error, unexpected reserved keyword, got" (lambda () (parse '=)))

;; parse-rsvfn tests (pretty much same as parse but only reserved functions)
(check-equal? (parse-rsvfn '{+ 5 5}) (BinOpC '+ (NumC 5) (NumC 5)))
(check-equal? (parse-rsvfn '{ifleq0? 5 x y}) (IfC (NumC 5) (IdC 'x) (IdC 'y)))


;; ---- Helper Tests ----

;; distinct-args? tests
(check-equal? (distinct-args? '(x y z)) #t)
(check-equal? (distinct-args? '(x y x)) #f)

;; reserved-symbol tests
(check-equal? (reserved-symbol? 'lambda) #t)
(check-equal? (reserved-symbol? '+++) #f)

;; parse-rsvfn tests
#;(check-equal? (binop-symbol? '*) #t)
#;(check-equal? (binop-symbol? '&) #f)
