#lang typed/racket
(require typed/rackunit)

;; SHEQ4
;; Status check here



;; Data definitions
;; ExprC type : NumC, BinOpC, IfC, IdC, AppC
(define-type ExprC (U NumC BinOpC IfC IdC AppC))

;; NumC : a Real
(struct NumC ([n : Real]) #:transparent)

;; IdC : a symbol representing an ID
(struct IdC ([name : Symbol]) #:transparent)

;; BinOpC : a binary operation with left and right ExprC's
(struct BinOpC ([op : Symbol] [l : ExprC] [r : ExprC]) #:transparent)

;; IfC : an if statement of ExprC, and ExprC's to act on if true or false
(struct IfC ([v : ExprC] [iftrue : ExprC] [iffalse : ExprC]) #:transparent)

;; AppC : a name (symbol) with a list of ExprC's
(struct AppC ([name : Symbol] [args : (Listof ExprC)]) #:transparent)

;; FundefC : a function definition
(struct FundefC ([name : Symbol] [args : (Listof Symbol)] [body : ExprC]) #:transparent)

;; ---- Keywords & Internal Functions
;; Binary Ops - a hash table of the binary operations
(define bin-ops (hash
                 '+ +
                 '* *
                 '/ /
                 '- -))
;; a list of key-words: + * - / ifleq0? def :
(define reserved-keywords '(+ * - / ifleq0? def :))


;; ---- Interpreters ----

;; top-interp - called to parse and evaluate the S-exp, return Real
(define (top-interp [s : Sexp]) : Real
  (interp-fns (parse-prog s)))


;; interp - takes the complete AST (ExprC) with a list of FundefC, returning a Real
(define (interp [e : ExprC] [fds : (Listof FundefC)]) : Real
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
    [(IfC v l r) (if (<= (interp v fds) 0) (interp l fds) (interp r fds))]
    [(BinOpC s l r) (interp-bin-op s l r fds)]
    [(AppC name args) (define fd (get-fundef name fds))
                      (define evaluated-args
                        (for/list : (Listof ExprC) ([a args])
                          (NumC (interp a fds))))
                      (interp (fold-args 
                               (zip (FundefC-args fd) evaluated-args)
                               (FundefC-body fd)) fds)]
    [(IdC name) (error 'interp "SHEQ: An unbound identifier ~a" name)]))

;; interp-bin-op - takes a binary operator symbol, ExprC left and right, list of FundefC, to return a Real
(define (interp-bin-op [s : Symbol] [l : ExprC] [r : ExprC] [fds : (Listof FundefC)]) : Real
  ; retrieve function from hashtable
  (define func (hash-ref bin-ops s))
  ; check division for divide by zero
  (cond
    [(eq? s '/)
     ; preemptively interpret right side for potential 0
     (define interped-r (interp r fds))
     ; if right is 0, throw a divide by 0 error
     (if (eq? interped-r 0)
         (error 'interp-bin-op "SHEQ: Divide by zero error")
         (func (interp l fds) interped-r))]
    [else (func (interp l fds) (interp r fds))]))

;; interp-fns - interprets the main function: takes a list of FundefC, returns a real
(define (interp-fns [fns : (Listof FundefC)]) : Real
  (interp (AppC 'main '()) fns))


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
    [(? symbol? name) #:when (not (reserved-symbol? name)) (IdC name)] 
    [(list (? symbol? fn) args ...)
     (if (reserved-symbol? fn)
         ; separately parse S-exp of reserved function
         (parse-rsvfn e)
         (AppC fn (for/list ([a args]) (parse a))))]
    [other (error 'parse "SHEQ: Syntax error, got ~e" other)]))



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

;; parse-prog - takes a S-exp to return a list of FundefC's 
(define (parse-prog [s : Sexp]) : (Listof FundefC)
  (match s
    ['() '()]
    [(cons f r) (cons (parse-fundef f) (parse-prog r))]))

;; parse-fundef - takes a S-exp to return a FundefC
(define (parse-fundef [e : Sexp]) : FundefC
  #;(match e
      [fundef with not reserved name -> validate arguments and parse]
      [else throw error])
  (match e
    [(list 'def (? symbol? name) (list (? symbol? args) ...) ': body)
     #:when (not (reserved-symbol? name))
     (define arg-symbols (cast args (Listof Symbol)))
     ; ensure all arguments are distinct
     (if (distinct-args? arg-symbols)
         (FundefC name arg-symbols (parse body))
         (error 'parse-fundef "SHEQ: Invalid argument list. Function: ~a Args: ~a" name args))]
    [_ (error 'parse-fundefc "SHEQ: Syntax error: Invalid function definition, got ~e" e)]))


;; ---- Helper functions ----

;; get-fundef - return a FundefC from defs given a symbol name and a list of defs
(define (get-fundef [name : Symbol] [defs : (Listof FundefC)]) : FundefC
  (cond
    [(empty? defs) (error 'get-fundef "SHEQ: Syntax error: Unknown function id, got ~e" name)]
    [else
     (define curr-def (first defs))
     (if (equal? name (FundefC-name curr-def))
         curr-def
         (get-fundef name (rest defs)))]))


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

;; subst - substitutes all occurences of "from" for "what" in the ExprC "in", returns an ExprC
(define (subst [what : ExprC] [from : Symbol] [in : ExprC]) : ExprC
  (match in
    [(NumC n) in]
    ; If matching symbol, replace
    [(IdC name) (cond
                  [(equal? name from) what]
                  [else in])]
    ; Traverse operations
    [(BinOpC op l r) (BinOpC op 
                             (subst what from l) 
                             (subst what from r))]
    ; Traverse ifleq0?
    [(IfC v t f) (IfC 
                  (subst what from v)
                  (subst what from t)
                  (subst what from f))]
    ; Traverse func apps, substituting all arguments
    [(AppC name args) (AppC name (for/list ([a args])
                                   (subst what from a)))]))


;; zip - combine two lists of the same length, returning one list of corresponding elements
(: zip (∀ (A B) ((Listof A) (Listof B) -> (Listof (List A B)))))
(define (zip l1 l2)
  (if (not (equal? (length l1) (length l2)))
      (error "SHEQ: zip lists of unequal lengths")
      (match* (l1 l2)
        [('() '()) '()]
        [((cons l1f l1r) 
          (cons l2f l2r)) 
         (cons 
          (list l1f l2f) 
          (zip l1r l2r))])))

;; fold-args - Fold substitute arguments into a function application (ExprC)
(define (fold-args [arg->exprc : (Listof (List Symbol ExprC))] [in : ExprC]) : ExprC
  ; Zip (fdargs, args) and iterate over this list
  (match arg->exprc
    ['() in]
    [(cons (list arg exprc) r) (fold-args r (subst exprc arg in))]))

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
(check-equal? (top-interp prog) 0)

;; ---- top-interp Tests ----
(check-equal? (top-interp '{
                            {def main () : {+ 1 {* 2 2}}}
                            }) 5)

(check-equal? (top-interp '{
                            {def main () : {* 2 {/ 1 2}}}}) 1)

(check-equal? (top-interp '{{def main () : {ifleq0? 1 10 -10}}}) -10)

(check-equal? (top-interp '{{def main() : {ifleq0? -1 10 -10}}}) 10)

;; divide by zero error test case (from handin)
(check-exn #rx"SHEQ: Divide by zero error"
           (lambda () (top-interp
                       '{{def ignoreit (x) : {+ 7 15}} {def main () : {ignoreit {/ 52 (+ 0 0)}}}})))

(check-exn #rx"SHEQ:"
           (lambda () (top-interp '{{def f (x) : {+ x 2}} {def main () : {f 1 2 3}}})))

;; top-interp error check empty
(check-exn #rx"SHEQ: Syntax error, got"
           (lambda () (top-interp '{ {def main () : {}}})))

;; ---- interp tests ----
(check-equal? (interp (NumC 89) '()) 89)
(check-equal? (interp (BinOpC '+ (NumC 8) (BinOpC '* (NumC 2) (NumC 3))) '()) 14)
(check-equal? (interp (AppC 'main '()) (list (FundefC 'main '() (NumC 89)))) 89)
(check-equal? (interp (AppC 'someFunction (list (NumC 3)))
                      (list
                       (FundefC 'someFunction '(x) (BinOpC '* (NumC 10) (IdC 'x)))
                       )) 30)

;; ---- interp error check - unbound id's ----
(check-exn #rx"SHEQ: An unbound identifier" (lambda () (interp (IdC 'x) '())))


;; ---- interp-fns tests ----
(check-equal? (interp-fns (list (FundefC 'main '() (NumC 101)))) 101)

(check-equal? (interp-fns
               (list
                (FundefC 'lovely '(aww)
                         (IfC (IdC 'aww)
                              (BinOpC '* (IdC 'aww) (NumC 2))
                              (BinOpC '- (IdC 'aww) (NumC 100))))
                (FundefC 'main '()
                         (AppC 'lovely (list (NumC 143)) )))) 43)

(check-equal? (interp-fns
               (list
                (FundefC 'lovely '(aww)
                         (IfC (IdC 'aww)
                              (BinOpC '* (IdC 'aww) (NumC 2))
                              (BinOpC '- (IdC 'aww) (NumC 100))))
                (FundefC 'main '()
                         (AppC 'lovely (list (NumC -143)) )))) -286)

;; ---- Recursion Test ----
(define reProg '{
                 {def minusTil0 (x) : {ifleq0? x x {minusTil0  {- x 10}}}}
              
                 {def main () : {minusTil0 15}}
                 })

(check-equal? (interp-fns (list
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
(check-equal? (parse '{+ 5 12}) (BinOpC '+ (NumC 5) (NumC 12)))
(check-equal? (parse '{applyThis 5 12}) (AppC 'applyThis (list (NumC 5) (NumC 12))))
(check-equal? (parse 'double) (IdC 'double))
(check-equal? (parse '{double x 2}) (AppC 'double (list (IdC 'x) (NumC 2))))
(check-equal? (parse '{ifleq0? 5 x y}) (IfC (NumC 5) (IdC 'x) (IdC 'y)))

; parse error checking
(check-exn #rx"SHEQ: Syntax error, got"
           (lambda () (parse '{})))

(check-exn #rx"SHEQ: Syntax error, unexpected reserved keyword, got"
           (lambda () (parse '{- 11 22 33 4 5})))

(check-exn #rx"SHEQ: Syntax error, got"
           (lambda () (parse '{+ - 4})))

(check-exn #rx"SHEQ: Syntax error, got"
           (lambda () (parse '{+ 93 /})))

;; parse ifleq0? error checking
(check-exn #rx"SHEQ: Syntax error, unexpected reserved keyword, got"
           (lambda () (parse '{ifleq0?})))

(check-exn #rx"SHEQ: Syntax error, unexpected reserved keyword, got"
           (lambda () (parse '{ifleq0? 2})))

(check-exn #rx"SHEQ: Syntax error, unexpected reserved keyword, got"
           (lambda () (parse '{ifleq0? 3 4 3 2})))

(check-exn #rx"SHEQ: Syntax error, got" (lambda () (parse 'ifleq0?)))

;; parse-rsvfn tests (pretty much same as parse but only reserved functions)
(check-equal? (parse-rsvfn '{+ 5 5}) (BinOpC '+ (NumC 5) (NumC 5)))
(check-equal? (parse-rsvfn '{ifleq0? 5 x y}) (IfC (NumC 5) (IdC 'x) (IdC 'y)))


;; parse-prog Tests
(check-equal? (parse-prog '{{def fiftyFive () : 55}}) (list (FundefC 'fiftyFive '() (NumC 55))))
(check-equal? (parse-prog '{{def plusOne (x) : {+ x 1}} {def main () : {plusOne 3}}})
              (list (FundefC 'plusOne '(x) (BinOpC '+ (IdC 'x) (NumC 1)))
                    (FundefC 'main '() (AppC 'plusOne (list (NumC 3))))))


;; parse-fundef tests
(check-equal? (parse-fundef '{def five () : 5}) (FundefC 'five '() (NumC 5)))
(check-equal? (parse-fundef '{def area (w h) : {* w h}}) (FundefC 'area '(w h) (BinOpC '* (IdC 'w) (IdC 'h))))
(check-equal? (parse-fundef '{def five () : 5}) (FundefC 'five '() (NumC 5)))

; parse-fundef error checking
(check-exn #rx"SHEQ: Syntax error: Invalid function definition, got"
           (lambda () (parse-fundef '{def badfunction () : es es e })))
(check-exn #rx"SHEQ: Syntax error: Invalid function definition, got"
           (lambda () (parse-fundef '{def badfunction () 32})))
(check-exn #rx"SHEQ: Syntax error: Invalid function definition, got"
           (lambda () (parse-fundef '{+ 1 2})))
(check-exn #rx"SHEQ: Syntax error: Invalid function definition, got"
           (lambda () (parse-fundef '{func 3})))
(check-exn #rx"SHEQ: Syntax error: Invalid function definition, got"
           (lambda () (parse-fundef '{def + () : 13})))
(check-exn #rx"SHEQ: Invalid argument list"
           (lambda () (parse-fundef '{def bad (x x) : {+ x 1}})))

;; ---- Helper Tests ----
;; get-fundef tests
(check-equal? (get-fundef 'runThis (list (FundefC 'notThis '() (NumC -1)) (FundefC 'runThis '(yes) (IdC 'yes))))
              (FundefC 'runThis '(yes) (IdC 'yes)))
(check-exn #rx"SHEQ: Syntax error: Unknown function id"
           (lambda () (get-fundef 'foo '())))


;; zip tests
(check-equal? (zip '(1 2 3) '(4 5 6)) '((1 4) (2 5) (3 6)))
(check-equal? (zip (list (IdC 'bobCredit) (IdC 'aliceCredit)) (list (NumC -13) (NumC 232)))
              (list (list
                     (IdC 'bobCredit) (NumC -13))
                    (list (IdC 'aliceCredit) (NumC 232))))

;; subst tests
(check-equal? (subst (NumC 55) 'num (BinOpC '/ (IdC 'num) (NumC 5))) (BinOpC '/ (NumC 55) (NumC 5)))
(check-equal? (subst (NumC 8) 's (NumC 9)) (NumC 9))
(check-equal? (subst (NumC -1) 'negativeone (IfC (IdC 'negativeone) (NumC -10) (NumC 10)))
              (IfC (NumC -1) (NumC -10) (NumC 10)))
(check-equal? (subst (NumC 0.4) 'x (AppC 'quadruple (list (BinOpC '* (IdC 'x) (NumC 4)))))
              (AppC 'quadruple (list (BinOpC '* (NumC 0.4) (NumC 4)))))


;; fold-arg tests
(check-equal? (fold-args (list
                          (list 'bunnies (NumC 38)))
                         (BinOpC '* (IdC 'bunnies) (NumC 1.8)))
              (BinOpC '* (NumC 38) (NumC 1.8)))

(check-equal? (fold-args (list
                          (list 'x (NumC 200))
                          (list 'y (NumC 1)))
                         (BinOpC '- (IdC 'x) (BinOpC '* (IdC 'y) (IdC 'y))))
              (BinOpC '- (NumC 200) (BinOpC '* (NumC 1) (NumC 1))))

;; distinct-args? tests
(check-equal? (distinct-args? '(x y z)) #t)
(check-equal? (distinct-args? '(x y x)) #f)

;; reserved-symbol tests
(check-equal? (reserved-symbol? '+) #t)
(check-equal? (reserved-symbol? '+++) #f)

;; parse-rsvfn tests
(check-equal? (binop-symbol? '*) #t)
(check-equal? (binop-symbol? '&) #f)
