#lang pl

#|

IN this part we only added a False. after we saw true it's very simple, 2 minutes.
<SOL> :: = { <NumList> }
        |  { scalar-mult <num> <SOL> }
        |  { intersect <SOL> <SOL>}
        |  { union <SOL> <SOL> }
        |  <id>
        |  { with {<id> <SOL>  <id> <SOL>} <SOL> } ;; this should be a syntactic sugar
        |  { fun { <id> <id> } <SOL> } ;; a function must have exactly two formal parameters
        |  { call-static <SOL> <SOL> <SOL> } ;; extends closure environment
        |  { call-dynamic <SOL> <SOL> <SOL> } ;; extends current environment

        |  True
        |  False
        | { if <SOL> then <SOL> else <SOL> }
        | { equal? <SOL> <SOL> }

<NumList> :: =  λ | <num> <NumList> ;; where λ stands for the empty word, i.e., { } is the empty set

;; where <num> is any expression identified by Racket as a Number
;; and <id> is any expression such that Racket identifies '<id> as a symbol
 
|#


;; -----------------------------------------------------

;;In This part we only filled the Equal part. Equal use to check if 2 SOL's are equal, 2 minutes.
;; The abstract syntax tree SOL
(define-type SET = (Listof Number))
(define-type SOL
  [Set  SET]
  [Smult Number SOL]
  [Inter SOL SOL]
  [Union SOL SOL]
  [Id    Symbol]
  [Fun   Symbol Symbol SOL]
  [CallS SOL SOL SOL]
  [CallD SOL SOL SOL]
  [Bool Boolean]
  [If SOL SOL SOL]
  [Equal SOL SOL])


;; ----------------------------------------------------
;; Operations on SETs 

(: ismember? : Number SET  -> Boolean)
(define (ismember? n l)
  (cond [(null? l) #f]
        [(= n (first l)) #t]
        [else (ismember? n (rest l))]))

(test (not (ismember? 1 '(3 4 5))))
(test (not (ismember? 1 '( 3 2 3 5 6))))
(test (not (ismember? 10 '( 1 2 3 4 5 6 7 8 9))))
(test (ismember? 1 '(3 4 5 1 3 4)))
(test (ismember? 1 '(1)))
(test (ismember? 10 '( 1 2 3 4 5 6 7 8 9 10)))


(: remove-duplicates : SET  -> SET)
(define (remove-duplicates l)
  (cond [(or (null? l) (null? (rest l))) l]
        [(ismember? (first l) (rest l)) (remove-duplicates (rest l))]
        [else (cons (first l) (remove-duplicates (rest l)))]))
  
(: create-sorted-set : SET -> SET)
(define (create-sorted-set l)
  (remove-duplicates (sort l <)))
  
(: set-union : SET SET -> SET)
(define (set-union A B)
  (create-sorted-set (append A B)))

(: set-intersection : SET SET -> SET)
(define (set-intersection A B)
  (: mem-filter : Number -> Boolean)
  (define (mem-filter n)
    (ismember? n A))
  (filter mem-filter (create-sorted-set B)))

  


;; ---------------------------------------------------------
#|
In The parser function we take Sexpr as an arguments and returning a SOL.
The parts we filled in:
    'False (Bool false) - If we see a 'False symbol return false boolean.
    CallS (Fun name1 name2 (parse-sexpr body)) (parse-sexpr named1)(parse-sexpr named2)) -
    We replaced the with by Calls and function, using statically calling.

    We filled some error text from the tests.
    The 'call-dynamic is filled very similiar to the 'call-statically part.
    In the equal and the cond we just call the parse-sexpr every time.

This took some time, mainly syntax and many testing.
We took turns on debugging after writing it together.
Overall it took 1 hour.
|#

;; Parser
(: parse-sexpr : Sexpr -> SOL)
;; to convert s-expressions into SOLs
(define (parse-sexpr sexpr)
  (match sexpr
    [(list (number: ns) ...) (Set (create-sorted-set ns))] ;; sort and remove-duplicates
    ['True (Bool true)] 
    ['False (Bool false)] 
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name1) named1 (symbol: name2) named2) body)
        (CallS (Fun name1 name2 (parse-sexpr body)) (parse-sexpr named1)(parse-sexpr named2))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(cons 'fun more)
     (match sexpr
       [(list 'fun (list (symbol: name1) (symbol: name2)) body)
        (if (equal? name1 name2)
            (error 'parse-sexpr "`fun' has a duplicate param name in ~s" sexpr) ;; cannot use the same param name twice
            (Fun name1 name2 (parse-sexpr body)))]
       [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
    [(list 'scalar-mult (number: sc) rhs) (Smult sc (parse-sexpr rhs))]
    [(list 'intersect lhs rhs) (Inter (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'union lhs rhs) (Union (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'call-static fun arg1 arg2) (CallS (parse-sexpr fun) (parse-sexpr arg1) (parse-sexpr arg2))]
    [(list 'call-dynamic fun arg1 arg2) (CallD (parse-sexpr fun) (parse-sexpr arg1) (parse-sexpr arg2))]
    [(list 'if cond 'then true-cond 'else false-cond) (If (parse-sexpr cond) (parse-sexpr true-cond) (parse-sexpr false-cond))]
    [(list 'equal? lhs rhs) (Equal (parse-sexpr lhs) (parse-sexpr rhs))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))



(: parse : String -> SOL)
;; parses a string containing a SOL expression to a SOL AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

  

;;; Tests for parse
 
(test (parse "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => (Set '(1 2 3 4)))
(test (parse "{union {1 2 3} {4 2 3}}") => (Union (Set '(1 2 3)) (Set '(2 3 4))))
(test (parse "{union {1 2 3 4 5} {5 4 2 3}}") => (Union (Set '(1 2 3 4 5)) (Set '(2 3 4 5))))
(test (parse "{fun {x x} x}") =error> "parse-sexpr: `fun' has a duplicate param name in (fun (x x) x)")
(test (parse "{intersect {1 2 3} {4 2 3}}") => (Inter (Set '(1 2 3)) (Set '(2 3 4))))
(test (parse "{with {S {intersect {1 2 3} {4 2 3}} c {}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}") 
      =>
      (CallS (Fun 'S
                  'c
                  (CallS (Fun 'x 'y (Union (Id 'x) (Id 'S))) 
                         (Smult 3 (Id 'S)) 
                         (Set '(4 5 6 7 8 9))))
             (Inter (Set '(1 2 3)) (Set '(2 3 4)))
             (Set '())))

(test (parse "False") => (Bool false))
(test (parse "{with {Avital} Pikovsky}") =error> "parse-sexpr: bad `with' syntax in (with (Avital) Pikovsky")
(test (parse "{with {Omer} Katz}") =error> "parse-sexpr: bad `with' syntax in (with (Omer) Katz")

(test (parse "{1 10 100 1000 10000 '100000'}") =error> "bad syntax")

(test (parse "{with {S {intersect {1 2 3} {4 2 3}} S1 {union {1 2 3} {4 2 3}}}
                          {fun {x} S}}")
      =error> "parse-sexpr: bad `fun' syntax in (fun (x) S)") ;; functions require two formal parameters
(test (parse "True") => (Bool true))
(test (parse "{if {equal? {1 2 3} {1 2}} then {1 2 3} else {1 2}}") =>
      (If (Equal (Set '(1 2 3)) (Set '(1 2))) (Set '(1 2 3)) (Set '(1 2))))

(test (parse "{with {S {intersect {1 2 3} {4 2 3}} c {}}
                 {call-dynamic {fun {x y} {union x S}}
                               {if {equal? S {scalar-mult 3 S}}
                                   then S
                                   else {4 5 7 6 9 8 8 8}}
                               {}}}")
      => (CallS (Fun 'S 'c
                     (CallD (Fun 'x 'y (Union (Id 'x) (Id 'S)))
                            (If (Equal (Id 'S) (Smult 3 (Id 'S)))
                                (Id 'S)
                                (Set '(4 5 6 7 8 9)))
                            (Set '())))
                (Inter (Set '(1 2 3)) (Set '(2 3 4)))
                (Set '())))

;;-----------------------------------------------------
;; Evaluation 
#|
------------------------------------------------------

Those are the evaluation rules.
We will implement the eval function by them.
There were few parts we needed to fill in here.
most of them were easy, like the part with if Then else and the True\False.

The Main issue is with the call static and call dynamic eval rules.
We needed to go and watch some classes again, and also to look on the assistant presentations.
After a long time Avital figures it out.
It took us 1 hour.

Evaluation rules:
    
    eval({ N1 N2 ... Nl })      = sort( create-set({ N1 N2 ... Nl })) ;; where create-set removes all duplications from
                                                                         the sequence (list) and sort is a sorting procedure

    eval({scalar-mult K E})     = { K*N1 K*N2 ... K*Nl }              ;; where eval(E)={ N1 N2 ... Nl }
    eval({intersect E1 E2})     = sort( create-set(set-intersection (eval(E1) , eval(E2)))     
    eval({union E1 E2})         = sort( create-set(set-union (eval(E1) , eval(E2)))
    eval({fun {x1 x2} E},env)   = <{fun {x1 x2} E}, env>
    eval({call-static E-op E1 E2},env)
                                = eval(Ef,extend(x2,eval(E2,env) ... (extend (x1, eval(E1,env),f-env)) )
                                                      if eval(E-op,env) = <{fun {x1 x2} Ef}, envf>
                                = error!              otherwise
    eval({call-dynamic E-op E1 E2},env)
                                = eval(Ef,extend(x2,eval(E2,env) (extend (x1, eval(E1,env),env))
                                                      if eval(E-op,env) = <{fun {x1 x2} Ef}, envf>
                                = error!              otherwise

    eval(True,env)              = true
    eval(False,env)             = false
    eval({if E1 then E2 else E3},env)
                                = eval(E3, env)       if eval(E1,env) = false
                                = eval(E2 ,env)       otherwise

    eval({equal? E1 E2},env)    = true                if eval(E1,env) is equal in content to eval(E2,env)
                                = false               otherwise
|#

;; Types for environments, values, and a lookup function
;;Only the word 'Boolean' was missing. very easy 3 minutes.
(define-type ENV
  [EmptyEnv]
  [Extend Symbol VAL ENV])

(define-type VAL
  [SetV SET]
  [FunV Symbol Symbol SOL ENV]
  [BoolV Boolean]) 

(: lookup : Symbol ENV -> VAL)
(define (lookup name env)
  (cases env
    [(EmptyEnv) (error 'lookup "no binding for ~s" name)]
    [(Extend id val rest-env)
     (if (eq? id name) val (lookup name rest-env))]))

(test (lookup 'sym (EmptyEnv)) =error> "no binding for sym")

;; Auxiliary procedures for eval  

(: SetV->set : VAL -> SET)
(define (SetV->set v)
  (cases v
    [(SetV S) S]
    [else (error 'SetV->set "expects a set, got: ~s" v)]))

(test (SetV->set (FunV 'x 'y (Id 'x) (EmptyEnv))) =error> "SetV->set: expects a set")

#|
This function get a number and a VAL type, and if the VAL is in form of a SET, it multiple the VAL by the given number.
It uses a map to multiple the whole set at once.

In the past tasks we used the map function, so we already know how to write it's syntax.
Omer did this question, 25 minutes.
|#
(: smult-set : Number VAL -> VAL)
(define (smult-set n s)
  (: mult-op : Number -> Number)
  (define (mult-op k)
    (* k n))
  (SetV (map mult-op (SetV->set s))))


#|
Here we needed to fill the first line of the funciton, the arguments and the returned type.
It's writen below on the parcial code that the function get an operator and use it on VAL's, returning a new VAL.
Avital did this question, 20 minutes.
|#
(: set-op :  (SET SET -> SET) VAL VAL -> VAL )
;; gets a binary SET operator, and uses it within a SetV
;; wrapper
(define (set-op op val1 val2)
  (SetV (op (SetV->set val1) (SetV->set val2))))


;;---------  the eval procedure ------------------------------

#|
The eval function. similiar to past tasks, get a SOL and ENV and returned a SOL.
It checks the type of the given VAL and return new VAL by it.
We filled some simple operators like union, smult and equal.

Again like the evaluation rules, the issue was the CallS and CallD type.
After we spend more that one hour on those rules specifically, it was ok.
Avital went on the presentation again, and Omer looked online.
in a combination between us we were able to find the right syntax.
it took 2 hours.
|#
(: eval : SOL ENV -> VAL)
;; evaluates SOL expressions by reducing them to set values
(define (eval expr env)
  (cases expr
    [(Set S) (SetV (create-sorted-set S))]
    [(Smult n set) (smult-set n (eval set env))]
    [(Inter l r) (set-op set-intersection (eval l env) (eval r env))]
    [(Union l r) (set-op set-union (eval l env) (eval r env))]
    [(Id name) (lookup name env)]
    [(Fun bound-id1 bound-id2 bound-body)
     (FunV bound-id1 bound-id2 bound-body env)]
    [(CallS fun-expr arg-expr1 arg-expr2)
     (let ([fval (eval fun-expr env)])
       (cases fval
         [(FunV bound-id1 bound-id2 bound-body f-env)
          (eval bound-body
                (Extend bound-id1 (eval arg-expr1 env)
                        (Extend bound-id2 (eval arg-expr2 env) 
                                f-env)))]
         [else (error 'eval "`call-static' expects a function, got: ~s"
                      fval)]))]
    [(CallD fun-expr arg-expr1 arg-expr2)
     (let ([fval (eval fun-expr env)])
       (cases fval
         [(FunV bound-id1 bound-id2 bound-body f-env)
          (eval bound-body
                (Extend bound-id2 (eval arg-expr2 env) 
                        (Extend bound-id1 (eval arg-expr1 env)
                                env)))]
         [else (error 'eval "`call-dynamic' expects a function, got: ~s"
                      fval)]))]
    [(Bool b) (BoolV b)]
    [(If cond true-cond false-cond)
     (let ([cval (eval cond env)])
       (cases cval
         [(BoolV b) (if b
                    (eval true-cond env)
                    (eval false-cond env))]
         [else (eval true-cond env)]))]
    [(Equal l r) (if (equal? (eval l env) (eval r env)) (BoolV true) (BoolV false))]))

(test (eval (CallD (Set '(100)) (Set '()) (Set '())) (EmptyEnv)) =error> "`call-dynamic' expects a function, got: #(struct:SetV (100))")

#|
In this funciton we create a global environment that builts on pairs, a first argument and second argument.
It can return the first element, the second element, and create the pair with cons.
There was some guidence in the task's pdf and we used it.
every one of the 'operators' gets an empty environment.
the first and the second got 2 params, one of them is a spare,
and using a statically calling it gets the 2 arguments, x and y,
and return one of them, depend if we asked the first ot the second.

In the cons part, we used the First and Second from the first two lines to create a pair.

We did this one together, and it took us 1 hours.
|#
(: createGlobalEnv : -> ENV)
(define (createGlobalEnv)
  (Extend 'second (FunV 'p 'spare-param (CallS (Id 'p) (Fun 'a 'b (Id 'b)) (Set '())) (EmptyEnv))
          (Extend 'first (FunV 'p 'spare-param (CallS (Id 'p) (Fun 'a 'b (Id 'a)) (Set '())) (EmptyEnv))
                  (Extend 'cons (FunV 'f 's (Fun 'selector 'sym (CallD (Id 'selector) (Id 'f) (Id 's))) (EmptyEnv)) 
                          (EmptyEnv)))))

#|
This is the run function that wrap it all.
it gets a string and return one of the following - SET, VAL or Boolean.
We wrote what to do if the case is SetV,
BoolV and in the Else section we return the VAL which is the result. we did it together and it took 35 minutes.
|#
(: run : String -> (U SET VAL Boolean))
;; evaluate a SOL program contained in a string
(define (run str)
  (let ([result (eval (parse str) (createGlobalEnv))])
    (cases result
      [(SetV S) S]
      [(BoolV B) B]
      [else result]))) ;;FunV

(test (run "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => '(1 2 3 4))
(test (run "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => '(1 2 3 4))
(test (run "{union {1 2 3} {4 2 3}}") => '(1 2 3 4))
(test (run "{intersect {1 2 3} {4 2 3}}") => '( 2 3))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}
                   S1 {}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}")
      => '(2 3 6 9))

(test (run "{call-static {fun {x y} {union x y}}
                              {4 5 7 6 9 8 8 8}
                              {4 5 7 6 9 8 8 8}}")
      => '(4 5 6 7 8 9))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}
                   S1 {}}
               {call-static {fun {x y} {union x y}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}")
      => '(4 5 6 7 8 9))

(test (run "{with {p {call-static cons {1 2 3} {4 2 3}}
                    S1 {}}
              {with {S {intersect {call-static first p {}}
                                  {call-static second p {}}}
                     S1 {}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}}")
      =>  '(2 3 6 9))

(test (run "{fun {x x} x}") =error> "parse-sexpr: `fun' has a duplicate param name in (fun (x x) x)")

(test (run "{with {p {call-dynamic cons {1 2 3} {4 2 3}}
                   S1 {}}
              {with {S {intersect {call-dynamic first p {}}
                                  {call-dynamic second p {}}}
                     S1 {}}
                 {call-dynamic {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}}")
      =>  '(2 3 6 9))

(test (run "{call-static {1} {2 2} {}}")
      =error> "eval: `call-static' expects a function, got: #(struct:SetV (1))")

(test (run "True") => #t)
(test (run "False") => #f)

(test (run "{if {equal? {1 2 3} {1 2}} then {1 2 3} else {1 2}}") => '(1 2))
(test (run "{if {equal? {1 2 3} {1 2 3}} then {1 2 3} else {1 2}}") => '(1 2 3))
(test (run "{if {1 2 3} then {1 2 3} else {1 2}}") => '(1 2 3))

(test (run "{call-static {1} {10} {100}}")=error> "`call-static' expects a function" )
(test (run "{call-dynamic {1} {10} {100}}")=error> "`call-dynamic' expects a function")
(test (run "{equal? {union {1 2 3} {4 2 3}} {1 2 3 4}}") => #t)
(test (run "{union {equal? {4} {4}} {4 2 3}}") =error> "SetV->set: expects a set, got: #(struct:BoolV #t)")


(test (run "{with {p {call-static cons {1 2 3} {4 2 3}} c{}}
{with {foo {fun {x y} {intersect x y}} S1 {}}
{call-static p foo {}}}}")
      => '(2 3))

(testד => (FunV
 'x
 'y
 (Union (Id 'x) (Id 'X))
 (Extend
  'second
  (FunV 'p 'spare-param (CallS (Id 'p) (Fun 'a 'b (Id 'b)) (Set '())) (EmptyEnv))
  (Extend
   'first
   (FunV 'p 'spare-param (CallS (Id 'p) (Fun 'a 'b (Id 'a)) (Set '())) (EmptyEnv))
   (Extend 'cons (FunV 'f 's (Fun 'selector 'sym (CallD (Id 'selector) (Id 'f) (Id 's))) (EmptyEnv)) (EmptyEnv))))))

#|
######################## 3 Part C : Open questions ########################

######################## Q1. ########################

  Set - a set which is sorted and unique.  
  Smult - multiplication by a scalar and contains 2 args, the first element is a number and the second one is an SOL
       and it multiplie each number on the list by the scalar.
  Inter - an Intersection between 2 SOL's, it gets 2 SOLS as arguments and rerurns the numbers that exists in both SOL's).
  Union - a Union between 2 SOL's, it gets 2 SOLS as arguments and returns the numbers that exists at the list in one 0f the SOL's.
  Id - used for unique names(Variables).
  Fun - Simple function that holds two params and had a name.
  CallS- allowing us to call a function in a static environment, gets 3 as arguments. the first one is a function, the second and third one are
        arguments for the functions.
  CallD - allowing us to call a function in a dynamic environment, gets 3 SOLS as arguments. the first one is a function and the second and third one are
        arguments for the functions.
  Bool - A simple Boolean.
  Ifs - An if expression that getting 3 SOL's. one for Condition, one for 'then' and one for 'else'
  Equal - checking if 2 SOL's are equal.

######################## Q2. ########################
In the parsing secction, if we see the expression call-static we are using CallS, calling the function statically.
                         if we see the expression call-dynamic we are using CallD, calling the function dynamically.
In addition, when there is a with expression we calls the function statically.
In one of the built in test we saw that after using with in the given string, it was expecting a usage of the CallS function.
We learned in class that with is syntetic sugar for call and lambda.
When we are using it statically , the variables get their values on definition and not duting run time.

######################## Q3. ########################
The function has 3 parts, The first element, the second element and the Cons that create a pair.
In the cons parts we used CallD, but it works fine with CallS as well.
In the other parts, the first element and the second element, it must be CallS. CallD will give an error.
Thats because Extend adds variables to the environment, and if it was using dynamic function it will add to his environment.
Thus, the cons would not recognize him, giving the error : no binding for f/s.

######################## Q4. ########################
There is no difference - we cheaked all the possibilities in that test.
The reason is that all the variables are inside the function.
There is no need for other variables from environments.

This test works fine :
(test (run "{with {p {call-dynamic cons {1 2 3} {4 2 3}}
S1 {}}
{with {S {intersect {call-dynamic first p {}}
{call-dynamic second p {}}}
S1 {}}
{call-dynamic {fun {x y} {union x S}}
{scalar-mult 3 S}
{4 5 7 6 9 8 8 8}}}}")
=> '(2 3 6 9))

|#
