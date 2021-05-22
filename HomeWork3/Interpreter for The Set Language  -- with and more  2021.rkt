#lang pl

#|-------------------Part A-------------------|#

#|
In this part we needed to fill the BNF, and some functions for the SOL type, which is a list of numbers.
Here at the BNF, we just checked the expressions that can be SOL form the Task PDF, and filled by it.

*very easy, we know how to write BNF from past tasks.
it took us 10 minutes.

 Please complete the missing rules below  
<SOL> :: = { <NumList> }
        |  { scalar-mult <num> <SOL> }
        |  { intersect <SOL> <SOL>}
        |  { union <SOL> <SOL> } 
        |  <id>
        |  { with {<id> <SOL> } <SOL> } ;; this should be a syntactic sugar
       
<NumList> :: =  λ | <num> | <num> <NumList> ;; where λ stands for the empty word, i.e., { } is the empty set

;; where <num> is any expression identified by Racket as a Number
;; and <id> is any expression such that Racket identifies '<id> as a symbol
 
|#


;; -----------------------------------------------------
;; The abstract syntax tree SOL
(define-type SET = (Listof Number))
(define-type SOL
  ;; Please complete the missing parts -- you are NOT allowed to use additional variants (constructors)
    [Set  SET]
    [Smult Number SOL]
    [Inter SOL SOL]
    [Union SOL SOL]
    [IdS    Symbol]
    [WithS  Symbol SOL SOL])

;; ----------------------------------------------------
;; Operations on SETs
;; Please complete the missing parts, and add comments (comments should specify 
;; the role of each procedure, but also describe your work process). Keep your code readable. 

#|
is member : this function gets a number and a set, and return true if the number is in the set,
 false otherwise.
It works recursively, checking on the first number, and then call the function with the rest of the set.

*Again, after some practice in recursion with the previous tasks, recursion in not a problem.
it took us 20 minutes.
|#

(: ismember? : Number SET  -> Boolean)
(define (ismember? n l)
  (cond
   [(null? l) #f]
   [(eq? (first l) n) #t]
   [else (ismember? n (rest l) )]))

(test (ismember? 1 '(3 4 5)) => #f)
(test (ismember? 1 '()) => #f)
(test (ismember? 1 '(1)) => #t)
(test (ismember? 1 '(1 2 3 4 5 6)) => #t)
(test (ismember? 1 '(111)) => #f)
(test (ismember? 100 '(10 100 1000)) => #t)

#|
remove-duplicates : this function get a SET, and return a uniqe SET from it, removing all the duplicate numbers.
we used the 'is member' function, recursively.
each time we check if the first number of the set is inside the rest of the set, if it is, he is a duplicate, and can be removed.
after the check move on to the rest of the set, with or without the first number, depends of the 'is member' result.

*We knew what to do right away,
but we had some problems with the 'cons' syntax.
We checked at the class presentation for help and found the right syntax.
it took us 30 minutes.
|#

(: remove-duplicates : SET  -> SET)
(define (remove-duplicates l)
  (cond
   [(null? l) null]
   [(ismember? (first l) (rest l)) (remove-duplicates (rest l))]
   [else (cons (first l) (remove-duplicates (rest l)))]))

(test (remove-duplicates '(3 4 5 1 3 4)) => '(5 1 3 4))
(test (remove-duplicates '(1)) => '(1))
(test (remove-duplicates '()) => '())
(test (remove-duplicates '(3 4 5 1 3 1 4)) => '(5 3 1 4))
(test (remove-duplicates '(10 100 1000 10 100)) => '(1000 10 100))
(test (remove-duplicates '(1 2 3 4 5 5)) => '(1 2 3 4 5))

#|
create-sorted-set : this function get a SET, and sort it by value of the numbers.
After removing the duplicates using the function that we built before,
 just using the built in racket function 'sort' on the set.

*Not much problems here, pretty strait forward.
it took us 20 minutes.
|#

(: create-sorted-set : SET -> SET)
(define (create-sorted-set l)
  (sort (remove-duplicates l) <))

(test (create-sorted-set '(3 4 5)) => '(3 4 5))
(test (create-sorted-set '( 3 2 3 5 6)) => '(2 3 5 6))
(test (create-sorted-set '()) => '())
(test (create-sorted-set '(3 4 5)) => '(3 4 5))
(test (create-sorted-set '( 1 2 3 4 5 6 5 4 3 2 1)) => '(1 2 3 4 5 6))
(test (create-sorted-set '(100 10 1)) => '(1 10 100))

#|
set-union: this function get two SET's, and return a SET which is a union of them.
It works recursively, checking if the first number of a is in B. if not, add it to B and continue moving on A.
if it is in B, don't add it and continue.


*this recursion was little more complicated,
because now we have 2 SET's to think about.
It took us time to figure out how to do it, but after many sketches and thinking we did it that way.
it took us 1 hour.
|#
(: set-union : SET SET -> SET)
(define (set-union A B)

  (cond 
   [(null? B) (create-sorted-set A)]
   [(null? A) (create-sorted-set B)]
   [(ismember? (first A) B) (set-union (rest A) B)]
   [else (set-union (rest A) (cons (first A) B))]))

(test (set-union '(3 4 5) '(3 4 5)) => '(3 4 5))
(test (set-union '(3 4 5) '()) => '(3 4 5))
(test (set-union '(3 4 5) '(1)) => '(1 3 4 5))
(test (set-union '(1 2 3) '(3 4 5)) => '(1 2 3 4 5))
(test (set-union '(10 100 10000) '(1000)) => '(10 100 1000 10000))
(test (set-union '() '(1 2 3 4 5)) => '(1 2 3 4 5))

#|
set-intersection : this function get two SET's, and return a SET which is a intersection of them.
It works recursively, using a filter on SET B, cheking if every memebr of B is part of A.
In other words, it loops on B and checks which numbers are in both of the lists.


*Needed some time to look on the racket documentation site for the filther method,
but after that it was fine.
It took us 30 minutes.
|#
(: set-intersection : SET SET -> SET)
(define (set-intersection A B)
  (: mem-filter : Number -> Boolean)
  (define (mem-filter n)
    (ismember? n A))
  (create-sorted-set (filter mem-filter B)))

(test (set-intersection '(3 4 5) '(3 4 5)) => '(3 4 5))
(test (set-intersection '(3 4 5) '(3)) => '(3))
(test (set-intersection '(3 4 5) '(1)) => '())
(test (set-intersection '(10 20 30 40 50) '(30 10 50)) => '(10 30 50))
(test (set-intersection '(3 4 5) '(5 3)) => '(3 5))
(test (set-intersection '() '(1 3 5)) => '())

#|
set-smult : this function get a number and a SET, return a SET which is the given SET multiplie by the number.
If the list in null(empty) it returns it.
if it's not empty, first multiplie the first number of the SET, and move forward to the rest.

*Now that we know how to use 'cond', it was very easy.It took us 15 minutes.
|#
(: set-smult : Number (Listof Number) -> SET)
(define (set-smult n l)
   (cond 
   [(null? l) null]
   [else (create-sorted-set (cons (* n (first l)) (set-smult n (rest l))))]))


(test (set-smult 3 '(4 3 5)) => '(9 12 15))
(test (set-smult 2 '()) => '())
(test (set-smult 1 '(1 2 3 4 5)) => '(1 2 3 4 5))
(test (set-smult 10 '(10 10 10)) => '(100))
(test (set-smult 2 '(20 10 30)) => '(20 40 60))
;; ---------------------------------------------------------
;; Parser
#|
The parser get a Sexpr and turn it to SOL.
it use match to check the status of the sexpr, and returnthe right SOL by it.
Basically it get a sytax of sexpr, i.e scalar-mult and switch it to SOL syntax, i.e Smult.

*Complicated. we had help from the class presentation that are in the moodle.
it took 1.5 hour to understand the With part, the rest was ok.

|#
(: parse-sexprS : Sexpr -> SOL)
;; to convert s-expressions into SOLs
(define (parse-sexprS sexpr)
  (match sexpr
    [(list (number: ns) ...) (Set ns)] 
    [(symbol: name) (IdS name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (WithS name (parse-sexprS named) (parse-sexprS body))] 
          [else (error 'parse-sexprS "bad `with' syntax in ~s" sexpr)])]
    [(list 'scalar-mult (number: sc) rhs) (Smult sc (parse-sexprS rhs))]
    [(list 'intersect lhs rhs) (Inter (parse-sexprS lhs) (parse-sexprS rhs))]
    [(list 'union lhs rhs) (Union (parse-sexprS lhs) (parse-sexprS rhs))]
    [else (error 'parse-sexprS "bad syntax in ~s" sexpr)]))


(: parseS : String -> SOL)
;; parses a string containing a SOL expression to a SOL AST
(define (parseS str)
  (parse-sexprS (string->sexpr str)))

  
(test (parseS "{1 3  4 1 4  4 2 3 4 1 2 3}") => (Set '(1 3 4 1 4 4 2 3 4 1 2 3)))
(test (parseS "{scalar-mult 2 {4 2 3}}") => (Smult 2 (Set '(4 2 3))))
(test (parseS "{union {1 2 3} {4 2 3}}") => (Union (Set '(1 2 3)) (Set '(4 2 3))))
(test (parseS "{intersect {1 2 3} {4 2 3}}") => (Inter (Set '(1 2 3)) (Set '(4 2 3))))
(test (parseS "{with {X {union {10 20 30} {40 20 30}}}
                 {intersect X X}}") => (WithS 'X
                                              (Union (Set '(10 20 30)) (Set '(40 20 30)))
                                              (Inter (IdS 'X) (IdS 'X))))
(test (parseS "{with S {intersect {1 2 3} {4 2 3}}
                 {union S S}}")
      =error> "parse-sexprS: bad `with' syntax in")

(test (parseS "{what S {A {1 2 3} {4 2 3}}
                 {union S S}}")
      =error> "parse-sexprS: bad syntax in")

;;-----------------------------------------------------
;; Substation 
#|
------------------------------------------------------
 Formal specs for `subst':
   (`Set' is a <NumList>, E, E1, E2 are <SOL>s, `x' is some <id>,
   `y' is a *different* <id>)
      Set[v/x]              = Set
      {smult n E}[v/x]      = {smult n E[v/x]}
      {inter E1 E2}[v/x]    = {inter E1[v/x] E2[v/x]}
      {union E1 E2}[v/x]    = {union E1[v/x] E2[v/x]}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}

substS: this function get a SOL, symbol and another SOL,
 and change the first sol, by the given symbol, to the second SOL.
it uses cases to figure out the 'status' of the first SOL, and return the subs by it.
i.e if the SOL is in form of Smult, it multiplie the number by the subs SOL.

*The description of the substS function that is given is kind of confusing,
 we didn't understand it at first.
 We found more inmormation after we revisit the classes.
It took us 3 hours.

|#
(: substS : SOL Symbol SOL -> SOL)
(define (substS expr from to)
  (cases expr
    [(Set n) expr]
    [(Smult n s) (Smult n (substS s from to))]
    [(Inter l r) (Inter (substS l from to) (substS r from to))]
    [(Union l r) (Union (substS l from to) (substS r from to))]
    [(IdS name) (if (eq? name from) to expr)]
    [(WithS bound-id named-expr bound-body)
     (WithS bound-id
           (substS named-expr from to)
           (if (eq? bound-id from)
               bound-body
               (substS bound-body from to)))]))

;;-----------------------------------------------------
;; Evaluation 
#|
------------------------------------------------------
Evaluation rules:
    ;; Please complete the missing parts in the formal specifications below

    eval({ N1 N2 ... Nl })  =  (sort (create-set (N1 N2 ... Nl)))
                               where create-set removes all duplications from
                              the sequence (list) and sort is a sorting procedure
    eval({scalar-mult K E}) =  (K*N1 K*N2 ... K*Nl) if (N1 N2 ... Nl) = eval(E,) is a sorted set AND
                                = error! otherwise (if S is not a sorted set)
    eval({intersect E1 E2}) = (sort (create-set (set-intersection (eval(E1,) , eval(E2,))))
                                    if both E1 and E2 evaluate to sorted sets
                                = error! otherwise
    eval({union E1 E2}) = (sort (create-set (eval(E1,) , eval(E2,))))
                                  if both E1 and E2 evaluate to sorted sets
                             = error! otherwise
    eval({with {x E1} E2}) = eval(E2,extend(x,eval(E1,),)) 
|#



;;---------  the eval procedure ------------------------------
#|
eval this function get a SOL and return an evalued SET of it.
it uses cases to figure out the SOL type.
 When it get a SET, it's removing the duplicates and sorting the set.
if it is a function like Union, it split it to seperated evals.
if the SOL if of shape WithS, we used the substS from before to give new value to the 'name'.

*The class presentation was really helpul in this part.
the examples there were on the point.
It took us 30 minutes.

|#
(: eval : SOL -> SET)
;; evaluates SOL expressions by reducing them to set values
(define (eval expr)
  (cases expr
    [(Set S) (create-sorted-set (remove-duplicates S))]  ;; sort and remove-duplicates
    [(Smult n set) (set-smult n (eval set))]
    [(Inter l r) (set-intersection (eval l) (eval r))]
    [(Union l r) (set-union (eval l) (eval r))]
    [(WithS name named body)
     (eval (substS body
                   name
                   named))]
    [(IdS name) (error 'eval "free identifier: ~s" name)]))

(: run : String -> SET)
;; evaluate a SOL program contained in a string
(define (run str)
  (eval (parseS str)))
    
(test (run "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => '(1 2 3 4))
(test (run "{scalar-mult 2 {4 2 3}}") => '(4 6 8))
(test (run "{intersect {1 2 3} {4 1 2 3}}") => '(1 2 3))
(test (run "{union {1 2 3} {4 2 3}}") => '(1 2 3 4))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
                 {with {x {4 5 7 6 9 8 8 8}}
                    {union x S}}}")
      => '(2 3 4 5 6 7 8 9))
(test (run "{with {x {1 2 3 4}}
                  {with {x x} x}}") => '(1 2 3 4))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
              {union {scalar-mult 3 B}
                 {4 5 7 9 8 8 8}}}")
      =error> "eval: free identifier:")


#|-------------------Part B-------------------|#


#|
<WAE> ::= <num> 
   | {+ <WAE> <WAE>}
   | {-  <WAE> <WAE>}
   | {* <WAE> <WAE>}
   | {/ <WAE> <WAE>}
   | {with {<id> <WAE>} <WAE>}
   | <id>
|#



(define-type WAE
  [Num Number]
  [Add WAE WAE]
  [Sub WAE WAE]
  [Mul WAE WAE]
  [Div WAE WAE]
  [Id Symbol]
  [With Symbol WAE WAE])


(: parse-sexpr : Sexpr -> WAE) 
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n) (Num n)]
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> WAE)
(define (parse str)
  (parse-sexpr (string->sexpr str)))

#| Formal specs for `subst':
   (`N' is a <num>, `E1', `E2' are <WAE>s, `x' is some <id>,
   `y' is a *different* <id>)
      N[v/x]                = N
      {+ E1 E2}[v/x]        = {+ E1[v/x] E2[v/x]}
      {- E1 E2}[v/x]        = {- E1[v/x] E2[v/x]}
      {* E1 E2}[v/x]        = {* E1[v/x] E2[v/x]}
      {/ E1 E2}[v/x]        = {/ E1[v/x] E2[v/x]}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
|#



(: subst : WAE Symbol WAE -> WAE)
(define (subst expr from to)
  (cases expr
    [(Num n) expr]
    [(Add l r) (Add (subst l from to) (subst r from to))]
    [(Sub l r) (Sub (subst l from to) (subst r from to))]
    [(Mul l r) (Mul (subst l from to) (subst r from to))]
    [(Div l r) (Div (subst l from to) (subst r from to))]
    [(Id name) (if (eq? name from) to expr)]
    [(With bound-id named-expr bound-body)
     (With bound-id
           (subst named-expr from to)
           (if (eq? bound-id from)
               bound-body
               (subst bound-body from to)))]))


(: ismember-WAE? : Symbol (Listof Symbol) -> Boolean)
(define (ismember-WAE? n l)
  (cond
   [(null? l) #f]
   [(eq? (first l) n) #t]
   [else (ismember-WAE? n (rest l) )]))

  (: remove-duplicates-WAE : (Listof Symbol) -> (Listof Symbol))
  (define (remove-duplicates-WAE l)
  (cond
   [(null? l) null]
   [(ismember-WAE? (first l) (rest l)) (remove-duplicates-WAE (rest l))]
   [else (cons (first l) (remove-duplicates-WAE (rest l)))]))

#|
This function get a WAE type as argument, and return all the free instances identifiers that are in it.
We used similiar functions to first Part, is-member, subst, and remove duplicated.
We used the building of the WAE from the class.
The function use cases to figure out the status of the WAE, if it's add, sub and such.
Then it removes the duplicated values from the Left list and the Right list, and calling freeInstanceList on each of them.

*After we wrote all the functions in the first Part, and with the help from the code in the classes,
we knew what's need to be done.
It took time to write it all and to convert it to fit the WAE type, but it was OK.
It took us 3 hours.
|#

(: freeInstanceList : WAE -> (Listof Symbol))
(define (freeInstanceList expr)  
  (cases expr
    [(Num n) '()]
    [(Add l r) (remove-duplicates-WAE (append (freeInstanceList l) (freeInstanceList r)))]
    [(Sub l r) (remove-duplicates-WAE (append (freeInstanceList l) (freeInstanceList r)))]
    [(Mul l r) (remove-duplicates-WAE (append (freeInstanceList l) (freeInstanceList r)))]
    [(Div l r) (remove-duplicates-WAE (append (freeInstanceList l) (freeInstanceList r)))]
    [(With bound-id named-expr bound-body)
            (remove-duplicates-WAE (append (freeInstanceList named-expr) (freeInstanceList (subst bound-body bound-id (Num 0)))))]
    [(Id name) (list name)]))


(test (freeInstanceList (parse "w")) => '(w))
(test (freeInstanceList (Num 100)) => '())
(test (freeInstanceList (parse "{with {xxx 2} {with {yyy 3} {+ {- xx y} z}}}")) => '(xx y z))
(test (freeInstanceList (With 'x (Num 2) (Add (Id 'x) (Num 3)))) => '())
(test (freeInstanceList (parse "{* x z}")) => '(x z))
(test (freeInstanceList (parse "A")) => '(A))
(test (freeInstanceList (parse "{with {x 0} y}")) => '(y))
(test (freeInstanceList (parse "{with {xx 2} {with {yyy 5} {+ {/ xyx y} z}}}")) => '(xyx y z))
(test (freeInstanceList (With 'x (Num 2) (Mul (Id 'x) (Num 3)))) => '())
(test (freeInstanceList (parse "{+ z {* x z}}")) => '(x z))
(test (freeInstanceList (parse "{- x {with {x 0} {with {x x} x}}}")) => '(x))
(test (freeInstanceList (parse "{with x 55 {with {y {- x 2}} {+ y y}}}")) =error> "bad `with' syntax in")
(test (freeInstanceList (parse "{{x 100} {+ x {with {x 200} 20}}}")) =error> "bad syntax in")


