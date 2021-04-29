#lang pl 02

#|
***Question 1.a:

description:
I divided the SE into three different operators:
1.Only numbers, 2.single char, 3. All string-related operators.
I defined 1-> NUM as individual digits.
I defined D as a string type and there is an option to concatenate any number of digits I want.
I defined 2-> CHAR as a single character using the NUM I created.
And the last operator became six different parts:
1. <λ> - empty string.
2. "<D>" - where <D> stands for a finite sequence of digits. 
3. string operator that consumes CHARS - concatenated CHAR.
4. string-append that consumes AppendStringsOperator - concatenated StringsOperators.
5. string-insert that consumes StringsOperators, CHAR, NUMS.
6. number->string that consumes NUMS.


<SE> ::= <NUMS> (1)
        |<CHAR> (2)
        |<StringsOperators> (3)

<NUM> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

<CHAR> ::= #\<NUM> (11)

<CHARS> ::= <CHAR> (15)
           |<CHARS><CHAR> (16) 


<NUMS> ::= <D> (12)
          |{string-length <StringsOperators>} (10)

<AppendStringsOperator> ::= <StringsOperators> (13)
                          |<AppendStringsOperator> <StringsOperators> (14)

<StringsOperators> ::= <λ> (4)
            |"<D>" (5)
            |<string <CHARS> (6)
            |{string-append <AppendStringsOperator>} (7)
            |{string-insert <StringsOperators> <CHAR> <NUMS>} (8)
            |{number->string <NUMS>} (9)

***Question 1.b

example #1:

( string-append "45" ( number->string ( string-length "123" ) ))

<SE> (3)
<StringsOperators> (7)
{string-append <AppendStringsOperator>} (14)
{string-append <AppendStringsOperator> <StringsOperators>} (13)
{string-append <StringsOperators> <StringsOperators>} (5)
{string-append "<D>" <StringsOperators>}
{string-append "45" <StringsOperators>} (9)
{string-append "45" {number->string <NUMS>}} (10)
{string-append "45" {number->string {string-length <StringsOperators>}}} (5)
{string-append "45" {number->string {string-length "<D>"}}}
( string-append "45" ( number->string ( string-length "123" ) ))

example #2:

( string-append "" ( string-insert "1804" #\4 96 ) "" )

<SE> (3)
<StringsOperators> (7)
{string-append <AppendStringsOperator>} (14)
{string-append <AppendStringsOperator> <StringsOperators>} (14)
{string-append <AppendStringsOperator> <StringsOperators> <StringsOperators>} (13)
{string-append <StringsOperators> <StringsOperators> <StringsOperators>} (4)
{string-append <λ> <StringsOperators> <StringsOperators>} (8)
{string-append <λ> {string-insert <StringsFunctions> <CHAR> <NUMS>} <StringsOperators>} (5)
{string-append <λ> {string-insert "<D>" <CHAR> <NUMS>} <StringsOperators>}
{string-append <λ> {string-insert "1804" <CHAR> <NUMS>} <StringsOperators>} (11)
{string-append <λ> {string-insert "1804" #\4 <NUMS>} <StringsOperators>} (12)
{string-append <λ> {string-insert "1804" #\4 96} <StringsOperators>} (4)
{string-append <λ> {string-insert "1804" #\4 96} <λ>}
( string-append "" ( string-insert "1804" #\4 96 ) "" )



example #3:

( number->string ( string-length ( string #\1 #\8 #\4 ) ) )

<SE> (3)
<StringsOperators> (9)
{number->string <NUMS>} (10)
{number->string {string-length <StringsOperators>}} (6)
{number->string {string-length <string <CHARS>}} (16)
{number->string {string-length <string <CHARS><CHAR>}} (11)
{number->string {string-length <string <CHARS><#\4>}} (16)
{number->string {string-length <string <CHARS><CHAR><#\4>}}(11)
{number->string {string-length <string <CHARS><#\8><#\4>}} (15)
{number->string {string-length <string <CHAR><#\8><#\4>}} (11)
{number->string {string-length <string <#\1><#\8><#\4>}}
( number->string ( string-length ( string #\1 #\8 #\4 ) ) )

***Question 2:

explanation:
I used the square-helpFunc help function to square every value in the ListOf Number that
sum-of-squares function consumes with map function,
and then foldl summed each value that already squared.

|#

(: sum-of-squares : (Listof Number) -> Number)
(define (sum-of-squares list)
  (: square-helpFunc : Number -> Number)
  (define (square-helpFunc num)
    (* num num)
    )
  (foldl + 0 (map square-helpFunc list)) 
  )

(test (sum-of-squares '()) => 0)
(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(1 -1 1 -1)) => 4)
(test (sum-of-squares '(10 10 10)) => 300)
(test (sum-of-squares '(10 100 1000)) => 1010100)
(test (sum-of-squares '(9 1 8 2 7 3 6 4 5)) => 285)
(test (sum-of-squares '(-9 -8 -7 -6 -5 -4 -3 -2 -1)) => 285)

#|
***Question 3.a:

explanation:

createPolynomial consumes a Listof Number and returns function: polyX : Number -> Number
polyX call the poly function with argsL = coeffs, x -> the base of the power argument that the polynom consumes, power = 0 and accum = 0
recursive function poly checks if the argsL null, if its null it returns the accum,
if its not null, the function call itself recursively with args = rest argsL, x (the same), power = (+ power 1) and 
(+ accum (* (first argsL) (x^power)))
And so the function creates the polynom and accumulator it.

|#

(: createPolynomial : (Listof Number) -> (Number -> Number))
(define (createPolynomial coeffs)
  
  (: poly : (Listof Number) Number Integer Number -> Number)
  (define (poly argsL x power accum)
    (if (null? argsL)
        accum
        (poly (rest argsL) x (+ power 1) (+ accum (* (first argsL) (expt x power))))))
  
  (: polyX : Number -> Number)
  (define (polyX x)
    (poly coeffs x 0 0))
  polyX)

(define p2345 (createPolynomial '(2 3 4 5)))

(test (p2345 0) => (+ (* 2 (expt 0 0)) (* 3 (expt 0 1)) (* 4 (expt 0 2)) (* 5 (expt 0 3))))
(test (p2345 4) =>(+ (* 2 (expt 4 0)) (* 3 (expt 4 1)) (* 4 (expt 4 2)) (* 5 (expt 4 3))))
(test (p2345 11) => (+ (* 2 (expt 11 0)) (* 3 (expt 11 1)) (* 4 (expt 11 2)) (* 5 (expt 11 3))))
(test (p2345 100) => (+ (* 2 (expt 100 0)) (* 3 (expt 100 1)) (* 4 (expt 100 2)) (* 5 (expt 100 3))))
(test (p2345 -11) => (+ (* 2 (expt -11 0)) (* 3 (expt -11 1)) (* 4 (expt -11 2)) (* 5 (expt -11 3))))


(define p536 (createPolynomial '(5 3 6)))

(test (p536 11) => (+ (* 5 (expt 11 0)) (* 3 (expt 11 1)) (* 6 (expt 11 2))))
(test (p536 0) => (+ (* 5 (expt 0 0)) (* 3 (expt 0 1)) (* 6 (expt 0 2))))
(test (p536 5) => (+ (* 5 (expt 5 0)) (* 3 (expt 5 1)) (* 6 (expt 5 2))))
(test (p536 222) => (+ (* 5 (expt 222 0)) (* 3 (expt 222 1)) (* 6 (expt 222 2))))
(test (p536 3000) => (+ (* 5 (expt 3000 0)) (* 3 (expt 3000 1)) (* 6 (expt 3000 2))))
(test (p536 -111) => (+ (* 5 (expt -111 0)) (* 3 (expt -111 1)) (* 6 (expt -111 2))))


(define p_0 (createPolynomial '()))

(test (p_0 4) => 0)

#|
***Question 3.b.i:

explanation:

<PLANG> => expressions of the form {{poly C1 C2...Ck} {P1 P2...Pl}}
<AEs> => where all Ci and all Pj are valid <AEs> expression and <AEs> are one <AE> expression or more.



<PLANG> ::= {{poly <AEs>} {<AEs}} 

<AEs> ::= <AE>
         |<AE> <AEs>

<AE> ::= <num>
        |{+ <AE> <AE>}
        |{- <AE> <AE>}
        |{* <AE> <AE>}
        |{/ <AE> <AE>}


***Question 3.b.ii:

explanation:

The PLANG is in shape of [Poly (Listof AE) (Listof AE)]
The parse consumes a String and returns a PLANG.
The parse use let to insert sexpr to argument named code, the sexpr is the cunsume string that turn to sexpr (string->sexpr str)
To access to the left and rigth side i used match,
and i checked if the code has at least one coefficient as required, if not i throwed an exception
and if the code has at least one point as required, if not i throwed an exception
if the code has at least one coefficient and has at least one point that means it PLANG expression,
so I call parse-sexpr with one argument from lhs to get AE
and I call parse-sexpr with one argument from rhs to get AE, every time creates POLY with them and return it.

parse-sexpr consumes sexpr and returns AE expression- include num, +, -, * and /

|#

(define-type PLANG
  [Poly (Listof AE) (Listof AE)])

(define-type AE
  [Num Number]
  [Add AE AE]
  [Sub AE AE]
  [Mul AE AE]
  [Div AE AE])

(: parse-sexpr : Sexpr -> AE)
;; to convert s-expressions into AEs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n) (Num n)]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs)
                            (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs)(parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs)
                            (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs)
                            (parse-sexpr rhs))]
    [else (error 'parse-sexpr "bad syntax in ~s"
                 sexpr)]))
(: parse : String -> PLANG)
;; parses a string containing a PLANG expression to a PLANG AST
(define (parse str)
  (let ([code (string->sexpr str)])
    (match code
      [(list (list 'poly lhs ...) (list rhs ...))
       (if (null? lhs) (error 'parse "at least one coefficient is required in ~s" code)
           (if (null? rhs)(error 'parse "at least one point is required in ~s" code)
               (Poly(map parse-sexpr lhs)(map parse-sexpr rhs))))]
      [else (error 'parse "bad syntax in ~s" code)])))


(test (parse "{{poly 1 2 3} {1 2 3}}")
      => (Poly (list (Num 1) (Num 2) (Num 3))
               (list (Num 1) (Num 2) (Num 3))))
(test (parse "{{poly 1 2 3 4 5} {1 2 3 4 5 }}")
      => (Poly (list (Num 1) (Num 2) (Num 3) (Num 4) (Num 5))
               (list (Num 1) (Num 2) (Num 3) (Num 4) (Num 5))))

(test (parse "{{poly } {1 2} }")
      =error> "parse: at least one coefficient is required in ((poly) (1 2))")
(test (parse "{{poly } {1 2 3 4 5} }")
      =error> "parse: at least one coefficient is required in ((poly) (1 2 3 4 5))")

(test (parse "{{poly 1 2} {} }")
      =error> "parse: at least one point is required in ((poly 1 2) ())")
(test (parse "{{poly 1 2 3 4 5 } {} }")
      =error> "parse: at least one point is required in ((poly 1 2 3 4 5) ())")

(test (parse "{{poly A} {A v i t a l} }")
      =error> "bad syntax in A")

(test (parse "{{poly 1} {a v i t a l} }")
      =error> "bad syntax in a")

(test (parse "{{foly 1 2 3} {1 2 3}}")
      =error> "bad syntax in ((foly 1 2 3) (1 2 3))")

(test (parse "{{pol 1 2 3 4 5} {1 2 3 4 5}}")
      =error> "bad syntax in ((pol 1 2 3 4 5) (1 2 3 4 5))")


#|
***Question 3.b.iii:

explanation:

The run function consumes string and returns Listof Number, this function call parse with the string
and the parse returns PLANG (Iv'e explained about it in the previous qeustion)
the eval-poly consumes PLANG and returns Listof Number, the eval-poly define pNum function and call createPolynomial
with Listof Numbers after every argument from l side (with map) sents to eval and returns number.
The createPolynomial function returns polyX and pNum turn into the polyX.
every argument from r side (with map) sent to eval and returns number,
that send to pNum every time with another number that represent the base of the power in the polynom we want to calculate.

(test (run "{{poly 2 3} {4}}") => '(14))
2*4^0 + 3*4^1 = 14

|#
;; evaluates AE expressions to numbers
(: eval : AE -> Number)

(define (eval expr)
(cases expr
[(Num n) n]
[(Add l r) (+ (eval l) (eval r))]
[(Sub l r) (- (eval l) (eval r))]
[(Mul l r) (* (eval l) (eval r))]
[(Div l r) (/ (eval l) (eval r))]))
(: eval-poly : PLANG -> (Listof Number) )
(define (eval-poly p-expr)
 (cases p-expr
   [(Poly l r) (define pNum (createPolynomial (map eval l )))
               (map pNum (map eval r ))]
  ))

(: run : String -> (Listof Number))
;; evaluate a FLANG program contained in a string
(define (run str)
(eval-poly (parse str)))

(test (run "{{poly 1 2 3} {1 2 3}}") => '(6 17 34))
(test (run "{{poly 4 2 7} {1 4 9}}") => '(13 124 589))
(test (run "{{poly 1 2 3} {1 2 3}}") => '(6 17 34))
(test (run "{{poly 4/5 } {1/2 2/3 3}}")=> '(4/5 4/5 4/5))
(test (run "{{poly 2 3} {4}}") => '(14))
(test (run "{{poly 1 1 0} {-1 3 3}}") => '(0 4 4))
(test (run "{{poly {/ 4 2} {- 4 1}} {{- 8 4}}}") => '(14))
(test (run "{{poly {+ 0 1} 1 {* 0 9}} {{- 4 5} 3 {/ 27 9}}}") => '(0 4 4))

(test (run "{{poly 10 10 10} {-1 -1 -1}}") => '(10 10 10))
(test (run "{{poly 22 33 44} {5 6 7}}") => '(1287 1804 2409))
(test (run "{{poly -111 -222 -333} {1 2 3}}") => '(-666 -1887 -3774))

