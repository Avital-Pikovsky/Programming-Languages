#lang pl

#|
==========Question 1.1==========

@input: five characters.
@output: String - concatenation of the characters we consumed.

@description: In this question the function consumes five characters and concatenation them to a String.
I used the built-in string function to turn the five characters that the function consumes into a String.
|#
(: append5 : Char Char Char Char Char -> String )
(define (append5 c1 c2 c3 c4 c5)
  (string c1 c2 c3 c4 c5))


(test (append5 #\ל #\ט #\י #\ב #\א) => "לטיבא")
(test (append5 #\a #\b #\c #\d #\e) => "abcde")
(test (append5 #\e #\d #\c #\b #\a) => "edcba")
(test (append5 #\z #\z #\z #\z #\z) => "zzzzz")
(test (append5 #\h #\e #\l #\l #\o) => "hello")

#|
==========Question 1.2==========

@input: three characters.
@output: list of Strings.

@description: In this question the function consumes three characters and return a list
of six Strings in different permutations according to the example given in the question.
Also in this function I used a built-in function that would turn the three characters into a string.

Listof String  ≡ List String String String String String String
|#
(: permute3 : Char Char Char -> (Listof String))
(define (permute3 c1 c2 c3)
  (list
  (string c1 c2 c3)
  (string c1 c3 c2)
  (string c2 c1 c3)
  (string c2 c3 c1)
  (string c3 c1 c2)
  (string c3 c2 c1)))

(test (permute3 #\a #\b #\c) => '("abc" "acb" "bac" "bca" "cab" "cba"))
(test (permute3 #\z #\o #\o) => '("zoo" "zoo" "ozo" "ooz" "ozo" "ooz"))
(test (permute3 #\a #\v #\i) => '("avi" "aiv" "vai" "via" "iav" "iva"))
(test (permute3 #\t #\a #\l) => '("tal" "tla" "atl" "alt" "lta" "lat"))
(test (permute3 #\h #\e #\y) => '("hey" "hye" "ehy" "eyh" "yhe" "yeh"))


#|
==========Question 2.a==========

@input: list of lists.
@output: natural number.

@description: In this question the recursive function consumes a list of lists (of any type) and returns the number
of inner lists that contain exactly 3 elements.

In this recursive function I used cond, and summed up the number of inner lists that have three elements,
The stop condition is trigered when the list is empty,
and in this situation the function returns the result.
|#
(: count-3lists : (Listof (Listof Any)) -> Natural)
(define (count-3lists list)
  (cond
   [(null? list) 0]
   [(eq? (length(first list)) 3) (+ 1 (count-3lists( rest list)))]
   [else (count-3lists(rest list))]))

(test (count-3lists '()) => 0)
(test (count-3lists '((1 3 4) (() (1 2 3)) ("tt" "Three" 7) (2 4 6 8) (1 2 3))) => 3)
(test (count-3lists '(() ("a" "b" "c") ("avital" 25 1996) (1 2 3 4) (4 3 2 1))) => 2)
(test (count-3lists '(("a" "v" "i") (() (5 5 5) ()) ("Programming" "Language" "Course") (12.2 16.6 8.18) (11 7 5))) => 5)
(test (count-3lists '(("a" "v" "i" "t" "a" "l") (() (28 03 2021) ()) ("Ariel" "University" ":)") (1.1 2.2 3.3) (11 7 5))) => 4)
(test (count-3lists '(("one" "two" "Three"))) => 1)



#|
==========Question 2.b==========

@input: list of lists.
@output: natural number.

@description: In this question the tail-recursive function consumes a list of lists (of any type) and returns the number
of inner lists that contain exactly 3 elements.

In this function I used the helper tail recursive function that consumed as input the list of lists (of any type)
and the number 0 as a sum, in the tail function I used the cond and I summed the number of inner lists that contain 3 elements,
The stop condition is trigered when the list is empty, and in this situation the function returns the result.
|#

(: count-3lists-tail : (Listof (Listof Any)) -> Natural)
(define (count-3lists-tail list)
  (: count-3lists-tail-help : (Listof (Listof Any)) Natural -> Natural)
  (define (count-3lists-tail-help list counter)
  (cond
   [(null? list) counter]
   [(eq? (length(first list)) 3) (count-3lists-tail-help(rest list) (+ counter 1))]
   [else (count-3lists-tail-help(rest list) counter )]))
(count-3lists-tail-help list 0))

(test (count-3lists-tail '()) => 0)
(test (count-3lists-tail '((1 3 4) (() (1 2 3)) ("tt" "Three" 7) (2 4 6 8) (1 2 3))) => 3)
(test (count-3lists-tail '(() ("a" "b" "c") ("avital" 25 1996) (1 2 3 4) (4 3 2 1))) => 2)
(test (count-3lists-tail '(("a" "v" "i") (() (5 5 5) ()) ("Programming" "Language" "Course") (12.2 16.6 8.18) (11 7 5))) => 5)
(test (count-3lists-tail '(("a" "v" "i" "t" "a" "l") (() (28 03 2021) ()) ("Ariel" "University" ":)") (1.1 2.2 3.3) (11 7 5))) => 4)
(test (count-3lists-tail '(("one" "two" "Three"))) => 1)

#|
==========Question 2.c==========
@input: list of lists.
@output: natural number.

@description: In this question the function consumes a list of lists (of any type) and returns the number
of lists that contain exactly 3 elements recursively.

Function count-3listsRec checks if the list it consumes is a null list,
if not it sends to function list-3counter (helper function) the list with counter 0.
The function list-3counter checks the list it consumes is a list of three elements,
If this is true, the function checks the first element and the rest of the element with counter 1,
if this is false, the function checks the first element and the rest of the element with counter 0.
In this way the whole consumed list is recursively checked and the counter grows respectively.
|#
(: list-3counter : (Listof Any) Natural -> Natural)
(define (list-3counter list counter)
  (cond
   [(null? list) counter]
   [(list?(first list))
    (if (eq? (length(first list)) 3)
        (+ (list-3counter (rest list) (+ 1 counter)) (list-3counter (first list) 0))
        (+ (list-3counter (rest list) counter) (list-3counter (first list) 0)))]
   [else (list-3counter(rest list) counter)]))

(: count-3listsRec : (Listof (Listof Any)) -> Natural)
(define (count-3listsRec list)
    (cond
      [(null? list) 0]
      [else (list-3counter list 0)]))


(test (count-3listsRec '()) => 0)
(test (count-3listsRec '((1 3 4) (() (1 2 3)) ("tt" "Three" 7) (2 4 6 8) (1 2 3))) => 4)
(test (count-3listsRec '(() ("a" "b" "c") ("avital" 25 1996) (1 2 (1 2 (1 2 ()))) (4 3 2 1))) => 5)
(test (count-3listsRec '(("a" "v" "i") (() (5 5 5) ()) ("Programming" "Language" "Course") ((2 2 2) (3 3 3) (4 4 4)) (11 7 5))) => 9)
(test (count-3listsRec '(("a" "v" "i" "t" "a" "l") (() (28 03 2021) ()) ("Ariel" "University" ":)") (1.1 2.2 3.3) (11 7 5))) => 5)
(test (count-3listsRec '( ("one" "two" "Three") () ("h" "e" "y"))) => 2)



#|
==========Question 3.1 & 3.2==========

Implement a keyed-stack data structure.
KeyStack – define a new type.
EmptyKS – a variant of the data type.
Push – a variant of the data type, takes as input a Symbol key, a String value and keyed-stack and added it to the stack.
|#

(define-type KeyStack
  [EmptyKS]
  [Push Symbol String KeyStack])

(test (EmptyKS) => (EmptyKS))
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))) => (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
(test (Push 'h "hey" (Push 'e "hello" (EmptyKS))) => (Push 'h "hey" (Push 'e "hello" (EmptyKS))))
(test (Push 'a "Ariel" (Push 'u "University" (EmptyKS))) => (Push 'a "Ariel" (Push 'u "University" (EmptyKS))))
#|
==========Question 3.3==========

@input: Symbol and keyed-stack.
@output: first value that keyed according.

@description: The operation is a recursive function that searches for the key given to it and returns the value of the key,
otherwise returns false.
If the key is not found, it calls the rest of the stack it didn't search for and so on until it is found,
if the stack is empty and the key is not found the function returns false.
I used cases and cond to implement the function.
|#
(: search-stack : Symbol KeyStack -> (U String #f))
   
(define (search-stack key ks)
  (cases ks
    [(EmptyKS) #f]
    [(Push symbol string restStack)
     (cond
       [(eq? key symbol) string]
       [else (search-stack key restStack)])]))

(test (search-stack 'a (EmptyKS)) => #f)
(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "AAA")
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => #f)
(test (search-stack 'a (Push 'a "Avital" (Push 'b "Bob" (Push 'c "Charley" (EmptyKS))))) => "Avital")
(test (search-stack 'c (Push 'a "Ariel" (Push 'b "University" (Push 'c ":)" (EmptyKS))))) => ":)")
(test (search-stack 'p (Push 'p "Programming" (Push 'l "Language" (Push 'c "Course" (EmptyKS))))) => "Programming")
#|
==========Question 3.4==========

@input: keyed-stack.
@output: the keyed-stack without its first value.

@description: The operation take as input a key-stack and returns it without the first value,
if the keystack was received as an empty, the function returns false.
I used cases to implement the function.
|#

(: pop-stack : KeyStack -> (U KeyStack #f))
   
(define (pop-stack ks)
  (cases ks
    [(EmptyKS) #f]
    [(Push symbol string restStack) restStack]))

(test (pop-stack (EmptyKS)) => #f)
(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (pop-stack (EmptyKS)) => #f)
(test (pop-stack (Push 'a "Avital" (Push 'b "Bob" (Push 'c "Charley" (EmptyKS))))) => (Push 'b "Bob" (Push 'c "Charley" (EmptyKS))))
(test (pop-stack (Push 'a "Ariel" (Push 'b "University" (Push 'c ":)" (EmptyKS))))) => (Push 'b "University" (Push 'c ":)" (EmptyKS))))
(test (pop-stack (Push 'p "Programming" (Push 'l "Language" (Push 'c "Course" (EmptyKS))))) => (Push 'l "Language" (Push 'c "Course" (EmptyKS))))

;;==========Question 4==========


(: is-odd? : Natural -> Boolean)
#|
@input: natural number.
@output: boolean, true if the natural number is odd and false if it is not odd.

@description: This function is responsible to determine if an input natural number is odd or not.
If x is zero then x is even and the function returns false.
If x is not zero then we subtract 1 from it and send to the is-even? function
The two functions work recursively with each other so the x will eventually be equal to
zero and return an answer from one of the functions,
which will actually be the function with the correct answer.
|#
(define (is-odd? x)
(if (zero? x)
false
(is-even? (- x 1))))

(: is-even? : Natural -> Boolean)
#|
@input: natural number.
@output: boolean, true if the natural number is even and false if it is not even.

@description: This function is responsible to determine if an input natural number is even or not.
If x is zero then x is even and the function returns true.
If x is not zero then we subtract 1 from it and send to the is-odd? function
The two functions work recursively with each other so the x will eventually be equal to
zero and return an answer from one of the functions,
which will actually be the function with the correct answer.
|#
(define (is-even? x)
(if (zero? x)
true
(is-odd? (- x 1))))


;; tests --- is-odd?/is-even?
(test (not (is-odd? 12)))
(test (is-even? 12))
(test (not (is-odd? 0)))
(test (is-even? 0))
(test (is-odd? 1))
(test (not (is-even? 1)))
(test (is-even? 222))
(test (is-odd? 333))
(test (not (is-even? 999)))
(test (not (is-odd? 1000)))


(: every? : (All (A) (A -> Boolean) (Listof A) -> Boolean))
#|
@input: pred of type A -> Boolean and lst of type A.
@output: true if for every element in lst (pred element) -> true.

@description: The function checks if every element of type A return true in pred function.
The function checks if the lst is null OR the first element in the lst AND the rest elemnts in the lst
return true from pred function recursively, then the function return true.
|#
(define (every? pred lst)
(or (null? lst)
(and (pred (first lst))
(every? pred (rest lst)))))

(: all-even? : (Listof Natural) -> Boolean)
#|
@input: list of natural number.
@output: boolean- true if all the numbers in the list are even.

@description: The function uses two functions from the question, every? and is-even?
The purpose of the function is to check if every element of the list is even.
|#
(define (all-even? lst)
(every? is-even? lst))

;; tests
(test (all-even? null))
(test (all-even? (list 0)))
(test (all-even? (list 2 4 6 8)))
(test (not (all-even? (list 1 3 5 7))))
(test (not (all-even? (list 1))))
(test (not (all-even? (list 2 4 1 6))))
(test (all-even? (list 222 4444 66666 88888)))
(test (not (all-even? (list 333 444 55555))))


(: every2? : (All (A B) (A -> Boolean) (B -> Boolean) (Listof A) (Listof B) ->
Boolean))
#|
@input: pred1 of type A -> Boolean, pred2 of type B -> Boolean, lst1 of type A and lst1 of type B.
@output: true if for every element in lst1 and lst2 (pred1 and pred2 element) -> true.

@description: The function checks if every element of type A return true in pred1 function and every element of type B return true in pred2 function.
The function checks if the lst1 (lst2-same length) is null OR the first element in the lst1 AND the first element in the lst2
AND the rest elemnts in the lst1 returns true from pred1 function recursively
AND the rest elemnts in the lst2 returns true from pred2 function recursively
returns true from pred function recursively, then the function return true.
|#
(define (every2? pred1 pred2 lst1 lst2)
(or (null? lst1) ;; both lists assumed to be of same length
(and (pred1 (first lst1))
(pred2 (first lst2))
(every2? pred1 pred2 (rest lst1) (rest lst2)))))

(test (every2? all-even? all-even? null null))
(test (every2? is-odd? is-even? (list 1 3 5) (list 2 4 6)))
(test (every2? is-even? is-odd? (list 6 4 2) (list 1 3 5)))
(test (every2? is-even? is-even? (list 10 12 14) (list 2 4 6)))
(test (every2? is-odd? is-odd? (list 11 13 15) (list 1 3 5)))
 



  
