#|
Implementation of a Deterministic Finite Automaton (DFA)

A function will receive the definition of a DFA and a string,
and return whether the string belongs in the language

The DFA in this file is used to identify valid arithmetic expressions

Gabriel Cordova   
2023-04-21
|#

#lang racket

(provide arithmetic-lexer)

; Declare the structure that describes a DFA
(struct dfa (initial accept func))

(define (arithmetic-lexer strng)
  " Call the function to validate using a specific DFA "
  (evaluate-dfa (dfa 'start '(int var float exp cls) delta-arithmetic) strng))

(define (evaluate-dfa dfa-to-evaluate strng)
   " This function will verify if a string is acceptable by a DFA "
   (let loop
      (; Get the initial state of the DFA
      [state (dfa-initial dfa-to-evaluate)]
      ; Convert the string into a list of characters
      [chars (string->list (trim-leading-spaces (trim-ending-spaces strng)))]
      ; The return list with all the tokens found
      [tokens '()]
      [result null])

      ; When the list of chars is over, check if the final state is acceptable
      (if (empty? chars) 
         (if (member state (dfa-accept dfa-to-evaluate))
           (reverse (cons (list(list->string (reverse tokens)) state) result)) #f)
         (let-values 
            ; Call the transition function and get the new state and whether or not a token was found
            ([(new-state state) ((dfa-func dfa-to-evaluate) state (car chars))])
            (loop
               state
               (cdr chars)
               ; The new list of tokens
               (if new-state (if (char-whitespace?( car chars))(empty-list tokens) (cons (car chars)(empty-list tokens))) (cons(car chars) tokens)) 
                     (if new-state (cons (list (list->string (reverse tokens)) new-state) result) result))))))


(define (empty-list lst)
  (if (not(empty? lst)) (begin
    (remove first lst)
    (empty-list (rest lst)))
    lst))

(define (trim-leading-spaces str)
  (let loop ((chars (string->list str)))
    (cond
      ((null? chars) "")
      ((char-whitespace? (car chars)) (loop (cdr chars)))
      (else (list->string chars)))))

(define (trim-ending-spaces str)
  (let loop ((chars (reverse (string->list str))))
    (cond
      ((null? chars) "")
      ((char-whitespace? (car chars)) (loop (cdr chars)))
      (else (list->string (reverse chars))))))

(define (char-operator? char)
  " Identify caracters that represent arithmetic operators "
  (member char '(#\+ #\- #\* #\^ #\/ #\=)))

(define (sign? char)
  " Identify caracters that represent arithmetic operators "
  (member char '(#\+ #\-)))

(define (delta-arithmetic state char)
  " Transition function to validate numbers
  Initial state: start
  Accept states: int float exp "
   (case state
      ['start (cond
         [(char-numeric? char) (values #f 'int)]
         [(sign? char) (values #f 'sign)]
         [(char-alphabetic? char) (values #f 'var)]
         [(eq? char #\_) (values #f 'var)]
         [(eq? char #\() (values #f 'opn)]
         [(char-whitespace? char) (values #f 'empty)]
         [else (values #f 'inv)])]
      ['sign (cond
         [(char-numeric? char) (values #f 'int)]
         [else (values #f 'inv)])]
      ['int (cond
         [(eq? char #\)) (values 'int 'cls)]
         [(char-numeric? char) (values #f 'int)]
         [(eq? char #\.) (values #f 'float)]
         [(or (eq? char #\e) (eq? char #\E)) (values 'exp)]
         [(char-operator? char) (values 'int 'op)]
         [(char-whitespace? char) (values 'int 'empty)]
         [else (values #f 'inv)])]
      ['dot (cond
         [(char-numeric? char) (values #f 'float)]
         [else (values #f 'inv)])]
      ['float (cond
         [(eq? char #\)) (values 'float 'cls)]
         [(char-numeric? char) (values #f 'float)]
         [(or (eq? char #\e) (eq? char #\E)) (values #f 'exp)]
         [(char-operator? char) (values 'float 'op)]
         [(char-whitespace? char) (values 'float 'empty)]
         [else (values #f 'inv)])]
      ['e (cond
         [(char-numeric? char) (values #f 'exp)]
         [(or (eq? char #\+) (eq? char #\-)) (values #f 'e_sign)]
         [else (values #f 'inv)])]
      ['e_sign (cond
         [(char-numeric? char) (values #f 'exp)]
         [else (values #f 'inv)])]
      ['exp (cond
         [(eq? char #\)) (values 'exp 'cls)]
         [(sign? char) (values #f 'exp)]
         [(char-numeric? char) (values #f 'exp)]
         [(char-operator? char) (values 'exp 'op)]
         [(char-whitespace? char) (values 'exp 'empty)]
         [else (values #f 'inv)])]
      ['var (cond
         [(eq? char #\)) (values 'var 'cls)]
         [(char-alphabetic? char) (values #f 'var)]
         [(char-numeric? char) (values #f 'var)]
         [(eq? char #\_) (values #f 'var)]
         [(char-operator? char) (values 'var 'op)]
         [(char-whitespace? char) (values 'var 'empty)]
         [else (values #f 'inv)])]
      ['op (cond
         [(char-numeric? char) (values 'op 'int)]
         [(sign? char) (values 'op 'sign)]
         [(char-alphabetic? char) (values 'op 'var)]
         [(eq? char #\_) (values 'op 'var)]
         [(char-whitespace? char) (values 'op 'op_spa)]
         [(eq? char #\() (values 'op 'opn)]
         [else (values 'inv #f)])]
      ['op_spa (cond
         [(char-numeric? char) (values #f 'int)]
         [(sign? char) (values #f 'int)]
         [(char-alphabetic? char) (values #f 'var)]
         [(eq? char #\_) (values #f 'var)]
         [(char-whitespace? char) (values #f 'op_spa)]
         [(eq? char #\() (values #f 'opn)]
         [(eq? char #\)) (values #f 'cls)]
         [else (values #f 'inv)])]
      ['opn (cond
         [(char-numeric? char) (values 'opn 'int)]
         [(char-alphabetic? char) (values #f 'var)]
         [(sign? char) (values 'opn 'sign)]
         [(eq? char #\_) (values #f 'var)]
         [(char-whitespace?) (values 'opn 'empty)]
         [(eq? char #\() (values #f 'opn)]
         [(eq? char #\)) (values 'opn 'cls)]
         [else (values #f 'inv)])]
      ['cls (cond
         [(char-operator? char) (values 'cls 'op)]
         [(eq? char #\space) (values #f 'empty)]
         [(eq? char #\() (values #f 'cls)]
         [(eq? char #\)) (values 'cls 'opn)]
         [(char-whitespace? char) (values 'cls 'empty)]
         [else (values #f 'inv)])]
      ['empty (cond
        [(eq? char #\() (values #f 'opn)]
        [(eq? char #\)) (values #f 'cls)]
        [(char-numeric? char) (values #f 'int)]
        [(or (char-alphabetic? char) (eq? char #\_)) (values #f 'var)]
        [(char-operator? char) (values #f 'op)]
        [(char-whitespace? char) (values #f 'empty)]
        [else (values #f 'inv)]
      )]
      ['inv (values #f 'inv)]
    ))
