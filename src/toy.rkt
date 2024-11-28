#lang typed/racket

(require "basic-types.rkt")
(require "string-parser.rkt")

(: input (Listof String))
(define input (file->lines "example-input.txt"))

(: find-numeric-char (StringParser Char))
(define find-numeric-char
  (string-parser-or [Char]
    (list (exact-char [Char] #\1 #\1)
          (exact-char [Char] #\2 #\2)
          (exact-char [Char] #\3 #\3)
          (exact-char [Char] #\4 #\4)
          (exact-char [Char] #\5 #\5)
          (exact-char [Char] #\6 #\6)
          (exact-char [Char] #\7 #\7)
          (exact-char [Char] #\8 #\8)
          (exact-char [Char] #\9 #\9)
          (exact-string [Char] "one"   #\1)
          (exact-string [Char] "two"   #\2)
          (exact-string [Char] "three" #\3)
          (exact-string [Char] "four"  #\4)
          (exact-string [Char] "five"  #\5)
          (exact-string [Char] "six"   #\6)
          (exact-string [Char] "seven" #\7)
          (exact-string [Char] "eight" #\8)
          (exact-string [Char] "nine"  #\9))))

(: first-numeric-char (-> String Char))
(define (first-numeric-char str)
  (match (find-first [Char] find-numeric-char str)
    [(Just c) c]
    [(Nothing) (error 'first-numeric-char "no numeric char found")]))

(: last-numeric-char (-> String Char))
(define (last-numeric-char str)
  (match (find-last [Char] find-numeric-char str)
    [(Just c) c]
    [(Nothing) (error 'last-numeric-char "no numeric char found")]))

(: get-integer (-> String Integer))
(define (get-integer str)
  (: relevant-digits (Listof Char))
  (define relevant-digits
    (list (first-numeric-char str)
          (last-numeric-char str)))
  
  (define number
    (string->number
      (list->string
        relevant-digits)))
  
  (assert number exact-integer?))

(: sum (-> (Listof Integer) Integer))
(define (sum lst)
  (apply + lst))

(: output Integer)
(define output (sum (map get-integer input)))

(displayln output)