#lang typed/racket

(require "basic-types.rkt")
(require "string-parser.rkt")

(: input (Listof String))
(define input (file->lines "example-input.txt"))

(: find-numeric-char (StringParser Char))
(define find-numeric-char
  (string-parser-or [Char]
    (list (exact-char #\1)
          (exact-char #\2)
          (exact-char #\3)
          (exact-char #\4)
          (exact-char #\5)
          (exact-char #\6)
          (exact-char #\7)
          (exact-char #\8)
          (exact-char #\9)
          (string-parser-value [String Char] #\1 (exact-string "one"))
          (string-parser-value [String Char] #\2 (exact-string "two"))
          (string-parser-value [String Char] #\3 (exact-string "three"))
          (string-parser-value [String Char] #\4 (exact-string "four"))
          (string-parser-value [String Char] #\5 (exact-string "five"))
          (string-parser-value [String Char] #\6 (exact-string "six"))
          (string-parser-value [String Char] #\7 (exact-string "seven"))
          (string-parser-value [String Char] #\8 (exact-string "eight"))
          (string-parser-value [String Char] #\9 (exact-string "nine")))))

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