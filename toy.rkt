#lang typed/racket

(: input (Listof String))
(define input (file->lines "full-input.txt"))

(: string-first-char (-> String Char))
(define (string-first-char str)
  (string-ref str 0))

(: string-last-char (-> String Char))
(define (string-last-char str)
  (string-ref str (sub1 (string-length str))))

(: string-drop-from-beginning (-> String Integer String))
(define (string-drop-from-beginning str n)
  (substring str n))

(: string-drop-from-end (-> String Integer String))
(define (string-drop-from-end str n)
  (substring str 0 (- (string-length str) n)))

(: string-drop-prefix (-> String String String))
(define (string-drop-prefix str prefix)
  (string-drop-from-beginning str (string-length prefix)))

(: string-drop-suffix (-> String String String))
(define (string-drop-suffix str suffix)
  (string-drop-from-end str (string-length suffix)))

(: first-numeric-char (-> String Char))
(define (first-numeric-char str)
  (cond
    [(string-prefix? str "one") #\1]
    [(string-prefix? str "two") #\2]
    [(string-prefix? str "three") #\3]
    [(string-prefix? str "four") #\4]
    [(string-prefix? str "five") #\5]
    [(string-prefix? str "six") #\6]
    [(string-prefix? str "seven") #\7]
    [(string-prefix? str "eight") #\8]
    [(string-prefix? str "nine") #\9]
    [(char-numeric? (string-first-char str))
     (string-first-char str)]
    [else
     (first-numeric-char
       (string-drop-from-beginning str 1))]))

(: last-numeric-char (-> String Char))
(define (last-numeric-char str)
  (cond
    [(string-suffix? str "one") #\1]
    [(string-suffix? str "two") #\2]
    [(string-suffix? str "three") #\3]
    [(string-suffix? str "four") #\4]
    [(string-suffix? str "five") #\5]
    [(string-suffix? str "six") #\6]
    [(string-suffix? str "seven") #\7]
    [(string-suffix? str "eight") #\8]
    [(string-suffix? str "nine") #\9]
    [(char-numeric? (string-last-char str))
     (string-last-char str)]
    [else
     (last-numeric-char
       (string-drop-from-end str 1))]))

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