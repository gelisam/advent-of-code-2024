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

(define-syntax-rule (find-numeric-char
                      recur
                      str
                      next-equal?
                      current-char
                      drop-current-char)
  (cond
    [(next-equal? str "one") #\1]
    [(next-equal? str "two") #\2]
    [(next-equal? str "three") #\3]
    [(next-equal? str "four") #\4]
    [(next-equal? str "five") #\5]
    [(next-equal? str "six") #\6]
    [(next-equal? str "seven") #\7]
    [(next-equal? str "eight") #\8]
    [(next-equal? str "nine") #\9]
    [(char-numeric? current-char) current-char]
    [else (recur (drop-current-char str 1))]))

(: first-numeric-char (-> String Char))
(define (first-numeric-char str)
  (find-numeric-char
    first-numeric-char
    str
    string-prefix?
    (string-first-char str)
    string-drop-from-beginning))

(: last-numeric-char (-> String Char))
(define (last-numeric-char str)
  (find-numeric-char
    last-numeric-char
    str
    string-suffix?
    (string-last-char str)
    string-drop-from-end))

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