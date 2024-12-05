#lang racket
(require rackunit)

; use the example input and run the asserts against it
(define input (file->string "example-input.txt"))
(check-equal?
  input
  "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))\n")

;; use the real input and disable the asserts
;(set! input (file->string "full-input.txt"))
;(define (check-equal? _x _y)
;  (void))
;(define (check-true _)
;  (void))

(define (string-drop s n)
  (substring s n))
(check-equal? (string-drop "abcdef" 3) "def")

(define (string-drop-prefix? s prefix)
  (and (string-prefix? s prefix)
       (string-drop s (string-length prefix))))
(check-equal? (string-drop-prefix? "abcdef" "abc") "def")
(check-equal? (string-drop-prefix? "abcdef" "def") #f)

(define (string-span pred s)
  (let loop ([i 0])
    (if (>= i (string-length s))
        (list s "")
        (if (pred (string-ref s i))
            (loop (+ i 1))
            (list (substring s 0 i) (substring s i))))))
(check-equal? (string-span char-alphabetic? "abc123") (list "abc" "123"))

(define (parse-line enabled? line)
  (define (try-next-char)
    (parse-line enabled? (string-drop line 1)))
  (if (equal? line "")
      (list)
      (match (string-drop-prefix? line "do()")
        [(? string? rest)
         (parse-line #t rest)]
        [_
         (match (string-drop-prefix? line "don't()")
           [(? string? rest)
            (parse-line #f rest)]
           [_
            (match (string-drop-prefix? line "mul(")
              [(? string? rest)
               (match (string-span char-numeric? rest)
                 [(list numeric-chars rest)
                  (if (non-empty-string? numeric-chars)
                      (let ([x (string->number numeric-chars)])
                        (match (string-drop-prefix? rest ",")
                          [(? string? rest)
                           (match (string-span char-numeric? rest)
                             [(list numeric-chars rest)
                              (if (non-empty-string? numeric-chars)
                                  (let ([y (string->number numeric-chars)])
                                    (match (string-drop-prefix? rest ")")
                                      [(? string? rest)
                                       (if enabled?
                                           (cons (list x y) (parse-line enabled? rest))
                                           (parse-line enabled? rest))]
                                      [_ (try-next-char)]))
                                  (try-next-char))])]
                          [_ (try-next-char)]))
                      (try-next-char))])]
              [_ (try-next-char)])])])))
(check-equal?
  (parse-line #t "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")
  (list (list 2 4) (list 8 5)))

(define output
  (for/sum ([pair (parse-line #t input)])
    (apply * pair)))
output