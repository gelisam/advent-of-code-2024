#lang racket
(require rackunit)

; use the example input and run the asserts against it
(define input (file->lines "example-input.txt"))
(check-equal?
  input
  (list "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"))

;; use the real input and disable the asserts
;(set! input (file->lines "full-input.txt"))
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

(define (parse-line line)
  (define (try-next-char)
    (parse-line (string-drop line 1)))
  (if (equal? line "")
      (list)
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
                                 (cons (list x y) (parse-line rest))]
                                [_ (try-next-char)]))
                            (try-next-char))])]
                    [_ (try-next-char)]))
                (try-next-char))])]
        [_ (try-next-char)])))
(check-equal?
  (parse-line "xxxmul(2,4)xxxmul(mul(2mul(2,mul(2,4mul(21,41)xxx")
  (list (list 2 4) (list 21 41)))

(define output
  (for/sum ([line input])
    (for/sum ([pair (parse-line line)])
      (apply * pair))))
output