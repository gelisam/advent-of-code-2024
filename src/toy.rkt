#lang racket
(require rackunit)

(define-syntax (and-let* stx)
  (syntax-case stx ()
    [(_ () body ...)
     #`(begin body ...)]
    [(_ ([x1 e1] [x2 v2] ...) body ...)
     #`(let ([x1 e1])
         (and x1
              (and-let* ([x2 v2] ...) body ...)))]))

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
  (if (equal? line "")
      (list)
      (or (and-let* ([rest (string-drop-prefix? line "do()")])
            (parse-line #t rest))
          (and-let* ([rest (string-drop-prefix? line "don't()")])
            (parse-line #f rest))
          (and-let* ([rest (string-drop-prefix? line "mul(")]
                     [numeric-chars-and-rest (string-span char-numeric? rest)]
                     [numeric-chars (first numeric-chars-and-rest)]
                     [rest (second numeric-chars-and-rest)]
                     [_ (non-empty-string? numeric-chars)]
                     [x (string->number numeric-chars)]
                     [rest (string-drop-prefix? rest ",")]
                     [numeric-chars-and-rest (string-span char-numeric? rest)]
                     [numeric-chars (first numeric-chars-and-rest)]
                     [rest (second numeric-chars-and-rest)]
                     [_ (non-empty-string? numeric-chars)]
                     [y (string->number numeric-chars)]
                     [rest (string-drop-prefix? rest ")")])
            (if enabled?
                (cons (list x y) (parse-line enabled? rest))
                (parse-line enabled? rest)))
          (parse-line enabled? (string-drop line 1)))))
(check-equal?
  (parse-line #t "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")
  (list (list 2 4) (list 8 5)))

(define output
  (for/sum ([pair (parse-line #t input)])
    (apply * pair)))
output