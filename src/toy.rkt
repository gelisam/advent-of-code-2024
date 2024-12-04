#lang racket
(require rackunit)

; use the example input and run the asserts against it
(define input (file->lines "example-input.txt"))
(check-equal?
  input
  (list "7 6 4 2 1" "1 2 7 8 9" "9 7 6 2 1" "1 3 2 4 5" "8 6 4 4 1" "1 3 6 7 9"))

;; use the real input and disable the asserts
;(set! input (file->lines "full-input.txt"))
;(define (check-equal? _x _y)
;  (void))
;(define (check-true _)
;  (void))

(define (parse-line line)
  (let ([parts (string-split line)])
    (map string->number parts)))
(check-equal?
  (parse-line "7 6 4 2 1")
  (list 7 6 4 2 1))

(define reports
  (map parse-line input))

(define (increasing? xs)
  (cond
    [(null? xs)
     #t]
    [(null? (cdr xs))
     #t]
    [(<= (car xs) (cadr xs))
     (increasing? (cdr xs))]
    [else #f]))
(check-true (increasing? (list 1 2 7 8 9)))
(check-true (not (increasing? (list 1 2 8 7 9))))

(define (decreasing? xs)
  (increasing? (reverse xs)))
(check-true (decreasing? (list 9 7 6 2 1)))
(check-true (not (decreasing? (list 1 2 7 8 9))))

(define (differences xs)
  (apply map
         (lambda (x y)
           (abs (- x y)))
         (list (cdr xs)
               (drop-right xs 1))))
(check-equal?
  (differences (list 1 2 7 8 9))
  (list 1 5 1 1))

(define (safe? xs)
  (let ([diffs (differences xs)])
    (and (or (increasing? xs)
            (decreasing? xs))
         (>= (apply min diffs) 1)
         (<= (apply max diffs) 3))))
(check-equal?
  (map safe? reports)
  (list #t #f #f #f #f #t))

(length (filter safe? reports))