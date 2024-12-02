#lang racket
(require rackunit)

; use the example input and run the asserts against it
(define input (file->lines "example-input.txt"))
(check-equal?
  input
  (list "3   4" "4   3" "2   5" "1   3" "3   9" "3   3"))

; use the real input and disable the asserts
(set! input (file->lines "full-input.txt"))
(define (check-equal? _x _y)
  (void))

(define (parse-line line)
  (let ([parts (string-split line)])
    (map string->number parts)))
(check-equal?
  (parse-line "3   4")
  (list 3 4))

(define unsorted-pairs
  (map parse-line input))
(check-equal?
  unsorted-pairs
  (list (list 3 4)
        (list 4 3)
        (list 2 5)
        (list 1 3)
        (list 3 9)
        (list 3 3)))

(define unsorted-lists
  (apply map list unsorted-pairs))
(check-equal?
  unsorted-lists
  (list (list 3 4 2 1 3 3)
        (list 4 3 5 3 9 3)))

(define (sort-integers xs)
  (sort xs <=))
(check-equal?
  (sort-integers (list 3 4 2 1 3 3))
  (list 1 2 3 3 3 4))

(define sorted-lists
  (map sort-integers unsorted-lists))
(check-equal?
  sorted-lists
  (list (list 1 2 3 3 3 4)
        (list 3 3 3 4 5 9)))

(define sorted-pairs
  (map sort-integers
       (apply map list sorted-lists)))
(check-equal?
  sorted-pairs
  (list (list 1 3)
        (list 2 3)
        (list 3 3)
        (list 3 4)
        (list 3 5)
        (list 4 9)))

(define differences
  (map (lambda (pair)
         (- (second pair) (first pair)))
       sorted-pairs))
(check-equal?
  differences
  (list 2 1 0 1 2 5))

(define sum-of-differences
  (apply + differences))
(check-equal?
  sum-of-differences
  11)

sum-of-differences