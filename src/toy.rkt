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
(define (check-true _)
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

(define keys
  (first unsorted-lists))
(define repeated-values
  (second unsorted-lists))

(define (hash-equal? h1 h2)
  (and (= (hash-count h1) (hash-count h2))
       (for/and ([(k v) (in-hash h1)])
         (and (hash-has-key? h2 k)
              (equal? v (hash-ref h2 k))))))

(define counts (make-hash))
(for ([value repeated-values])
  (hash-update! counts value add1 0))
(check-true
  (hash-equal?
    counts
    (hash 4 1
          3 3
          5 1
          9 1)))

(define similarity-score
  (for/sum ([key keys])
    (* key
       (hash-ref counts key 0))))
(check-equal?
  similarity-score
  31)

similarity-score