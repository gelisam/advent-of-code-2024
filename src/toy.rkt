#lang racket
(require rackunit)

; use the example input and run the asserts against it
(define input (file->lines "example-input.txt"))
(check-equal?
  input
  (list "7 6 4 2 1" "1 2 7 8 9" "9 7 6 2 1" "1 3 2 4 5" "8 6 4 4 1" "1 3 6 7 9"))

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
  (parse-line "7 6 4 2 1")
  (list 7 6 4 2 1))

(define reports
  (map parse-line input))

(define (safe-step? up? prev x)
  (let ([down? (not up?)]
        [diff (abs (- x prev))])
    (and (or (and up? (< prev x))
             (and down? (> prev x)))
         (>= diff 1)
         (<= diff 3))))
(check-equal? (safe-step? #t 0 0) #f)
(check-equal? (safe-step? #t 1 2) #t)
(check-equal? (safe-step? #t 1 4) #t)
(check-equal? (safe-step? #t 1 5) #f)
(check-equal? (safe-step? #f 0 0) #f)
(check-equal? (safe-step? #f 1 2) #f)
(check-equal? (safe-step? #f 4 1) #t)
(check-equal? (safe-step? #f 5 1) #f)

(define (safe? xs)
  (define n (length xs))
  (define memo (make-hash))
  (define (safe-from? dampener? up? prev i)
    (hash-ref! memo (list dampener? up? prev i)
      (λ ()
        (if (= i n)
            #t
            (let ([x (list-ref xs i)]
                  [next-i (+ i 1)])
              (or (and (safe-step? up? prev x)
                       (safe-from? dampener? up? x next-i))
                  (and dampener?
                       (safe-from? #f up? prev next-i))))))))
  (let ([x0 (list-ref xs 0)]
        [x1 (list-ref xs 1)]
        [x2 (list-ref xs 2)])
    (or (safe-from? #t (< x0 x1) x0 1)
        (safe-from? #f (< x0 x2) x0 2)
        (safe-from? #f (< x1 x2) x1 2))))
(check-equal? (safe? (list 7 6 4 2 1)) #t)
(check-equal? (safe? (list 1 2 7 8 9)) #f)
(check-equal? (safe? (list 9 7 6 2 1)) #f)
(check-equal? (safe? (list 1 3 2 4 5)) #t)
(check-equal? (safe? (list 8 6 4 4 1)) #t)
(check-equal? (safe? (list 1 3 6 7 9)) #t)

(length (filter safe? reports))