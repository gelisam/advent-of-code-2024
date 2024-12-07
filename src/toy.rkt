#lang racket
(require rackunit)

; use the example input and run the asserts against it
(define input (file->lines "example-input.txt"))
(check-equal?
  input
  (list "47|53"
        "97|13"
        "97|61"
        "97|47"
        "75|29"
        "61|13"
        "75|53"
        "29|13"
        "97|29"
        "53|29"
        "61|53"
        "97|53"
        "61|29"
        "47|13"
        "75|47"
        "97|75"
        "47|61"
        "75|61"
        "47|29"
        "75|13"
        "53|13"
        ""
        "75,47,61,53,29"
        "97,61,53,29,13"
        "75,29,13"
        "75,97,47,61,53"
        "61,13,29"
        "97,13,75,29,47"))

;; use the real input and disable the asserts
;(set! input (file->lines "full-input.txt"))
;(define (check-equal? _x _y)
;  (void))
;(define (check-true _)
;  (void))

(define (halve-by xs sep)
  (match/values (splitf-at xs (lambda (x) (not (equal? x sep))))
    [(prefix (cons _sep suffix))
     (list prefix suffix)]
    [(_ _)
     #f]))
(check-equal? (halve-by (list 1 2 3 4 3 2 1) 3)
              (list (list 1 2) (list 4 3 2 1)))
(check-equal? (halve-by (list 1 2 3) 10)
              #f)

(match-define (list rule-strings update-strings) (halve-by input ""))

(define rules
  (for/list ([rule-string rule-strings])
    (map string->number (string-split rule-string "|"))))
(check-equal? rules
              (list '(47 53)
                    '(97 13)
                    '(97 61)
                    '(97 47)
                    '(75 29)
                    '(61 13)
                    '(75 53)
                    '(29 13)
                    '(97 29)
                    '(53 29)
                    '(61 53)
                    '(97 53)
                    '(61 29)
                    '(47 13)
                    '(75 47)
                    '(97 75)
                    '(47 61)
                    '(75 61)
                    '(47 29)
                    '(75 13)
                    '(53 13)))

(define updates
  (for/list ([update-string update-strings])
    (map string->number (string-split update-string ","))))
(check-equal? updates
              (list '(75 47 61 53 29)
                    '(97 61 53 29 13)
                    '(75 29 13)
                    '(75 97 47 61 53)
                    '(61 13 29)
                    '(97 13 75 29 47)))

(define (valid? update)
  (define positions
    (make-immutable-hash
      (for/list ([i (in-naturals)]
                 [x update])
        (cons x i))))
  (for/and ([rule rules])
    (match-define (list x y) rule)
    (match* ((hash-ref positions x #f)
             (hash-ref positions y #f))
      [(#f _)
       #t]
      [(_ #f)
       #t]
      [(i j)
       (< i j)])))
(check-equal? (map valid? updates)
              '(#t #t #t #f #f #f))

(define (middle-page xs)
  (define n (length xs))
  (list-ref xs (quotient n 2)))
(check-equal? (middle-page (list 1 2 3 4 5))
              3)

(for/sum ([update updates] #:when (valid? update))
  (middle-page update))