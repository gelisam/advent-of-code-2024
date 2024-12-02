#lang typed/racket

(require "basic-types.rkt")
(require "string-parser.rkt")

(: input (Listof String))
(define input (file->lines "example-input.txt"))

(: parse-line (-> String (Pair Integer Integer)))
(define (parse-line line)
  (match (split-at-first [String]
                         (matching-chars (lambda (c) (equal? c #\ )))
                         line)
    [(Just (Triple s1 _ s2))
     (let ([n1 (string->number s1)]
           [n2 (string->number s2)])
       (assert n1 exact-integer?)
       (assert n2 exact-integer?)
       (Pair n1 n2))]
    [(Nothing)
     (error 'parse-line "Failed to parse line ~s" line)]))

(: output (Listof (Pair Integer Integer)))
(define output
  (map parse-line input))

(displayln output)