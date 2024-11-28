#lang typed/racket

(provide assert-equal)

(define-syntax (assert-equal stx)
  (syntax-case stx ()
    [(_ x y)
     #`(unless (equal? x y)
         (error 'assert
           (format "line ~a: expected\n~a\ngot\n~a"
             #,(syntax-line stx)
             y
             x)))]))