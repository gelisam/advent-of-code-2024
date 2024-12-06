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
(define input (file->lines "example-input.txt"))
(check-equal?
  input
  (list "MMMSXXMASM"
        "MSAMXMSMSA"
        "AMXSXMAAMM"
        "MSAMASMSMX"
        "XMASAMXAMM"
        "XXAMMXXAMA"
        "SMSMSASXSS"
        "SAXAMASAAA"
        "MAMMMXMMMM"
        "MXMXAXMASX"))

;; use the real input and disable the asserts
;(set! input (file->lines "full-input.txt"))
;(define (check-equal? _x _y)
;  (void))
;(define (check-true _)
;  (void))

(define grid (make-hash))
(for ([y (in-naturals 0)]
      [row input])
  (for ([x (in-naturals 0)]
        [char (in-string row)])
    (hash-set! grid (list x y) char)))

(define dirs
  (for*/list ([dx (in-list '(1 0 -1))]
              [dy (in-list '(1 0 -1))]
              #:when (not (and (zero? dx)
                               (zero? dy))))
    (list dx dy)))

(define (add xy dxy)
  (match-define (list x y) xy)
  (match-define (list dx dy) dxy)
  (list (+ x dx) (+ y dy)))
(check-equal? (add '(1 2) '(3 4)) '(4 6))

(define (lookup-word xy0 dxy n)
  (define xy xy0)
  (define result
    (for/list ([i (in-range n)])
      (define c (hash-ref grid xy #f))
      (set! xy (add xy dxy))
      c))
  (and (andmap values result)
       result))
(check-equal? (lookup-word '(0 0) '(1 1) 3) '(#\M #\S #\X))

(for/sum ([y (in-naturals 0)]
          [row input])
  (for/sum ([x (in-naturals 0)]
            [char (in-string row)])
    (for/sum ([dir dirs])
      (or (and-let* ([word (lookup-word (list x y) dir 4)]
                     [_ (equal? word (list #\X #\M #\A #\S))])
            1)
          0))))