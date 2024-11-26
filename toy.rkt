#lang typed/racket

(struct Unit ())
(struct [A] Identity ([first : A]))
(struct [A B] Pair ([first : A] [second : B]))
(struct [A B C] Triple ([first : A] [second : B] [third : C]))

(define-type (Maybe A) (U Nothing (Just A)))
(struct Nothing ())
(struct [A] Just ([value : A]))

;;;;;;

(define-type (StringParser A)
  (All [X] (-> (-> Integer (Pair String X))
               (Maybe (Pair A X)))))

(: split-at-first
   (All [A] (-> (StringParser A)
                String
                (Maybe (Triple String A String)))))
(define (split-at-first parser str)
  (let ([n (string-length str)])
    (let loop ([start 0])
      (: f (-> Integer (Pair String Integer)))
      (define (f len)
        (let* ([end (+ start len)]
               [middle (substring str start end)])
          (Pair middle end)))
      (match (parser f)
        [(Just (Pair a end))
         (let* ([prefix (substring str 0 start)]
                [suffix (substring str end)])
           (Just (Triple prefix a suffix)))]
        [(Nothing)
         (if (>= start n)
             (Nothing)
             (loop (+ start 1)))]))))

(: split-at-last
   (All [A] (-> (StringParser A)
                String
                (Maybe (Triple String A String)))))
(define (split-at-last parser str)
  (let ([n (string-length str)])
    (let loop ([end n])
      (: f (-> Integer (Pair String Integer)))
      (define (f len)
        (let* ([start (- end len)]
               [middle (substring str start end)])
          (Pair middle start)))
      (match (parser f)
        [(Just (Pair a start))
         (let* ([prefix (substring str 0 start)]
                [suffix (substring str end)])
           (Just (Triple prefix a suffix)))]
        [(Nothing)
         (if (<= end 0)
             (Nothing)
             (loop (- end 1)))]))))

(: find-first
   (All [A] (-> (StringParser A)
                String
                (Maybe A))))
(define (find-first (parser : (StringParser A)) str)
  (match ((inst split-at-first A) parser str)
    [(Just (Triple _ a _))
     (Just a)]
    [(Nothing)
     (Nothing)]))

(: find-last
   (All [A] (-> (StringParser A)
                String
                (Maybe A))))
(define (find-last parser str)
  (match ((inst split-at-last A) parser str)
    [(Just (Triple _ a _))
     (Just a)]
    [(Nothing)
     (Nothing)]))

(: matching-char
   (All [A] (-> (-> Char (Maybe A))
                (StringParser A))))
(define (matching-char pred)
  (lambda (f)
    (match (f 1)
      [(Pair str x)
       (let [(actual (string-ref str 0))]
         (match (pred actual)
           [(Just a)
            (Just (Pair a x))]
           [(Nothing)
            (Nothing)]))])))

(: matching-chars
   (All [A] (-> (-> Char (Maybe A))
                (StringParser (Listof A)))))
(define (matching-chars pred)
  (lambda #:forall [X] ((f : (-> Integer (Pair String X))))
    (let loop ([reversed-as : (Listof A)
                (list)]
               [n 0]
               [r : (-> (Maybe (Pair (Listof A) X)))
                (lambda () (Nothing))])
      (let ([next-n (+ n 1)])
        (match (f (+ n 1))
          [(Pair str x)
           (let [(char (string-ref str n))]
             (match (pred char)
               [(Just next-a)
                (let ([next-reversed-as
                       (cons next-a reversed-as)])
                  (loop next-reversed-as
                        next-n
                        (lambda ()
                          (Just (Pair (reverse next-reversed-as)
                                      x)))))]
               [Nothing
                (r)]))])))))

(: exact-string
   (All [A] (-> String A (StringParser A))))
(define (exact-string expected a)
  (lambda (f)
    (match (f (string-length expected))
      [(Pair actual x)
       (if (string=? actual expected)
           (Just (Pair a x))
           (Nothing))])))

(: exact-char
   (All [A] (-> Char A (StringParser A))))
(define (exact-char expected a)
  (let ([pred : (-> Char (Maybe A))
        (lambda (actual)
          (if (char=? actual expected)
              (Just a)
              (Nothing)))])
    (lambda (f)
      ((matching-char pred) f))))

(: string-matcher-map
   (All [A B] (-> (-> A B)
                  (StringParser A)
                  (StringParser B))))
(define (string-matcher-map a2b parser)
  (lambda (f)
    (match (parser f)
      [(Just (Pair a x))
       (Just (Pair (a2b a) x))]
      [(Nothing)
       (Nothing)])))

(: match-no-strings
   (All [A] (-> (StringParser A))))
(define (match-no-strings)
  (lambda (f)
    (Nothing)))

(: string-matcher-or
   (All [A] (-> (Listof (StringParser A))
                (StringParser A))))
(define string-matcher-or
  (lambda (matchers)
    (lambda (f)
      (let loop ([matchers matchers])
        (match matchers
          [(list)
           (Nothing)]
          [(cons matcher rest)
           (match (matcher f)
             [(Just result)
              (Just result)]
             [(Nothing)
              (loop rest)])])))))


;;;;;;;

(: input (Listof String))
(define input (file->lines "example-input.txt"))

(: string-first-char (-> String Char))
(define (string-first-char str)
  (string-ref str 0))

(: string-last-char (-> String Char))
(define (string-last-char str)
  (string-ref str (sub1 (string-length str))))

(: string-drop-from-beginning (-> String Integer String))
(define (string-drop-from-beginning str n)
  (substring str n))

(: string-drop-from-end (-> String Integer String))
(define (string-drop-from-end str n)
  (substring str 0 (- (string-length str) n)))

(: string-drop-prefix (-> String String String))
(define (string-drop-prefix str prefix)
  (string-drop-from-beginning str (string-length prefix)))

(: string-drop-suffix (-> String String String))
(define (string-drop-suffix str suffix)
  (string-drop-from-end str (string-length suffix)))

(define-syntax (find-numeric-char stx)
  (syntax-case stx ()
    [(_ recur str next-equal? current-char drop-current-char)
     (with-syntax ([(cases ...)
                    (for/list ([num (in-list '("one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))]
                               [char (in-list '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))])
                      #`[(next-equal? str #,num) #,char])])
       #'(cond
           cases ...
           [(char-numeric? current-char) current-char]
           [else (recur (drop-current-char str 1))]))]))

(: first-numeric-char (-> String Char))
(define (first-numeric-char str)
  (find-numeric-char
    first-numeric-char
    str
    string-prefix?
    (string-first-char str)
    string-drop-from-beginning))

(: last-numeric-char (-> String Char))
(define (last-numeric-char str)
  (find-numeric-char
    last-numeric-char
    str
    string-suffix?
    (string-last-char str)
    string-drop-from-end))

(: get-integer (-> String Integer))
(define (get-integer str)
  (: relevant-digits (Listof Char))
  (define relevant-digits
    (list (first-numeric-char str)
          (last-numeric-char str)))
  
  (define number
    (string->number
      (list->string
        relevant-digits)))
  
  (assert number exact-integer?))

(: sum (-> (Listof Integer) Integer))
(define (sum lst)
  (apply + lst))

(: output Integer)
(define output (sum (map get-integer input)))

(displayln output)