#lang typed/racket

(define-syntax (assert-equal stx)
  (syntax-case stx ()
    [(_ x y)
     #`(unless (equal? x y)
         (error 'assert
           (format "line ~a: expected\n~a\ngot\n~a"
             #,(syntax-line stx)
             y
             x)))]))

;;;;;;

(struct Unit () #:transparent)
(struct [A] Identity ([first : A]) #:transparent)
(struct [A B] Pair ([first : A] [second : B]) #:transparent)
(struct [A B C] Triple ([first : A] [second : B] [third : C]) #:transparent)

(define-type (Maybe A) (U Nothing (Just A)))
(struct Nothing () #:transparent)
(struct [A] Just ([value : A]) #:transparent)

(: pred->maybe
   (All [A] (-> (-> A Boolean)
                (-> A (Maybe A)))))
(define (pred->maybe pred)
  (lambda (a)
    (if (pred a)
        (Just a)
        (Nothing))))

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
(assert-equal
  ((inst split-at-first Char) ((inst matching-char Char)
                               (pred->maybe char-numeric?))
                              "abc123def")
  (Just (Triple "abc" #\1 "23def")))

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
(assert-equal
  ((inst split-at-first (Listof Char)) ((inst matching-chars Char)
                                        (pred->maybe char-numeric?))
                                        "abc123def")
  (Just (Triple "abc" (list #\1 #\2 #\3) "def")))

(: exact-string
   (All [A] (-> String A (StringParser A))))
(define (exact-string expected a)
  (lambda (f)
    (match (f (string-length expected))
      [(Pair actual x)
       (if (string=? actual expected)
           (Just (Pair a x))
           (Nothing))])))
(assert-equal
  ((inst split-at-first Unit) ((inst exact-string Unit) "-"
                                                        (Unit))
                              "abc-def-ghi")
  (Just (Triple "abc" (Unit) "def-ghi")))
(assert-equal
  ((inst split-at-last Unit) ((inst exact-string Unit) "-"
                                                       (Unit))
                             "abc-def-ghi")
  (Just (Triple "abc-def" (Unit) "ghi")))

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
(assert-equal
  ((inst split-at-first Unit) ((inst exact-char Unit) #\-
                                                      (Unit))
                              "abc-def-ghi")
  (Just (Triple "abc" (Unit) "def-ghi")))

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
(assert-equal
  ((inst split-at-first String) ((inst string-matcher-map (Listof Char) String)
                                 list->string
                                 ((inst matching-chars Char)
                                  (pred->maybe char-numeric?)))
                                "abc123def")
  (Just (Triple "abc" "123" "def")))

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
(assert-equal
  ((inst split-at-first Char) ((inst string-matcher-or Char)
                               (list ((inst exact-char Char) #\- #\-)
                                     ((inst exact-char Char) #\: #\:)))
                              "abc-123:def")
  (Just (Triple "abc" #\- "123:def")))
(assert-equal
  ((inst split-at-first Char) ((inst string-matcher-or Char)
                               (list ((inst exact-char Char) #\- #\-)
                                     ((inst exact-char Char) #\: #\:)))
                              "abc:123-def")
  (Just (Triple "abc" #\: "123-def")))


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